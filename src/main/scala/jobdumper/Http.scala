package jobdumper

import scala.util.{Failure, Success, Try}

/** Shared HTTP helpers: retry with exponential backoff, default User-Agent. */
object Http:
  val UserAgent  = "job-dumper/0.1 (+https://example.com)"
  val TimeoutMs  = 30_000
  val MaxRetries = 3

  def get(
      url: String,
      params: Map[String, String] = Map.empty,
      headers: Map[String, String] = Map.empty,
      auth: Option[(String, String)] = None
  ): String =
    val baseHeaders = Map("User-Agent" -> UserAgent, "Accept" -> "application/json") ++ headers
    val authVal     = auth.map((u, p) => requests.RequestAuth.Basic(u, p))
                          .getOrElse(requests.RequestAuth.Empty)
    retry(url) {
      val r = requests.get(
        url,
        params         = params,
        headers        = baseHeaders,
        auth           = authVal,
        readTimeout    = TimeoutMs,
        connectTimeout = TimeoutMs
      )
      r.text()
    }

  def getJson(
      url: String,
      params: Map[String, String] = Map.empty,
      headers: Map[String, String] = Map.empty,
      auth: Option[(String, String)] = None
  ): ujson.Value =
    ujson.read(get(url, params, headers, auth))

  /** Retries a thunk on any throwable up to MaxRetries with exponential backoff. */
  private def retry[A](label: String)(thunk: => A): A =
    var attempt = 0
    var last: Throwable = null
    while attempt < MaxRetries do
      Try(thunk) match
        case Success(v) => return v
        case Failure(e) =>
          last = e
          attempt += 1
          if attempt < MaxRetries then
            val backoff = (1L << attempt).min(10L) * 1000
            Thread.sleep(backoff)
    throw new RuntimeException(s"giving up after $MaxRetries attempts: $label", last)
