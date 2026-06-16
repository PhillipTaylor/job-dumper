package jobdumper

import scala.util.control.NonFatal

/** A single raw posting plus the name of the source it came from. */
final case class RawPosting(source: String, data: ujson.Value)

/** Abstract source adapter. Implementations yield raw, unprocessed postings. */
trait Source:
  def name: String
  def fetch(): Iterator[RawPosting]

  /** Runs [[fetch]] swallowing exceptions; returns [] on failure. */
  def safeFetch(): List[RawPosting] =
    try
      val items = fetch().toList
      System.err.println(s"[$name] fetched=${items.size}")
      items
    catch
      case NonFatal(e) =>
        System.err.println(s"[$name] fetch failed: ${e.getClass.getSimpleName}: ${e.getMessage}")
        Nil

object Sources:
  /** Comma-separated env list helper. */
  private def envList(key: String, default: String): List[String] =
    sys.env.getOrElse(key, default)
      .split(",").iterator.map(_.trim).filter(_.nonEmpty).toList

  /** All built-in adapters in deterministic order. */
  val all: List[Source] = List(
    Remotive, Arbeitnow, Jobicy, Himalayas, RemoteOK,
    UsaJobs, Greenhouse, Lever, Ashby, Workable,
    Recruitee, HnWhoIsHiring, Rss, Reed, Adzuna
  )

  /** Honours the SOURCES env var (comma-separated); empty/unset = all. */
  def enabled: List[Source] =
    val requested = sys.env.getOrElse("SOURCES", "").trim
    if requested.isEmpty then all
    else
      val wanted = requested.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSet
      all.filter(s => wanted.contains(s.name))

  // ---------------------------------------------------------------------------
  // JSON sources requiring no credentials
  // ---------------------------------------------------------------------------

  object Remotive extends Source:
    val name = "remotive"
    def fetch() =
      Http.getJson("https://remotive.com/api/remote-jobs")("jobs").arr.iterator
        .map(j => RawPosting(name, j))

  object Arbeitnow extends Source:
    val name = "arbeitnow"
    def fetch() =
      var url: String  = "https://www.arbeitnow.com/api/job-board-api"
      var pages        = 0
      val out          = collection.mutable.ListBuffer.empty[RawPosting]
      while url != null && pages < 5 do
        val payload = Http.getJson(url)
        payload.obj.get("data").foreach(_.arr.foreach(j => out += RawPosting(name, j)))
        val next = payload.obj.get("links").flatMap(_.obj.get("next"))
        url = next match
          case Some(v) if v.strOpt.isDefined => v.str
          case _                              => null
        pages += 1
      out.iterator

  object Jobicy extends Source:
    val name = "jobicy"
    def fetch() =
      Http.getJson("https://jobicy.com/api/v2/remote-jobs", Map("count" -> "50"))
        .obj.get("jobs").map(_.arr.iterator.map(j => RawPosting(name, j))).getOrElse(Iterator.empty)

  object Himalayas extends Source:
    val name = "himalayas"
    def fetch() =
      val out    = collection.mutable.ListBuffer.empty[RawPosting]
      var offset = 0
      var page   = 0
      var keepGoing = true
      while keepGoing && page < 5 do
        val payload = Http.getJson(
          "https://himalayas.app/jobs/api",
          Map("limit" -> "100", "offset" -> offset.toString)
        )
        val jobs = payload.obj.get("jobs").orElse(payload.obj.get("data"))
          .map(_.arr.toList).getOrElse(Nil)
        if jobs.isEmpty then keepGoing = false
        else
          jobs.foreach(j => out += RawPosting(name, j))
          offset += jobs.size
          page   += 1
      out.iterator

  object RemoteOK extends Source:
    val name = "remoteok"
    def fetch() =
      // First element is usually a legal/meta entry without an "id".
      Http.getJson("https://remoteok.com/api").arr.iterator
        .filter(j => j.objOpt.exists(o => o.contains("id")))
        .map(j => RawPosting(name, j))

  // ---------------------------------------------------------------------------
  // Per-company / per-board ATS feeds
  // ---------------------------------------------------------------------------

  object Greenhouse extends Source:
    val name = "greenhouse"
    def fetch() =
      envList("GREENHOUSE_BOARDS", "airbnb,stripe,gitlab,figma,airtable").iterator.flatMap { board =>
        try
          val payload = Http.getJson(
            s"https://boards-api.greenhouse.io/v1/boards/$board/jobs",
            Map("content" -> "true")
          )
          payload("jobs").arr.iterator.map { j =>
            j.obj("_board") = board
            RawPosting(name, j)
          }
        catch case NonFatal(_) => Iterator.empty
      }

  object Lever extends Source:
    val name = "lever"
    def fetch() =
      envList("LEVER_COMPANIES", "netflix,palantir,brex,plaid,benchling").iterator.flatMap { company =>
        try
          Http.getJson(s"https://api.lever.co/v0/postings/$company", Map("mode" -> "json"))
            .arr.iterator.map { j =>
              j.obj("_company") = company
              RawPosting(name, j)
            }
        catch case NonFatal(_) => Iterator.empty
      }

  object Ashby extends Source:
    val name = "ashby"
    def fetch() =
      envList("ASHBY_BOARDS", "Ashby,Posthog,Linear,Vercel").iterator.flatMap { board =>
        try
          Http.getJson(
            s"https://api.ashbyhq.com/posting-api/job-board/$board",
            Map("includeCompensation" -> "true")
          )("jobs").arr.iterator.map { j =>
            j.obj("_board") = board
            RawPosting(name, j)
          }
        catch case NonFatal(_) => Iterator.empty
      }

  object Workable extends Source:
    val name = "workable"
    def fetch() =
      envList("WORKABLE_ACCOUNTS", "workable,thoughtworks").iterator.flatMap { acct =>
        try
          Http.getJson(s"https://apply.workable.com/api/v3/accounts/$acct/jobs")("results")
            .arr.iterator.map { j =>
              j.obj("_account") = acct
              RawPosting(name, j)
            }
        catch case NonFatal(_) => Iterator.empty
      }

  object Recruitee extends Source:
    val name = "recruitee"
    def fetch() =
      envList("RECRUITEE_COMPANIES", "recruitee").iterator.flatMap { company =>
        try
          Http.getJson(s"https://$company.recruitee.com/api/offers/")("offers")
            .arr.iterator.map { j =>
              j.obj("_company") = company
              RawPosting(name, j)
            }
        catch case NonFatal(_) => Iterator.empty
      }

  // ---------------------------------------------------------------------------
  // Hacker News "Who is hiring" via Algolia
  // ---------------------------------------------------------------------------

  object HnWhoIsHiring extends Source:
    val name = "hn_whoishiring"
    private val SearchUrl = "https://hn.algolia.com/api/v1/search"
    def fetch() =
      val stories = Http.getJson(
        SearchUrl,
        Map(
          "tags"        -> "story,author_whoishiring",
          "query"       -> "Ask HN: Who is hiring",
          "hitsPerPage" -> "2"
        )
      )("hits").arr
      if stories.isEmpty then Iterator.empty
      else
        val story = stories.head
        story.obj.get("objectID").flatMap(_.numOpt).map(_.toLong.toString) match
          case None => Iterator.empty
          case Some(storyId) =>
            val comments = Http.getJson(
              SearchUrl,
              Map("tags" -> s"comment,story_$storyId", "hitsPerPage" -> "200")
            )("hits").arr
            comments.iterator.flatMap { c =>
              val text = c.obj.get("comment_text").flatMap(_.strOpt).getOrElse("")
              if text.trim.isEmpty then Iterator.empty
              else
                val firstLine = text.split("<p>|\\n", 2).head.replaceAll("<[^>]+>", "").take(200)
                c.obj("_first_line") = firstLine
                c.obj("_story_id")   = storyId
                Iterator.single(RawPosting(name, c))
            }

  // ---------------------------------------------------------------------------
  // Credentialed sources (no-op if env vars unset)
  // ---------------------------------------------------------------------------

  object UsaJobs extends Source:
    val name = "usajobs"
    def fetch() =
      val key = sys.env.getOrElse("USAJOBS_API_KEY", "").trim
      if key.isEmpty then Iterator.empty
      else
        val ua = sys.env.getOrElse("USAJOBS_EMAIL", "anon@example.com")
        val payload = Http.getJson(
          "https://data.usajobs.gov/api/search",
          Map("ResultsPerPage" -> "100"),
          headers = Map(
            "Host"              -> "data.usajobs.gov",
            "User-Agent"        -> ua,
            "Authorization-Key" -> key
          )
        )
        payload.obj.get("SearchResult").flatMap(_.obj.get("SearchResultItems"))
          .map(_.arr.iterator.map(j => RawPosting(name, j))).getOrElse(Iterator.empty)

  object Reed extends Source:
    val name = "reed"
    private val DefaultKeywords = List(
      "scala", "sc cleared", "dv cleared", "security cleared",
      "contract scala", "contract python", "contract java"
    )
    def fetch() =
      val key = sys.env.getOrElse("REED_API_KEY", "").trim
      if key.isEmpty then Iterator.empty
      else
        val keywords =
          val csv = sys.env.getOrElse("REED_KEYWORDS", "").trim
          if csv.isEmpty then DefaultKeywords
          else csv.split(",").iterator.map(_.trim).filter(_.nonEmpty).toList
        val location = sys.env.getOrElse("REED_LOCATION", "").trim
        val perKw    = sys.env.getOrElse("REED_RESULTS_PER_KEYWORD", "100")
        val seen     = collection.mutable.HashSet.empty[String]
        keywords.iterator.flatMap { kw =>
          try
            val params = Map("keywords" -> kw, "resultsToTake" -> perKw) ++
                         (if location.nonEmpty then Map("locationName" -> location) else Map.empty)
            Http.getJson(
              "https://www.reed.co.uk/api/1.0/search",
              params,
              auth = Some((key, ""))
            )("results").arr.iterator.flatMap { j =>
              val jid = j.obj.get("jobId").flatMap(_.numOpt).map(_.toLong.toString).getOrElse("")
              if jid.nonEmpty && !seen.add(jid) then Iterator.empty
              else
                j.obj("_keyword") = kw
                Iterator.single(RawPosting(name, j))
            }
          catch case NonFatal(_) => Iterator.empty
        }

  object Adzuna extends Source:
    val name = "adzuna"
    private val DefaultKeywords = List(
      "scala", "sc cleared", "dv cleared",
      "scala contract", "python contract", "java contract", "kafka contract"
    )
    def fetch() =
      val appId  = sys.env.getOrElse("ADZUNA_APP_ID", "").trim
      val appKey = sys.env.getOrElse("ADZUNA_APP_KEY", "").trim
      if appId.isEmpty || appKey.isEmpty then Iterator.empty
      else
        val country  = sys.env.getOrElse("ADZUNA_COUNTRY", "gb").trim.toLowerCase
        val keywords =
          val csv = sys.env.getOrElse("ADZUNA_KEYWORDS", "").trim
          if csv.isEmpty then DefaultKeywords
          else csv.split(",").iterator.map(_.trim).filter(_.nonEmpty).toList
        val pages   = sys.env.getOrElse("ADZUNA_PAGES_PER_KEYWORD", "1").toInt
        val perPage = sys.env.getOrElse("ADZUNA_RESULTS_PER_PAGE", "50")
        val seen    = collection.mutable.HashSet.empty[String]
        keywords.iterator.flatMap { kw =>
          (1 to pages).iterator.flatMap { page =>
            try
              Http.getJson(
                s"https://api.adzuna.com/v1/api/jobs/$country/search/$page",
                Map(
                  "app_id"           -> appId,
                  "app_key"          -> appKey,
                  "what"             -> kw,
                  "results_per_page" -> perPage,
                  "content-type"     -> "application/json"
                )
              )("results").arr.iterator.flatMap { j =>
                val jid = j.obj.get("id").flatMap(_.numOpt).map(_.toLong.toString).getOrElse("")
                if jid.nonEmpty && !seen.add(jid) then Iterator.empty
                else
                  j.obj("_keyword") = kw
                  j.obj("_country") = country.toUpperCase
                  Iterator.single(RawPosting(name, j))
              }
            catch case NonFatal(_) => Iterator.empty
          }
        }

  // ---------------------------------------------------------------------------
  // RSS / Atom — converted to a small JSON object per entry
  // ---------------------------------------------------------------------------

  object Rss extends Source:
    val name = "rss"
    private val DefaultFeeds = List(
      "https://weworkremotely.com/categories/remote-programming-jobs.rss",
      "https://weworkremotely.com/categories/remote-devops-sysadmin-jobs.rss",
      "https://himalayas.app/jobs/rss",
      "https://jobicy.com/?feed=job_feed",
      "https://remoteok.com/remote-jobs.rss",
      "https://www.python.org/jobs/feed/rss/"
    )
    private val UkFeeds = List("https://devitjobs.uk/rss")

    def fetch() =
      val extra   = envList("RSS_FEEDS", "")
      val ukExtra = envList("UK_RSS_FEEDS", "")
      val feeds   = (DefaultFeeds ++ extra).map(_ -> "") ++ (UkFeeds ++ ukExtra).map(_ -> "GB")
      feeds.iterator.flatMap { (url, country) =>
        try fetchFeed(url, country)
        catch case NonFatal(_) => Iterator.empty
      }

    private def fetchFeed(url: String, country: String): Iterator[RawPosting] =
      val body = requests.get(
        url,
        headers = Map(
          "User-Agent" -> Http.UserAgent,
          "Accept"     -> "application/rss+xml, application/atom+xml, application/xml;q=0.9, */*;q=0.8"
        ),
        readTimeout    = 10_000,
        connectTimeout = 10_000
      ).text()
      val xml         = scala.xml.XML.loadString(body)
      val feedTitle   = (xml \\ "channel" \ "title").headOption.map(_.text)
                          .orElse((xml \ "title").headOption.map(_.text)).getOrElse("")
      // RSS uses <item>, Atom uses <entry>
      val entries = (xml \\ "item") ++ (xml \\ "entry")
      entries.iterator.map { e =>
        val title     = (e \ "title").headOption.map(_.text).getOrElse("")
        val link      = (e \ "link").headOption.map { n =>
                          val href = n.attribute("href").map(_.text).getOrElse("")
                          if href.nonEmpty then href else n.text
                        }.getOrElse("")
        val id        = (e \ "id").headOption.map(_.text)
                          .orElse((e \ "guid").headOption.map(_.text)).getOrElse(link)
        val summary   = (e \ "summary").headOption.map(_.text)
                          .orElse((e \ "description").headOption.map(_.text)).getOrElse("")
        val author    = (e \ "author").headOption.map(_.text).getOrElse("")
        val published = (e \ "published").headOption.map(_.text)
                          .orElse((e \ "pubDate").headOption.map(_.text))
                          .orElse((e \ "updated").headOption.map(_.text)).getOrElse("")
        val tags      = (e \ "category").map(_.text).toList
        val obj = ujson.Obj(
          "_feed_url"   -> url,
          "_feed_title" -> feedTitle,
          "_country"    -> country,
          "title"       -> title,
          "link"        -> link,
          "id"          -> id,
          "summary"     -> summary,
          "author"      -> author,
          "published"   -> published,
          "tags"        -> ujson.Arr.from(tags.map(ujson.Str(_)))
        )
        RawPosting(name, obj)
      }
