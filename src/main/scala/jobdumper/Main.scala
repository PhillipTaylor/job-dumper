package jobdumper

import com.github.mjakubowski84.parquet4s.{ParquetWriter, Path as P4sPath}
import java.nio.file.{Files, Paths}

/** One row per posting, written verbatim. `data` holds the raw JSON of the
 *  posting; Spark can crack it open with `from_json(schema_of_json(...))`. */
final case class RawPostingRow(
    source: String,
    fetched_at: Long,
    data: String
)

/** Entry point.
 *
 *  Usage: `sbt "run [output.parquet]"`. The output path defaults to
 *  `jobs.parquet` (or the `OUTPUT_PATH` env var).
 *
 *  Set `SOURCES` (comma-separated) to limit which adapters run. Per-source
 *  credentials and tuning knobs are read from the same env vars as the
 *  reference Python project (see `~/jobscanner/.env.example`).
 */
@main def dump(args: String*): Unit =
  val outArg  = args.headOption.orElse(sys.env.get("OUTPUT_PATH")).getOrElse("jobs.parquet")
  val outPath = toAbsoluteFileUri(outArg)
  val now     = System.currentTimeMillis()

  System.err.println(s"job-dumper: writing to $outPath")
  val enabled = Sources.enabled
  System.err.println(s"job-dumper: enabled sources = ${enabled.map(_.name).mkString(",")}")

  val rows = enabled.iterator.flatMap { s =>
    s.safeFetch().iterator.map(p => RawPostingRow(p.source, now, ujson.write(p.data)))
  }.toList

  System.err.println(s"job-dumper: total rows = ${rows.size}")
  ParquetWriter.of[RawPostingRow].writeAndClose(P4sPath(outPath), rows)
  System.err.println(s"job-dumper: done -> $outPath")

/** Normalise a user-supplied path to an absolute `file://` URI for parquet4s. */
private def toAbsoluteFileUri(p: String): String =
  if p.contains("://") then p
  else
    val abs = Paths.get(p).toAbsolutePath.normalize
    Files.createDirectories(Option(abs.getParent).getOrElse(Paths.get(".")))
    abs.toUri.toString
