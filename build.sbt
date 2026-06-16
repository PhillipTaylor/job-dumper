ThisBuild / scalaVersion := "3.3.6"
ThisBuild / organization := "jobdumper"
ThisBuild / version      := "0.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "job-dumper",
    libraryDependencies ++= Seq(
      "com.lihaoyi"                  %% "requests"        % "0.9.3",
      "com.lihaoyi"                  %% "ujson"           % "4.0.2",
      "com.github.mjakubowski84"     %% "parquet4s-core"  % "2.23.0",
      "org.apache.hadoop"             % "hadoop-client"   % "3.4.1",
      "org.scala-lang.modules"       %% "scala-xml"       % "2.3.0",
      "org.slf4j"                     % "slf4j-simple"    % "2.0.13"
    ),
    Compile / scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:imports"),
    run / fork := true,
    // Hadoop 3.4.1 (pulled in by parquet4s) is incompatible with JDK 24+ because
    // `Subject.getSubject(AccessControlContext)` throws unconditionally
    // (see HADOOP-19212). Until Hadoop 3.4.2 ships, run on JDK 17 / 21 / 23.
    // `-Djava.security.manager=allow` is required on JDK 23.
    // `--add-opens` silences the requests-scala HttpClient thread-leak warning on JDK <21.
    run / javaOptions ++= Seq(
      "-Dorg.slf4j.simpleLogger.defaultLogLevel=warn",
      "-Djava.security.manager=allow",
      "--add-opens=java.net.http/jdk.internal.net.http=ALL-UNNAMED"
    ),
    assembly / mainClass       := Some("jobdumper.dump"),
    assembly / assemblyJarName := s"${name.value}-${version.value}.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "services", _ @ _*)                                 => MergeStrategy.concat
      case PathList("META-INF", xs @ _*) if xs.lastOption.exists(_.endsWith(".SF"))  => MergeStrategy.discard
      case PathList("META-INF", xs @ _*) if xs.lastOption.exists(_.endsWith(".DSA")) => MergeStrategy.discard
      case PathList("META-INF", xs @ _*) if xs.lastOption.exists(_.endsWith(".RSA")) => MergeStrategy.discard
      case "module-info.class"                                                      => MergeStrategy.discard
      case PathList("META-INF", "versions", _, "module-info.class")                 => MergeStrategy.discard
      case "reference.conf" | "application.conf"                                    => MergeStrategy.concat
      case _                                                                        => MergeStrategy.first
    }
  )
