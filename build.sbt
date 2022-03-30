enablePlugins(JavaAppPackaging)

organization := "org.monarchinitiative"

name := "boomer"

version := "0.1"

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

scalaVersion := "2.13.8"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

javaOptions += "-Xmx4G"

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

fork in Test := true

val zioVersion = "1.0.13"

libraryDependencies ++= {
  Seq(
    "org.geneontology"           %% "whelk-owlapi"        % "1.1.1",
    "dev.zio"                    %% "zio"                 % zioVersion,
    "org.phenoscape"             %% "scowl"               % "1.4.1",
    "net.sourceforge.owlapi"      % "owlapi-distribution" % "4.5.21",
    "org.geneontology"            % "obographs"           % "0.2.1",
    "commons-codec"               % "commons-codec"       % "1.15",
    "com.outr"                   %% "scribe-slf4j"        % "3.8.2",
    "io.circe"                   %% "circe-core"          % "0.13.0",
    "io.circe"                   %% "circe-generic"       % "0.13.0",
    "io.circe"                   %% "circe-yaml"          % "0.14.1",
    "com.github.alexarchambault" %% "case-app"            % "2.0.6",
    "org.scalaz"                 %% "scalaz-core"         % "7.3.6",
    "dev.zio"                    %% "zio-test"            % zioVersion % Test,
    "dev.zio"                    %% "zio-test-sbt"        % zioVersion % Test
  )
}
