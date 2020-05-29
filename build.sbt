enablePlugins(JavaAppPackaging)

organization := "org.monarchinitiative"

name := "boomer"

version := "0.1"

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

scalaVersion := "2.13.1"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

javaOptions += "-Xmx4G"

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

fork in Test := true

val zioVersion = "1.0.0-RC20"

libraryDependencies ++= {
  Seq(
    "org.geneontology" %% "whelk" % "0.5.1",
    "dev.zio" %% "zio" % zioVersion,
    "org.phenoscape" %% "scowl" % "1.3.4",
    "net.sourceforge.owlapi" % "owlapi-distribution" % "4.5.16",
    "commons-codec" % "commons-codec" % "1.14",
    "com.outr" %% "scribe-slf4j" % "2.7.12",
    "io.circe" %% "circe-yaml" % "0.13.1",
    "com.github.alexarchambault" %% "case-app" % "2.0.0-M16",
    "dev.zio" %% "zio-test" % zioVersion % Test,
    "dev.zio" %% "zio-test-sbt" % zioVersion % Test
  )
}
