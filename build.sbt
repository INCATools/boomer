enablePlugins(JavaAppPackaging)

organization  := "org.monarchinitiative"

name          := "boomer"

version       := "0.1"

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

scalaVersion  := "2.13.1"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

javaOptions += "-Xmx4G"

testFrameworks += new TestFramework("utest.runner.Framework")

fork in Test := true

libraryDependencies ++= {
  Seq(
    "org.geneontology"       %% "whelk"               % "0.4",
    "dev.zio"                %% "zio"                 % "1.0.0-RC17",
    "net.sourceforge.owlapi" %  "owlapi-distribution" % "4.5.14",
    "commons-codec"          %  "commons-codec"       % "1.13"
  )
}
