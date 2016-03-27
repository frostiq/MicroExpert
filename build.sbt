name := "MicroExpert"
scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.7.2" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
)

scalacOptions in Test ++= Seq("-Yrangepos")