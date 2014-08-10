scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.4" % "test",
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "asm" % "asm-all" % "3.3.1"
)

scalacOptions in Test ++= Seq("-Yrangepos")

