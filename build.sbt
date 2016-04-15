import NativePackagerKeys._

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.4" % "test",
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "org.ow2.asm" % "asm-debug-all" % "5.0.3",
  "com.google.guava" % "guava" % "18.0"
  //"com.chuusai" %% "shapeless" % "1.2.4"
  //"org.ow2.asm" % "asm" % "5.0.3",
  //"org.ow2.asm" % "asm-commons" % "5.0.3"
)

scalacOptions ++= Seq("-deprecation","-feature")

scalacOptions in Test ++= Seq("-Yrangepos")

packageArchetype.java_application

deploymentSettings

mainClass in Compile := Some("doge.compiler.Compiler")

name := "dogec"

organization := "com.jsuereth.doge"

version := "0.1"


val bintrayUrl = "https://api.bintray.com/content/jsuereth/doge/dogc"

val bintrayPattern = Patterns("[revision]/[module]-[revision].[ext]")

publishTo in Universal :=
  Some(Resolver.url("bintray-doge", url(bintrayUrl))(bintrayPattern))

resolvers ++= (publishTo in Universal).value.toSeq

mappings in Universal += ((baseDirectory.value / "README.md") -> "README.md")