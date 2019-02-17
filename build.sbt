import sbt.url

resolvers += Resolver.bintrayRepo("cakesolutions", "maven")
resolvers += "Local Maven" at Path.userHome.asFile.toURI.toURL + ".m2/repository"

// language features
scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps",
  "-deprecation"
)

enablePlugins(JavaAppPackaging)

//
// common settings
//
lazy val commonSettings = Seq(
  organization := "com.digitalcipher.spiked",
  version := "0.0.3-snapshot",
  scalaVersion := "2.12.7"
)

//
// dependencies
//
lazy val app = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "spikes-core",

    //
    // dependencies
    //
    libraryDependencies ++= Seq(
      // scala modules
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",

      // utilities for parsing and describing spikes networks
      "com.digitalcipher.spiked" % "spikes-utils_2.12" % "0.0.1-snapshot",

      // akka (2.5.9 previously)
      "com.typesafe.akka" %% "akka-actor" % "2.5.20",
      "com.typesafe.akka" %% "akka-slf4j" % "2.5.20",
      "com.typesafe.akka" %% "akka-stream" % "2.5.20",
      "com.typesafe.akka" %% "akka-remote" % "2.5.20",

      // kafka
      "net.cakesolutions" %% "scala-kafka-client" % "2.1.0",
      "net.cakesolutions" %% "scala-kafka-client-akka" % "2.1.0",

      // logging
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "ch.qos.logback" % "logback-core" % "1.2.3",

      // dimensions (provided by the util classes)
      "org.typelevel" %% "squants" % "1.4.0" % Provided,

      // json serialization/deserialization
      "io.spray" %%  "spray-json" % "1.3.5",

      // kyro serialization/deserialization
      "com.github.romix.akka" %% "akka-kryo-serialization" % "0.5.2",

      // testing
//      "org.scalactic" %% "scalactic" % "3.0.1",
      "com.typesafe.akka" %% "akka-testkit" % "2.5.16" % Test,
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    ),

//    unmanagedBase := baseDirectory.value,

//    unmanagedJars ++= Seq(
//      "../hashed-wheel-timer/build/libs/hashed-wheel-timer-0.0.1-SNAPSHOT.jar"
//    )
  )

// publishing to sonatype
ThisBuild / organization := "com.digitalcipher"
ThisBuild / organizationName := "digitalcipher"
ThisBuild / organizationHomepage := Some(url("https://github.com/robphilipp"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/robphilipp/spikes-core"),
    "scm:git:git://github.com/robphilipp/spikes-core"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "rob.philipp",
    name  = "Rob Philipp",
    email = "rob.philipp@gmail.com",
    url   = url("https://github.com/robphilipp")
  )
)

ThisBuild / description := "Core classes for building spikes networks"
ThisBuild / licenses := List("MIT" -> new URL("https://opensource.org/licenses/MIT"))
ThisBuild / homepage := Some(url("https://github.com/robphilipp/spikes-core"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true