organization  := "com.example"

version       := "0.1"

scalaVersion  := "2.11.2"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  val akkaV = "2.3.6"
  val sprayV = "1.3.2"
  Seq(
    "io.spray"            %%  "spray-can"     % sprayV,
    "io.spray"            %%  "spray-routing" % sprayV,
    "io.spray"            %%  "spray-testkit" % sprayV  % "test",
    "com.typesafe.akka"   %%  "akka-cluster"  % akkaV,
    "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
    "com.typesafe.akka"   %%  "akka-testkit"  % akkaV   % "test",
    "org.specs2"          %%  "specs2-core"   % "2.3.11" % "test",
    "com.orientechnologies" % "orient-commons" % "1.7.10",
    "com.orientechnologies" % "orientdb-core" % "1.7.10",
    "com.orientechnologies" % "orientdb-client" % "1.7.10",
    "com.orientechnologies" % "orientdb-enterprise" % "1.7.10",
    "com.google.guava" % "guava" % "18.0",
    "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
    "io.spray" %%  "spray-json" % "1.3.1",
    "org.apache.commons" % "commons-lang3" % "3.1",
    "commons-io" % "commons-io" % "2.4",
    "net.ruippeixotog" %% "scala-scraper" % "0.1.1"
  )
}

Revolver.settings