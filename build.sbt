name := "game"

version := "1.0"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq("eval", "collection", "logging") map { name =>
    "com.twitter" % ("util-"+name) % "5.3.12"
}

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.0.M4",
    "junit" % "junit" % "4.10"
) map (_ % "test")

resolvers += "T repo" at "http://maven.twttr.com/"
