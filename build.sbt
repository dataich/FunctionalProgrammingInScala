name := "FunctionalProgrammingInScala"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "2.2.6" % "test")

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")
    