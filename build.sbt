name := "ModelSTP"

version := "0.1"

scalaVersion := "2.12.6"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-swing" % "2.10.5",
  "com.typesafe.akka" %% "akka-actor" % "2.5.12")