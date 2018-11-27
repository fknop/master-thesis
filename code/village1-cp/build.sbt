import sbt.librarymanagement.Resolver

name := "village1-cp"

version := "0.1"

scalaVersion := "2.12.4"

resolvers += "Oscar Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot-local/"

libraryDependencies += "oscar" %% "oscar-cp" % "4.1.0-SNAPSHOT" withSources()
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.0"