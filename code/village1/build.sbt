name := "village1"

version := "0.1"

scalaVersion := "2.12.4"

offline := true

resolvers += "Oscar Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot-local/"
resolvers += "emueller-bintray" at "http://dl.bintray.com/emueller/maven"
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies += "oscar" %% "oscar-cp" % "4.1.0-SNAPSHOT" withSources()
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.0"
libraryDependencies += "com.eclipsesource"  %% "play-json-schema-validator" % "0.9.5-M4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2"



assemblyJarName in assembly := "village1.jar"
test in assembly := {}
assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")
