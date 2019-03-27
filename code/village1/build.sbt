name := "village1"
//resolvers += "Oscar Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot-local/"
//resolvers += "emueller-bintray" at "http://dl.bintray.com/emueller/maven"
//resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

lazy val settings = Seq(
  resolvers ++= Seq(
    "Oscar Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot-local/",
    "emueller-bintray" at "http://dl.bintray.com/emueller/maven",
    "Artima Maven Repository" at "http://repo.artima.com/releases"
  ),
  scalaVersion := "2.12.4",
  version := "1.0"
)

lazy val dependencies =
  new {
    val oscar = "oscar" %% "oscar-cp" % "4.1.0-SNAPSHOT" withSources()
    val playJson = "com.typesafe.play" %% "play-json" % "2.6.0"
    val jsonValidator = "com.eclipsesource"  %% "play-json-schema-validator" % "0.9.5-M4"

    val scalactic = "org.scalactic" %% "scalactic" % "3.0.5"
    val scalatest = "org.scalatest" %% "scalatest" % "3.0.5" % "test"

    val scopt = "com.github.scopt" %% "scopt" % "4.0.0-RC2"
  }


lazy val global = project
  .in(file("."))
  .settings(settings)
  .aggregate(
    v1Solver,
    v1Benchmark
  )

lazy val v1Solver = project.in(file("v1-solver"))
  .settings(
    settings,
    name := "v1-solver",
    assemblySettings,
    libraryDependencies ++= Seq(
      dependencies.oscar.withSources(),
      dependencies.playJson,
      dependencies.jsonValidator,
      dependencies.scalactic,
      dependencies.scalatest,
      dependencies.scopt
    )
  )

lazy val v1Benchmark = project.in(file("v1-benchmark")).dependsOn(
  v1Solver
).settings(
  settings,
  assemblySettings,
  name := "v1-benchmark",
  libraryDependencies ++= Seq(
    dependencies.scopt,
    dependencies.playJson
  )
)

lazy val assemblySettings = Seq(
  assemblyJarName in assembly := name.value + ".jar",
  test in assembly := {},
  assemblyMergeStrategy in assembly := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case x => MergeStrategy.first
  }
)
//libraryDependencies += "oscar" %% "oscar-cp" % "4.1.0-SNAPSHOT" withSources()
//libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.0"
//libraryDependencies += "com.eclipsesource"  %% "play-json-schema-validator" % "0.9.5-M4"
//
//libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
//libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

//libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2"






