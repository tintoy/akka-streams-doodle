lazy val root = (project in file("."))
	.settings(
		name := "scala-doodle",
		version := "1.0",
		scalaVersion := "2.11.7",
		libraryDependencies ++= Seq(
			"com.typesafe.akka" % "akka-actor_2.11" % "2.3.14"
		)
	)
