name := "tfpl"
version := "1.0"

scalaVersion := "2.12.7"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

resourceDirectory in Compile := baseDirectory.value / "data"

unmanagedResourceDirectories in Compile += baseDirectory.value / "docs"

autoAPIMappings := true
