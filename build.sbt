name := "sparse"

organization := "org.gnieh"

lazy val root = project.in(file(".")).aggregate(core)

lazy val core = project.in(file("core")).settings(
  scalaVersion := "2.11.6",
  scalacOptions ++= Seq("-deprecation", "-feature"),
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies += "com.chuusai" %% "shapeless" % "2.0.0")
