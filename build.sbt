lazy val defaultSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8", // Specify character encoding used by source files.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds", // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    // "-Xfatal-warnings", // Fail the compilation if there are any warnings.
    "-unchecked" // Enable additional warnings where generated code depends on assumptions.
  )
)

lazy val root = project
  .in(file("."))
  .settings(defaultSettings: _*)
  .settings(
    name := "advent-of-code-2020",
    version := "0.1",
    organization := "mikkom",
    scalaVersion := "3.0.0-M1",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.0.0-M3"
  )

// Format *all* Scala and SBT files
addCommandAlias("format", ";scalafmtAll;scalafmtSbt")
