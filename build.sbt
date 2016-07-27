name := "cupboard"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "io.spray" %%  "spray-json" % "1.3.2",
  "org.scalatest" %% "scalatest" % "2.2.0" % "test",
  "com.google.cloud" % "gcloud-java-datastore" % "0.2.1",
  "org.typelevel" %% "cats" % "0.6.0",
  "org.typelevel" %% "macro-compat" % "1.1.1"
)

scalacOptions ++= Seq(
  "-language:reflectiveCalls"   // necessary until we implement "vampire methods" to eliminate this warning
)

enablePlugins(CommonSettingsPlugin)
enablePlugins(NexusPlugin)
enablePlugins(CoverallsWrapper)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
