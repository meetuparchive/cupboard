name := "cupboard"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.1",
  "io.spray" %%  "spray-json" % "1.3.2",
  "org.scalatest" %% "scalatest" % "2.2.0" % "test",
  "com.google.cloud" % "gcloud-java-datastore" % "0.2.1"
)

enablePlugins(CommonSettingsPlugin)
enablePlugins(NexusPlugin)



