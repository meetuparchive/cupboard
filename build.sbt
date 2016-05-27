name := "cupboard"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.1",
  "io.spray" %%  "spray-json" % "1.3.2",
  "org.scalatest" %% "scalatest" % "2.2.0" % "test"
)

enablePlugins(CommonSettingsPlugin)
enablePlugins(NexusPlugin)



