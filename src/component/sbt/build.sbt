enablePlugins(CommonSettingsPlugin)

val projectDir = file("./../../..")
val projectVersion = s"make -s -C $projectDir version".!!.trim

libraryDependencies += "com.meetup" %% "cupboard" % projectVersion

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.1",
  "io.spray" %%  "spray-json" % "1.3.2",
  "com.meetup" %% "cupboard" % projectVersion
)

scalacOptions += "-language:reflectiveCalls"