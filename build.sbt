
lazy val commonSettings = Seq(
  organization := "org.backuity",
  scalaVersion := "2.11.7",

  scalacOptions ++= Seq("-deprecation", "-unchecked")
)

lazy val root = project.in(file(".")).
  aggregate(core,macros)

lazy val core = project.in(file("core")).
  settings(commonSettings : _*).
  settings(
    name := "json-poc-core",

    libraryDependencies ++= Seq(
      "org.json4s"             %% "json4s-native"         % "3.2.11",
      "com.novocode"           %  "junit-interface"       % "0.10"      % "test-internal",          
      "junit"                  %  "junit"                 % "4.10"      % "test")
  ).    
  dependsOn(macros)


lazy val macros = project.in(file("macros")).
  settings(commonSettings : _*).
  settings(
    name := "json-poc-macros",

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value)
    )
