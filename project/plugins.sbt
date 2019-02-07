logLevel := Level.Warn

// plugin for native-packager used to build packages (deb, dmg, etc) that can be run
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.15")

// scala-test plugin (doesn't seem to work with scala 2.12)
//addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.0")