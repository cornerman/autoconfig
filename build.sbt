name := "autoconfig"
version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.11"
crossScalaVersions := Seq("2.11.11", "2.12.2")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++=
  "org.scala-lang" % "scala-reflect" % scalaVersion.value ::
  "com.typesafe" % "config" % "1.3.1" ::
  "com.github.cornerman" %% "macroni" % "0.1.0-SNAPSHOT" % "test" ::
  "org.specs2" %% "specs2-core" % "3.8.9" % "test" ::
  "org.specs2" %% "specs2-mock" % "3.8.9" % "test" ::
  Nil

scalacOptions ++=
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-deprecation" ::
  "-explaintypes" ::
  "-feature" ::
  "-language:_" ::
  "-Xlint:_" ::
  "-Ywarn-unused" ::
  Nil

fork in Test := true

envVars in Test := Map("SECRET" -> "secret")

organization in Global := "com.github.cornerman"

pgpSecretRing in Global := file("secring.gpg")
pgpPublicRing in Global := file("pubring.gpg")
pgpPassphrase in Global := Some("".toCharArray)

pomExtra := {
  <url>https://github.com/cornerman/autoconfig</url>
  <licenses>
    <license>
      <name>The MIT license</name>
      <url>http://www.opensource.org/licenses/mit-license.php</url>
    </license>
  </licenses>
  <scm>
    <url>https://github.com/cornerman/autoconfig</url>
    <connection>scm:git:git@github.com:cornerman/autoconfig.git</connection>
  </scm>
  <developers>
    <developer>
      <id>jkaroff</id>
      <name>Johannes Karoff</name>
      <url>https://github.com/cornerman</url>
    </developer>
  </developers>
}
