name := "lexicos"

version := "0.1.0"

scalaVersion := "2.10.0"

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "com.github.axel22" %% "scalameter" % "0.3"

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
