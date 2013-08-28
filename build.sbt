name := "lexicos"

version := "0.1.0"

scalaVersion := "2.10.0"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "com.github.axel22" %% "scalameter" % "0.3"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
