import sbt.Package.ManifestAttributes

name := "picosa"

version := "0.11"

organization := "www.latestbit.com"

homepage := Some(url("http://www.latestbit.com"))

licenses := Seq(("Apache License v2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html")))

scalaVersion := "2.10.2"

resolvers ++= Seq(
	"Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
	"Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/"
) 

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "1.+" % "test",
	"commons-codec" % "commons-codec" % "1.+"
)

scalacOptions ++= Seq(
	"-deprecation",
	"-unchecked"
)

javacOptions += "-Xlint:deprecation"

packageOptions := Seq(ManifestAttributes(
	("Build-Jdk"  , System.getProperty("java.version")),
	("Build-Date" , new java.text.SimpleDateFormat("MMddyyyy-hhmm").format(java.util.Calendar.getInstance().getTime()))
))

retrieveManaged := true
