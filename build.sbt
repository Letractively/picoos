import sbt.Package.ManifestAttributes

name := "picoos"

version := "0.7"

organization := "www.latestbit.com"

homepage := Some(url("http://www.latestbit.com"))

licenses := Seq(("Apache License v2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html")))

scalaVersion := "2.9.2"

resolvers ++= Seq(
	"Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
	"Jerkson" at "http://repo.codahale.com",
	"Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/"
) 

libraryDependencies ++= Seq(
	"org.codehaus.jackson" % "jackson-core-asl" % "1.9.+",
	"org.codehaus.jackson" % "jackson-mapper-asl" % "1.9.+",
	"com.codahale" % "jerkson_2.9.1" % "0.5.+",
	"javax.servlet" % "javax.servlet-api" % "3.0.+",
	"org.scalatest" %% "scalatest" % "1.+" % "test"
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

