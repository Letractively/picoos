import sbt.Package.ManifestAttributes

name := "picoos"

version := "0.14"

organization := "www.latestbit.com"

homepage := Some(url("http://www.latestbit.com"))

licenses := Seq(("Apache License v2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html")))

scalaVersion := "2.11.7"

sbtVersion := "0.13.7"

autoScalaLibrary := false

resolvers ++= Seq(
	"Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
	"Jerkson" at "http://repo.codahale.com",
	"Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/"
) 

libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-library" % scalaVersion.value,
	"org.scala-lang.modules" %% "scala-xml" % "1.0.+",
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.+",
	"com.fasterxml.jackson.module" % "jackson-module-scala_2.11" % "2.4.0-rc2",
	"javax.servlet" % "javax.servlet-api" % "3.0.+",
	"org.scalatest" % "scalatest_2.11" % "2.+" % "test",
	"junit" % "junit" % "4.+" % "test"
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
