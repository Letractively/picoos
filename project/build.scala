import sbt._
import Keys._

object PicoosBuild extends Build {
    lazy val root = Project(id = "picoos-framework",
                            base = file(".")) aggregate(picoos, picosa) 
    lazy val picoos = Project(id="picoos", base = file("picoos"))
    lazy val picosa = Project(id="picosa", base = file("picosa")) dependsOn(picoos)
}
