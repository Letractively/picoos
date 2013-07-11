import sbt._
import Keys._

object PicoosBuild extends Build {
    lazy val root = Project(id = "picoos",
                            base = file(".")) aggregate(picosa) 
    lazy val picosa = Project(id="picosa", base = file("picosa"))
}
