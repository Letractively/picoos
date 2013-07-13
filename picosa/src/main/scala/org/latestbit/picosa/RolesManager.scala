package org.latestbit.picosa

import java.io.InputStream
import scala.xml._

trait RolesManager {
	def getRoles() : Seq[String] 
	def getPermissions(roles : Seq[String]) : PicosaPermissions	
}

class BasicRolesManager(val rolesDescXML : InputStream) extends RolesManager {
  
  val permissions : Map[String, PicosaPermissions] = loadRolesConfig()
  
  def loadRolesConfig() : Map[String, PicosaPermissions] = {
    val data = XML.load(rolesDescXML)
    (data \\ "roles" \\ "role").map( role => {
      (role.text, new PicosaPermissions(
          (role \\ "permissions" \\ "permission").map(
              permNode => Permission( 
                  (permNode \ "@object").text,
                  (permNode \ "@action").text
              )
          ))
      )
    }).toMap 
  }
  
  override def getRoles() : Seq[String] = permissions.keys.toSeq
  
  override def getPermissions(roles : Seq[String]) : PicosaPermissions = {
    roles.foldLeft(new PicosaPermissions())( (all, item) => all ++ permissions(item))
  }
  
  
}