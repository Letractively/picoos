/*
   Copyright 2013 Abdulla Abdurakhmanov (abdulla@latestbit.com)
   Original sources are available at www.latestbit.com

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

package org.latestbit.picosa

import java.io.InputStream
import scala.xml._

case class Role(name : String)(description : Option[String])

trait RolesManager {
	def getRoles() : Seq[Role]
	def getPermissionsByRoles(roles : Seq[Role]) : PicosaPermissions
	def getPermissions(role : String) : PicosaPermissions = getPermissions(Seq(role))
	def getPermissions(roles : Seq[String]) : PicosaPermissions = getPermissionsByRoles(roles.map(Role(_)(None)))
}

class BasicRolesManager(val rolesDescXML : InputStream) extends RolesManager {
  
  val permissions : Map[Role, PicosaPermissions] = loadRolesConfig()
  
  def loadRolesConfig() : Map[Role, PicosaPermissions] = {
    val data = XML.load(rolesDescXML)
    
    (data \\ "roles" \\ "role").map( role => {
      ( (Role( (role \ "@name").text)(Some((role \ "@desc").text)) ), new PicosaPermissions(
          (role \\ "permissions" \\ "permission").map(
              permNode => Permission( 
                  (permNode \ "@action").text,
                  (permNode \ "@object").text
              )
          ))
      )
    }).toMap 
  }
  
  override def getRoles() : Seq[Role] = permissions.keys.toSeq
  
  override def getPermissionsByRoles(roles : Seq[Role]) : PicosaPermissions = {
    roles.foldLeft(new PicosaPermissions())( (all, item) => all ++ permissions(item))
  }
  
  
}