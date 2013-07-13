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