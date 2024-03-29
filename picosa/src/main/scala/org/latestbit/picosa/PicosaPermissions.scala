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

case class Permission(permAction : String, permObject : String)

class PicosaPermissions(val permissions : Seq[Permission] = Seq()) {				 
  
	def ++ (that : PicosaPermissions) : PicosaPermissions = {
		new PicosaPermissions( permissions ++ that.permissions.filter( item => !this.hasPermission(item)) )
	}
	
	def stringToPermission( perm : String ) : Permission = {
	  val parsedPerm = perm.split("\\.")
	  if(parsedPerm.length == 1)
	    parsedPerm :+ ""	  
	  Permission(parsedPerm(0),parsedPerm(1))
	}
	
	def hasPermissionStrForm ( perm: String ) : Boolean = hasPermission(stringToPermission(perm))
	
	def hasPermissionsStrForm ( perms: Seq[String] ) : Boolean = hasPermissions( perms.map(stringToPermission(_)))
	
	def hasPermissions ( perms: Seq[Permission] ) : Boolean = {
	  !perms.filter(perm => hasPermission(perm)).isEmpty
	} 
	
	def hasPermission ( permAction: String, permObject : String) : Boolean = hasPermission(Permission(permAction, permObject))
	
	def hasPermission( perm : Permission) : Boolean = {
	  permissions.find( 
	      item => (
	          ((item.permObject.toUpperCase() == perm.permObject.toUpperCase()) || item.permObject=="*")
	          && ((item.permAction.toUpperCase() == perm.permAction.toUpperCase() || item.permAction == "*")) 
	      )
	  ).isDefined
	}
	
}