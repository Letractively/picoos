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

case class Permission(permObject : String, permAction : String)

class PicosaPermissions(val permissions : Seq[Permission] = Seq()) {				 
  
	def ++ (that : PicosaPermissions) : PicosaPermissions = {
			new PicosaPermissions( permissions ++ that.permissions.filter( item => !this.hasPermission(item)) )
	}
	
	def hasPermission(perm : Permission) : Boolean = {
	  permissions.find( 
	      item => (
	          ((item.permObject == perm.permObject) || item.permObject=="*")
	          && ((item.permAction == perm.permAction || item.permAction == "*")) 
	      )
	  ).isDefined
	}
	
}