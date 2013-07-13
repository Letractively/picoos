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