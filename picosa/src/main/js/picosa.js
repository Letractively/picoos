function Picosa(permissions) {
	"use strict";
	permissions = permissions || [];
	
	this.checkAccessForStr = function(permission) {
		var parsedPermission = permission.split(".");
		return this.checkAccessFor(parsedPermission[0],parsedPermission[1]);
	};
	
	this.checkAccessFor = function(permAction, permObject) {
		return _.find(permissions, function(item) {
			return (
					((item.permObject == permObject) || item.permObject=="*")
			        && ((item.permAction == permAction || item.permAction == "*")) 
			);
		})!=undefined;
	};
}