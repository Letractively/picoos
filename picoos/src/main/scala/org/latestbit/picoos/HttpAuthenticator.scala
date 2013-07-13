package org.latestbit.picoos

import org.latestbit.picoos.dsl.RestMethodBodyDef

trait HttpAuthenticator {
	def checkAccess(
	    req : HttpResourceRequest, 
	    resp : HttpResourceResponse, 
	    resource : HttpResource, 
	    methodName : String,
	    method : RestMethodBodyDef) : Boolean  
}