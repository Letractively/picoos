package org.latestbit.picoos

trait HttpAuthenticator {
	def checkAccess(
	    req : HttpResourceRequest, 
	    resp : HttpResourceResponse, 
	    resource : HttpResource, 
	    methodName : String ) : Boolean  
}