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

package org.latestbit.picoos

import org.latestbit.picoos._
import org.latestbit.picoos.dsl._

case class HttpResourceExecutor(resource : HttpResource, path : String, restMethod : Option[RestMethodBodyDef]) {
	  
	  def execute(req: HttpResourceRequest, resp: HttpResourceResponse) = {
	    if(restMethod.isDefined) {
			val accessGranted : Boolean = (
			    restMethod.get.authParams.isDefined			    
			    && (resource.httpAuthenticator match {
				    case Some(authenticator : HttpAuthenticator) => authenticator.checkAccess(req, resp, resource, path, restMethod.get)
				    case _ => throw new Exception("Restricted method at "+resource.resourcePath+" required authenticator");
				  })
				&& (restMethod.get.authParams.get.authFunction match {
				  case Some(func : ((String, HttpResourceRequest)=>Boolean)) => func(path, req)
			      case _ => true
			    })
			   ) || (restMethod.get.authParams.isEmpty)
			
			if(accessGranted) {
			    val result : RestMethodResult = restMethod.get.execute(req, resp)
			    
			    if(resource.corsMode) {
			    	if(resource.corsAllowOrigin.isDefined)
			    		resp.http.setHeader("Access-Control-Allow-Origin", resource.corsAllowOrigin.get)
			    	if(resource.corsAllowHeaders.isDefined)
			    		resp.http.setHeader("Access-Control-Allow-Headers", resource.corsAllowHeaders.get)		      
			    }
			    if(resource.cachingOptions.isDefined)
			      resource.cachingOptions.get.proceedOptions(resp)
			    
			    result.proceedHttpResponse( resp )
			}
			else {
			  if(!resp.http.isCommitted()) {
				  httpErrorResult(403, "Access denied to the method "+path).proceedHttpResponse(resp)
			  }
			}
	    }
	    else
	      throw new Exception("Unable to execute undefined method at "+path)
	  }  
}