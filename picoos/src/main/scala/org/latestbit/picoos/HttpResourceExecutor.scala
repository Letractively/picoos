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

case class HttpResourceExecutor(resource : HttpResource, path : String, apiHandler : Option[ApiMethodBodyHandler]) {
	  
	  def execute(req: HttpResourceRequest, resp: HttpResourceResponse) = {	    
		val accessGranted : Boolean = (
		    apiHandler.isDefined && apiHandler.get.properties.getOrElse(ApiMethodBodyProperties.PROTECTED, false).asInstanceOf[Boolean] &&
			  (resource.httpAuthenticator match {
			    case Some(authenticator : HttpAuthenticator) => authenticator.checkAccess(req, resp, resource, path);
			    case _ => throw new Exception("Restricted method at "+resource.resourcePath+" required authenticator");
			  })
		   ) || (apiHandler.isDefined && !apiHandler.get.properties.getOrElse(ApiMethodBodyProperties.PROTECTED, false).asInstanceOf[Boolean])
		
		if(accessGranted) {
		    val result : ApiMethodResult = apiHandler match {
		      case Some(body : ApiMethodBodyHandlerNoParams) => body.handler
		      case Some(body : ApiMethodBodyHandlerHttpRequest) => body.handler(req)
		      case Some(body : ApiMethodBodyHandlerHttpRequestAndResponse) => body.handler(req, resp)
		      case _ => throw new Exception("Found empty or wrong request handler!")
		    }
		    
		    if(resource.corsMode) {
		    	if(resource.corsAllowOrigin.isDefined)
		    		resp.http.setHeader("Access-Control-Allow-Origin", resource.corsAllowOrigin.get)
		    	if(resource.corsAllowHeaders.isDefined)
		    		resp.http.setHeader("Access-Control-Allow-Headers", resource.corsAllowHeaders.get)		      
		    }
		    
		    result.proceedHttpResponse( resp )
		}
	  }  
}