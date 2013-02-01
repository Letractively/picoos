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

import org.latestbit.picoos.dsl._

trait HttpResourceCustomHandler {
  def proceedRequest(req: HttpResourceRequest, resp: HttpResourceResponse, resource : HttpResource ) : Boolean
}

class HttpResource(val resourcePath : String) extends ApiDsl {
	
	private val allApiMethods = getClass.getMethods.filter( method => method.getReturnType().eq(classOf[ApiMethodDef]) )
	private val allApiMethodsNames = allApiMethods.map(_.getName).sorted
	var customHandlers : List[HttpResourceCustomHandler] = List()  
	var httpAuthenticator : Option[HttpAuthenticator] = None
	val localResourceRegistry = new StdHttpResourcesRegistry()
	
	protected def proceedResourceRequest( req : HttpResourceRequest, resp : HttpResourceResponse  ) : Unit = {
	  val processedHandlers = customHandlers.takeWhile( item => item.proceedRequest(req, resp, this ))
	  
	  if(processedHandlers.length == customHandlers.length)
		  localResourceRegistry.proceedRequest ( req, resp )
	}
	    	
	private def buildResourceApiRoutes(registry : HttpResourcesRegistry) = {
		localResourceRegistry.clearAllHandlers();
		
		allApiMethods.foreach(item => {
		  
		  val methodDef : ApiMethodDef = item.invoke(this).asInstanceOf[ApiMethodDef]
		  
		  val handlerPath = methodDef.path match {
		    case Some(pathStr) => pathStr
		    case _ => "/" + item.getName()
		  }
		  
		  localResourceRegistry.registerHandler(
		      resourcePath+handlerPath, 
		      HttpResourceRequestAPIHandler( this, handlerPath, methodDef.handler ).httpRequestHandler, 
		      methodDef.httpMethod)		  
		})
		
		registry.registerHandler(resourcePath, proceedResourceRequest)
	}
  
	def register() = {
	  buildResourceApiRoutes(DefaultHttpResourcesRegistry)
	}
	
	def register(registry : HttpResourcesRegistry) = {
	  buildResourceApiRoutes(registry)
	}
	
	def protectWith(httpAuthenticator : HttpAuthenticator)  = {
	  this.httpAuthenticator = Option(httpAuthenticator)
	}
	
	def addCustomHandler( handler : HttpResourceCustomHandler ) = {
	  customHandlers = customHandlers :+ handler
	}
	
	def removeCustomRequestHandler( handler : HttpResourceCustomHandler ) = {
	  customHandlers = customHandlers.filterNot( _ == handler)
	}

}
