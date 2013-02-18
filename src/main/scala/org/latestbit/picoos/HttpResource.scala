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
	val localResourceRegistry = new StdHttpResourcesRegistry("{"+resourcePath+"} Registry")
	
	protected def proceedResourceCustomHandlers( req : HttpResourceRequest, resp : HttpResourceResponse  ) : Boolean = {
	  val processedHandlers = customHandlers.takeWhile( item => item.proceedRequest(req, resp, this ))	  
	  processedHandlers.length == customHandlers.length
	}
	
	protected def proceedResourceRequest( req : HttpResourceRequest, resp : HttpResourceResponse  ) : Unit = {
	  if(proceedResourceCustomHandlers(req,resp))
		localResourceRegistry.proceedRequest ( req, resp )
	}
	    	
	protected def buildResourceApiRoutes(registry : HttpResourcesRegistry) = {
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
  
	/*def register() : HttpResource = {
	  buildResourceApiRoutes(DefaultHttpResourcesRegistry)
	  this
	}*/
	
	def register(registry : HttpResourcesRegistry) : HttpResource = {
	  buildResourceApiRoutes(registry)
	  this
	}
	
	def protectWith(httpAuthenticator : HttpAuthenticator) : HttpResource  = {
	  this.httpAuthenticator = Option(httpAuthenticator)
	  this
	}
	
	def addCustomHandler( handler : HttpResourceCustomHandler ) : HttpResource = {
	  customHandlers = customHandlers :+ handler
	  this
	}
	
	def removeCustomRequestHandler( handler : HttpResourceCustomHandler ) : HttpResource = {
	  customHandlers = customHandlers.filterNot( _ == handler)
	  this
	}
}

abstract class HttpProxyResource(resourcePath : String) extends HttpResource(resourcePath) {
  
  override def proceedResourceRequest( req : HttpResourceRequest, resp : HttpResourceResponse  ) : Unit = {
    if(proceedResourceCustomHandlers(req,resp))
      if(!dispatch(req,resp))
    	  localResourceRegistry.proceedRequest ( req, resp )
  }
  
  protected def dispatch(req : HttpResourceRequest, resp : HttpResourceResponse ) : Boolean
  
}