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
	
	protected val allApiMethods = getClass.getMethods.filter( method => method.getReturnType().eq(classOf[ApiMethodDef]) && !method.getName().startsWith("$") )
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
		      HttpResourceExecutor( this, handlerPath, methodDef.handler ).execute, 
		      methodDef.httpMethod)		  
		})
		
		registry.registerHandler(resourcePath, proceedResourceRequest)
	}
  
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

abstract class HttpCanonicalResource(resourcePath : String, extMethodsParamName : String = "f") extends HttpResource(resourcePath) {
	
	override def proceedResourceRequest( req : HttpResourceRequest, resp : HttpResourceResponse  ) : Unit = {
	  if(proceedResourceCustomHandlers(req,resp)) {
	    val additionalMethodParam = req.http.getParameter(extMethodsParamName)
	    if(additionalMethodParam!=null) {
	    	val serviceFuncPath = additionalMethodParam match {
	    	  case s:String if additionalMethodParam.startsWith("/") => s
	    	  case s:String => "/"+s
	    	} 
	    	localResourceRegistry.proceedRequest ( req, resp, req.servicePath+serviceFuncPath )
	    }
		else {
			// Check canonical paths
		    val idxResPathStr = req.servicePath.indexOf(resourcePath)
		    val resourceId = req.servicePath.substring(idxResPathStr+resourcePath.length()) match {
		      case "" | "/" => "/"
		      case str : String if(str.startsWith("/")) => str.substring(1)
		      case str : String => str
		    }
		    
		    val canonicalMethodBody : Option[ApiMethodBodyHandler] = req.httpMethod match {		      
		      case HttpMethod.GET =>
		        resourceId match {
		          case "/" => $list.handler
		          case str : String => $getResource(resourceId).handler
		        }
		      case HttpMethod.PUT =>
		        resourceId match {
		          case "/" => $replaceAll.handler
		          case str : String => $replaceResource(resourceId).handler
		        }
		      case HttpMethod.POST =>
		        resourceId match {
		          case "/" => $newResource.handler
		          case str : String => $replaceResource(resourceId).handler
		        }
		      case HttpMethod.DELETE =>
		        resourceId match {
		          case "/" => $deleteAll.handler
		          case str : String => $deleteResource(resourceId).handler
		        }	        
		        
		      case HttpMethod.OPTIONS => $opt(resourceId).handler
		      case HttpMethod.HEAD => $head(resourceId).handler
		      case _ => None
		    }
		    
		    canonicalMethodBody match {
		      case Some(handler : ApiMethodBodyHandler) => HttpResourceExecutor(this, req.servicePath, canonicalMethodBody).execute(req, resp)
		      case _ => httpErrorResult(404, "Method is not supported for canonical RESTful service!")
		    }
		  }
	   }
	}
	
	def $list : ApiMethodDef
	def $replaceAll : ApiMethodDef = apiMethod as {
	  httpErrorResult(501, "Method replace collection is not implemented!")
	}
	def $deleteAll : ApiMethodDef = apiMethod as {
	  httpErrorResult(501, "Method delete all collection is not implemented!")
	}
	def $newResource: ApiMethodDef
	def $getResource( resourceId : String ) : ApiMethodDef
	def $replaceResource( resourceId : String) : ApiMethodDef	
	def $deleteResource( resourceId : String) : ApiMethodDef
	
	def $opt( resourceId : String ) : ApiMethodDef = apiMethod as {
	  httpOkResult
	}
	
	def $head( resourceId : String ) : ApiMethodDef = apiMethod as {
	  httpOkResult
	}
	
}
