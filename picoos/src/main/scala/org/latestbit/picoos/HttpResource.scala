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
	
	protected val allApiMethods = 
	  getClass.getMethods.filter( 
	      method => method.getReturnType().eq(classOf[RestMethodBodyDef]) 
	      && !method.getName().startsWith("$") 
	  )
	var customHandlers : List[HttpResourceCustomHandler] = List()  
	var httpAuthenticator : Option[HttpAuthenticator] = None
	val localResourceRegistry = new StdHttpResourcesRegistry("{"+resourcePath+"} Registry")
	var corsMode : Boolean = false
	var corsAllowOrigin : Option[String] = Some("*")
	var corsAllowHeaders : Option[String] = Some("Origin, X-Requested-With, Content-Type, Accept")
	
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
		  
		  val methodDef : RestMethodBodyDef = item.invoke(this).asInstanceOf[RestMethodBodyDef]
		  
		  val handlerPath = Option(methodDef.restParams.path) match {
		    case Some(pathStr) => pathStr
		    case _ => "/" + item.getName()
		  }
		  
		  localResourceRegistry.registerHandler(
		      resourcePath+handlerPath, 
		      HttpResourceExecutor( this, handlerPath, Some(methodDef) ).execute, 
		      methodDef.restParams.httpMethod)		  
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
	
	def setCORSMode(mode : Boolean, corsAllowOrigin : String ="*") : HttpResource = {
	  this.corsMode = mode
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
		    
		    val canonicalMethodBody : Option[RestMethodBodyDef] = req.httpMethod match {		      
		      case HttpMethod.GET =>
		        resourceId match {
		          case "/" => Some($list)
		          case str : String => Some($getResource(resourceId))
		        }
		      case HttpMethod.PUT =>
		        resourceId match {
		          case "/" => Some($replaceAll)
		          case str : String => Some($replaceResource(resourceId))
		        }
		      case HttpMethod.POST =>
		        resourceId match {
		          case "/" => Some($newResource)
		          case str : String => Some($replaceResource(resourceId))
		        }
		      case HttpMethod.DELETE =>
		        resourceId match {
		          case "/" => Some($deleteAll)
		          case str : String => Some($deleteResource(resourceId))
		        }	        
		        
		      case HttpMethod.OPTIONS => Some($opt(resourceId))
		      case HttpMethod.HEAD => Some($head(resourceId))
		      case _ => None
		    }
		    
		    canonicalMethodBody match {
		      case Some(handler : RestMethodBodyDef) => executeCanonicalMethod(handler, req, resp)
		      case _ => httpErrorResult(404, "Method is not supported for canonical RESTful service!")
		    }
		  }
	   }
	}
	
	protected def executeCanonicalMethod(handler : RestMethodBodyDef, req: HttpResourceRequest, resp: HttpResourceResponse) = {
	  HttpResourceExecutor(this, req.servicePath, Some(handler)).execute(req, resp)
	}
	
	def $list : RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "Method list entiry collection is not implemented!")
	}
	
	def $replaceAll : RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "Method replace collection is not implemented!")
	}
	def $deleteAll : RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "Method delete all collection is not implemented!")
	}
	
	def $newResource: RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "Method new resource is not implemented!")
	}
	def $replaceResource( resourceId : String) : RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "Method replace resource is not implemented!")
	}	
	def $deleteResource( resourceId : String) : RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "Method delete resource is not implemented")
	}
	
	def $getResource( resourceId : String ) : RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "Method get resource is not implemented")
	}	
	
	def $opt( resourceId : String ) : RestMethodBodyDef = restMethod as {
	  httpOkResult
	}
	
	def $head( resourceId : String ) : RestMethodBodyDef = restMethod as {
	  httpOkResult
	}
	
}


abstract class HttpCanonicalCollectionResource(resourcePath : String, extMethodsParamName : String = "f") extends HttpCanonicalResource(resourcePath, extMethodsParamName) {
	private def getColAndResId(resourceId : String) : Tuple2[String,String] = {
	  val idxRes = resourceId.indexOf("/")
	  if(idxRes != -1) {
	    Tuple2(resourceId.substring(0, idxRes),resourceId.substring(idxRes+1))
	  }
	  else {
	    Tuple2(resourceId, "/")
	  }
	}
	
	override def $list : RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "Method list all collections is not implemented!")
	}
	
	final override def $newResource: RestMethodBodyDef = $newCollection
	
	override def $replaceAll : RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "Method replace all collections is not implemented!")
	}
	override def $deleteAll : RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "Method delete all collections is not implemented!")
	}
	
	def $newCollection : RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "Method a new collection is not implemented!")
	}
	
	def $deleteCollection(collectionId : String): RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "Replace collection is not implemented!")
	}
		
	final override def $replaceResource( resourceId : String) : RestMethodBodyDef = {
	  getColAndResId(resourceId) match {
	    case (vl : Tuple2[String, String]) if vl._2=="/" => $newResource(vl._1)
	    case (vl : Tuple2[String, String]) => $replaceResource(vl._1, vl._2)
	    case _ => null
	  }
	}
	
	def $newResource(collectionId : String): RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "New resource is not implemented!")
	}
	
	def $replaceResource( collectionId : String, resourceId : String) : RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "Replace a resource is not implemented!")
	}
	
	final override def $deleteResource( resourceId : String) : RestMethodBodyDef = {
	  getColAndResId(resourceId) match {
	    case (vl : Tuple2[String, String]) if vl._2=="/" => $deleteCollection(vl._1)
	    case (vl : Tuple2[String, String]) => $deleteResource(vl._1, vl._2)
	    case _ => null
	  }
	}
	
	def $deleteResource( collectionId : String, resourceId : String ) : RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "Method delete resource is not implemented")
	}
	
	final override def $getResource( resourceId : String ) : RestMethodBodyDef = {
	  getColAndResId(resourceId) match {
	    case (vl : Tuple2[String, String]) if vl._2=="/" => $list(vl._1)
	    case (vl : Tuple2[String, String]) => $getResource(vl._1, vl._2)
	    case _ => null
	  }
	}
	
	def $list(collectionId : String) : RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "Method list all collections is not implemented!")
	}
	
	def $getResource( collectionId : String, resourceId : String ) : RestMethodBodyDef = restMethod as {
	  httpErrorResult(501, "Method get resource is not implemented")
	}

}