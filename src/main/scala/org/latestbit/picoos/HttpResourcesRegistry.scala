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

import scala.collection.mutable.{Map, HashMap, SynchronizedMap}
import org.latestbit.picoos.HttpMethod._
import java.util.logging.Logger
import java.util.logging.Level
import javax.servlet.ServletException
import javax.servlet.http.HttpServletResponse

trait HttpResourcesRegistry {
	def registerHandler(
		    path : String, 
		    handler : (HttpResourceRequest, HttpResourceResponse) => Unit, 
		    httpMethod : HttpMethod = HttpMethod.ANY_METHOD)
	
	def findHandler (  httpMethod : HttpMethod, path : String ) : Option[(HttpResourceRequest, HttpResourceResponse) =>Unit]
	def proceedRequest( req : HttpResourceRequest , resp : HttpResourceResponse )
	def clearAllHandlers()
}

class StdHttpResourcesRegistry(val registryName : String) extends HttpResourcesRegistry {
	val KEY_DELIMETER="~"
	private final val log : Logger  = Logger.getLogger(classOf[StdHttpResourcesRegistry].getName())
	  
	val handlersRegistry = new HashMap[String, 
	  (HttpResourceRequest, HttpResourceResponse) =>Unit] with SynchronizedMap[String, (HttpResourceRequest, HttpResourceResponse) =>Unit]		
	
	override def registerHandler(
	    path : String, 
	    handler : (HttpResourceRequest, HttpResourceResponse) => Unit, 
	    httpMethod : HttpMethod = HttpMethod.ANY_METHOD) = {
	  httpMethod match {	    
	    case HttpMethod.ANY_METHOD => {
	      if(handlersRegistry.contains(path)) 
	        throw new Exception("Handler at "+path+" has already been registered!"); 
	      handlersRegistry += ( path -> handler ) 
	    }
	    case value => {
	      if(handlersRegistry.contains(value.toString + KEY_DELIMETER +  path )) 
	        throw new Exception("Handler at "+value.toString + KEY_DELIMETER +  path +" has already been registered!"); 	      
	      handlersRegistry += ( value.toString + KEY_DELIMETER +  path -> handler )
	    }
	  }	  
	}
	
	def reverseFindHandler ( path : String ) : Option[(HttpResourceRequest, HttpResourceResponse) =>Unit] = {
	  handlersRegistry.get(path) match {
	    case res : Some[_] => res
	    case _ => {
	      val idx = path.lastIndexOf("/")
	      if(idx != -1) {
	        reverseFindHandler(path.substring(0, idx))
	      }
	      else
	        None
	    }
	  }
	}
	
	override def findHandler (  httpMethod : HttpMethod, path : String ) : Option[(HttpResourceRequest, HttpResourceResponse) =>Unit] = {	  
	  handlersRegistry.get(httpMethod.toString() + KEY_DELIMETER +  path) match {
	    case res : Some[_] => res
	    case _ => reverseFindHandler(path) 
	  }
	}
	
	def hasHandler(req : HttpResourceRequest) : Boolean = {
	  findHandler( req.httpMethod, req.servicePath ).isDefined
	}
	
	def proceedRequest(req : HttpResourceRequest , resp : HttpResourceResponse) = {
	    try {	      
	      findHandler( req.httpMethod, req.servicePath ) match {
	        case Some(handler) => handler(req,resp )
	        case _ => resp.http.sendError(
	            HttpServletResponse.SC_NOT_FOUND, 
	            "Not found any API handler at '"+req.servicePath
	            +"'. Registry: "+registryName
	            +". Size: "+handlersRegistry.size)
	      }
	    }
	    catch{
	      case ex: Exception => throw new ServletException(ex) 
	    }      
	  }	
	
	override def clearAllHandlers() = {
	  handlersRegistry.clear()
	}
}

//object DefaultHttpResourcesRegistry extends StdHttpResourcesRegistry("Default Registry")