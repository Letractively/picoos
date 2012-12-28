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

package org.latestbit.msf

import scala.collection.mutable.{Map, HashMap, SynchronizedMap}
import org.latestbit.msf.HttpMethod._

trait HttpResourcesRegistry {
	def registerHandler(
		    path : String, 
		    handler : (HttpResourceRequest, HttpResourceResponse) => Unit, 
		    httpMethod : HttpMethod = HttpMethod.ANY_METHOD)
	
	def findHandler (  httpMethod : HttpMethod, path : String ) : Option[(HttpResourceRequest, HttpResourceResponse) =>Unit]
	
	def clearAllHandlers()
}

object DefaultHttpResourcesRegistry extends HttpResourcesRegistry {
	val KEY_DELIMETER="~"
	  
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
	
	override def findHandler (  httpMethod : HttpMethod, path : String ) : Option[(HttpResourceRequest, HttpResourceResponse) =>Unit] = {	  
	  handlersRegistry.get(httpMethod.toString() + KEY_DELIMETER +  path) match {
	    case res : Some[_] => res
	    case _ => handlersRegistry.get(path)
	  }
	}
	
	override def clearAllHandlers() = {
	  handlersRegistry.clear()
	}
}