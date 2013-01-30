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

class HttpResource(val resourcePath : String) extends ApiDsl {
	
	private val allApiMethods = getClass.getMethods.filter( method => method.getReturnType().eq(classOf[ApiMethodDef]) )
	private val allApiMethodsNames = allApiMethods.map(_.getName).sorted
	var httpAuthenticator : Option[HttpAuthenticator] = None
	    	
	private def buildResourceApiRoutes(registry : HttpResourcesRegistry) = {
		allApiMethods.foreach(item => {
		  
		  val methodDef : ApiMethodDef = item.invoke(this).asInstanceOf[ApiMethodDef]
		  
		  val handlerPath = methodDef.path match {
		    case Some(pathStr) => pathStr
		    case _ => "/" + item.getName()
		  }		  
		  
		  registry.registerHandler(
		      resourcePath+handlerPath, 
		      HttpResourceRequestHandler( this, handlerPath, methodDef.handler ).httpRequestHandler, 
		      methodDef.httpMethod)		  
		})
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
}
