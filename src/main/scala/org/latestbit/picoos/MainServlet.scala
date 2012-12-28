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


import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import javax.servlet.ServletException
import java.util.logging.Logger
import java.util.logging.Level
import org.latestbit.picoos.HttpMethod._

class MainServlet extends HttpServlet {      
  private final val log : Logger  = Logger.getLogger(classOf[MainServlet].getName())
  
  val DEFAULT_API_CONTEXT = "/rest-api/"
    
  override def doGet(req : HttpServletRequest , resp : HttpServletResponse) = {
    processRequest(HttpMethod.GET,req,resp)
  }
  
  override def doPut(req : HttpServletRequest , resp : HttpServletResponse) = {
    processRequest(HttpMethod.PUT,req,resp)
  }  

  override def doPost(req : HttpServletRequest , resp : HttpServletResponse) = {
    processRequest(HttpMethod.POST,req,resp)
  }  

  override def doDelete(req : HttpServletRequest , resp : HttpServletResponse) = {
	  processRequest(HttpMethod.DELETE,req,resp)
  }
  
  override def doTrace(req : HttpServletRequest , resp : HttpServletResponse) = {
	  processRequest(HttpMethod.TRACE,req,resp)
  }
  
  override def doHead(req : HttpServletRequest , resp : HttpServletResponse) = {
	  processRequest(HttpMethod.HEAD,req,resp)
  }
  
  override def doOptions(req : HttpServletRequest , resp : HttpServletResponse) = {
	  processRequest(HttpMethod.OPTIONS,req,resp)
  }  
  
  def getAPIPathPrefix()  : String = {
    Option(getServletConfig().getInitParameter("api-prefix")) match {
      case Some(str : String) => str
      case _ => DEFAULT_API_CONTEXT
    }
  }
  
  var registry : HttpResourcesRegistry = null
  
  override def init() = {
    super.init()

    registry = Option(getServletConfig().getInitParameter("registry-class")) match {
      case Some(registryClassName: String) => {
    	val registryClass = Class.forName(registryClassName.trim())
    	registryClass.newInstance().asInstanceOf[HttpResourcesRegistry]        
      }
      case _ => DefaultHttpResourcesRegistry      
    }
    
    val resourcesFactory : String = getServletConfig().getInitParameter("resources-factory-classes")
    
    if(resourcesFactory==null)
      throw new ServletException("Required 'resources-factory-classes' servlet parameter doesn't not found in web.xml!")
    
    resourcesFactory.split(",").foreach(resourcesFactoryClassName => {
    	val resourcesFactoryClass = Class.forName(resourcesFactoryClassName.trim())
    	val resourcesFactoryInstance = resourcesFactoryClass.newInstance().asInstanceOf[HttpResourcesFactory]
    	resourcesFactoryInstance.createResources(registry)
    })

  }
  
  def processRequest(httpMethod : HttpMethod, req : HttpServletRequest , resp : HttpServletResponse) = {
    val requestUri = req.getRequestURI()
    val idx = requestUri.lastIndexOf(getAPIPathPrefix())
    if(idx != -1) {
    	val fullPath = requestUri.substring(idx)
    	val servicePath = "/"+fullPath.replaceFirst(getAPIPathPrefix(), "")
	    try {	      
	      registry.findHandler(httpMethod, servicePath) match {
	        case Some(handler) => handler(HttpResourceRequest(httpMethod,req),HttpResourceResponse(resp))
	        case _ => resp.sendError(HttpServletResponse.SC_NOT_FOUND, "Not found any API handler at "+servicePath)
	      }
	    }
	    catch{
	      case ex: Exception => throw new ServletException(ex) 
	    }      
    }
    else
      log.log(Level.WARNING,"Unable to process request to empty or unknow prefix ("+requestUri+")")
  }  
}