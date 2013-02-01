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
package org.latestbit.picoos.tests

import org.scalatest.FeatureSpec
import org.latestbit.picoos._
import org.latestbit.picoos.dsl._
import org.latestbit.picoos.tests.model._

class HttpResourcesRegistryTests extends FeatureSpec  {
  	feature("Common HttpResourcesRegistry Functionality") {
  	  DefaultHttpResourcesRegistry.clearAllHandlers()
  	  val httpResource = new TestHttpResource()
  	  httpResource.register()
	  scenario("Check registered handlers") {
  	    assert ( httpResource.localResourceRegistry.findHandler(HttpMethod.GET, "/resource1/unknown") == None )
  	    assert ( httpResource.localResourceRegistry.findHandler(HttpMethod.GET, "/resource1/getUsers") != None )
  	    assert ( DefaultHttpResourcesRegistry.findHandler(HttpMethod.GET, "/resource1/unknown") != None)
  	    assert ( DefaultHttpResourcesRegistry.findHandler(HttpMethod.GET, "/resource1/getUsers") != None)
  	    assert ( DefaultHttpResourcesRegistry.findHandler(HttpMethod.POST, "/resource1/getUsers") != None)
  	    assert ( httpResource.localResourceRegistry.findHandler(HttpMethod.POST, "/resource1/getUsers2") == None )
  	    assert ( DefaultHttpResourcesRegistry.findHandler(HttpMethod.POST, "/resource1/getUsers2") != None)
  	    assert ( DefaultHttpResourcesRegistry.findHandler(HttpMethod.POST, "/resource1/getUsers2Mapped") != None)
  	    assert ( DefaultHttpResourcesRegistry.findHandler(HttpMethod.POST, "/resource1/getUsers3") != None)
  	    assert ( httpResource.localResourceRegistry.findHandler(HttpMethod.POST, "/resource1/getUsers4") == None )
  	    assert ( DefaultHttpResourcesRegistry.findHandler(HttpMethod.POST, "/resource1/getUsers4") != None)
  	    assert ( DefaultHttpResourcesRegistry.findHandler(HttpMethod.GET, "/resource1/getUsers4") != None)
  	  }
  	}
}