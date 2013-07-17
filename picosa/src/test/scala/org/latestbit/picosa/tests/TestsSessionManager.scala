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

package org.latestbit.picosa.tests

import org.scalatest.FeatureSpec
import org.latestbit.picosa._

class TestsSessionManager extends FeatureSpec {
	feature("Check basic session manager functionality" ) {
	  val sessionManager = new SessionManager()
	  
	  scenario("Generate key & session") {
	    val key = sessionManager.generateKey()
	    val keyAsStr = sessionManager.keyToString(key)
	    println(keyAsStr)
	    assert ( !keyAsStr.isEmpty() && keyAsStr.length() > 0 )
	    
	    val userId = "TestUser"
	    val authType = "OAuth2"
	    val time = System.currentTimeMillis()
	    val sessionKey1 = sessionManager.createSessionKey( key, userId, authType, time )
	    val sessionKey2 = sessionManager.createSessionKey( keyAsStr, userId, authType, time )
	    val sessionKey3 = sessionManager.createSessionKey( key, "AnotherUser", authType, time )
	    assert (sessionKey1.equals(sessionKey2))
	    assert (!sessionKey2.equals(sessionKey3))
	    
	    val anotherKey = sessionManager.generateKey()
	    val sessionKey4 = sessionManager.createSessionKey( anotherKey, userId, authType, time )
	    assert (!sessionKey1.equals(sessionKey4))
	  }
	  
	  scenario("Check session security") {
	    val key = sessionManager.generateKey()
	    val keyAsStr = sessionManager.keyToString(key)

	    val userId = "TestUser"
	    val authType = "OAuth2"
	    val time = System.currentTimeMillis()
	    
	    val sessionKey = sessionManager.createSessionKey( key, userId, authType, time )
	    val sessionParams = sessionManager.decodeSessionParams(key, sessionKey)
	    assert(sessionParams.userId.equals(userId))
	    assert(sessionParams.authParams.equals(authType))
	    assert(sessionParams.timestamp == time)
	    
	    val invalidSessionKey = "TestUser:OO:"+time+sessionKey.substring(sessionKey.lastIndexOf(":"))
	    println(invalidSessionKey)
	    val sessionParams2 = sessionManager.decodeSessionParams(key, invalidSessionKey)
	    assert( sessionParams2 == null)
	  }
	}
}