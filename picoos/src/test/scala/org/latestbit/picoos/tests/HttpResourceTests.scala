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

class HttpResourceTests extends FeatureSpec {

	feature( "API tests" ) {
		val testDsl = new TestDsl()

		assert( testDsl.test0.restParams.path == null )
		assert( testDsl.test0.restParams.httpMethod == HttpMethod.ANY_METHOD )
		assert( testDsl.test0.authParams.isDefined )
		assert( testDsl.test0.authParams.get.permissions.isEmpty )

		assert( testDsl.test1.restParams.path == null )
		assert( testDsl.test1.restParams.httpMethod == HttpMethod.ANY_METHOD )
		assert( testDsl.test1.authParams.isEmpty )

		assert( testDsl.test5.restParams.path != null )
		assert( testDsl.test5.restParams.httpMethod == HttpMethod.GET )
		assert( testDsl.test5.authParams.isDefined )
		assert( testDsl.test5.authParams.get.permissions.size > 0 )
	}

	feature( "Common HttpResource Functionality" ) {
		val httpResource = new TestHttpResource()
		scenario( "Execute handlers of HttpResource" ) {
			val res = httpResource.getUsers.execute()
			assert( httpResource.getUsers.execute().equals( httpOkResult ) )
			assert( httpResource.getUsers2.execute().equals( httpTextResult( "Hello!" ) ) )
			assert( httpResource.getUsers3.execute( null ).isInstanceOf[httpJsonResult[_]] )

			intercept[Exception] {
				httpResource.getUsers3.execute()
			}
			intercept[Exception] {
				httpResource.getUsers6.execute()
			}
			intercept[Exception] {
				httpResource.getUsers6.execute( null )
			}
		}

		scenario( "Checking JSon & XML results for HttpResource actions" ) {
			val jsonResult = httpResource.getUsers3.execute( null ).asInstanceOf[httpJsonResult[TestJson]]
			assert( jsonResult.jsonObj.value == "Test" )
			assert( jsonResult.textResult.trim() == "{\"value\":\"Test\"}" )
		}
	}

	feature( "Common HttpCanonicalResource Functionality" ) {
		val httpCanResource = new TestCanonicalRestful()
		scenario( "Execute handlers of HttpCanResource" ) {
			assert( httpCanResource.$newResource.execute().equals( httpOkResult ) )
		}

		val httpCanColResource = new TestCanonicalCollectionRestful()
		scenario( "Execute handlers of HttpCanColResource" ) {
			assert( httpCanColResource.$newResource( "ss" ).execute().equals( httpTextResult( "NewResult" ) ) )
			assert( httpCanColResource.$replaceResource( "ss" ).execute().equals( httpTextResult( "NewResult" ) ) )
			assert( httpCanColResource.$replaceResource( "ss/dd" ).execute().equals( httpTextResult( "UpdateResult" ) ) )
		}
	}

}