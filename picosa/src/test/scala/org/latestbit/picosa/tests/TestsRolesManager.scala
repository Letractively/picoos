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


class TestsRolesManager extends FeatureSpec {
	feature("Test basic roles manager" ) {
	  val rolesMgr : RolesManager = new BasicRolesManager(classOf[TestsRolesManager].getResourceAsStream("test-roles.xml"))
	  
	  scenario("Checking roles") {
		assert ( rolesMgr.getRoles().size > 0)
		assert ( rolesMgr.getRoles().find( _.name.equalsIgnoreCase("Administrator")).isDefined )
	  }
	  
	  scenario("Checking permissions") {
	    val testRolePerms = rolesMgr.getPermissions("TestRole")
	  	assert ( testRolePerms.hasPermission(Permission("Order", "Report")) )
	  	assert ( testRolePerms.hasPermission("Order", "Report") )
	  	assert ( testRolePerms.hasPermission("Order.Report") )	  	
	  	assert ( testRolePerms.hasPermission("Read.User") )
	  	
	  	val adminRolePerms = rolesMgr.getPermissions("Administrator")
	  	assert ( adminRolePerms.hasPermission("Anything.Anything") )
	  	assert ( adminRolePerms.hasPermission("Test.Test") )
	  	
	  	val viewerRolePerms = rolesMgr.getPermissions("Viewer")
	  	assert ( !viewerRolePerms.hasPermission("Anything.Anything") )
	  	assert ( viewerRolePerms.hasPermission("Read.Anything") )
	  	
	  	val reportViewerRolePerms = rolesMgr.getPermissions("ReportViewer")
	  	assert ( !reportViewerRolePerms.hasPermission("Anything.Anything") )
	  	assert ( reportViewerRolePerms.hasPermission("Read.Report") )
	  	assert ( reportViewerRolePerms.hasPermission("Order.Report") )
	  	assert ( reportViewerRolePerms.hasPermission("Anything.Report") )
	  }
	  
	  scenario("Checking permissions for mixed roles") {
   	    val testPerms = rolesMgr.getPermissions(Seq("TestRole", "ReportViewer"))
	  	assert ( testPerms.hasPermission("Order.Report") )	  	
	  	assert ( testPerms.hasPermission("Read.User") )
	  	assert ( testPerms.hasPermission("Anything.Report") )
	  	assert ( !testPerms.hasPermission("Anything.Anything") )

	  }
	}
}