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
import scala.io.Source

class TestsRolesManager extends FeatureSpec {
	feature("Test basic roles manager" ) {	  
	  val rolesMgr : RolesManager = new BasicRolesManager(
	      getClass.getResource("/test-roles.xml").openStream()  /*classOf[TestsRolesManager].getResourceAsStream("test-roles.xml")*/
	  )
	  
	  scenario("Checking roles") {
		assert ( rolesMgr.getRoles().size > 0)
		assert ( rolesMgr.getRoles().find( _.name.equalsIgnoreCase("Administrator")).isDefined )
	  }
	  
	  scenario("Checking permissions") {
	    val testRolePerms = rolesMgr.getPermissions("TestRole")
	  	assert ( testRolePerms.hasPermission(Permission("Order", "Report")) )
	  	assert ( testRolePerms.hasPermission("Order", "Report") )
	  	assert ( testRolePerms.hasPermissionStrForm("Order.Report") )	  	
	  	assert ( testRolePerms.hasPermissionStrForm("Read.User") )
	  	
	  	val adminRolePerms = rolesMgr.getPermissions("Administrator")
	  	assert ( adminRolePerms.hasPermissionStrForm("Anything.Anything") )
	  	assert ( adminRolePerms.hasPermissionStrForm("Test.Test") )	  	
	  	
	  	val viewerRolePerms = rolesMgr.getPermissions("Viewer")
	  	assert ( !viewerRolePerms.hasPermissionStrForm("Anything.Anything") )
	  	assert ( viewerRolePerms.hasPermissionStrForm("Read.Anything") )
	  	
	  	val reportViewerRolePerms = rolesMgr.getPermissions("ReportViewer")
	  	assert ( !reportViewerRolePerms.hasPermissionStrForm("Anything.Anything") )
	  	assert ( reportViewerRolePerms.hasPermissionStrForm("Read.Report") )
	  	assert ( reportViewerRolePerms.hasPermissionStrForm("Order.Report") )
	  	assert ( reportViewerRolePerms.hasPermissionStrForm("Anything.Report") )
	  }
	  
	  scenario("Checking permissions for mixed roles") {
   	    val testPerms = rolesMgr.getPermissions(Seq("TestRole", "ReportViewer"))
	  	assert ( testPerms.hasPermissionStrForm("Order.Report") )	  	
	  	assert ( testPerms.hasPermissionStrForm("Read.User") )
	  	assert ( testPerms.hasPermissionStrForm("Anything.Report") )
	  	assert ( testPerms.hasPermissionsStrForm(Seq("Read.User", "Anything.Report", "Order.Report")) )
	  	assert ( !testPerms.hasPermissionStrForm("Anything.Anything") )

	  }
	}
}