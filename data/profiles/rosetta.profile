#Version
1.0

#Module
Volume	asset.require('scene/solarsystem/missions/messenger/mercurymagnetosphere')	openspace.printWarning("Volume module is not loaded, skipping asset: mercurymagnetosphere")

#Asset
util/property_helper	require	propertyHelper
util/renderable_helper	require	renderableHelper
base	require	
scene/solarsystem/missions/rosetta/67p	require	
scene/solarsystem/missions/rosetta/rosetta	require	

#Property
setPropertyValue	Scene.67P.Renderable.PerformShading	false
setPropertyValue	Scene.ImagePlaneRosetta.Renderable.Enabled	false

#Keybinding
a	Sets the focus of the camera on '67P'.	Focus on 67P	/Missions/Rosetta	false	"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', '67P'); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"
s	Sets the focus of the camera on 'Rosetta'	Focus on Rosetta	/Missions/Rosetta	false	"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'Rosetta'); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"
F5	Jumps to the time of initial approach of Rosetta to 67P.	Set initial approach time	/Missions/Rosetta	false	"openspace.time.setTime('2014-08-01T03:05:18.101')"
F6	Jumps to the time when the Philae lander is released.	Set lander release time	/Missions/Rosetta	false	"openspace.time.setTime('2014-11-12T08:20:00.00')"
F8	Removes all image projections from 67P.	Clear 67P projections	/Missions/Rosetta	false	"openspace.setPropertyValue('Scene.67P.Renderable.ProjectionComponent.ClearAllProjections', true)"
e	Toggles the visibility of all trails further from the Sun than 67P.	Toggle outer planetary trails	/Missions/Rosetta	false	renderableHelper.toggle('Scene.JupiterTrail')..renderableHelper.toggle('Scene.SaturnTrail')..renderableHelper.toggle('Scene.UranusTrail')..renderableHelper.toggle('Scene.NeptuneTrail')
i	Toggles the visibility of the free floating image plane.	Toggle image plane	/Missions/Rosetta	false	renderableHelper.toggle('Scene.ImagePlaneRosetta')
g	Toggles the visibility of Philae's trail.	Toggle Philae trail	/Missions/Rosetta	false	renderableHelper.toggle('Scene.PhilaeTrail')
p	Enables or disables the image projection on 67P.	Toggle 67P projection	/Missions/Rosetta	false	propertyHelper.invert('Scene.67P.Renderable.ProjectionComponent.PerformProjection')

#Time
absolute	2014-08-01T03:05:00.000

#Camera
setNavigationState	"67P"		"Root"	-7.294781E5 , -6.657894E5, 2.509047E6	0.146529E0, 0.944727E0, 0.293290E0		

#MarkNodes
67P
Rosetta
Philae
