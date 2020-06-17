#Version
1.0

#Module
Touch	local webGui = asset.require('util/webgui'); webGui.setCefRoute("ontouch")	openspace.printFatal('Could not load scene due to missing module "touch"')

#Asset
util/asset_helper	require	assetHelper
util/property_helper	require	propertyHelper
util/scene_helper	require	sceneHelper
util/renderable_helper	require	renderableHelper
base	require	
scene/solarsystem/planets/earth/earth	require	
util/webgui	require	

#Property
setPropertyValueSingle	Scene.Pluto.Renderable.Enabled	false
setPropertyValueSingle	Scene.Charon.Renderable.Enabled	false
setPropertyValueSingle	Scene.PlutoBarycenterTrail.Renderable.Enabled	false

#Time
relative	-1d

#Camera
setNavigationState	"Earth"			58.5877,16.1924,20000000			

#MarkNodes
Earth
Mars
Moon
