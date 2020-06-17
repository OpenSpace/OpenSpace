#Version
1.0

#Asset
util/asset_helper	require	assetHelper
util/property_helper	require	propertyHelper
util/scene_helper	require	sceneHelper
util/renderable_helper	require	renderableHelper
base	require	
scene/solarsystem/planets/earth/earth	require	
scene/solarsystem/planets/earth/satellites/satellites	require	

#Property
setPropertyValue	{earth_satellites}.Renderable.Enabled	false

#Time
relative	-1d

#Camera
goToGeo	"Earth"	58.5877	16.1924	20000000

#MarkNodes
Earth
Mars
Moon
Sun
