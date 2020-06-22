#Version
1.0

#Module
Gaia		openspace.printFatal('Could not load scene due to missing module "gaia"')

#Asset
base	require	
scene/solarsystem/planets/earth/earth	require	
scene/milkyway/gaia/gaiastars	require	
scene/milkyway/gaia/apogee	require	
scene/milkyway/gaia/galah	require	
scene/solarsystem/missions/gaia/gaia	require	
scene/solarsystem/missions/gaia/trail	require	

#Property
setPropertyValueSingle	Scene.Stars.Renderable.Enabled	false

#Camera
setNavigationState	"Earth"			1000000000000.0, 1000000000000.0, 1000000000000.0			

#MarkNodes
Gaia
