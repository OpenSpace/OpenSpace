#Version
1.0

#Module
Gaia		openspace.printFatal('Could not load scene due to missing module "gaia"')

#Asset
scene/solarsystem/planets/earth/earth	required
scene/milkyway/gaia/gaiastars	required
scene/milkyway/gaia/apogee	required
scene/milkyway/gaia/galah	required
scene/solarsystem/missions/gaia/gaia	required
scene/solarsystem/missions/gaia/trail	required

#Property
setPropertyValueSingle	Scene.Stars.Renderable.Enabled	false

#Camera
setNavigationState	"Earth"			1000000000000.0, 1000000000000.0, 1000000000000.0			

#MarkNodes
Gaia
