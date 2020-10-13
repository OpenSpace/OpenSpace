#Version
1.0

#Module
Gaia		openspace.printFatal('Could not load gaia profile due to missing module "gaia"')

#Asset
base	
scene/solarsystem/planets/earth/earth	
scene/milkyway/gaia/gaiastars	
scene/milkyway/gaia/apogee	
scene/milkyway/gaia/galah	
scene/solarsystem/missions/gaia/gaia	
scene/solarsystem/missions/gaia/trail	
scene/solarsystem/missions/gaia/dashboard	

#Property
setPropertyValueSingle	Scene.Stars.Renderable.Enabled	false

#Camera
setNavigationState	"Earth"			1000000000000.0, 1000000000000.0, 1000000000000.0			

#MarkNodes
Gaia

#DeltaTimes
1
2
5
10
30
60
120
300
600
1800
3600
7200
10800
21600
43200
86400
172800
345600
604800
1209600
2592000
5184000
7776000
15552000
31536000
63072000
157680000
315360000
630720000
1576800000
