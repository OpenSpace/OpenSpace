#Version
1.0

#Asset
base	
scene/solarsystem/planets/earth/earth	earthAsset
scene/solarsystem/planets/earth/satellites/satellites	

#Property
setPropertyValue	{earth_satellites}.Renderable.Enabled	false

#Time
relative	-1d

#Camera
goToGeo	earthAsset.Earth.Identifier	58.5877	16.1924	20000000

#MarkNodes
Earth
Mars
Moon
Sun

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
