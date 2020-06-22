#Version
1.0

#Asset
base	require	
scene/solarsystem/planets/earth/earth	require	
scene/solarsystem/planets/earth/satellites/satellites   require 
scene/solarsystem/planets/jupiter/minor_moons	require	
scene/solarsystem/planets/saturn/minor_moons	require	
scene/solarsystem/planets/uranus/minor_moons	require	
scene/solarsystem/planets/neptune/inner_moons	require	
scene/solarsystem/planets/neptune/irregular_prograde_moons	require	
scene/solarsystem/planets/neptune/irregular_retrograde_moons	require	

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
