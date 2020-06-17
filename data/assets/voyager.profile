#Version
1.0

#Asset
util/asset_helper	require	assetHelper
util/property_helper	require	propertyHelper
util/scene_helper	require	sceneHelper
util/renderable_helper	require	renderableHelper
base	require	
scene/solarsystem/planets/jupiter/minor_moons	require	
scene/solarsystem/planets/saturn/minor_moons	require	
scene/solarsystem/planets/uranus/minor_moons	require	
scene/solarsystem/planets/neptune/inner_moons	require	
scene/solarsystem/planets/neptune/irregular_prograde_moons	require	
scene/solarsystem/planets/neptune/irregular_retrograde_moons	require	
scene/solarsystem/missions/voyager/voyager1	require	
scene/solarsystem/missions/voyager/voyager2	require	

#Keybinding
1	Setting the simulation speed to 1 seconds per realtime second	Set sim speed 1	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(1)"
2	Setting the simulation speed to 5 seconds per realtime second	Set sim speed 5	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(5)"
3	Setting the simulation speed to 10 seconds per realtime second	Set sim speed 10	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(10)"
4	Setting the simulation speed to 20 seconds per realtime second	Set sim speed 20	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(20)"
5	Setting the simulation speed to 40 seconds per realtime second	Set sim speed 40	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(40)"
6	Setting the simulation speed to 90 seconds per realtime second	Set sim speed 90	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(90)"
7	Setting the simulation speed to 360 seconds per realtime second	Set sim speed 360	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(360)"
8	Setting the simulation speed to 720 seconds per realtime second	Set sim speed 720	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(720)"
9	Setting the simulation speed to 2880 seconds per realtime second	Set sim speed 2880	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(2880)"
0	Setting the simulation speed to 14400 seconds per realtime second	Set sim speed 14400	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(14400)"
Shift+1	Setting the simulation speed to 28800 seconds per realtime second	Set sim speed 28800	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(28800)"
Shift+2	Setting the simulation speed to 57600 seconds per realtime second	Set sim speed 57600	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(57600)"
Shift+3	Setting the simulation speed to 115200 seconds per realtime second	Set sim speed 115200	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(115200)"
Shift+4	Setting the simulation speed to 230400 seconds per realtime second	Set sim speed 230400	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(230400)"
Shift+5	Setting the simulation speed to 460800 seconds per realtime second	Set sim speed 460800	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(460800)"
Shift+6	Setting the simulation speed to 921600 seconds per realtime second	Set sim speed 921600	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(921600)"
Shift+7	Setting the simulation speed to 1843200 seconds per realtime second	Set sim speed 1843200	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(1843200)"
Shift+8	Setting the simulation speed to 3686400 seconds per realtime second	Set sim speed 3686400	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(3686400)"
Shift+9	Setting the simulation speed to 7372800 seconds per realtime second	Set sim speed 7372800	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(7372800)"
Shift+0	Setting the simulation speed to 14745600 seconds per realtime second	Set sim speed 14745600	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(14745600)"

#Property
setPropertyValueSingle	Scene.Pluto.Renderable.Enabled	false
setPropertyValueSingle	Scene.Charon.Renderable.Enabled	false
setPropertyValueSingle	Scene.PlutoBarycenterTrail.Renderable.Enabled	false

#Time
absolute	1977 SEP 10 12:00:00

#Camera
setNavigationState	"Voyager_1"	"Root"		526781518487.171326, 257168309890.072144, -1381125204152.817383			

#MarkNodes
Earth
Voyager 1
Voyager 2
Jupiter
Saturn
Uranus
Neptune
