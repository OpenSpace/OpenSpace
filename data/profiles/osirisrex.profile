#Version
1.0

#Asset
util/property_helper	propertyHelper
base	
scene/solarsystem/missions/osirisrex/model	OsirisRexAsset
scene/solarsystem/missions/osirisrex/osirisrex	
scene/solarsystem/missions/osirisrex/dashboard	

#Property
setPropertyValueSingle	NavigationHandler.OrbitalNavigator.FollowAnchorNodeRotationDistance	20.000000
setPropertyValueSingle	Scene.Pluto.Renderable.Enabled	false
setPropertyValueSingle	Scene.Charon.Renderable.Enabled	false
setPropertyValueSingle	Scene.PlutoBarycenterTrail.Renderable.Enabled	false

#Keybinding
a	Sets the focus of the camera on 'OsirisRex'	Focus on OsirisRex	/Missions/Osiris Rex	false	"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'OsirisRex'); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"
s	Sets the focus of the camera on 'Bennu'	Focus on Bennu	/Missions/Osiris Rex	false	"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'BennuBarycenter'); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"
F8	Sets the time to the approach at Bennu	Set Bennu approach time	/Missions/Osiris Rex	false	"openspace.printInfo('Set time: Approach'); openspace.time.setTime('2018-SEP-11 21:31:01.183')"
F9	Sets the time to the preliminary survey of Bennu	Set Bennu survey time	/Missions/Osiris Rex	false	"openspace.printInfo('Set time: Preliminary Survey'); openspace.time.setTime('2018-NOV-20 01:13:12.183')"
F10	Sets the time to the orbital B event	Set orbital B event time	/Missions/Osiris Rex	false	"openspace.printInfo('Set time: Orbital B'); openspace.time.setTime('2019-APR-08 10:35:27.186')"
F11	Sets the time to the recon event	Set recon event time	/Missions/Osiris Rex	false	"openspace.printInfo('Set time: Recon'); openspace.time.setTime('2019-MAY-25 03:50:31.195')"
q	Toggles the visibility of the text marking the location of the Sun	Toggle Sun marker	/Missions/Osiris Rex	false	propertyHelper.invert('Scene.SunMarker.Renderable.Enabled')
1	Setting the simulation speed to 1 seconds per realtime second	Set sim speed 1	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(1)"
2	Setting the simulation speed to 5 seconds per realtime second	Set sim speed 5	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(5)"
3	Setting the simulation speed to 10 seconds per realtime second	Set sim speed 10	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(10)"
4	Setting the simulation speed to 20 seconds per realtime second	Set sim speed 20	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(20)"
5	Setting the simulation speed to 40 seconds per realtime second	Set sim speed 40	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(40)"
6	Setting the simulation speed to 60 seconds per realtime second	Set sim speed 60	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(60)"
7	Setting the simulation speed to 120 seconds per realtime second	Set sim speed 120	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(120)"
8	Setting the simulation speed to 360 seconds per realtime second	Set sim speed 360	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(360)"
9	Setting the simulation speed to 540 seconds per realtime second	Set sim speed 540	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(540)"
0	Setting the simulation speed to 1080 seconds per realtime second	Set sim speed 1080	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(1080)"
Shift+1	Setting the simulation speed to 2160 seconds per realtime second	Set sim speed 2160	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(2160)"
Shift+2	Setting the simulation speed to 4320 seconds per realtime second	Set sim speed 4320	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(4320)"
Shift+3	Setting the simulation speed to 8640 seconds per realtime second	Set sim speed 8640	/Simulation Speed	false	"openspace.time.interpolateDeltaTime(8640)"

#Time
absolute	2018 10 30 23:00:00.500

#Camera
setNavigationState	OsirisRexAsset.OsirisRex.Identifier			26974590199.661884, 76314608558.908020, -127086452897.101791			

#MarkNodes
OsirisRex
BennuBarycenter
Earth
