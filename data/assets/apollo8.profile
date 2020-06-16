#Version
1.0

#Asset
scene/solarsystem/planets/earth/moon/moon	required
scene/solarsystem/missions/apollo/8/apollo8	required
scene/solarsystem/planets/earth/earth	required

#Property
setPropertyValueSingle	NavigationHandler.OrbitalNavigator.MinimumAllowedDistance	0.000000
setPropertyValueSingle	Scene.Moon.Renderable.LodScaleFactor	24.0

#Keybinding
E	Jump to right before the earthrise photo	Set Earthrise time	/Missions/Apollo/8	false	"openspace.time.setPause(true); openspace.time.setDeltaTime(1); openspace.time.setTime('1968 DEC 24 16:37:31'); openspace.navigation.setNavigationState({Anchor = 'Apollo8', Position = { 1.494592E1, 3.236777E1, -4.171296E1 }, ReferenceFrame = 'Root', Up = { 0.960608E0, -0.212013E0, 0.179675E0 }}); openspace.setPropertyValue('*Trail.Renderable.Enabled', false)"
U	Jump to time right before Apollo 8 liftoff, with its trail enabled	       Set Apollo 8 launch time	/Missions/Apollo/8	false	"openspace.time.setTime('1968-12-21T12:51:37.00'); openspace.setPropertyValueSingle('Scene.Apollo8LaunchTrail.Renderable.Enabled', true)"
K	Toggles Moon Kaguya color layer	Toggle Kaguya layer on the Moon	/Missions/Apollo	false	propertyHelper.invert('Scene.Moon.Renderable.Layers.ColorLayers.Kaguya_Utah.Enabled')
T	Toggles the trails of the Apollo 8 orbits, focused around the Moon	Toggle Apollo 8 orbits	/Missions/Apollo/8	false	propertyHelper.invert('Scene.Apollo8MoonTrail.Renderable.Enabled')
SHIFT+T	Toggles the trails of the Apollo 8 Launch, focused around the Earth	Toggle Apollo 8 launch trail	/Missions/Apollo/8	false	propertyHelper.invert('Scene.Apollo8LaunchTrail.Renderable.Enabled')
CTRL+T	Toggles the trails of the full Apollo 8, with Earth's frame of reference	Toggles Apollo 8 full trail	/Missions/Apollo/8	false	propertyHelper.invert('Scene.Apollo8EarthBarycenterTrail.Renderable.Enabled')
S	Toggles shading for the Moon	Toggle Moon shading	/Missions/Apollo	false	propertyHelper.invert('Scene.Moon.Renderable.PerformShading')
PAGE_UP	Set camera focus to Apollo 8	Focus on Apollo 8	/Missions/Apollo/8	false	"openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Apollo8'); openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"
PAGE_DOWN	Set camera focus to the Moon	Focus on Moon	/Missions/Apollo	false	"openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Moon'); openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"
HOME	Set camera focus to the Earth	Focus on Earth	/Missions/Apollo	false	"openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Earth'); openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"

#Time
absolute	1968-12-21T12:51:51.0

#Camera
goToGeo	"Earth"	20	-60	15000000

#MarkNodes
Earth
Moon
Apollo8
Apollo8Launch
