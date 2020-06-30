#Version
1.0

#Asset
scene/solarsystem/planets/earth/moon/moon	required
scene/solarsystem/missions/apollo/8/apollo8	required
scene/solarsystem/missions/apollo/11/apollo11	required
scene/solarsystem/missions/apollo/17/lem	required
scene/solarsystem/missions/apollo/apollo_globebrowsing	required
scene/solarsystem/missions/apollo/11/lem_flipbook	required
scene/solarsystem/missions/apollo/insignias_map	required

#Property
setPropertyValueSingle	Scene.Moon.Renderable.Layers.ColorLayers.A17_travmap.BlendMode	0
setPropertyValueSingle	Scene.Moon.Renderable.PerformShading	false

#Keybinding
m	Focus on Moon	Focus on Moon	/Missions/Apollo	false	"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'Moon'); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
F9	Disable apollo site on moon when switching	Disable Apollo site	/Missions/Apollo	false	"openspace.setPropertyValue('Scene.Moon.Renderable.Layers.ColorLayers.A17_*.Enabled', false); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.HeightLayers.LRO_NAC_Apollo_11.Enabled', false); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A11_M177481212_p_longlat.Enabled', false); openspace.setPropertyValueSingle('Scene.Apollo11MoonTrail.Renderable.Enabled', false); openspace.setPropertyValueSingle('Scene.Apollo11LemTrail.Renderable.Enabled', false); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.HeightLayers.LRO_NAC_Apollo_17.Enabled', false);"
F11	Setup for A11 site	Setup A11 site	/Missions/Apollo/11	false	"openspace.time.setTime('1969 JUL 20 20:17:40'); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.HeightLayers.LRO_NAC_Apollo_11.Enabled', true); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A11_M177481212_p_longlat.Enabled', true); openspace.setPropertyValueSingle('Scene.Moon.Renderable.LodScaleFactor', 20.11); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'Apollo11LemPosition'); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil); openspace.setPropertyValueSingle('Scene.Apollo11MoonTrail.Renderable.Enabled', true); openspace.setPropertyValueSingle('Scene.Apollo11LemTrail.Renderable.Enabled', true);"
F7	Setup for A17 site	Setup A17 site	/Missions/Apollo/17	false	"openspace.time.setTime('1972 DEC 12 19:47:11'); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A17_travmap.BlendMode', 0.000000); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A17_travmap.Enabled', true); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.HeightLayers.LRO_NAC_Apollo_17.Enabled', true); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A17_LEM.Enabled', true); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A17_LEM.BlendMode', 0.000000); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A17_NAC_Alt_p.Enabled', true); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A17_NAC_Alt_p.BlendMode', 0.000000); openspace.setPropertyValueSingle('Scene.Moon.Renderable.LodScaleFactor', 20.17); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'Apollo17LemModel'); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A17_station7.BlendMode', 0.000000);"

#Time
absolute	1972 DEC 12 19:47:11

#Camera
goToGeo	"Moon"	20	-60	15000000

#MarkNodes
Moon
Apollo11LemModel
Apollo17LemModel
Apollo11
Apollo11LunarLander
