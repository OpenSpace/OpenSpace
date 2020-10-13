#Version
1.0

#Asset
base	
scene/solarsystem/planets/earth/moon/moon	moonAsset
scene/solarsystem/missions/apollo/8/apollo8	
scene/solarsystem/missions/apollo/11/apollo11	
scene/solarsystem/missions/apollo/17/lem	
scene/solarsystem/missions/apollo/apollo_globebrowsing	
scene/solarsystem/missions/apollo/11/lem_flipbook	
scene/solarsystem/missions/apollo/insignias_map	

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
goToGeo	moonAsset.Moon.Identifier	20	-60	15000000

#MarkNodes
Moon
Apollo11LemModel
Apollo17LemModel
Apollo11
Apollo11LunarLander

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
