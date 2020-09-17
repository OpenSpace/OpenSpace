#Version
1.0

#Asset
base	
scene/solarsystem/missions/insight/edl	insightAsset

#Property
setPropertyValueSingle	Scene.PlutoBarycenterTrail.Renderable.Enabled	false
setPropertyValueSingle	Scene.Mars.Renderable.Layers.HeightLayers.Mola_Utah.Settings.Offset	-469.300000
setPropertyValueSingle	Scene.Mars.Renderable.Layers.HeightLayers.OnMarsHiRISELS.Settings.Offset	-470.800006
setPropertyValueSingle	Scene.Mars.Renderable.Layers.HeightLayers.OnMarsHiRISELS.Enabled	true
setPropertyValueSingle	Scene.Mars.Renderable.Layers.ColorLayers.MOC_WA_Color_Utah.Settings.Multiplier	2.81690
setPropertyValueSingle	Scene.Mars.Renderable.Layers.ColorLayers.OnMarsHiRISELS.Settings.Gamma	0.938970
setPropertyValueSingle	Scene.Mars.Renderable.Layers.ColorLayers.MOC_WA_Color_Utah.Settings.Gamma	2.394370
setPropertyValueSingle	Scene.Mars.Renderable.Layers.ColorLayers.OnMarsHiRISELS.Enabled	true


#Keybinding
i	Setup Insight landing layers	Setup Insight layers	/Missions/Insight	false	"openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.Mola_Utah.Settings.Offset', -469.300000); openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.OnMarsHiRISELS.Settings.Offset', -470.800006); openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.ColorLayers.insight_ctx.Enabled', true); openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.OnMarsHiRISELS.Enabled', true); openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.ColorLayers.MOC_WA_Color_Utah.Settings.Multiplier', 2.816900); openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.ColorLayers.OnMarsHiRISELS.Settings.Gamma', 0.938970); openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.ColorLayers.MOC_WA_Color_Utah.Settings.Gamma', 2.394370); openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.ColorLayers.OnMarsHiRISELS.Enabled', true);"
SHIFT+i	Undo Insight landing layers setup	Unset Insight layers	/Missions/Insight	false	"openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.Mola_Utah.Settings.Offset', 0); openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.OnMarsHiRISELS.Settings.Offset', 0); openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.ColorLayers.insight_ctx.Enabled', false); openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.OnMarsHiRISELS.Enabled', false); openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.ColorLayers.OnMarsHiRISELS.Enabled', false); openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.ColorLayers.MOC_WA_Color_Utah.Settings.Multiplier', 1.0); openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.ColorLayers.OnMarsHiRISELS.Settings.Gamma', 1.0); openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.ColorLayers.MOC_WA_Color_Utah.Settings.Gamma', 1.0);"

#Time
absolute	2018-11-26T19:39:03.68

#Camera
setNavigationState	insightAsset.Insight.Identifier		"Root"	8.430115E0, -1.791710E1, 2.813660E0	0.494659E0, 0.357162E0, 0.792306E0		

#MarkNodes
Insight
