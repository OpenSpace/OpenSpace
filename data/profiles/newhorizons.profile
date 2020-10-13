#Version
1.0

#Asset
util/property_helper	propertyHelper
util/renderable_helper	renderableHelper
base	
scene/solarsystem/missions/newhorizons/newhorizons	
scene/solarsystem/missions/newhorizons/model	
scene/solarsystem/missions/newhorizons/dashboard	

#Property
setPropertyValueSingle	NavigationHandler.OrbitalNavigator.FollowAnchorNodeRotationDistance	20.000000
setPropertyValueSingle	Scene.Pluto.Renderable.Enabled	false
setPropertyValueSingle	Scene.Charon.Renderable.Enabled	false
setPropertyValueSingle	Scene.PlutoBarycenterTrail.Renderable.Enabled	false

#Keybinding
a	Sets the focus of the camera on 'NewHorizons'.	Focus on New Horizons	/New Horizons	false	"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'NewHorizons');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"
SHIFT+a	Sets the focus of the camera on 'NewHorizons'.	Anchor at New Horizons, Aim at Pluto	/New Horizons	false	"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'NewHorizons');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', 'Pluto');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"
s	Sets the focus of the camera on 'Pluto'	Focus on Pluto	/New Horizons	false	"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'Pluto') ;openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"
d	Sets the focus of the camera on 'Charon'.	Focus on New Charon	/New Horizons	false	"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'Charon');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"
F7	Toggles New Horizons image projection.	Toggle NH Image Projection	/New Horizons	false	[[local enabled = openspace.getPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.PerformProjection'); openspace.setPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.PerformProjection', not enabled); openspace.setPropertyValue('Scene.CharonProjection.Renderable.ProjectionComponent.PerformProjection', not enabled)]]
F8	Removes all image projections from Pluto and Charon.	Clear image projections	/New Horizons	false	"openspace.setPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.ClearAllProjections', true); openspace.setPropertyValue('Scene.CharonProjection.Renderable.ProjectionComponent.ClearAllProjections', true)"
F9	Jumps to the 14th of July 2015 at 0900 UTC and clears all projections.	Reset time and projections	/New Horizons	false	"openspace.time.setTime('2015-07-14T09:00:00.00');openspace.setPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.ClearAllProjections', true);openspace.setPropertyValue('Scene.CharonProjection.Renderable.ProjectionComponent.ClearAllProjections', true)"
KP_8	Increases the height map exaggeration on Pluto.	Pluto HeightExaggeration +	/New Horizons	false	propertyHelper.increment('Scene.PlutoProjection.Renderable.HeightExaggeration', 5000)
CTRL+I	Increases the height map exaggeration on Pluto.	Pluto HeightExaggeration +	/New Horizons	false	propertyHelper.increment('Scene.PlutoProjection.Renderable.HeightExaggeration', 5000)
KP_2	Decreases the height map exaggeration on Pluto.	Pluto HeightExaggeration -	/New Horizons	false	propertyHelper.decrement('Scene.PlutoProjection.Renderable.HeightExaggeration', 5000)
CTRL+K	Decreases the height map exaggeration on Pluto.	Pluto HeightExaggeration -	/New Horizons	false	propertyHelper.decrement('Scene.PlutoProjection.Renderable.HeightExaggeration', 5000)
KP_9	Increases the height map exaggeration on Charon.	Charon HeightExaggeration +	/New Horizons	false	propertyHelper.increment('Scene.CharonProjection.Renderable.HeightExaggeration', 5000)
CTRL+O	Increases the height map exaggeration on Charon.	Charon HeightExaggeration +	/New Horizons	false	propertyHelper.increment('Scene.CharonProjection.Renderable.HeightExaggeration', 5000)
KP_3	Decreases the height map exaggeration on Charon.	Charon HeightExaggeration -	/New Horizons	false	propertyHelper.decrement('Scene.CharonProjection.Renderable.HeightExaggeration', 5000)
CTRL+L	Decreases the height map exaggeration on Charon.	Charon HeightExaggeration -	/New Horizons	false	propertyHelper.decrement('Scene.CharonProjection.Renderable.HeightExaggeration', 5000)
o	Toggles the visibility of the trail behind Pluto.	Toggle Pluto Trail	/New Horizons	false	propertyHelper.invert('Scene.PlutoBarycentricTrail.Renderable.Enabled')
j	Toggles the visibility of the text labels of Pluto, Charon, Hydra, Nix, Kerberos, and Styx.	Toggle Pluto Labels	/New Horizons	false	renderableHelper.toggle('Scene.PlutoText') .. renderableHelper.toggle('Scene.CharonText') .. renderableHelper.toggle('Scene.HydraText') .. renderableHelper.toggle('Scene.NixText') .. renderableHelper.toggle('Scene.KerberosText') .. renderableHelper.toggle('Scene.StyxText')
l	Toggles the visibility of the labels for the New Horizons instruments.	Toggle New Horizons Labels	/New Horizons	false	propertyHelper.fadeInOut('Scene.Labels.Renderable.Opacity', 2.0)
m	Draws the instrument field of views in a solid color or as lines.	Toggle instrument FOVs	/New Horizons	false	propertyHelper.invert('Scene.NH_LORRI.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_LEISA.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_PAN1.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_PAN2.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_RED.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_BLUE.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_FT.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_METHANE.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_NIR.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_ALICE_AIRGLOW.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_ALICE_SOC.Renderable.SolidDraw')
Shift+t	Toggles the visibility of the shadow visualization of Pluto and Charon.	Toggle Shadows	/New Horizons	false	renderableHelper.toggle('Scene.PlutoShadow') .. renderableHelper.toggle('Scene.CharonShadow')
t	Toggles the trail of New Horizons.	Toggle NH Trail	/New Horizons	false	renderableHelper.toggle('Scene.NewHorizonsTrailPluto')
h	Disables visibility of the trails	Hide Trails	/Rendering	false	"local list = openspace.getProperty('*Trail.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"

#Time
absolute	2015-07-14T08:00:00.00

#Camera
setNavigationState	"NewHorizons"		"Root"	-6.572656E1, -7.239404E1, -2.111890E1	0.102164, -0.362945, 0.926193		

#MarkNodes
Pluto
NewHorizons
Charon

#DeltaTimes
1
5
10
20
40
120
360
540
1080
2160
4320
8640
