#Version
1.0

#Asset
scene/solarsystem/missions/messenger/mercurymagnetosphere	required
scene/solarsystem/sun/heliosphere/mas/bastille_day/density_volume	required
scene/solarsystem/sun/heliosphere/mas/bastille_day/fieldlines	required
scene/solarsystem/sun/heliosphere/mas/bastille_day/transforms	required
scene/solarsystem/sun/heliosphere/mas/bastille_day/magnetogram	required
scene/solarsystem/sun/heliosphere/mas/bastille_day/bastille_day_sun_textures	required
scene/solarsystem/sun/magnetogram_textures	required
examples/debugcoordinateaxes	required

#Property
setPropertyValueSingle	Scene.Sun.Renderable.Enabled	true
setPropertyValueSingle	Scene.Sun.Renderable.Layers.ColorLayers.Texture.Enabled	false
setPropertyValueSingle	Scene.SunGlare.Renderable.Enabled	false
setPropertyValueSingle	Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-2.Enabled	true

#Keybinding
Shift+s	Resets the subscene for showing the Sun volume rendering + fieldlines	Reset MHD back	/CCMC/MHD	false	"openspace.setPropertyValueSingle('Scene.SunGlare.Renderable.Enabled', true);openspace.setPropertyValueSingle('Scene.Sun.Renderable.Enabled', false);openspace.setPropertyValueSingle('Scene.MAS_MHD_density.Renderable.Enabled', false);openspace.setPropertyValueSingle('Scene.MAS_MHD_Fieldlines.Renderable.Enabled', false);"
F6	Toggle density volume	Toggle density volume	/CCMC/MHD	false	propertyHelper.invert('Scene.MAS_MHD_density.Renderable.Enabled')	
F7	Toggle suns fieldlines	Toggle suns fieldlines	/CCMC/MHD	false	propertyHelper.invert('Scene.MAS_MHD_Fieldlines.Renderable.Enabled')
F8	Toggle sun glare	Toggle sun glare	/CCMC/MHD	false	propertyHelper.invert('Scene.SunGlare.Renderable.Enabled')
F9	Toggle sun	Toggle sun	/CCMC/MHD	false	propertyHelper.invert('Scene.Sun.Renderable.Enabled')
i	Display next sun texture in list of textures	Display next sun texture in list of textures	/CCMC/MHD	false	[[local textureList = openspace.globebrowsing.getLayers('Sun', 'ColorLayers'); if (textureIndex == nil) then textureIndex = 2; end; textureIndex = textureIndex + 1; if (textureIndex >= #textureList) then textureIndex = 0; end; if (textureIndex == 0) then openspace.setPropertyValue("Scene.Sun.Renderable.Layers.ColorLayers.*.Enabled", false); openspace.setPropertyValueSingle("Scene.Sun.Renderable.Layers.ColorLayers.Texture.Enabled", true); else openspace.setPropertyValue("Scene.Sun.Renderable.Layers.ColorLayers.*.Enabled", false); str = "Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-" .. textureIndex .. ".Enabled"; openspace.setPropertyValueSingle(str, true); end;]]

#Time
absolute	2000 JUL 14 08:33:37

#Camera
setNavigationState	"Sun"		"Root"	-15016171000, 30814181000, -4218126500	0.102164, -0.362945, 0.926193		

#MarkNodes
Earth
Sun
Moon