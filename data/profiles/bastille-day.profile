{
  "assets": [
    "base",
    "dashboard/default_dashboard",
    "scene/solarsystem/sun/sun_textures",
    "scene/solarsystem/sun/EUV_layer",
    "scene/solarsystem/heliosphere/bastille_day/bastille_day_sun_textures",
    "scene/solarsystem/heliosphere/bastille_day/density_volume",
    "scene/solarsystem/heliosphere/bastille_day/fieldlines",
    "scene/solarsystem/heliosphere/bastille_day/focuspoint",
    "scene/solarsystem/heliosphere/bastille_day/lightindicator",
    "scene/solarsystem/heliosphere/bastille_day/magnetogram",
    "scene/solarsystem/heliosphere/bastille_day/magnetogram_textures",
    "scene/solarsystem/heliosphere/bastille_day/fluxnodes",
    "scene/solarsystem/heliosphere/bastille_day/fluxnodescutplane",
    "scene/solarsystem/heliosphere/bastille_day/fluxnodeslegend",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/magnetosphere",
    "scene/solarsystem/planets/earth/satellites/satellites",
    "scene/solarsystem/planets/earth/satellites/misc/iss"
  ],
  "camera": {
    "altitude": 3400000000.0,
    "anchor": "Sun",
    "latitude": 20.5877,
    "longitude": -35.1924,
    "type": "goToGeo"
  },
  "delta_times": [
    1.0,
    5.0,
    30.0,
    60.0,
    300.0,
    1800.0,
    3600.0,
    43200.0,
    86400.0,
    604800.0,
    1209600.0,
    2592000.0,
    5184000.0,
    7776000.0,
    15552000.0,
    31536000.0,
    63072000.0,
    157680000.0,
    315360000.0,
    630720000.0
  ],
  "keybindings": [
    {
      "documentation": "Toggle trails on or off for satellites around Earth",
      "gui_path": "/Earth",
      "is_local": false,
      "key": "S",
      "name": "Toggle satellite trails",
      "script": "local list = openspace.getProperty('{earth_satellites}.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Show the legend image",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "F2",
      "name": "Show the legend image",
      "script": "openspace.setPropertyValueSingle('ScreenSpace.Legendfluxnodes.Opacity', 0.000000);openspace.setPropertyValueSingle('ScreenSpace.Legendfluxnodes.Opacity', 1.000000, 4);openspace.setPropertyValueSingle('ScreenSpace.Legendfluxnodes.Enabled', true);"
    },
    {
      "documentation": "Hides the legend image",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "F3",
      "name": "Hides the legend image",
      "script": "openspace.setPropertyValueSingle('ScreenSpace.Legendfluxnodes.Opacity', 0.000000, 2);"
    },
    {
      "documentation": "Start to focus on Earth",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "F4",
      "name": "Change to Earth focus",
      "script": "openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Earthfocus.distancePlanetThreshold', 155022826061.149994);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Earthfocus.enhanceMethod', 0.000000);openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Earth');openspace.navigation.loadNavigationState('Earth');"
    },
    {
      "documentation": "Toggle volume rendering of CME",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "F6",
      "name": "Toggle volume",
      "script": "propertyHelper.invert('Scene.MAS_MHD_density.Renderable.Enabled');"
    },
    {
      "documentation": "Loop time 10:03 - 11:00",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "F7",
      "name": "Loop Nodedata time 10:03 - 11:00",
      "script": "EndtimeSlowLoop = '2000 JUL 14 11:00:00';StarttimescriptSlowLoop = \"openspace.time.setTime('2000 JUL 14 10:03:00')\";openspace.scriptScheduler.loadScheduledScript(EndtimeSlowLoop, StarttimescriptSlowLoop);"
    },
    {
      "documentation": "Loop time 08:34- 19:00",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "F8",
      "name": "Loop Nodedata time 08:34- 19:00",
      "script": "Endtime1 = '2000 JUL 14 19:00:00';Starttimescript = \"openspace.time.setTime('2000 JUL 14 08:38:27')\";openspace.scriptScheduler.loadScheduledScript(Endtime1, Starttimescript);"
    },
    {
      "documentation": "Clear scriptScheduler",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "F9",
      "name": "Clear loop scripts",
      "script": "openspace.scriptScheduler.clear();"
    },
    {
      "documentation": "Restart button to start of CME",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "F10",
      "name": "Restart button to start of CME",
      "script": "openspace.time.setTime('2000-JUL-14 10:03:00.00');"
    },
    {
      "documentation": "Display next sun texture in list of textures",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "I",
      "name": "Next sun texture",
      "script": "textureList = openspace.globebrowsing.getLayers('Sun', 'ColorLayers');if (textureIndex == nil) then textureIndex = 2;end;textureIndex = textureIndex + 1;if (textureIndex >= #textureList) then textureIndex = 0;end;if (textureIndex == 0) then openspace.setPropertyValue(\"Scene.Sun.Renderable.Layers.ColorLayers.*.Enabled\", false);openspace.setPropertyValueSingle(\"Scene.Sun.Renderable.Layers.ColorLayers.Texture.Enabled\", true);else openspace.setPropertyValue(\"Scene.Sun.Renderable.Layers.ColorLayers.*.Enabled\", false);str = \"Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-\" .. textureIndex .. \".Enabled\";openspace.setPropertyValueSingle(str, true);end;"
    },
    {
      "documentation": "Transition Magnetosphere",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "SHIFT+A",
      "name": "Transition Magnetosphere",
      "script": "propertyHelper.invert('Scene.Earths_Magnetosphere.Renderable.Enabled');openspace.setPropertyValueSingle('Scene.Earths_Magnetosphere.Renderable.lineWidth', 1.500000);openspace.setPropertyValueSingle('Scene.Earths_Magnetosphere.Renderable.Color.uniform', {0.300000,0.570000,0.750000,0.00000});openspace.setPropertyValueSingle('Scene.Earths_Magnetosphere.Renderable.Color.uniform', {0.300000,0.570000,0.750000,0.50000}, 8);"
    },
    {
      "documentation": "transition when zooming out nodes+cutplane, nmbr 12",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "SHIFT+B",
      "name": "Transition zooming out nodes + cutplane",
      "script": "openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Streams.filterUpper', 5, 5);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Streams.thresholdFlux', -1);openspace.setPropertyValueSingle('Scene.MAS_MHD_Fieldlines.Renderable.Enabled', false);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.NodeGroup.nodeSizeLargerFlux', 2.00000, 10);"
    },
    {
      "documentation": "New loop: Restarts time at 10:03 and stops at 10:40, sets delta time to 4 min/ second (240 seconds/ second)",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "SHIFT+D",
      "name": "Loop 10:03 - 10:40, delta time 4 min/ second",
      "script": "openspace.scriptScheduler.clear();openspace.time.setDeltaTime(240);openspace.time.setTime('2000-JUL-14 10:03:00.00');starttimeEarth = '2000 JUL 14 10:40:00';StarttimescriptSlowLoop = \"openspace.time.setTime('2000 JUL 14 10:03:00')\";openspace.scriptScheduler.loadScheduledScript(starttimeEarth, StarttimescriptSlowLoop);"
    },
    {
      "documentation": "Turn off sun glare, turn on field lines, increase field lines line width, change layer on the Sun to magnetogram 3",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "SHIFT+E",
      "name": "Third transition Emilie",
      "script": "openspace.setPropertyValueSingle('Scene.SunGlare.Renderable.Enabled', false);openspace.setPropertyValueSingle('Scene.MAS_MHD_Fieldlines.Renderable.lineWidth', 4.000000);openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'focusPoint');openspace.setPropertyValueSingle('Scene.MAS_MHD_Fieldlines.Renderable.Enabled', true);openspace.setPropertyValueSingle('Scene.MAS_MHD_Fieldlines.Renderable.Color.uniform', {0.300000,0.570000,0.750000,0.000000});openspace.setPropertyValueSingle('Scene.MAS_MHD_Fieldlines.Renderable.Color.uniform', {0.300000,0.570000,0.750000,0.500000}, 6);openspace.setPropertyValueSingle('Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-1.Enabled', false);openspace.setPropertyValueSingle('Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-2.Enabled', false);openspace.setPropertyValueSingle('Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-4.Enabled', false);openspace.setPropertyValueSingle('Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-5.Enabled', false);openspace.setPropertyValueSingle('Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-6.Enabled', false);openspace.setPropertyValueSingle('Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-3.Enabled', true);"
    },
    {
      "documentation": "Enable nodes with some filters",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "SHIFT+H",
      "name": "Last transition Emilie",
      "script": "openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Enabled', true);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.NodeGroup.nodeSize', 5.200000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.NodeGroup.nodeSizeLargerFlux', 5.200000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Streams.filterUpper', 0.190000);"
    },
    {
      "documentation": "New loop: Restarts time at 10:03 and stops at 10:16, sets delta time to 2 min/ second (120 seconds/ second)",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "SHIFT+M",
      "name": "Loop 10:03 - 10:16, delta time 2 min/ second",
      "script": "openspace.scriptScheduler.clear();openspace.time.setDeltaTime(120);openspace.time.setTime('2000-JUL-14 10:03:00.00');EndtimeShortLoop = \"2000 JUL 14 10:16:00\";StarttimescriptSlowLoop = \"openspace.time.setTime('2000 JUL 14 10:03:00')\";openspace.scriptScheduler.loadScheduledScript(EndtimeShortLoop, StarttimescriptSlowLoop);"
    },
    {
      "documentation": "transition from cutplane to nodes",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "SHIFT+N",
      "name": "Transition cutplane to nodes",
      "script": "openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Streams.zLimit', {-2.00000,2.000000});openspace.setPropertyValueSingle('Scene.NodesMAS_MHD_FluxNodes.Renderable.Earthfocus.enhanceMethod', 3.000000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Color.fluxColorAlphaIlluminance', 1, 5);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.NodeGroup.nodeSkipEarth', 11.000000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable. CameraPerspective.maxNodeSize', 40.0000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Streams.thresholdFlux', -1.00000);openspace.setPropertyValueSingle('Scene.SunGlare.Renderable.Enabled', true);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.NodeGroup.amountOfNodes', 17.000000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable. CameraPerspective.renderingcircles', true);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.CameraPerspective.cameraPerspectiveEnabled', true);openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Earth');openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.NodeGroup.nodeSkip', 17.000000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Earthfocus.distancePlanetThreshold', 0.20000, 10);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable. CameraPerspective.minNodeSize', 3.00000);openspace.setPropertyValueSingle('Scene.Cutplane.Renderable.Opacity', 0.000000, 7);"
    },
    {
      "documentation": "Appearance change for trails",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "SHIFT+Q",
      "name": "Appearance change for trails",
      "script": "openspace.setPropertyValueSingle('Scene.MercuryTrail.Renderable.Appearance.EnableFade', true);openspace.setPropertyValueSingle('Scene.MercuryTrail.Renderable.Appearance.Color', {0.700000,0.700000,0.700000});openspace.setPropertyValueSingle('Scene.MercuryTrail.Renderable.Appearance.LineWidth', 10.000000)openspace.setPropertyValueSingle('Scene.MercuryTrail.Renderable.Appearance.Fade', 2.270000);openspace.setPropertyValueSingle('Scene.MarsTrail.Renderable.Appearance.EnableFade', true);openspace.setPropertyValueSingle('Scene.MarsTrail.Renderable.Appearance.LineWidth', 10.000000);openspace.setPropertyValueSingle('Scene.MarsTrail.Renderable.Appearance.Fade', 2.710000);openspace.setPropertyValueSingle('Scene.VenusTrail.Renderable.Appearance.EnableFade', true);openspace.setPropertyValueSingle('Scene.VenusTrail.Renderable.Appearance.LineWidth', 10.000000);openspace.setPropertyValueSingle('Scene.VenusTrail.Renderable.Appearance.Fade', 1.830000);openspace.setPropertyValueSingle('Scene.EarthTrail.Renderable.Appearance.EnableFade', true);openspace.setPropertyValueSingle('Scene.EarthTrail.Renderable.Appearance.LineWidth', 10.000000);openspace.setPropertyValueSingle('Scene.EarthTrail.Renderable.Appearance.Fade', 3.440000);openspace.setPropertyValueSingle('Scene.MoonTrail.Renderable.Appearance.Color', {1.000000,1.000000,1.000000});openspace.setPropertyValueSingle('Scene.MoonTrail.Renderable.Appearance.LineWidth', 10.00000);openspace.setPropertyValueSingle('Scene.MoonTrail.Renderable.Appearance.Fade', 5.420000);"
    },
    {
      "documentation": "Resets the visualisation of fluxnodes",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "SHIFT+R",
      "name": "Reset Fluxnodes Vis back",
      "script": "openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Color.colorMode', 0.000000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.NodeGroup.skippingNodes', 1.000000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.NodeGroup.amountOfNodes', 1.000000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Color.fluxColorAlphaIlluminance', 1.000000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.NodeGroup.nodeSkip', 1.000000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.NodeGroup.nodeSize', 2.000000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.NodeGroup.nodeSizeLargerFlux', 2.00000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.NodeGroup.skippingNodesByFlux', 0.000000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.NodeGroup.skippingNodesByRadius', 0.000000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Streams.thresholdFlux', 0.800000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Streams.filterLower', 0.000000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Streams.zLimit', {-2.00000,2.000000});openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Earthfocus.distancePlanetThreshold', 0.000000);openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Sun');openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Streams.zLimit', {-2.00000,2.000000});openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Streams.filterUpper', 5.000000);"
    },
    {
      "documentation": "New loop: Restarts time at 10:03 and stops at 11:00, delta time to 4 min/ second (240 seconds/ second)",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "SHIFT+S",
      "name": "Loop 10:03 - 11:00, delta time 4 min/ second",
      "script": "StarttimescriptSlowLoop = \"openspace.time.setTime('2000 JUL 14 10:03:00')\";openspace.scriptScheduler.clear();openspace.time.setDeltaTime(240);openspace.time.setTime('2000-JUL-14 10:03:00.00');openspace.scriptScheduler.loadScheduledScript('2000 JUL 14 11:00:00', StarttimescriptSlowLoop);"
    },
    {
      "documentation": "transition from nodes to cutplane",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "SHIFT+T",
      "name": "Transition nodeviz to cutplane",
      "script": "openspace.setPropertyValueSingle('Scene.Cutplane.Renderable.Enabled', true);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Earthfocus.distancePlanetThreshold', 0.000000, 5);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Earthfocus.enhanceMethod', 3.000000);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Color.fluxColorAlphaIlluminance', 0.0, 6);openspace.setPropertyValueSingle('Scene.Cutplane.Renderable.Opacity', 1.000000, 10);"
    },
    {
      "documentation": "Fast loop: Starts from 10:03 and sets delta time to 15 min/ second (900 seconds/ second)",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "SHIFT+V",
      "name": "Starts from 10:03, delta time 15 min/ second",
      "script": "openspace.scriptScheduler.clear();openspace.time.setDeltaTime(900);openspace.time.setTime('2000-JUL-14 10:03:00.00');StarttimescriptSlowLoop = \"openspace.time.setTime('2000 JUL 14 10:03:00')\";openspace.scriptScheduler.loadScheduledScript('2000 JUL 14 12:00:00', StarttimescriptSlowLoop);"
    },
    {
      "documentation": "Turn on sun glare, reset field line line width, change layer on the Sun to Orange",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "SHIFT+W",
      "name": "First transition Emilie",
      "script": "openspace.time.setDeltaTime(0);openspace.time.setTime('2000-JUL-14 08:42:00.00');openspace.setPropertyValueSingle('Scene.SunGlare.Renderable.Enabled', true);openspace.setPropertyValueSingle('Scene.MAS_MHD_Fieldlines.Renderable.lineWidth', 1);openspace.setPropertyValueSingle('Scene.Cutplane.Renderable.Enabled', false);openspace.setPropertyValueSingle('Scene.MAS_MHD_density.Renderable.Enabled', false);openspace.setPropertyValueSingle('Scene.MAS_MHD_Fieldlines.Renderable.Enabled', false);openspace.setPropertyValueSingle('Scene.MAS_MHD_FluxNodes.Renderable.Enabled', false);openspace.setPropertyValueSingle('Scene.travelSpeedIndicator.Renderable.Enabled', false);openspace.setPropertyValueSingle('Scene.Earths_Magnetosphere.Renderable.Enabled', false);openspace.setPropertyValueSingle('Scene.Earths_Magnetosphere.Renderable.maskingEnabled', true);openspace.setPropertyValueSingle('Scene.Earths_Magnetosphere.Renderable.Masking.maskingQuantity', 5.000000);openspace.setPropertyValueSingle('Scene.Earths_Magnetosphere.Renderable.Masking.maskingMinLimit', '0');openspace.setPropertyValueSingle('Scene.Earths_Magnetosphere.Renderable.Masking.maskingMaxLimit', '0.5');openspace.setPropertyValueSingle('Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-1.Enabled', false);openspace.setPropertyValueSingle('Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-2.Enabled', false);openspace.setPropertyValueSingle('Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-3.Enabled', false);openspace.setPropertyValueSingle('Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-4.Enabled', false);openspace.setPropertyValueSingle('Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-5.Enabled', false);openspace.setPropertyValueSingle('Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-6.Enabled', false);openspace.setPropertyValueSingle('Scene.Sun.Renderable.Layers.ColorLayers.Texture.Enabled', true);"
    },
    {
      "documentation": "Long loop: Restarts time at 09:30 and stops at 11:50, delta time to 4 min/ second (240 seconds/ second)",
      "gui_path": "/CCMC/Nodes",
      "is_local": false,
      "key": "SHIFT+Y",
      "name": "Loop 09:30 - 11:50, delta time 4 min/ second",
      "script": "openspace.scriptScheduler.clear();openspace.time.setDeltaTime(240);openspace.time.setTime('2000-JUL-14 09:30:00.00');StarttimescriptLongLoop = \"openspace.time.setTime('2000 JUL 14 09:30:00')\";openspace.scriptScheduler.loadScheduledScript('2000 JUL 14 11:50:00', StarttimescriptLongLoop);"
    }
  ],
  "mark_nodes": [
    "Earth",
    "Mars",
    "Moon",
    "Sun",
    "Venus",
    "ISS"
  ],
  "meta": {
    "author": "CCMC",
    "description": "This profile is showing the Coronal mass ejection of the bastille day 2000-07-14. The profile is data heavy and will require a powerful GPU.",
    "license": "MIT License",
    "name": "Bastille day",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "properties": [
    {
      "name": "{earth_satellites}.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.Sun.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "true"
    },
    {
      "name": "Scene.SunGlare.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.Sun.Renderable.Layers.ColorLayers.Texture.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-2.Enabled",
      "type": "setPropertyValue",
      "value": "true"
    },
    {
      "name": "Scene.Earth.Renderable.Layers.ColorLayers.ESRI_VIIRS_Combo.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.Earth.Renderable.Layers.ColorLayers.ESRI_World_Imagery.Enabled",
      "type": "setPropertyValue",
      "value": "true"
    }
  ],
  "time": {
    "type": "absolute",
    "value": "2000-07-14T08:42:00"
  },
  "version": {
    "major": 1,
    "minor": 0
  }
}