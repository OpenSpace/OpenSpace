{
  "actions": [
    {
      "documentation": "Sets up scene for all-sky camera",
      "gui_path": "/sky-camera",
      "identifier": "sky-camera_setup_loop",
      "is_local": false,
      "name": "sky-camera setup",
      "script": "openspace.setPropertyValueSingle('Scene.Earth.Renderable.Layers.ColorLayers.Oval_North.Enabled', false);openspace.setPropertyValueSingle('Scene.Earth.Renderable.Layers.ColorLayers.Oval_South.Enabled', false);openspace.setPropertyValueSingle('ScreenSpace.color-legend-aurora-oval.Enabled', false);openspace.setPropertyValueSingle('ScreenSpace.Legend-aurora-oval.Enabled', false);openspace.setPropertyValueSingle('ScreenSpace.ScreenSpaceKPindex.Enabled', false);openspace.setPropertyValueSingle('ScreenSpace.target-marker.Enabled', false);openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.FollowAnchorNodeRotationDistance', 16.360000);openspace.setPropertyValueSingle('Scene.EarthAtmosphere.Renderable.Enabled', false);openspace.scriptScheduler.clear();openspace.time.setDeltaTime(300);openspace.time.setTime('2024-MAY-11 04:22:00.00');Endtime2012 = \"2024 MAY 11 08:10:00\";Starttimescript12 = \"openspace.time.setTime('2024 MAY 11 04:22:00')\";openspace.scriptScheduler.loadScheduledScript(Endtime2012, Starttimescript12);"
    }
  ],
  "assets": [
    "base",
    "base_keybindings",
    "events/toggle_sun",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/satellites/satellites",
    "scene/solarsystem/planets/earth/aurorasaurus/main"
  ],
  "camera": {
    "altitude": 17000000.0,
    "anchor": "Earth",
    "latitude": 58.5877,
    "longitude": 16.1924,
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
      "action": "os.solarsystem.FocusEarth",
      "key": "HOME"
    },
    {
      "action": "sky-camera_setup_loop",
      "key": "R"
    },
    {
      "action": "os.solarsystem.Toggleskycamera",
      "key": "E"
    },
    {
      "action": "os.disableAllAuroras",
      "key": "D"
    },
    {
      "action": "os.allAuroras",
      "key": "S"
    },
    {
      "action": "os.ToggleNorthLayer",
      "key": "Q"
    },
    {
      "action": "os.ToggleSouthLayer",
      "key": "W"
    },
    {
      "action": "os.toggleNotSeenAuroras",
      "key": "N"
    },
    {
      "action": "os.ToggleColorLegend",
      "key": "O"
    },
    {
      "action": "os.ToggleLegend",
      "key": "P"
    },
    {
      "action": "os.enableViewline",
      "key": "Z"
    },
    {
      "action": "os.disableViewline",
      "key": "X"
    },
    {
      "action": "os.ToggleKPindex",
      "key": "H"
    },
    {
      "action": "os.ToggleISWACygnet",
      "key": "J"
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
    "author": "OpenSpace Team",
    "description": "Default OpenSpace Profile. Adds Earth satellites not contained in other profiles",
    "license": "MIT License",
    "name": "Default",
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
      "name": "Scene.Earth.Renderable.Layers.ColorLayers.ESRI_VIIRS_Combo.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.Earth.Renderable.Layers.ColorLayers.Blue_Marble.Enabled",
      "type": "setPropertyValueSingle",
      "value": "true"
    },
    {
      "name": "Scene.Earth.Renderable.Layers.NightLayers.Earth_at_Night_2012.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.Earth.Renderable.Layers.Overlays.Reference_Features.Enabled",
      "type": "setPropertyValueSingle",
      "value": "true"
    },
    {
      "name": "Scene.Earth.Renderable.Layers.Overlays.Reference_Features.Settings.Multiplier",
      "type": "setPropertyValueSingle",
      "value": "15"
    },
    {
      "name": "RenderEngine.HorizFieldOfView",
      "type": "setPropertyValueSingle",
      "value": "37.130000"
    },
    {
      "name": "NavigationHandler.OrbitalNavigator.FollowAnchorNodeRotationDistance",
      "type": "setPropertyValueSingle",
      "value": "0.000000"
    }
  ],
  "time": {
    "is_paused": false,
    "type": "absolute",
    "value": "2024-05-11T05:15:00"
  },
  "version": {
    "major": 1,
    "minor": 3
  }
}