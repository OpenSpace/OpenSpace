{
  "actions": [
    {
      "documentation": "Toggle trails on or off for satellites around Earth",
      "gui_path": "/Earth",
      "identifier": "profile.toggle.satellite",
      "is_local": false,
      "name": "Toggle satellite trails",
      "script": "local list = openspace.getProperty('{earth_satellites}.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Refocuses the camera on the ISS",
      "gui_path": "/Earth",
      "identifier": "profile.focus.iss",
      "is_local": false,
      "name": "Focus ISS",
      "script": "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'ISS');openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    },
    {
      "documentation": "Retargets the camera on Earth",
      "gui_path": "/Earth",
      "identifier": "profile.focus.earth",
      "is_local": false,
      "name": "Focus on Earth",
      "script": "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Earth')openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    }
  ],
  "additional_scripts": [
    "openspace.setPropertyValueSingle('Dashboard.IsEnabled', false);",
    "openspace.setPropertyValueSingle(\"RenderEngine.ShowLog\", false);",
    "openspace.setPropertyValueSingle(\"RenderEngine.ShowVersion\", false);",
    "openspace.setPropertyValueSingle(\"RenderEngine.ShowCamera\", false)",
    "openspace.setPropertyValueSingle('Modules.CefWebGui.Visible', false);",
    "openspace.time.setPause(true);",
    "openspace.setPropertyValueSingle(\"Scene.Earth.Renderable.Layers.NightLayers.Earth_at_Night_2012.Enabled\", false);",
    "openspace.setPropertyValueSingle(\"Scene.Mars.Renderable.Layers.ColorLayers.Themis_IR_Day_Sweden.Enabled\", true);",
    "openspace.setPropertyValueSingle(\"Scene.MarsAtmosphere.Renderable.SunFollowingCamera\", true);",
    "openspace.setPropertyValueSingle(\"Scene.EarthAtmosphere.Renderable.SunFollowingCamera\", true);",
    "openspace.setPropertyValueSingle(\"Scene.Moon.Renderable.Layers.ColorLayers.WAC_Utah.Enabled\", false);",
    "openspace.setPropertyValueSingle(\"Scene.Moon.Renderable.Layers.ColorLayers.WAC_Sweden.Enabled\", true);",
    "openspace.setPropertyValueSingle(\"Scene.Moon.Renderable.Layers.HeightLayers.LolaDem_Sweden.Enabled\", true);",
    "openspace.setPropertyValueSingle(\"Scene.Moon.Renderable.Layers.HeightLayers.LolaDem_Utah.Enabled\", false);",
    "openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.MinimumAllowedDistance\", 300);"
  ],
  "assets": [
    "base",
    "events/toggle_globe_trails",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/satellites/satellites",
    "spaceship/spaceship",
    "util/joysticks/X52_HOTAS"
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
      "action": "profile.toggle.satellite",
      "key": "S"
    },
    {
      "action": "profile.focus.iss",
      "key": "I"
    },
    {
      "action": "profile.focus.earth",
      "key": "HOME"
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
    "description": "Default OpenSpace Profile. Adds Earth satellites not contained in other profiles.",
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
    }
  ],
  "time": {
    "type": "absolute",
    "value": "2021-08-26T03:20:55.505"
  },
  "version": {
    "major": 1,
    "minor": 1
  }
}