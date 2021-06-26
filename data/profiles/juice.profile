{
  "additional_scripts": [
    "openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.MinimumAllowedDistance\", 0.000000)",
    "openspace.setPropertyValueSingle(\"Scene.JuiceJMC1.Renderable.Enabled\", false)",
    "openspace.setPropertyValueSingle(\"Scene.JuiceJMC2.Renderable.Enabled\", false)",
    "openspace.setPropertyValueSingle(\"Scene.JuiceMajis.Renderable.Enabled\", false)",
    "openspace.setPropertyValueSingle(\"Scene.JuiceMajisIrB2.Renderable.Enabled\", false)",
    "openspace.setPropertyValueSingle(\"Scene.JuiceMajisIrB4.Renderable.Enabled\", false)",
    "openspace.setPropertyValueSingle(\"Scene.JuiceMajisIr.Renderable.Enabled\", false)",
    "openspace.setPropertyValueSingle(\"Scene.JuiceMajisVisnirB2.Renderable.Enabled\", false)",
    "openspace.setPropertyValueSingle(\"Scene.JuiceMajisVisnirB4.Renderable.Enabled\", false)",
    "openspace.setPropertyValueSingle(\"Scene.JuiceMajisVisnir.Renderable.Enabled\", false)",
    "openspace.setPropertyValueSingle(\"Scene.JuiceStarOH1.Renderable.Enabled\", false)",
    "openspace.setPropertyValueSingle(\"Scene.JuiceStarOH2.Renderable.Enabled\", false)",
    "openspace.setPropertyValueSingle(\"Scene.JuiceStarOH3.Renderable.Enabled\", false)",
    "openspace.setPropertyValueSingle(\"Scene.JuiceSwiFull.Renderable.Enabled\", false)",
    "openspace.setPropertyValueSingle(\"Scene.JuiceUVS.Renderable.Enabled\", false)",
    "",
    ""
  ],
  "assets": [
    "base",
    "scene/solarsystem/missions/juice/model",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/satellites/satellites"
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
      "documentation": "Toggle trails on or off for satellites around Earth",
      "gui_path": "/Earth",
      "is_local": false,
      "key": "S",
      "name": "Toggle satellite trails",
      "script": "local list = openspace.getProperty('{earth_satellites}.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Refocuses the camera on the ISS",
      "gui_path": "/Earth",
      "is_local": false,
      "key": "I",
      "name": "Focus ISS",
      "script": "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'ISS');openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    },
    {
      "documentation": "Retargets the camera on Earth",
      "gui_path": "/Earth",
      "is_local": false,
      "key": "HOME",
      "name": "Focus on Earth",
      "script": "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Earth')openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    }
  ],
  "mark_nodes": [
    "Earth",
    "Juice",
    "Jupiter",
    "Ganymede"
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
    "value": "2029-10-27T00:00:00"
  },
  "version": {
    "major": 1,
    "minor": 0
  }
}