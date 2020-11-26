{
  "assets": [
    "base",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/satellites/satellites",
    "scene/solarsystem/sssb/amor_asteroid",
    "scene/solarsystem/sssb/apollo_asteroid",
    "scene/solarsystem/sssb/aten_asteroid",
    "scene/solarsystem/sssb/atira_asteroid",
    "scene/solarsystem/sssb/centaur_asteroid",
    "scene/solarsystem/sssb/inner_main_belt_asteroid",
    "scene/solarsystem/sssb/main_belt_asteroid",
    "scene/solarsystem/sssb/outer_main_belt_asteroid",
    "scene/solarsystem/sssb/pha",
    "installationspecific/lights_state_machine"
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
    },
    {
      "name": "Modules.AutoNavigation.AutoNavigationHandler.IncludeRoll",
      "type": "setPropertyValueSingle",
      "value": "true"
    },
    {
      "name": "NavigationHandler.OrbitalNavigator.FlightDestinationFactor",
      "type": "setPropertyValueSingle",
      "value": "0.05"
    },
    {
      "name": "Scene.Constellations.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "true"
    },
    {
      "name": "Scene.Constellations.Renderable.Opacity",
      "type": "setPropertyValueSingle",
      "value": "0.0"
    },
    {
      "name": "Scene.ConstellationArt*.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "true"
    },
    {
      "name": "Scene.ConstellationArt*.Renderable.Opacity",
      "type": "setPropertyValue",
      "value": "0.0"
    },
    {
      "name": "Scene.Mars.Renderable.Layers.ColorLayers.Themis_IR_Day_Sweden.Enabled",
      "type": "setPropertyValueSingle",
      "value": "true"
    },
    {
      "name": "{solarsystem_labels}.Renderable.FontSize",
      "type": "setPropertyValue",
      "value": "70"
    },
    {
      "name": "{solarsystem_labels}.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "true"
    },
    {
      "name": "{solarsystem_labels}.Renderable.Opacity",
      "type": "setPropertyValue",
      "value": "0.0"
    }
  ],
  "time": {
    "type": "absolute",
    "value": "2020-09-30T09:00:00"
  },
  "version": {
    "major": 1,
    "minor": 0
  }
}