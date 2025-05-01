{
  "additional_scripts": [
    "openspace.action.triggerAction(\"os.nightsky.LevelHorizonPitch\")"
  ],
  "assets": [
    "base",
    "base_keybindings",
    "events/toggle_sun",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/satellites/satellites"
  ],
  "camera": {
    "altitude": 50.0,
    "anchor": "Earth",
    "latitude": 58.5877,
    "longitude": 16.1652,
    "type": "goToGeo"
  },
  "delta_times": [
    1.0,
    5.0,
    10.0,
    30.0,
    60.0,
    120.0,
    300.0,
    600.0,
    900.0,
    1800.0,
    3600.0,
    7200.0,
    14400.0
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
    "description": "A profile starting at night on the surface of earth looking out at the horizon. The city lights map has been disabled.",
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
      "name": "Scene.Earth.Renderable.Layers.NightLayers.Earth_at_Night_2012.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    }
  ],
  "time": {
    "is_paused": false,
    "type": "absolute",
    "value": "2023-04-13T19:40:00"
  },
  "version": {
    "major": 1,
    "minor": 4
  }
}