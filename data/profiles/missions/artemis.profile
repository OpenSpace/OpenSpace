{
  "assets": [
    "base",
    "base_keybindings",
    "events/toggle_sun",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/satellites/satellites",
    "scene/solarsystem/missions/artemis/artemis",
    "scene/solarsystem/missions/artemis/toggle_trail"
  ],
  "camera": {
    "aim": "",
    "anchor": "ArtemisModel",
    "frame": "Root",
    "yaw": -0.003474,
    "pitch":  0.008681,
    "type": "setNavigationState",
    "position": {
      "x": 364.907409,
      "y": -65.898746,
      "z": 361.510673
    },
    "up": {
      "x": -0.128611,
      "y": 0.944590,
      "z": 0.302006
    }
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
  "mark_nodes": [
    "ArtemisModel",
    "Earth",
    "Moon",
    "Sun"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "Artemis Profile. Adds the Orion capsule (Artemis-1) model with an estimated trajectery",
    "license": "MIT License",
    "name": "Artemis",
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
      "name": "Scene.MoonTrail.Renderable.Appearance.Color",
      "type": "setPropertyValueSingle",
      "value": "{0.5, 0.5, 0.5}"
    },
    {
      "name": "Scene.Earth.Renderable.Layers.ColorLayers.ESRI_NOAA20_Combo.Enabled",
      "type": "setPropertyValueSingle",
      "value": "true"
    },
    {
      "name": "Scene.Earth.Renderable.Layers.ColorLayers.ESRI_VIIRS_Combo.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.ISS.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.ISS_trail.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    }
  ],
  "time": {
    "type": "absolute",
    "value": "2022-11-21T12:00:00"
  },
  "version": {
    "major": 1,
    "minor": 1
  }
}
