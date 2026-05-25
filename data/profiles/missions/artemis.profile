{
  "assets": [
    "base",
    "base_keybindings",
    "events/toggle_sun",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/satellites/satellites",
    "scene/solarsystem/missions/artemis/artemis",
    "scene/solarsystem/missions/artemis/toggle_trail",
    "scene/solarsystem/missions/artemis2/artemis2"
  ],
  "camera": {
    "aim": "",
    "anchor": "Artemis2Model",
    "frame": "Root",
    "pitch": 0.00047288274629576405,
    "position": {
      "x": 593334.3991851807,
      "y": -80421.27866363525,
      "z": 139200.5686187744
    },
    "type": "setNavigationState",
    "up": {
      "x": 0.003356826933219176,
      "y": 0.8720068806196053,
      "z": 0.4894821057658859
    },
    "yaw": -0.0060679163186436935
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
    "description": "Artemis Profile. Adds the Orion capsule (Artemis-1) model with its trajectery.",
    "license": "MIT License",
    "name": "Artemis",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "panel_visibility": {
    "mission": true
  },
  "properties": [
    {
      "name": "{earth_satellites~space_stations}.Renderable.Enabled",
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
    "is_paused": false,
    "type": "absolute",
    "value": "2026 APR 06 22:40:09"
  },
  "version": {
    "major": 1,
    "minor": 5
  }
}
