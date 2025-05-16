{
  "assets": [
    "base",
    "base_keybindings",
    "events/toggle_sun",
    "scene/solarsystem/telescopes/euclid/euclid",
    "scene/solarsystem/planets/earth/earth"
  ],
  "camera": {
    "anchor": "Euclid",
    "type": "goToNode"
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
    "Earth",
    "Sun",
    "Euclid"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "A profile showing the trajectory of ESA's Euclid mission",
    "license": "MIT License",
    "name": "Euclid",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "panel_visibility": {
    "mission": true
  },
  "properties": [
    {
      "name": "Scene.L2.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.L2Label.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.L2SmallLabel.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.EuclidTrailSun.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.EuclidTrailEarth.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.EuclidTrailOrbit.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    }
  ],
  "time": {
    "is_paused": false,
    "type": "relative",
    "value": "-1d"
  },
  "version": {
    "major": 1,
    "minor": 4
  }
}
