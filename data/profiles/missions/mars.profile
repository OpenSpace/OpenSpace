{
  "assets": [
    "base",
    "base_keybindings",
    "scene/solarsystem/missions/insight/edl",
    "scene/solarsystem/missions/perseverance/perseverance"
  ],
  "camera": {
    "anchor": "Mars",
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
      "action": "os.insight.Setup",
      "key": "I"
    },
    {
      "action": "os.insight.DisableLayers",
      "key": "SHIFT+I"
    },
    {
      "action": "os.perseverance.Setup",
      "key": "P"
    }
  ],
  "mark_nodes": [
    "Mars",
    "Insight",
    "Perseverance"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "This profile shows the landing of the NASA InSight lander on Mars. The final minutes of the approach are shown with the lander finishing on the surface of Mars.  This profile also includes the landing trail and model for the Mars2020 rover Perseverence.",
    "license": "MIT License",
    "name": "Mars",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "panel_visibility": {
    "mission": true
  },
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
