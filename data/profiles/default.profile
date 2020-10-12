{
  "version": {
    "major": 1,
    "minor": 0
  },
  "assets": [
    "base",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/satellites/satellites"
  ],
  "properties": [
    {
      "type": "setPropertyValue",
      "name": "{earth_satellites}.Renderable.Enabled",
      "value": "false"
    }
  ],
  "time": {
    "type": "relative",
    "value": "-1d"
  },
  "camera": {
    "type": "goToGeo",
    "anchor": "Earth",
    "latitude": 58.5877,
    "longitude": 16.1924,
    "altitude": 20000000
  },
  "mark_nodes": [ "Earth", "Mars", "Moon", "Sun" ],
  "delta_times": [
    1,
    2,
    5,
    10,
    30,
    60,
    120,
    300,
    600,
    1800,
    3600,
    7200,
    10800,
    21600,
    43200,
    86400,
    172800,
    345600,
    604800,
    1209600,
    2592000,
    5184000,
    7776000,
    15552000,
    31536000,
    63072000,
    157680000,
    315360000,
    630720000,
    1576800000
  ]
}
