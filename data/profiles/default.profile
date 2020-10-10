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
  "mark_nodes": [ "Earth", "Mars", "Moon", "Sun" ]
}
