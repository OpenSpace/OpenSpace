{
  "version": 1.0,
  "assets": [
    {
      "path": "base"
    },
    {
      "path": "scene/solarsystem/planets/earth/earth",
      "name": "earthAsset"
    },
    {
      "path": "scene/solarsystem/planets/earth/satellites/satellites"
    }
  ],
  "properties": [
    {
      "type": "setPropertyValue",
      "identifier": "{earth_satellites}.Renderable.Enabled",
      "value": "false"
    }
  ],
  "time": {
    "type": "relative",
    "value": "-1d"
  },
  "camera": {
    "type": "goToGeo",
    "anchor": "earthAsset.Earth.Identifier",
    "latitude": 58.5877,
    "longitude": 16.1924,
    "altitude": 20000000
  },
  "mark_nodes": [ "Earth", "Mars", "Moon", "Sun" ]
}
