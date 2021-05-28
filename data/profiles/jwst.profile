{
  "assets": [
    "base",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/satellites/satellites",
    "scene/solarsystem/missions/jwst/jwst",
    "scene/solarsystem/missions/jwst/HUDFImage",
    "scene/digitaluniverse/hdf"
  ],
  "camera": {
    "altitude": 17000000.0,
    "anchor": "Earth",
    "latitude": 3.5559,
    "longitude": -53.0515,
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
      "documentation": "Toggle points and labels for the Lagrangian points for Earth Sun system",
      "gui_path": "/JWST",
      "is_local": false,
      "key": "P",
      "name": "Toggle Lagrangian points",
      "script": "local list = openspace.getProperty('{lagrange_points}.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Toggle Hubble Ultra Deep Field image and line towards its coordinate",
      "gui_path": "/JWST",
      "is_local": false,
      "key": "U",
      "name": "Toggle Hubble Ultra Deep Field",
      "script": "local list = openspace.getProperty('{hudf}.*.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Toggle L2 label, point and line",
      "gui_path": "/JWST",
      "is_local": false,
      "key": "O",
      "name": "Toggle L2",
      "script": "local list = openspace.getProperty('{L2}.*.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Toggle James Webb Space Telecope field of view and view band",
      "gui_path": "/JWST",
      "is_local": false,
      "key": "V",
      "name": "Toggle JWST field of view and view band",
      "script": "local list = openspace.getProperty('{fov}.*.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    }
  ],
  "mark_nodes": [
    "JWSTModel",
    "JWSTTrail",
    "L2",
    "Earth",
    "Moon",
    "Sun"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "James Webb Space Telecope Profile. Adds the James Webb Space Telecope model with an estimated trajectery.",
    "license": "MIT License",
    "name": "JWST",
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
    "value": "2021-10-31T00:00:00"
  },
  "version": {
    "major": 1,
    "minor": 0
  }
}
