{
  "actions": [
    {
      "documentation": "Toggle trails on or off for satellites around Earth",
      "gui_path": "/Earth",
      "identifier": "profile.keybind.0",
      "is_local": false,
      "name": "Toggle satellite trails",
      "script": "local list = openspace.getProperty('{earth_satellites}.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Toggle points and labels for the Lagrangian points for Earth Sun system",
      "gui_path": "/JWST",
      "identifier": "profile.keybind.1",
      "is_local": false,
      "name": "Toggle Lagrangian points",
      "script": "local list = openspace.getProperty('{lagrange_points_earth}.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Toggle Hubble Ultra Deep Field image and line towards its coordinate",
      "gui_path": "/JWST",
      "identifier": "profile.keybind.2",
      "is_local": false,
      "name": "Toggle Hubble Ultra Deep Field",
      "script": "local list = openspace.getProperty('{mission_jwst_hudf}.*.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Toggle L2 label, point and line",
      "gui_path": "/JWST",
      "identifier": "profile.keybind.3",
      "is_local": false,
      "name": "Toggle L2",
      "script": "local list = openspace.getProperty('{lagrange_points_earth_l2_small}.*.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Toggle James Webb Space Telecope field of view and view band",
      "gui_path": "/JWST",
      "identifier": "profile.keybind.4",
      "is_local": false,
      "name": "Toggle JWST field of view and view band",
      "script": "local list = openspace.getProperty('{mission_jwst_fov}.*.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    }
  ],
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
      "action": "profile.keybind.0",
      "key": "S"
    },
    {
      "action": "profile.keybind.1",
      "key": "P"
    },
    {
      "action": "profile.keybind.2",
      "key": "U"
    },
    {
      "action": "profile.keybind.3",
      "key": "O"
    },
    {
      "action": "profile.keybind.4",
      "key": "V"
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
    "description": "James Webb Space Telescope Profile. Adds the James Webb Space Telescope model with an estimated trajectery.",
    "license": "MIT License",
    "name": "James Webb Space Telescope",
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
    "minor": 1
  }
}