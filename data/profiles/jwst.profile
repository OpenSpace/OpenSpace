{
  "actions": [
    {
      "documentation": "Toggle trails on or off for satellites around Earth",
      "gui_path": "/Solar System/Earth",
      "identifier": "profile.toggle.satellitetrails",
      "is_local": false,
      "name": "Toggle satellite trails",
      "script": "local list = openspace.getProperty('{earth_satellites}.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Toggle points and labels for the Lagrangian points for Earth Sun system",
      "gui_path": "/JWST",
      "identifier": "profile.toggle.lagrangianpoints",
      "is_local": false,
      "name": "Toggle Lagrangian points",
      "script": "local list = openspace.getProperty('{lagrange_points_earth}.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Toggle Hubble Ultra Deep Field image and line towards its coordinate",
      "gui_path": "/JWST",
      "identifier": "profile.toggle.hudf",
      "is_local": false,
      "name": "Toggle Hubble Ultra Deep Field",
      "script": "local list = openspace.getProperty('{mission_jwst_hudf}.*.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Toggle L2 label, point and line",
      "gui_path": "/JWST",
      "identifier": "profile.toggle.l2",
      "is_local": false,
      "name": "Toggle L2 line and small L2 label",
      "script": "local list = openspace.getProperty('{lagrange_points_earth_l2_small}.*.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Toggle James Webb Space Telecope field of view and view band",
      "gui_path": "/JWST",
      "identifier": "profile.toggle.jwst_fov",
      "is_local": false,
      "name": "Toggle JWST field of view and view band",
      "script": "local list = openspace.getProperty('{mission_jwst_fov}.*.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Set the time to the launch time of JWST",
      "gui_path": "/JWST",
      "identifier": "profile.set.jwst_launch",
      "is_local": false,
      "name": "Set to JWST launch time",
      "script": "openspace.time.setDeltaTime(1); openspace.time.setTime('2021-12-25T12:20:01');"
    },
    {
      "documentation": "Set the time to the detach time of JWST",
      "gui_path": "/JWST",
      "identifier": "profile.set.jwst_detach",
      "is_local": false,
      "name": "Set to JWST detach time",
      "script": "openspace.time.setDeltaTime(1); openspace.time.setTime('2021-12-25T12:50:00');"
    },
    {
      "documentation": "Toggle JWST trail relative to the Sun",
      "gui_path": "/JWST",
      "identifier": "profile.toggle.sun_trail",
      "is_local": false,
      "name": "Toggle JWST Sun trail",
      "script": "local value = openspace.getPropertyValue('Scene.JWSTSunTrail.Renderable.Enabled'); openspace.setPropertyValueSingle('Scene.JWSTSunTrail.Renderable.Enabled', not value);"
    },
    {
      "documentation": "Toggle all planet and moon trails, except the Moon",
      "gui_path": "/JWST",
      "identifier": "profile.toggle.trails_not_moon",
      "is_local": false,
      "name": "Toggle trails (except Moon)",
      "script": "local list = openspace.getProperty('{planetTrail_solarSystem}.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end local moonlist = openspace.getProperty('{moonTrail_solarSystem}.Renderable.Enabled') for _,v in pairs(moonlist) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end openspace.setPropertyValueSingle('Scene.MoonTrail.Renderable.Enabled', true)"
    },
    {
      "documentation": "Toggle JWST launch, cruise and orbit trails, not the Sun trail",
      "gui_path": "/JWST",
      "identifier": "profile.toggle.jwst_trails",
      "is_local": false,
      "name": "Toggle JWST trail",
      "script": "local list = {'Scene.JWSTTrailLaunch.Renderable.Enabled', 'Scene.JWSTTrailCruise.Renderable.Enabled', 'Scene.JWSTTrailOrbit.Renderable.Enabled'}; for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)); end"
    }
  ],
  "additional_scripts": [
    "openspace.setPropertyValue(\"Scene.MoonTrail.Renderable.Appearance.Color\", {0.7, 0.5, 0.5});"
  ],
  "assets": [
    "base",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/satellites/satellites",
    "scene/solarsystem/planets/earth/satellites/misc/hubble_trail",
    "scene/solarsystem/planets/earth/lagrange_points/lagrange_points",
    "scene/solarsystem/missions/jwst/jwst",
    "scene/solarsystem/missions/jwst/trail",
    "scene/solarsystem/missions/jwst/targets/targets",
    "scene/solarsystem/missions/jwst/timelapse",
    "scene/solarsystem/missions/jwst/toggle_trail",
    "scene/solarsystem/missions/jwst/point_jwst",
    "scene/digitaluniverse/hdf"
  ],
  "camera": {
    "aim": "",
    "anchor": "JWSTModel",
    "frame": "Root",
    "yaw": -0.005731,
    "pitch": -0.001656,
    "type": "setNavigationState",
    "position": {
      "x": 30.188156,
      "y": -9.477188,
      "z": -9.203491
    },
    "up": {
      "x": 0.361587,
      "y": 0.893643,
      "z": 0.265813
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
  "keybindings": [
    {
      "action": "profile.toggle.satellitetrails",
      "key": "S"
    },
    {
      "action": "profile.toggle.lagrangianpoints",
      "key": "P"
    },
    {
      "action": "profile.toggle.hudf",
      "key": "U"
    },
    {
      "action": "profile.toggle.l2",
      "key": "O"
    },
    {
      "action": "profile.toggle.jwst_fov",
      "key": "V"
    },
    {
      "action": "profile.set.jwst_launch",
      "key": "J"
    },
    {
      "action": "profile.toggle.sun_trail",
      "key": "K"
    },
    {
      "action": "jwst.play.forwards",
      "key": "M"
    },
    {
      "action": "jwst.play.backwards",
      "key": "N"
    },
    {
      "action": "jwst.play.clear",
      "key": "B"
    },
    {
      "action": "profile.toggle.trails_not_moon",
      "key": "G"
    },
    {
      "action": "jwst.toggle.direction",
      "key": "Y"
    },
    {
      "action": "profile.toggle.jwst_trails",
      "key": "T"
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
    "description": "James Webb Space Telescope Profile. Adds the James Webb Space Telescope model with an estimated trajectery",
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
    },
    {
      "name": "Scene.MoonTrail.Renderable.Appearance.Fade",
      "type": "setPropertyValueSingle",
      "value": "3.0"
    },
    {
      "name": "Scene.JWSTTrailLaunch.Renderable.Appearance.EnableFade",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.JWSTTrailCruise.Renderable.Appearance.EnableFade",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.L1.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.L1Label.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
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
      "name": "Scene.L4.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.L4Label.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.L5.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.L5Label.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.L2Small.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.L2SmallLabel.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.L2SunLine.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "{mission_jwst_target}.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.HUDFJWSTLine.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.JWSTSunTrail.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.JWSTBand.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.JWSTFov.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.JWSTModel.ApproachFactor",
      "type": "setPropertyValueSingle",
      "value": "900"
    },
    {
      "name": "Scene.Earth.Renderable.Layers.ColorLayers.Terra_Modis_Temporal.Enabled",
      "type": "setPropertyValueSingle",
      "value": "true"
    },
    {
      "name": "Scene.Earth.Renderable.Layers.ColorLayers.ESRI_VIIRS_Combo.Enabled",
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
    "value": "2021-12-25T12:50:01"
  },
  "version": {
    "major": 1,
    "minor": 1
  }
}
