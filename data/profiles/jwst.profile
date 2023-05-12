{
  "additional_scripts": [
    "openspace.setPropertyValue(\"Scene.MoonTrail.Renderable.Appearance.Color\", {0.7, 0.5, 0.5});"
  ],
  "assets": [
    "base",
    "base_keybindings",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/layers/colorlayers/viirs_noaa20_temporal.asset",
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
    "pitch": -0.001656,
    "position": {
      "x": 30.188156,
      "y": -9.477188,
      "z": -9.203491
    },
    "type": "setNavigationState",
    "up": {
      "x": 0.361587,
      "y": 0.893643,
      "z": 0.265813
    },
    "yaw": -0.005731
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
      "action": "os.solarsystem.earth.togglesatellitetrails",
      "key": "S"
    },
    {
      "action": "os.missions.jwst.togglelagrangianpoints",
      "key": "P"
    },
    {
      "action": "os.missions.jwst.togglehudf",
      "key": "U"
    },
    {
      "action": "os.missions.jwst.togglel2",
      "key": "O"
    },
    {
      "action": "os.missions.jwst.togglefov",
      "key": "V"
    },
    {
      "action": "os.missoins.jwst.setup.launch",
      "key": "J"
    },
    {
      "action": "os.missions.jwst.togglesuntrail",
      "key": "K"
    },
    {
      "action": "os.missions.jwst.play.forwards",
      "key": "M"
    },
    {
      "action": "os.missions.jwst.play.backwards",
      "key": "N"
    },
    {
      "action": "os.missions.jwst.play.clear",
      "key": "B"
    },
    {
      "action": "os.missions.jwst.toggletrialsexceptmoon",
      "key": "G"
    },
    {
      "action": "os.missions.jwst.toggledirection",
      "key": "Y"
    },
    {
      "action": "os.missions.jwst.togglejwsttrails",
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
      "name": "Scene.JWSTTrailOrbit.Renderable.Enabled",
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
      "name": "Scene.Earth.Renderable.Layers.ColorLayers.ESRI_VIIRS_Combo.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.ISS_trail.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.Earth.Renderable.Layers.ColorLayers.VIIRS_NOAA20_Temporal.Enabled",
      "type": "setPropertyValueSingle",
      "value": "true"
    }
  ],
  "time": {
    "is_paused": false,
    "type": "absolute",
    "value": "2021-12-25T12:50:01"
  },
  "version": {
    "major": 1,
    "minor": 2
  }
}
