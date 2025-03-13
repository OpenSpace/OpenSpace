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
    "scene/solarsystem/telescopes/jwst/jwst",
    "scene/solarsystem/telescopes/jwst/trail",
    "scene/solarsystem/telescopes/jwst/targets/targets",
    "scene/solarsystem/telescopes/jwst/timelapse",
    "scene/solarsystem/telescopes/jwst/toggle_trail",
    "scene/solarsystem/telescopes/jwst/point_jwst",
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
      "action": "os.solarsystem.ToggleSatelliteTrails",
      "key": "S"
    },
    {
      "action": "os.jwst.ToggleLagrangianPoints",
      "key": "P"
    },
    {
      "action": "os.jwst.ToggleHudf",
      "key": "U"
    },
    {
      "action": "os.jwst.ToggleL2",
      "key": "O"
    },
    {
      "action": "os.jwst.ToggleFov",
      "key": "V"
    },
    {
      "action": "os.jwst.SetupLaunch",
      "key": "J"
    },
    {
      "action": "os.jwst.ToggleSunTrail",
      "key": "K"
    },
    {
      "action": "os.jwst.PlayForwards",
      "key": "M"
    },
    {
      "action": "os.jwst.PlayBackwards",
      "key": "N"
    },
    {
      "action": "os.jwst.ClearTimelapse",
      "key": "B"
    },
    {
      "action": "os.jwst.ToggleTrailsExceptMoon",
      "key": "G"
    },
    {
      "action": "os.jwst.ToggleDirection",
      "key": "Y"
    },
    {
      "action": "os.jwst.ToggleJwstTrails",
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
