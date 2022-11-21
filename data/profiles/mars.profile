{
  "actions": [
    {
      "documentation": "Set and goto Insight Landing",
      "gui_path": "/Missions/Insight",
      "identifier": "profile.setup.insight",
      "is_local": false,
      "name": "Setup scene for insight EDL",
      "script": "openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.Mola_Utah.Settings.Offset', -469.300000);openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.HiRISE-LS-DEM.Settings.Offset', -470.850006);openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.MDEM200M.Settings.Offset', -470.000000);openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.HiRISE-LS-DEM.Enabled', true);openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.ColorLayers.HiRISE-LS.Enabled', true);openspace.time.setPause(true);openspace.time.setTime('2018 NOV 26 19:39:01.68');openspace.navigation.setNavigationState({Anchor = 'Insight',Pitch = 0.567457E-4,Position = { 1.240506E1,-1.369270E1,-2.423553E0 },ReferenceFrame = 'Root',Up = { 0.441211E0,0.247019E0,0.862737E0 },Yaw = -0.446853E-4});"
    },
    {
      "documentation": "Disable Mars layer settings used for insight EDL",
      "gui_path": "/Missions/Insight",
      "identifier": "profile.unsetup.insight",
      "is_local": false,
      "name": "Unset Insight Landing",
      "script": "openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.Mola_Utah.Settings.Offset', 0);openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.HiRISE-LS-DEM.Settings.Offset', 0);openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.HiRISE-LS-DEM.Enabled', false);openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.ColorLayers.HiRISE-LS.Enabled', false);"
    },
    {
      "documentation": "Sets time and layers for Perseverance landing",
      "gui_path": "/Missions/Perseverance",
      "identifier": "profile.setup.perseverance",
      "is_local": false,
      "name": "Setup and Goto Perseverance",
      "script": "openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.Mola_Utah.Settings.Offset', -1685.5);openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.HiRISE-LS-DEM.Settings.Offset', -1686.0);openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.MDEM200M.Settings.Offset', -1686);openspace.time.setPause(true);openspace.time.setTime('2021 FEB 18 20:32:16');openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.HeightLayers.HiRISE-LS-DEM.Enabled', true);openspace.setPropertyValueSingle('Scene.Mars.Renderable.Layers.ColorLayers.HiRISE-LS.Enabled', true);openspace.navigation.setNavigationState({Anchor = 'Perseverance',Pitch = 0.567457E-4,Position = { 1.240506E1,-1.369270E1,-2.423553E0 },ReferenceFrame = 'Root',Up = { 0.441211E0,0.247019E0,0.862737E0 },Yaw = -0.446853E-4});"
    }
  ],
  "assets": [
    "base",
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
      "action": "profile.setup.insight",
      "key": "I"
    },
    {
      "action": "profile.unsetup.insight",
      "key": "SHIFT+I"
    },
    {
      "action": "profile.setup.perseverance",
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
    "description": "This profile shows the landing of the NASA InSight lander on Mars. The final minutes of the approach are shown with the lander finishing on the surface of Mars.  This profile also includes the landing trail and model for the Mars2020 rover Perseverence. ",
    "license": "MIT License",
    "name": "Mars",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "time": {
    "type": "relative",
    "value": "-1d"
  },
  "version": {
    "major": 1,
    "minor": 1
  }
}
