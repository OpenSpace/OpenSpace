{
  "actions": [
    {
      "documentation": "Retargets the camera on Earth",
      "gui_path": "/Solar System/Earth",
      "identifier": "profile.focus.earth",
      "is_local": false,
      "name": "Focus on Earth",
      "script": "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Earth')openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    }
  ],
  "additional_scripts": [
    "openspace.time.setPause(true)"
  ],
  "assets": [
    "base_exoplanets",
    "exoplanetexplorer/events/fade_glyphs",
    "exoplanetexplorer/kepler_prism",
    "exoplanetexplorer/milkyway_center_line",
    "exoplanetexplorer/size_indicators",
    "exoplanetexplorer/show_info_at_startup",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/default_layers"
  ],
  "camera": {
    "aim": "",
    "anchor": "Earth",
    "frame": "",
    "position": {
      "x": 3.2880097113726516e+16,
      "y": -1.5814411415922362e+17,
      "z": -2.1204404236347456e+16
    },
    "type": "setNavigationState",
    "up": {
      "x": -0.00907,
      "y": 0.97831,
      "z": -0.206947
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
      "action": "profile.focus.earth",
      "key": "HOME"
    }
  ],
  "mark_nodes": [
    "Earth",
    "Mars",
    "Moon",
    "Sun",
    "Venus",
    "ISS"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "Specific profile for the ExoplanetExplorer tool.",
    "license": "MIT License",
    "name": "Default",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "properties": [
    {
      "name": "Dashboard.StartPositionOffset",
      "type": "setPropertyValueSingle",
      "value": "{20.000000,-50.000000}"
    },
    {
      "name": "NavigationHandler.OrbitalNavigator.StereoscopicDepthOfFocusSurface",
      "type": "setPropertyValueSingle",
      "value": "100"
    },
    {
      "name": "Modules.ExoplanetsExpertTool.DataConfigFile",
      "type": "setPropertyValueSingle",
      "value": "${MODULE_EXOPLANETSEXPERTTOOL}/scripts/datasettings_exoplanets.json"
    }
  ],
  "time": {
    "type": "relative",
    "value": "-1d"
  },
  "version": {
    "major": 1,
    "minor": 1
  }
}