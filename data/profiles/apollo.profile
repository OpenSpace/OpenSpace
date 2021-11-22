{
  "actions": [
    {
      "documentation": "Jump to right before the earthrise photo",
      "gui_path": "/Missions/Apollo/8",
      "identifier": "profile.setup.earthrise",
      "is_local": false,
      "name": "Set Earthrise time",
      "script": "openspace.time.setPause(true); openspace.time.setDeltaTime(1); openspace.time.setTime('1968 DEC 24 16:37:31'); openspace.navigation.setNavigationState({Anchor = 'Apollo8', Position = { 1.494592E1, 3.236777E1, -4.171296E1 }, ReferenceFrame = 'Root', Up = { 0.960608E0, -0.212013E0, 0.179675E0 }}); openspace.setPropertyValue('*Trail.Renderable.Enabled', false);"
    },
    {
      "documentation": "Jump to time right before Apollo 8 liftoff, with its trail enabled",
      "gui_path": "/Missions/Apollo/8",
      "identifier": "profile.setup.apollo8",
      "is_local": false,
      "name": "Set Apollo 8 launch time",
      "script": "openspace.time.setTime('1968-12-21T12:51:37.00'); openspace.setPropertyValueSingle('Scene.Apollo8LaunchTrail.Renderable.Enabled', true);"
    },
    {
      "documentation": "Toggles Moon Kaguya color layer",
      "gui_path": "/Missions/Apollo",
      "identifier": "profile.moon.kaguyalayer",
      "is_local": false,
      "name": "Toggle Kaguya layer",
      "script": "openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.Kaguya_Utah.Enabled', not openspace.getPropertyValue('Scene.Moon.Renderable.Layers.ColorLayers.Kaguya_Utah.Enabled'));"
    },
    {
      "documentation": "Toggles shading for the Moon",
      "gui_path": "/Missions/Apollo",
      "identifier": "profile.moon.shading",
      "is_local": false,
      "name": "Toggle Moon shading",
      "script": "openspace.setPropertyValueSingle('Scene.Moon.Renderable.PerformShading', not openspace.getPropertyValue('Scene.Moon.Renderable.PerformShading'));"
    },
    {
      "documentation": "Set camera focus to the Earth",
      "gui_path": "/Missions/Apollo",
      "identifier": "profile.focus.earth",
      "is_local": false,
      "name": "Focus on Earth",
      "script": "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Earth'); openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    },
    {
      "documentation": "Set camera focus to the Moon",
      "gui_path": "/Missions/Apollo",
      "identifier": "profile.focus.moon",
      "is_local": false,
      "name": "Focus on Moon",
      "script": "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Moon'); openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    },
    {
      "documentation": "Disable apollo site on moon when leaving",
      "gui_path": "/Missions/Apollo",
      "identifier": "profile.moon.disableapollosites",
      "is_local": false,
      "name": "Disable Apollo sites",
      "script": "openspace.setPropertyValue('Scene.Moon.Renderable.Layers.ColorLayers.A17_*.Enabled', false); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.HeightLayers.LRO_NAC_Apollo_11.Enabled', false); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A11_M177481212_p_longlat.Enabled', false); openspace.setPropertyValueSingle('Scene.Apollo11MoonTrail.Renderable.Enabled', false); openspace.setPropertyValueSingle('Scene.Apollo11LemTrail.Renderable.Enabled', false); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.HeightLayers.LRO_NAC_Apollo_17.Enabled', false);"
    },
    {
      "documentation": "Setup for Apollo 11 landing site",
      "gui_path": "/Missions/Apollo/11",
      "identifier": "profile.setup.apollo11",
      "is_local": false,
      "name": "Setup A11 site",
      "script": "openspace.time.setTime('1969 JUL 20 20:17:40'); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.HeightLayers.LRO_NAC_Apollo_11.Enabled', true); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A11_M177481212_p_longlat.Enabled', true); openspace.setPropertyValueSingle('Scene.Moon.Renderable.TargetLodScaleFactor', 20.11); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'Apollo11LemPosition'); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil); openspace.setPropertyValueSingle('Scene.Apollo11MoonTrail.Renderable.Enabled', true); openspace.setPropertyValueSingle('Scene.Apollo11LemTrail.Renderable.Enabled', true);"
    },
    {
      "documentation": "Setup for Apollo 17 landing site",
      "gui_path": "/Missions/Apollo/17",
      "identifier": "profile.setup.apollo17",
      "is_local": false,
      "name": "Setup A17 site",
      "script": "openspace.time.setTime('1972 DEC 12 19:47:11'); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A17_travmap.BlendMode', 0.000000); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A17_travmap.Enabled', true); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.HeightLayers.LRO_NAC_Apollo_17.Enabled', true); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A17_LEM.Enabled', true); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A17_LEM.BlendMode', 0.000000); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A17_NAC_Alt_p.Enabled', true); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A17_NAC_Alt_p.BlendMode', 0.000000); openspace.setPropertyValueSingle('Scene.Moon.Renderable.TargetLodScaleFactor', 20.17); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'Apollo17LemModel'); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil); openspace.setPropertyValueSingle('Scene.Moon.Renderable.Layers.ColorLayers.A17_station7.BlendMode', 0.000000);"
    }
  ],
  "assets": [
    "base",
    "scene/solarsystem/missions/apollo/11/apollo11",
    "scene/solarsystem/missions/apollo/11/lem_flipbook",
    "scene/solarsystem/missions/apollo/17/lem",
    "scene/solarsystem/missions/apollo/8/apollo8",
    "scene/solarsystem/missions/apollo/apollo_globebrowsing",
    "scene/solarsystem/missions/apollo/insignias_map"
  ],
  "camera": {
    "altitude": 15000000.0,
    "anchor": "Earth",
    "latitude": 20.0,
    "longitude": -60.0,
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
      "action": "profile.setup.earthrise",
      "key": "E"
    },
    {
      "action": "profile.setup.apollo8",
      "key": "U"
    },
    {
      "action": "profile.moon.kaguyalayer",
      "key": "K"
    },
    {
      "action": "profile.moon.shading",
      "key": "S"
    },
    {
      "action": "profile.focus.earth",
      "key": "Home"
    },
    {
      "action": "profile.focus.moon",
      "key": "M"
    },
    {
      "action": "profile.moon.disableapollosites",
      "key": "F9"
    },
    {
      "action": "profile.setup.apollo11",
      "key": "F11"
    },
    {
      "action": "profile.setup.apollo17",
      "key": "F7"
    }
  ],
  "mark_nodes": [
    "Earth",
    "Moon",
    "Apollo8",
    "Apollo11",
    "Apollo11LemModel",
    "Apollo17LemModel"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "This profile contains all the apollo assets in openspace. Apollo 8,11,17 and some associated materials. ",
    "license": "MIT License",
    "name": "Apollo",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "properties": [
    {
      "name": "NavigationHandler.OrbitalNavigator.MinimumAllowedDistance",
      "type": "setPropertyValue",
      "value": "0"
    },
    {
      "name": "Scene.Moon.Renderable.Layers.ColorLayers.A17_travmap.BlendMode",
      "type": "setPropertyValue",
      "value": "0"
    }
  ],
  "time": {
    "type": "absolute",
    "value": "1968-12-21T12:51:51"
  },
  "version": {
    "major": 1,
    "minor": 1
  }
}