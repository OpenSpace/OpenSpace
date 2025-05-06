{
  "assets": [
    "base",
    "base_keybindings",
    "scene/solarsystem/missions/apollo/8/apollo8",
    "scene/solarsystem/missions/apollo/11/apollo11",
    "scene/solarsystem/missions/apollo/11/lem_flipbook",
    "scene/solarsystem/missions/apollo/17/apollo17",
    "scene/solarsystem/missions/apollo/insignias_map",
    "scene/solarsystem/missions/apollo/mission"
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
      "action": "os.apollo8.SetupEarthrise",
      "key": "E"
    },
    {
      "action": "os.apollo8.SetupLaunch",
      "key": "U"
    },
    {
      "action": "os.apollo.moon.ToggleKaguyaLayer",
      "key": "K"
    },
    {
      "action": "os.apollo.FocusEarth",
      "key": "HOME"
    },
    {
      "action": "os.apollo.FocusMoon",
      "key": "M"
    },
    {
      "action": "os.apollo17.setup.LandingSite",
      "key": "F7"
    },
    {
      "action": "os.apollo.DisableApolloSites",
      "key": "F9"
    },
    {
      "action": "os.apollo11.setup.LandingSite",
      "key": "F11"
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
    "description": "This profile contains all the apollo assets in openspace. Apollo 8,11,17 and some associated materials.",
    "license": "MIT License",
    "name": "Apollo",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "panel_visibility": {
    "mission": true
  },
  "properties": [
    {
      "name": "NavigationHandler.OrbitalNavigator.LimitZoom.MinimumAllowedDistance",
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
    "is_paused": false,
    "type": "absolute",
    "value": "1968-12-21T12:51:51"
  },
  "version": {
    "major": 1,
    "minor": 4
  }
}
