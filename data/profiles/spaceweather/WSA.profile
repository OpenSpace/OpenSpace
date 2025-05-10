{
  "assets": [
    "base",
    "base_keybindings",
    "scene/solarsystem/heliosphere/todayssun/actions",
    "scene/solarsystem/heliosphere/todayssun/mission",
    "scene/solarsystem/heliosphere/todayssun/whiteBackground",
    "scene/solarsystem/heliosphere/todayssun/wsa54",
    "scene/solarsystem/heliosphere/todayssun/grid",
    "scene/solarsystem/heliosphere/todayssun/sunEarthLine",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/satellites/satellites"
  ],
  "camera": {
    "altitude": 4578000000.0,
    "anchor": "Sun",
    "latitude": 1.5877,
    "longitude": -150.1924,
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
      "action": "os.solarsystem.ToggleSatelliteTrails",
      "key": "S"
    },
    {
      "action": "os.solarsystem.FocusIss",
      "key": "I"
    },
    {
      "action": "os.solarsystem.FocusEarth",
      "key": "HOME"
    }
  ],
  "mark_nodes": [
    "Sun",
    "Earth",
    "Mars",
    "Moon",
    "Sun",
    "Venus",
    "ISS"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "Default OpenSpace Profile. Adds Earth satellites not contained in other profiles",
    "license": "MIT License",
    "name": "Default",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "panel_visibility": {
    "actions": true,
    "exoplanets": false,
    "flightControl": false,
    "geoLocation": false,
    "gettingStartedTour": false,
    "keybindingsLayout": false,
    "mission": true,
    "navigation": true,
    "scene": true,
    "screenSpaceRenderables": true,
    "scriptLogPanel": false,
    "sessionRecording": true,
    "settings": false,
    "skyBrowser": false,
    "timePanel": true,
    "userPanels": true
  },
  "properties": [
    {
      "name": "{earth_satellites}.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.Sun.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "true"
    },
    {
      "name": "Scene.SunGlare.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.Earth.Renderable.Layers.ColorLayers.Blue_Marble.Enabled",
      "type": "setPropertyValue",
      "value": "true"
    },
    {
      "name": "Scene.Earth.Renderable.Layers.ColorLayers.ESRI_VIIRS_Combo.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    }
  ],
  "time": {
    "is_paused": false,
    "type": "relative",
    "value": "-2h"
  },
  "version": {
    "major": 1,
    "minor": 4
  }
}