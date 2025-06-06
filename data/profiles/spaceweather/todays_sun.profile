{
  "assets": [
    "base",
    "base_keybindings",
    "scene/solarsystem/heliosphere/todayssun/actions",
    "scene/solarsystem/heliosphere/todayssun/fieldlines",
    "scene/solarsystem/heliosphere/todayssun/grid",
    "scene/solarsystem/heliosphere/todayssun/mission",
    "scene/solarsystem/heliosphere/todayssun/sun_earth_line",
    "scene/solarsystem/heliosphere/todayssun/surfaces",
    "scene/solarsystem/heliosphere/todayssun/white_background",
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
    }
  ],
  "mark_nodes": [
    "Sun",
    "Earth"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "This profile uses simulation outputs in real time from the model WSA to visualize the magnetic fluctuations on the Sun.",
    "license": "MIT License",
    "name": "Todays Sun",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "panel_visibility": {
    "exoplanets": false,
    "flightControl": false,
    "geoLocation": false,
    "gettingStartedTour": false,
    "mission": true,
    "skyBrowser": false
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
    },
    {
      "name": "Scene.WSA_54_velocity_GONGZ_Outer_Boundary.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.WSA_54_velocity_ADAPT_Outer_Boundary.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.WSA_54_Magnetograms_ADAPT.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.WSA_54_Magnetograms_GONGZ.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "true"
    },
    {
      "name": "Scene.WSA_54_Magnetic_Field_ADAPT_5_Rs.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.WSA_54_Magnetic_Field_GONGZ_5_Rs.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.WSA_54_Fieldlines_PFSS_IO.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "true"
    },
    {
      "name": "Scene.WSA_54_Fieldlines_PFSS_OI.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "true"
    },
    {
      "name": "Scene.WSA_54_Fieldlines_SCS_OI.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.WSA_54_Fieldlines_Earth.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.WSA_54_Fieldlines_PFSS_IO.Renderable.ABlendingEnabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.WSA_54_Fieldlines_PFSS_IO.Renderable.LineWidth",
      "type": "setPropertyValueSingle",
      "value": "2.0"
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
