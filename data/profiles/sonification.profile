{
  "assets": [
    "${USER_ASSETS}/Kosmos/shows/default_show",
    "modules/sonification/actions",
    "modules/sonification/planets"
  ],
  "camera": {
    "altitude": 17000000.0,
    "anchor": "Earth",
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
      "action": "blackout.on",
      "key": "KP_0"
    },
    {
      "action": "blackout.off",
      "key": "CTRL+KP_0"
    },
    {
      "action": "satellites.visual.on",
      "key": "KP_4"
    },
    {
      "action": "satellites.visual.off",
      "key": "CTRL+KP_4"
    },
    {
      "action": "satellites.gps.on",
      "key": "KP_5"
    },
    {
      "action": "satellites.gps.off",
      "key": "CTRL+KP_5"
    },
    {
      "action": "satellites.geostationary.on",
      "key": "KP_6"
    },
    {
      "action": "satellites.geostationary.off",
      "key": "CTRL+KP_6"
    },
    {
      "action": "apollo8.setup_earthrise",
      "key": "E"
    },
    {
      "action": "apollo8.setup_launch",
      "key": "U"
    },
    {
      "action": "apollo8.show_barycenter_trial",
      "key": "KP_7"
    },
    {
      "action": "apollo8.show_barycenter_trial",
      "key": "CTRL+KP_7"
    },
    {
      "action": "apollo17.show_landing_site_layers.usgs",
      "key": "KP_8"
    },
    {
      "action": "apollo17.travmap.on",
      "key": "KP_9"
    },
    {
      "action": "apollo17.travmap.off",
      "key": "F7"
    },
    {
      "action": "apollo17.show_station_6",
      "key": "KP_DIVIDE"
    },
    {
      "action": "apollo17.show_images",
      "key": "KP_2"
    },
    {
      "action": "apollo17.hide_images",
      "key": "CTRL+KP_2"
    },
    {
      "action": "apollo8.show_earthrise",
      "key": "KP_1"
    },
    {
      "action": "apollo8.hide_earthrise",
      "key": "CTRL+KP_1"
    },
    {
      "action": "mars.ctx.on",
      "key": "KP_SUBTRACT"
    },
    {
      "action": "mars.candor_chasma.on",
      "key": "KP_ENTER"
    },
    {
      "action": "asteroids.potentially_hazardous.on",
      "key": "HOME"
    },
    {
      "action": "asteroids.potentially_hazardous.off",
      "key": "SHIFT+HOME"
    },
    {
      "action": "pioneervoyager.trails.on",
      "key": "PAGEUP"
    },
    {
      "action": "pioneervoyager.trails.off",
      "key": "SHIFT+PAGEUP"
    },
    {
      "action": "grandtour.constellations.on",
      "key": "ALT+KP_1"
    },
    {
      "action": "grandtour.constellations.off",
      "key": "ALT+CTRL+KP_1"
    },
    {
      "action": "grandtour.orion.on",
      "key": "ALT+KP_2"
    },
    {
      "action": "grandtour.orion.off",
      "key": "ALT+CTRL+KP_2"
    },
    {
      "action": "grandtour.radiosphere.on",
      "key": "ALT+KP_3"
    },
    {
      "action": "grandtour.radiosphere.off",
      "key": "ALT+CTRL+KP_3"
    },
    {
      "action": "grandtour.exoplanets.on",
      "key": "ALT+KP_4"
    },
    {
      "action": "grandtour.exoplanets.off",
      "key": "ALT+CTRL+KP_4"
    },
    {
      "action": "grandtour.sunorbit.on",
      "key": "ALT+KP_5"
    },
    {
      "action": "grandtour.sunorbit.off",
      "key": "ALT+CTRL+KP_5"
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
    "description": "Profile that adds sonification for the planets. The sonificaiton needs to be run in SuperCollider in parallel to OpenSpace.",
    "license": "MIT License",
    "name": "Default",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "time": {
    "is_paused": false,
    "type": "relative",
    "value": "-1d"
  },
  "version": {
    "major": 1,
    "minor": 2
  }
}