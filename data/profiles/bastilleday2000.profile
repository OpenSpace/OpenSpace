{
  "actions": [
    {
      "documentation": "Reset button to start of CME",
      "gui_path": "/Bastille-Day 2000",
      "identifier": "bastille-day.reset_loops",
      "is_local": false,
      "name": "Reset button. Start of CME, no loop",
      "script": "openspace.time.setTime('2000-JUL-14 10:03:00.00');openspace.scriptScheduler.clear();"
    },
    {
      "documentation": "New loop: Restarts time at 10:03 and stops at 10:16, sets delta time to 2 min/ second (120 seconds/ second)",
      "gui_path": "/Bastille-Day 2000",
      "identifier": "bastille-day.short_loop",
      "is_local": false,
      "name": "Loop 10:03 - 10:16, at 2 min/ second",
      "script": "openspace.scriptScheduler.clear();openspace.time.setDeltaTime(120);openspace.time.setTime('2000-JUL-14 10:03:00.00');EndtimeShortLoop = \"2000 JUL 14 10:16:00\";StarttimescriptSlowLoop = \"openspace.time.setTime('2000 JUL 14 10:03:00')\";openspace.scriptScheduler.loadScheduledScript(EndtimeShortLoop, StarttimescriptSlowLoop);"
    },
    {
      "documentation": "New loop: Restarts time at 10:03 and stops at 11:00, delta time to 4 min/ second (240 seconds/ second)",
      "gui_path": "/Bastille-Day 2000",
      "identifier": "bastille-day.standard_loop",
      "is_local": false,
      "name": "Loop 10:03 - 11:00, at 4 min/ second",
      "script": "StarttimescriptSlowLoop = \"openspace.time.setTime('2000 JUL 14 10:03:00')\";openspace.scriptScheduler.clear();openspace.time.setDeltaTime(240);openspace.time.setTime('2000-JUL-14 10:03:00.00');openspace.scriptScheduler.loadScheduledScript('2000 JUL 14 11:00:00', StarttimescriptSlowLoop);"
    },
    {
      "documentation": "Fast loop: Starts from 10:03 and sets delta time to 15 min/ second (900 seconds/ second)",
      "gui_path": "/Bastille-Day 2000",
      "identifier": "bastille-day.fast_loop",
      "is_local": false,
      "name": "Loop 10:03 - 11.48, at 15 min/ second",
      "script": "openspace.scriptScheduler.clear();openspace.time.setDeltaTime(900);openspace.time.setTime('2000-JUL-14 10:03:00.00');StarttimescriptFastLoop = \"openspace.time.setTime('2000 JUL 14 10:03:00')\";openspace.scriptScheduler.loadScheduledScript('2000 JUL 14 11:48:00', StarttimescriptFastLoop);"
    },
    {
      "documentation": "Long loop: Restarts time at 09:30 and stops at 11:50, delta time to 4 min/ second (240 seconds/ second)",
      "gui_path": "/Bastille-Day 2000",
      "identifier": "bastille-day.long_loop",
      "is_local": false,
      "name": "Loop 09:30 - 13:00, at 4 min/ second",
      "script": "openspace.scriptScheduler.clear();openspace.time.setDeltaTime(240);openspace.time.setTime('2000-JUL-14 09:30:00.00');StarttimescriptLongLoop = \"openspace.time.setTime('2000 JUL 14 09:30:00')\";openspace.scriptScheduler.loadScheduledScript('2000 JUL 14 13:00:00', StarttimescriptLongLoop);"
    }
  ],
  "assets": [
    "base",
    "dashboard/default_dashboard",
    "scene/solarsystem/heliosphere/bastille_day/density_volume",
    "scene/solarsystem/heliosphere/bastille_day/fieldlines",
    "scene/solarsystem/heliosphere/bastille_day/fluxnodes",
    "scene/solarsystem/heliosphere/bastille_day/fluxnodescutplane",
    "scene/solarsystem/heliosphere/bastille_day/fluxnodeslegend",
    "scene/solarsystem/heliosphere/bastille_day/lightindicator",
    "scene/solarsystem/heliosphere/bastille_day/magnetogram",
    "scene/solarsystem/heliosphere/bastille_day/magnetogram_textures",
    "scene/solarsystem/planets/earth/magnetosphere/magnetosphere",
    "scene/solarsystem/planets/earth/magnetosphere/transforms_magnetosphere",
    "scene/solarsystem/planets/earth/satellites/satellites",
    "scene/solarsystem/sun/euv_layer"
  ],
  "camera": {
    "altitude": 3400000000.0,
    "anchor": "Sun",
    "latitude": 20.5877,
    "longitude": -35.1924,
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
      "action": "bastille-day.reset_loops",
      "key": "R"
    },
    {
      "action": "bastille-day.short_loop",
      "key": "CTRL+1"
    },
    {
      "action": "bastille-day.standard_loop",
      "key": "CTRL+2"
    },
    {
      "action": "bastille-day.fast_loop",
      "key": "CTRL+3"
    },
    {
      "action": "bastille-day.long_loop",
      "key": "CTRL+4"
    },
    {
      "action": "density_volume.toggle_volume",
      "key": "D"
    },
    {
      "action": "fluxnodelegend.show_legend",
      "key": "N"
    },
    {
      "action": "fluxnodelegend.hide_legend",
      "key": "M"
    },
    {
      "action": "magnetogram_texture.switch_color_layer",
      "key": "I"
    },
    {
      "action": "euv_layer.toggle_EUV",
      "key": "E"
    },
    {
      "action": "fluxnodes.toggle_fluxnodes",
      "key": "O"
    },
    {
      "action": "fieldlines.toggle_fieldlines",
      "key": "U"
    },
    {
      "action": "fluxnodescutplane.toggle_equatorial",
      "key": "P"
    },
    {
      "action": "fluxnodescutplane.toggle_meridial",
      "key": "LEFTBRACKET"
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
    "author": "CCMC",
    "description": "This profile is showing the Coronal mass ejection of the bastille day 2000-07-14. The profile is data intensive and will require a powerful GPU",
    "license": "MIT License",
    "name": "Bastille day 2000",
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
      "name": "Scene.Sun.Renderable.Layers.ColorLayers.Texture.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.Sun.Renderable.Layers.ColorLayers.magnetogram-2.Enabled",
      "type": "setPropertyValue",
      "value": "true"
    },
    {
      "name": "Scene.Earth.Renderable.Layers.ColorLayers.ESRI_VIIRS_Combo.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.Earth.Renderable.Layers.ColorLayers.ESRI_World_Imagery.Enabled",
      "type": "setPropertyValue",
      "value": "true"
    }
  ],
  "time": {
    "type": "absolute",
    "value": "2000-07-14T08:42:00"
  },
  "version": {
    "major": 1,
    "minor": 1
  }
}
