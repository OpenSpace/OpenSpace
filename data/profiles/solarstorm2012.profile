{
  "actions": [
    {
      "documentation": "Reset button. Sets time to start of dataset. No loop",
      "gui_path": "2012July",
      "identifier": "2012july.reset_loop",
      "is_local": false,
      "name": "Reset button",
      "script": "openspace.time.setTime('2012-JUL-01 07:00:00.00');\nopenspace.scriptScheduler.clear();"
    },
    {
      "documentation": "Makes the Sun turn black",
      "gui_path": "2012July",
      "identifier": "2012july.dark_sun",
      "is_local": false,
      "name": "Dark sun",
      "script": "openspace.setPropertyValueSingle('Scene.Sun.Renderable.Layers.ColorLayers.Texture.Settings.Multiplier', 0.000000);"
    },
    {
      "documentation": "Sets time to start of data, sets high delta time and loops back from start when at end of data",
      "gui_path": "2012July",
      "identifier": "2012july.loop_enlil",
      "is_local": false,
      "name": "Loop ENLIL",
      "script": "openspace.scriptScheduler.clear();\nopenspace.time.setDeltaTime(43200);\nopenspace.time.setTime('2012-JUL-01 07:00:00.00');\nlocal StarttimescriptENLILLoop = \"openspace.time.setTime('2012 JUL 01 07:00:00')\";\nopenspace.scriptScheduler.loadScheduledScript('2012 AUG 01 06:58:00', StarttimescriptENLILLoop);"
    },
    {
      "documentation": "Sets time to start of data, sets higher delta time and loops back from start, when at end of data.",
      "gui_path": "2012July",
      "identifier": "2012july.loop_batsrus",
      "is_local": false,
      "name": "Loop BATSRUS",
      "script": "openspace.scriptScheduler.clear();\nopenspace.time.setDeltaTime(1400);\nopenspace.time.setTime('2012-JUL-14 06:00:00.00');\nlocal StarttimescriptBatsrusLoop = \"openspace.time.setTime('2012 JUL 14 06:00:00')\";\nopenspace.scriptScheduler.loadScheduledScript('2012 JUL 16 07:30:00', StarttimescriptBatsrusLoop);"
    }
  ],
  "assets": [
    "base",
    "dashboard/default_dashboard",
    "scene/solarsystem/heliosphere/2012/sun_earth_2012_fieldlines"
  ],
  "camera": {
    "altitude": 74000000000.0,
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
      "action": "2012july.reset_loop",
      "key": "R"
    },
    {
      "action": "2012july.dark_sun",
      "key": "D"
    },
    {
      "action": "2012july.loop_enlil",
      "key": "E"
    },
    {
      "action": "2012july.loop_batsrus",
      "key": "B"
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
    "author": "Community Coordinated Modeling Center, NASA Goddard",
    "description": "This profile is showing several coronal mass ejection (CMEs) during July 2012, where the last one was incredible intense. Its strength was comparable to the most intense CME in recorded history, the Carrington Event of 1859, which caused damage to electric equipment world wide. Luckily this 2012 event missed earth. The event is modeled with ENLIL which spands across the solarsystem, from the Sun to Earth, Batsrus which is showing the interaction of the flow of the solar wind and Earths magnetosphere. There is also one time step of the PFSS model showing the Suns local magnetic structure.",
    "license": "MIT License",
    "name": "Solar storm 2012",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "properties": [
    {
      "name": "Scene.Sun.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "true"
    },
    {
      "name": "Scene.SunGlare.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    }
  ],
  "time": {
    "type": "absolute",
    "value": "2012-07-14T07:00:00"
  },
  "version": {
    "major": 1,
    "minor": 1
  }
}