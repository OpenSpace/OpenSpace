{
  "assets": [
    "base",
    "base_keybindings",
    "dashboard/default_dashboard",
    "scene/solarsystem/heliosphere/2012/sun_earth_2012_fieldlines",
    "scene/solarsystem/planets/earth/magnetosphere/magnetosphere"
  ],
  "camera": {
    "altitude": 294000000000.0,
    "anchor": "Sun",
    "latitude": 35.8,
    "longitude": 87.1,
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
      "action": "os.2012july.ResetLoop",
      "key": "R"
    },
    {
      "action": "os.2012july.DarkSun",
      "key": "D"
    },
    {
      "action": "os.2012july.LoopEnlil",
      "key": "E"
    },
    {
      "action": "os.2012july.LoopBatsrus",
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
    },
    {
      "name": "Scene.EarthMagnetosphere.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    }
  ],
  "time": {
    "is_paused": false,
    "type": "absolute",
    "value": "2012-07-14T07:00:00"
  },
  "version": {
    "major": 1,
    "minor": 4
  }
}