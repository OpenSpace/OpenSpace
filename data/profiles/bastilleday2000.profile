{
  "assets": [
    "base",
    "base_keybindings",
    "dashboard/default_dashboard",
    "scene/solarsystem/heliosphere/bastille_day/actions",
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
      "action": "os.events.bastilleday.setup.startofcme",
      "key": "R"
    },
    {
      "action": "os.events.bastilleday.setup.shortloop",
      "key": "CTRL+1"
    },
    {
      "action": "os.events.bastilleday.setup.standardloop",
      "key": "CTRL+2"
    },
    {
      "action": "os.events.bastilleday.setup.fastloop",
      "key": "CTRL+3"
    },
    {
      "action": "os.events.bastilleday.setup.longloop",
      "key": "CTRL+4"
    },
    {
      "action": "os.events.bastilleday.densityvolume.togglevolume",
      "key": "D"
    },
    {
      "action": "os.events.bastilleday.fluxnodelegend.showlegend",
      "key": "N"
    },
    {
      "action": "os.events.bastilleday.fluxnodelegend.hidelegend",
      "key": "M"
    },
    {
      "action": "os.events.bastilleday.magnetogramtexture.switchcolorlayer",
      "key": "I"
    },
    {
      "action": "os.solarsystem.sun.toggleeuv",
      "key": "E"
    },
    {
      "action": "os.events.bastilleday.fluxnodes.togglefluxnodes",
      "key": "O"
    },
    {
      "action": "os.events.bastilleday.fieldlines.togglefieldlines",
      "key": "U"
    },
    {
      "action": "os.events.bastilleday.fluxnodescutplane.toggleequatorial",
      "key": "P"
    },
    {
      "action": "os.events.bastilleday.fluxnodescutplane.togglemeridial",
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
    "version": "1.1"
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
