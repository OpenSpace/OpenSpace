{
  "assets": [
    "base",
    "base_keybindings",
    "scene/solarsystem/missions/juice/fieldlines",
    "scene/solarsystem/missions/juice/fov/janus",
    "scene/solarsystem/missions/juice/fov/navcam",
    "scene/solarsystem/missions/juice/dashboard",
    "scene/solarsystem/missions/juice/model",
    "scene/solarsystem/missions/juice/plane",
    "scene/solarsystem/missions/juice/trail",
    "scene/solarsystem/missions/juice/mission",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/jupiter/layers/colorlayers/jupiter_video"
  ],
  "camera": {
    "altitude": 9220000000.0,
    "anchor": "Jupiter",
    "latitude": -7.1689,
    "longitude": -173.3037,
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
  "mark_nodes": [
    "Earth",
    "Juice",
    "Jupiter",
    "Ganymede"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "Juice profile that visualizes the currently best known trajectory for the JUICE mission the Jupiter and its moons. See https://sci.esa.int/documents/33960/35865/1567260128466-JUICE_Red_Book_i1.0.pdf for more information about the JUICE mission. Currently, only the Janus and NavCam instruments are included in this profile, but the other instruments are available for a custom profile. Some of these are not behaving correctly, which will be addressed later.",
    "license": "MIT License",
    "name": "Juice",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "panel_visibility": {
    "mission": true
  },
  "properties": [
    {
      "name": "NavigationHandler.OrbitalNavigator.LimitZoom.MinimumAllowedDistance",
      "type": "setPropertyValueSingle",
      "value": "0.0"
    },
    {
      "name": "Scene.JuiceTrailEarth.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.JuiceTrailJupiter.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "true"
    },
    {
      "name": "Scene.JuiceTrailGanymede.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.GanymedeMagnetosphere.Renderable.DomainEnabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.JuiceTrail.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.GanymedeMagnetosphere.Renderable.LineWidth",
      "type": "setPropertyValueSingle",
      "value": "2.0"
    },
    {
      "name": "Scene.GanymedeMagnetosphere.Renderable.Flow.ParticleSize",
      "type": "setPropertyValueSingle",
      "value": "25.000000"
    },
    {
      "name": "Scene.GanymedeMagnetosphere.Renderable.Flow.ParticleSpacing",
      "type": "setPropertyValueSingle",
      "value": "40"
    },
    {
      "name": "Scene.GanymedeMagnetosphere.Renderable.Flow.Speed",
      "type": "setPropertyValueSingle",
      "value": "150.000000"
    },
    {
      "name": "Scene.GanymedeMagnetosphere.Renderable.FlowEnabled",
      "type": "setPropertyValueSingle",
      "value": "true"
    },
    {
      "name": "Scene.JuiceNavCam.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.JuiceJanus.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    }
  ],
  "time": {
    "is_paused": false,
    "type": "absolute",
    "value": "2031-08-01T03:01:30"
  },
  "version": {
    "major": 1,
    "minor": 4
  }
}
