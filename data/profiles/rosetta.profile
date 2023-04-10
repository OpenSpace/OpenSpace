{
  "assets": [
    "base",
    "scene/solarsystem/missions/rosetta/67p",
    "scene/solarsystem/missions/rosetta/dashboard",
    "scene/solarsystem/missions/rosetta/rosetta",
    "scene/solarsystem/missions/rosetta/actions",
    "scene/solarsystem/missions/rosetta/mission"
  ],
  "camera": {
    "aim": "",
    "anchor": "67P",
    "frame": "",
    "position": {
      "x": -729478.0,
      "y": -665789.0,
      "z": 2509050.0
    },
    "type": "setNavigationState",
    "up": {
      "x": 0.146529,
      "y": 0.944727,
      "z": 0.29329
    }
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
      "action": "os.missions.rosetta.67p.focus",
      "key": "A"
    },
    {
      "action": "os.missions.rosetta.focus",
      "key": "S"
    },
    {
      "action": "os.missions.rosetta.setup.landerrelease",
      "key": "F6"
    },
    {
      "action": "os.missions.rosetta.67p.clearimageprojections",
      "key": "F8"
    },
    {
      "action": "os.missions.rosetta.toggleouterplanetarytrails",
      "key": "E"
    },
    {
      "action": "os.missions.rosetta.toggleimageplane",
      "key": "I"
    },
    {
      "action": "os.mission.rosetta.togglephilaetrail",
      "key": "O"
    },
    {
      "action": "os.missions.rosetta.toggle67pprojection",
      "key": "P"
    }
  ],
  "mark_nodes": [
    "67P",
    "Rosetta",
    "Philae"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "The Rosetta scene shows the entire mission of ESA's Rosetta spacecraft around comet 67P, also known as Churyumov-Gerasimenko. The spacecraft's images are projected onto the comet and the separation of the Philae lander is visible as well",
    "license": "MIT License",
    "name": "Rosetta",
    "url": "https://www.openspaceproject.com",
    "version": "1.1"
  },
  "properties": [
    {
      "name": "Scene.67P.Renderable.PerformShading",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.ImagePlaneRosetta.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    }
  ],
  "time": {
    "type": "absolute",
    "value": "2014-08-01T03:05:00"
  },
  "version": {
    "major": 1,
    "minor": 1
  }
}
