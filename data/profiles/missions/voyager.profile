{
  "additional_scripts": [
    "openspace.action.triggerAction('os.solarsystem.JupiterMinorMoonsOff')",
    "openspace.action.triggerAction('os.solarsystem.SaturnMinorMoonsOff')"
  ],
  "assets": [
    "base",
    "base_keybindings",
    "scene/solarsystem/missions/voyager/dashboard",
    "scene/solarsystem/missions/voyager/mission",
    "scene/solarsystem/missions/voyager/voyager1",
    "scene/solarsystem/missions/voyager/voyager2",
    "scene/solarsystem/missions/voyager/actions",
    "scene/solarsystem/planets/jupiter/minor_moons",
    "scene/solarsystem/planets/neptune/minor_moons",
    "scene/solarsystem/planets/saturn/minor_moons",
    "scene/solarsystem/planets/uranus/minor_moons"
  ],
  "camera": {
    "aim": "",
    "anchor": "Voyager_1",
    "frame": "",
    "position": {
      "x": 526781518487.1713,
      "y": 257168309890.07214,
      "z": -1381125204152.8174
    },
    "type": "setNavigationState"
  },
  "delta_times": [
    1.0,
    30.0,
    60.0,
    300.0,
    720.0,
    2880.0,
    14400.0,
    57600.0,
    230400.0,
    921600.0,
    3686400.0,
    7372800.0,
    14745600.0
  ],
  "keybindings": [
    {
      "action": "os.voyager.FocusVoyager1",
      "key": "V"
    },
    {
      "action": "os.voyager.FocusVoyager2",
      "key": "SHIFT+V"
    },
    {
      "action": "os.voyager.SetupJupiterApproach",
      "key": "SHIFT+J"
    },
    {
      "action": "os.voyager.SetupSaturnApproach",
      "key": "SHIFT+S"
    },
    {
      "action": "os.voyager.JupiterFocus",
      "key": "J"
    },
    {
      "action": "os.voyager.SaturnFocus",
      "key": "S"
    },
    {
      "action": "os.voyager.ToggleMinorMoonTrails",
      "key": "SHIFT+H"
    }
  ],
  "mark_nodes": [
    "Voyager_1",
    "Voyager_2",
    "Earth",
    "Jupiter",
    "Saturn",
    "Uranus",
    "Neptune"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "This scene contains the NASA Voyager 1 and Voyager 2 missions as they were launched from Earth in the 1970s and observed the gas giants in the Solar System. The spacecraft models are included and are pointed accurately throughout the mission. Position and orientation information are available until the second half of the 21st century",
    "license": "MIT License",
    "name": "Voyager",
    "url": "https://www.openspaceproject.com",
    "version": "1.1"
  },
  "properties": [
    {
      "name": "Scene.PlutoBarycenterTrail.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    }
  ],
  "time": {
    "type": "absolute",
    "value": "1977-09-10T12:00:00"
  },
  "version": {
    "major": 1,
    "minor": 1
  }
}
