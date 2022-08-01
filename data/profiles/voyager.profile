{
  "actions": [
    {
      "documentation": "Set camera focus to Voyager 1",
      "gui_path": "/Missions/Voyager",
      "identifier": "profile.focus.voyager1",
      "is_local": false,
      "name": "Focus on Voyager",
      "script": "openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.RetargetAnchor\", nil);openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Anchor\", 'Voyager_1');openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Aim\", '')"
    },
    {
      "documentation": "Sets the camera focus on Voyager 2",
      "gui_path": "/Missions/Voyager",
      "identifier": "profile.focus.voyager2",
      "is_local": false,
      "name": "Focus on Voyager2",
      "script": "openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.RetargetAnchor\", nil);openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Anchor\", 'Voyager_2');openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Aim\", '');"
    },
    {
      "documentation": "Sets the time for Voyager's approach to Jupiter",
      "gui_path": "/",
      "identifier": "profile.setup.jupiter_approach",
      "is_local": false,
      "name": "Set Jupiter Approach",
      "script": "openspace.time.setTime(\"1979-01-20T01:32:07.914\")"
    },
    {
      "documentation": "Sets the time for Voyager's approach to Saturn",
      "gui_path": "/Missions/Voyager",
      "identifier": "profile.setup.saturn_approach",
      "is_local": false,
      "name": "Set Saturn Approach",
      "script": "openspace.time.setTime(\"1980-10-20T07:43:42.645\");"
    },
    {
      "documentation": "Set the camera focus to Jupiter",
      "gui_path": "/Missions/Voyager",
      "identifier": "profile.focus.jupiter",
      "is_local": false,
      "name": "Focus on Jupiter",
      "script": "openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.RetargetAnchor\", nil);openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Anchor\", 'Jupiter');openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Aim\", '');"
    },
    {
      "documentation": "Sets the camera focus on Saturn",
      "gui_path": "/Missions/Voyager",
      "identifier": "profile.focus.saturn",
      "is_local": false,
      "name": "Focus on Saturn",
      "script": "openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.RetargetAnchor\", nil);openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Anchor\", 'Saturn');openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Aim\", '');"
    },
    {
      "documentation": "Toggles the trails of the minor moons",
      "gui_path": "/Trails",
      "identifier": "profile.toggle.minor_trails",
      "is_local": false,
      "name": "Toggle minor trails",
      "script": "local list = openspace.getProperty('{moonTrail_minor}.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    }
  ],
  "assets": [
    "base",
    "scene/solarsystem/missions/voyager/dashboard",
    "scene/solarsystem/missions/voyager/voyager1",
    "scene/solarsystem/missions/voyager/voyager2",
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
      "action": "profile.focus.voyager1",
      "key": "V"
    },
    {
      "action": "profile.focus.voyager2",
      "key": "SHIFT+V"
    },
    {
      "action": "profile.setup.jupiter_approach",
      "key": "SHIFT+J"
    },
    {
      "action": "profile.setup.saturn_approach",
      "key": "SHIFT+S"
    },
    {
      "action": "profile.focus.jupiter",
      "key": "J"
    },
    {
      "action": "profile.focus.saturn",
      "key": "S"
    },
    {
      "action": "profile.toggle.minor_trails",
      "key": "SHIFT+H"
    }
  ],
  "mark_nodes": [
    "Earth",
    "Voyager 1",
    "Voyager 2",
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
    "version": "1.0"
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
