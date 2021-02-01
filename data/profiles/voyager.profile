{
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
      "documentation": "Set camera focus to Voyager 1",
      "gui_path": "/Missions/Voyager",
      "is_local": false,
      "key": "V",
      "name": "Focvus on Voyager",
      "script": "openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.RetargetAnchor\", nil);openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Anchor\", 'Voyager_1');openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Aim\", '')"
    },
    {
      "documentation": "Sets the camera focus on Voyager 2",
      "gui_path": "/Missions/Voyager",
      "is_local": false,
      "key": "SHIFT+V",
      "name": "Focus on Voyager2",
      "script": "openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.RetargetAnchor\", nil);openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Anchor\", 'Voyager_2');openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Aim\", '');"
    },
    {
      "documentation": "Sets the time for Voyager's approach to Jupiter",
      "gui_path": "/",
      "is_local": false,
      "key": "SHIFT+J",
      "name": "Set Jupiter Approach",
      "script": "openspace.time.setTime(\"1979-01-20T01:32:07.914\")"
    },
    {
      "documentation": "Sets the time for Voyager's approach to Saturn",
      "gui_path": "/Missions/Voyager",
      "is_local": false,
      "key": "SHIFT+S",
      "name": "Set Saturn Approach",
      "script": "openspace.time.setTime(\"1980-10-20T07:43:42.645\");"
    },
    {
      "documentation": "Set the camera focus to Jupiter",
      "gui_path": "/Missions/Voyager",
      "is_local": false,
      "key": "J",
      "name": "Focus on Jupiter",
      "script": "openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.RetargetAnchor\", nil);openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Anchor\", 'Jupiter');openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Aim\", '');"
    },
    {
      "documentation": "Sets the camera focus on Saturn",
      "gui_path": "/Missions/Voyager",
      "is_local": false,
      "key": "S",
      "name": "Focus on Saturn",
      "script": "openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.RetargetAnchor\", nil);openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Anchor\", 'Saturn');openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.Aim\", '');"
    },
    {
      "documentation": "Toggles the trails of the minor moons",
      "gui_path": "/Solar System",
      "is_local": false,
      "key": "SHIFT+H",
      "name": "Toggle minor trails",
      "script": "local list = openspace.getProperty('{moonTrail_minor}.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
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
    "description": "This scene contains the NASA Voyager 1 and Voyager 2 missions as they were launched from Earth in the 1970s and observed the gas giants in the Solar System. The spacecraft models are included and are pointed accurately throughout the mission. Position and orientation information are available until the second half of the 21st century.",
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
    "minor": 0
  }
}