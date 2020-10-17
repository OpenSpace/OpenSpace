{
  "assets": [
    "base",
    "scene/solarsystem/missions/rosetta/67p",
    "scene/solarsystem/missions/rosetta/dashboard",
    "scene/solarsystem/missions/rosetta/rosetta"
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
      "documentation": "Sets the focus of the camera on 67P",
      "gui_path": "/Missions/Rosetta",
      "is_local": false,
      "key": "A",
      "name": "Focus on 67P",
      "script": "openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', '67P'); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    },
    {
      "documentation": "Sets the focus of the camera on Rosetta",
      "gui_path": "/Missions/Rosetta",
      "is_local": false,
      "key": "S",
      "name": "Focus on Rosetta",
      "script": "openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'Rosetta'); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    },
    {
      "documentation": "Jumps to the time when the Philae lander is released.",
      "gui_path": "/Missions/Rosetta",
      "is_local": false,
      "key": "F6",
      "name": "Set lander release time",
      "script": "openspace.time.setTime('2014-11-12T08:20:00.00');"
    },
    {
      "documentation": "Removes all image projections from 67P.",
      "gui_path": "/Missions/Rosetta",
      "is_local": false,
      "key": "F8",
      "name": "Clear 67P projections",
      "script": "openspace.setPropertyValue('Scene.67P.Renderable.ProjectionComponent.ClearAllProjections', true);"
    },
    {
      "documentation": "Toggles the visibility of all trails further from the Sun than 67P.",
      "gui_path": "/Missions/Rosetta",
      "is_local": false,
      "key": "E",
      "name": "Toggle outer planetary trails",
      "script": "local list = openspace.getProperty('{planetTrail_giants}.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Toggles the visibility of the free floating image plane.",
      "gui_path": "/Missions/Rosetta",
      "is_local": false,
      "key": "I",
      "name": "Toggle image plane",
      "script": "openspace.setPropertyValueSingle('Scene.ImagePlaneRosetta.Renderable.Enabled', not openspace.getPropertyValue('Scene.ImagePlaneRosetta.Renderable.Enabled'));"
    },
    {
      "documentation": "Toggles the visibility of Philae's trail.",
      "gui_path": "/Missions/Rosetta",
      "is_local": false,
      "key": "O",
      "name": "Toggle Philae trail",
      "script": "openspace.setPropertyValueSingle('Scene.PhilaeTrail.Renderable.Enabled', not openspace.getPropertyValue('Scene.PhilaeTrail.Renderable.Enabled'));"
    },
    {
      "documentation": "Enables or disables the image projection on 67P.",
      "gui_path": "/Missions/Rosetta",
      "is_local": false,
      "key": "P",
      "name": "Toggle 67P projection",
      "script": "openspace.setPropertyValueSingle('Scene.67P.Renderable.ProjectionComponent.PerformProjection', not openspace.getPropertyValue('Scene.67P.Renderable.ProjectionComponent.PerformProjection'));"
    }
  ],
  "mark_nodes": [
    "67P",
    "Rosetta",
    "Philae"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "The Rosetta scene shows the entire mission of ESA’s Rosetta spacecraft around comet 67P, also known as Churyumov-Gerasimenko. The spacecraft’s images are projected onto the comet and the separation of the Philae lander is visible as well.",
    "license": "MIT License",
    "name": "Rosetta",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
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
    "minor": 0
  }
}