{
  "actions": [
    {
      "documentation": "Sets the focus of the camera on 'NewHorizons'",
      "gui_path": "/Missions/New Horizons",
      "identifier": "profile.focus.newhorizons",
      "is_local": false,
      "name": "Focus on New Horizons",
      "script": "openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'NewHorizons');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    },
    {
      "documentation": "Anchor at New Horizons, Aim at Pluto",
      "gui_path": "/Missions/New Horizons",
      "identifier": "profile.focus.anchor_nh|aim_pluto",
      "is_local": false,
      "name": "Anchor NH, Aim  Pluto",
      "script": "openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'NewHorizons');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', 'Pluto');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"
    },
    {
      "documentation": "Sets the focus of the camera on 'Pluto'",
      "gui_path": "/Missions/New Horizons",
      "identifier": "profile.focus.pluto",
      "is_local": false,
      "name": "Focus on Pluto",
      "script": "openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'PlutoProjection') ;openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    },
    {
      "documentation": "Sets the focus of the camera on 'Charon'",
      "gui_path": "/Missions/New Horizons",
      "identifier": "profile.focus.charon",
      "is_local": false,
      "name": "Focus on Charon",
      "script": "openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'Charon');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    },
    {
      "documentation": "Toggles New Horizons image projection",
      "gui_path": "/Missions/New Horizons",
      "identifier": "profile.toggle_nh_image_projections",
      "is_local": false,
      "name": "Toggle NH Image Projection",
      "script": "local enabled = openspace.getPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.PerformProjection'); openspace.setPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.PerformProjection', not enabled); openspace.setPropertyValue('Scene.CharonProjection.Renderable.ProjectionComponent.PerformProjection', not enabled);"
    },
    {
      "documentation": "Removes all image projections from Pluto and Charon",
      "gui_path": "/Missions/New Horizons",
      "identifier": "profile.clear_image_projections",
      "is_local": false,
      "name": "Clear image projections",
      "script": "openspace.setPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.ClearAllProjections', true); openspace.setPropertyValue('Scene.CharonProjection.Renderable.ProjectionComponent.ClearAllProjections', true);"
    },
    {
      "documentation": "Jumps to the 14th of July 2015 at 0900 UTC and clears all projections",
      "gui_path": "/Missions/New Horizons",
      "identifier": "profile.setup.approach",
      "is_local": false,
      "name": "Reset time and projections",
      "script": "openspace.time.setTime('2015-07-14T09:00:00.00');openspace.setPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.ClearAllProjections', true);openspace.setPropertyValue('Scene.CharonProjection.Renderable.ProjectionComponent.ClearAllProjections', true);"
    },
    {
      "documentation": "Increases the height map exaggeration on Pluto",
      "gui_path": "/Missions/New Horizons",
      "identifier": "profile.pluto.increase_height_exaggeration",
      "is_local": false,
      "name": "Pluto HeightExaggeration +",
      "script": "openspace.setPropertyValueSingle(\"Scene.PlutoProjection.Renderable.HeightExaggeration\", openspace.getPropertyValue(\"Scene.PlutoProjection.Renderable.HeightExaggeration\") + 5000);"
    },
    {
      "documentation": "Decreases the height map exaggeration on Pluto",
      "gui_path": "/Missions/New Horizons",
      "identifier": "profile.pluto.decrease_height_exaggeration",
      "is_local": false,
      "name": "Pluto HeightExaggeration -",
      "script": "openspace.setPropertyValueSingle(\"Scene.PlutoProjection.Renderable.HeightExaggeration\", openspace.getPropertyValue(\"Scene.PlutoProjection.Renderable.HeightExaggeration\") - 5000);"
    },
    {
      "documentation": "Increases the height map exaggeration on Charon",
      "gui_path": "/Missions/New Horizons",
      "identifier": "profile.charon.increase_height_exaggeration",
      "is_local": false,
      "name": "Charon HeightExaggeration +",
      "script": "openspace.setPropertyValueSingle(\"Scene.CharonProjection.Renderable.HeightExaggeration\", openspace.getPropertyValue(\"Scene.CharonProjection.Renderable.HeightExaggeration\") + 5000);"
    },
    {
      "documentation": "Decreases the height map exaggeration on Charon",
      "gui_path": "/Missions/New Horizons",
      "identifier": "profile.charon.decrease_height_exaggeration",
      "is_local": false,
      "name": "Charon HeightExaggeration -",
      "script": "openspace.setPropertyValueSingle(\"Scene.CharonProjection.Renderable.HeightExaggeration\", openspace.getPropertyValue(\"Scene.CharonProjection.Renderable.HeightExaggeration\") - 5000);"
    },
    {
      "documentation": "Toggles the visibility of the trail behind Pluto",
      "gui_path": "/Missions/New Horizons",
      "identifier": "profile.toggle.pluto_trail",
      "is_local": false,
      "name": "Toggle Pluto Trail",
      "script": "openspace.setPropertyValueSingle('Scene.PlutoBarycentricTrail.Renderable.Enabled', not openspace.getPropertyValue('Scene.PlutoBarycentricTrail.Renderable.Enabled'));"
    },
    {
      "documentation": "Toggles the visibility of the text labels of Pluto, Charon, Hydra, Nix, Kerberos, and Styx",
      "gui_path": "/Missions/New Horizons",
      "identifier": "profile.toggle.pluto_labels",
      "is_local": false,
      "name": "Toggle Pluto Labels",
      "script": "local list = {\"Scene.PlutoText.Renderable.Enabled\", \"Scene.CharonText.Renderable.Enabled\", \"Scene.HydraText.Renderable.Enabled\", \"Scene.NixText.Renderable.Enabled\", \"Scene.KerberosText.Renderable.Enabled\", \"Scene.StyxText.Renderable.Enabled\"}; for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Toggles the visibility of the labels for the New Horizons instruments",
      "gui_path": "/Missions/New Horizons",
      "identifier": "profile.toggle_nh_labels",
      "is_local": false,
      "name": "Toggle New Horizons Labels",
      "script": "local v = openspace.getPropertyValue(\"Scene.Labels.Renderable.Opacity\"); if v <= 0.5 then openspace.setPropertyValueSingle(\"Scene.Labels.Renderable.Opacity\",1.0,2.0) else openspace.setPropertyValueSingle(\"Scene.Labels.Renderable.Opacity\",0.0,2.0) end"
    },
    {
      "documentation": "Toggles the visibility of the shadow visualization of Pluto and Charon",
      "gui_path": "/Missions/New Horizons",
      "identifier": "profile.toggle_shadows",
      "is_local": false,
      "name": "Toggle Shadows",
      "script": "openspace.setPropertyValueSingle('Scene.PlutoShadow.Renderable.Enabled', not openspace.getPropertyValue('Scene.PlutoShadow.Renderable.Enabled'));openspace.setPropertyValueSingle('Scene.CharonShadow.Renderable.Enabled', not openspace.getPropertyValue('Scene.CharonShadow.Renderable.Enabled'));"
    },
    {
      "documentation": "Toggles the trail of New Horizons",
      "gui_path": "/Missions/New Horizons",
      "identifier": "profile.toggle.nh_trail",
      "is_local": false,
      "name": "Toggle NH Trail",
      "script": "openspace.setPropertyValueSingle('Scene.NewHorizonsTrailPluto.Renderable.Enabled', not openspace.getPropertyValue('Scene.NewHorizonsTrailPluto.Renderable.Enabled'));"
    }
  ],
  "assets": [
    "base",
    "scene/solarsystem/missions/newhorizons/dashboard",
    "scene/solarsystem/missions/newhorizons/model",
    "scene/solarsystem/missions/newhorizons/newhorizons"
  ],
  "camera": {
    "aim": "",
    "anchor": "NewHorizons",
    "frame": "Root",
    "pitch": 0.036092,
    "position": {
      "x": -111.9326,
      "y": -35.20605,
      "z": 33.42737
    },
    "type": "setNavigationState",
    "up": {
      "x": -0.188963,
      "y": 0.921904,
      "z": 0.338209
    },
    "yaw": 0.0563239
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
    604800.0
  ],
  "keybindings": [
    {
      "action": "profile.focus.newhorizons",
      "key": "A"
    },
    {
      "action": "profile.focus.anchor_nh|aim_pluto",
      "key": "SHIFT+A"
    },
    {
      "action": "profile.focus.pluto",
      "key": "S"
    },
    {
      "action": "profile.focus.charon",
      "key": "D"
    },
    {
      "action": "profile.toggle_nh_image_projections",
      "key": "F7"
    },
    {
      "action": "profile.clear_image_projections",
      "key": "F8"
    },
    {
      "action": "profile.setup.approach",
      "key": "F9"
    },
    {
      "action": "profile.pluto.increase_height_exaggeration",
      "key": "KP_8"
    },
    {
      "action": "profile.pluto.increase_height_exaggeration",
      "key": "CTRL+I"
    },
    {
      "action": "profile.pluto.decrease_height_exaggeration",
      "key": "CTRL+K"
    },
    {
      "action": "profile.pluto.decrease_height_exaggeration",
      "key": "KP_2"
    },
    {
      "action": "profile.charon.increase_height_exaggeration",
      "key": "KP_9"
    },
    {
      "action": "profile.charon.increase_height_exaggeration",
      "key": "CTRL+O"
    },
    {
      "action": "profile.charon.decrease_height_exaggeration",
      "key": "KP_3"
    },
    {
      "action": "profile.charon.decrease_height_exaggeration",
      "key": "CTRL+L"
    },
    {
      "action": "profile.toggle.pluto_trail",
      "key": "O"
    },
    {
      "action": "profile.toggle.pluto_labels",
      "key": "J"
    },
    {
      "action": "profile.toggle_nh_labels",
      "key": "I"
    },
    {
      "action": "profile.toggle_shadows",
      "key": "SHIFT+T"
    },
    {
      "action": "profile.toggle.nh_trail",
      "key": "T"
    }
  ],
  "mark_nodes": [
    "NewHorizons",
    "CharonProjection",
    "PlutoProjection"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "This profile shows the acquisition of NASA New Horizons' images of the Plutonian system in July 2015. The profile starts at around 10:00 on July 14th, around 10 minutes before a new image campaign starts. By selecting Pluto as the Origin and moving time faster, you can see the imprint of the instrument's field-of-view on the planetary surface and see the images being projected. A timer on the top left of the screen shows when the next image is being taken",
    "license": "MIT License",
    "name": "New Horizons",
    "url": "https://www.openspaceproject.com",
    "version": "1.1"
  },
  "properties": [
    {
      "name": "Scene.Pluto.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.Charon.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.PlutoBarycenterTrail.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.PlutoProjection.Renderable.ColorTexturePaths",
      "type": "setPropertyValue",
      "value": "1.000000"
    }
  ],
  "time": {
    "type": "absolute",
    "value": "2015-07-14T08:00:00"
  },
  "version": {
    "major": 1,
    "minor": 1
  }
}
