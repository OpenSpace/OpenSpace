{
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
      "documentation": "Sets the focus of the camera on 'NewHorizons'.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "A",
      "name": "Focus on New Horizons",
      "script": "openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'NewHorizons');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    },
    {
      "documentation": "Anchor at New Horizons, Aim at Pluto",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "SHIFT+A",
      "name": "Anchor NH, Aim  Pluto",
      "script": "openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'NewHorizons');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', 'Pluto');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)"
    },
    {
      "documentation": "Sets the focus of the camera on 'Pluto'",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "S",
      "name": "Focus on Pluto",
      "script": "openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'PlutoProjection') ;openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    },
    {
      "documentation": "Sets the focus of the camera on 'Charon'.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "D",
      "name": "Focus on Charon",
      "script": "openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'Charon');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    },
    {
      "documentation": "Toggles New Horizons image projection",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "F7",
      "name": "Toggle NH Image Projection",
      "script": "local enabled = openspace.getPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.PerformProjection'); openspace.setPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.PerformProjection', not enabled); openspace.setPropertyValue('Scene.CharonProjection.Renderable.ProjectionComponent.PerformProjection', not enabled);"
    },
    {
      "documentation": "Removes all image projections from Pluto and Charon.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "F8",
      "name": "Clear image projections",
      "script": "openspace.setPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.ClearAllProjections', true); openspace.setPropertyValue('Scene.CharonProjection.Renderable.ProjectionComponent.ClearAllProjections', true);"
    },
    {
      "documentation": "Jumps to the 14th of July 2015 at 0900 UTC and clears all projections.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "F9",
      "name": "Reset time and projections",
      "script": "openspace.time.setTime('2015-07-14T09:00:00.00');openspace.setPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.ClearAllProjections', true);openspace.setPropertyValue('Scene.CharonProjection.Renderable.ProjectionComponent.ClearAllProjections', true);"
    },
    {
      "documentation": "Increases the height map exaggeration on Pluto.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "KP8",
      "name": "Pluto HeightExaggeration + (KP)",
      "script": "openspace.setPropertyValueSingle(\"Scene.PlutoProjection.Renderable.HeightExaggeration\", openspace.getPropertyValue(\"Scene.PlutoProjection.Renderable.HeightExaggeration\") + 5000);"
    },
    {
      "documentation": "Increases the height map exaggeration on Pluto.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "CTRL+I",
      "name": "Pluto HeightExaggeration +",
      "script": "openspace.setPropertyValueSingle(\"Scene.PlutoProjection.Renderable.HeightExaggeration\", openspace.getPropertyValue(\"Scene.PlutoProjection.Renderable.HeightExaggeration\") + 5000);"
    },
    {
      "documentation": "Decreases the height map exaggeration on Pluto.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "CTRL+K",
      "name": "Pluto HeightExaggeration -",
      "script": "openspace.setPropertyValueSingle(\"Scene.PlutoProjection.Renderable.HeightExaggeration\", openspace.getPropertyValue(\"Scene.PlutoProjection.Renderable.HeightExaggeration\") - 5000);"
    },
    {
      "documentation": "Decreases the height map exaggeration on Pluto.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "KP2",
      "name": "Pluto HeightExaggeration - (KP)",
      "script": "openspace.setPropertyValueSingle(\"Scene.PlutoProjection.Renderable.HeightExaggeration\", openspace.getPropertyValue(\"Scene.PlutoProjection.Renderable.HeightExaggeration\") - 5000);"
    },
    {
      "documentation": "Increases the height map exaggeration on Charon.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "KP9",
      "name": "Charon HeightExaggeration + (KP)",
      "script": "openspace.setPropertyValueSingle(\"Scene.CharonProjection.Renderable.HeightExaggeration\", openspace.getPropertyValue(\"Scene.CharonProjection.Renderable.HeightExaggeration\") + 5000);"
    },
    {
      "documentation": "Increases the height map exaggeration on Charon.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "CTRL+O",
      "name": "Charon HeightExaggeration +",
      "script": "openspace.setPropertyValueSingle(\"Scene.CharonProjection.Renderable.HeightExaggeration\", openspace.getPropertyValue(\"Scene.CharonProjection.Renderable.HeightExaggeration\") + 5000);"
    },
    {
      "documentation": "Decreases the height map exaggeration on Charon.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "KP3",
      "name": "Charon HeightExaggeration - (KP)",
      "script": "openspace.setPropertyValueSingle(\"Scene.CharonProjection.Renderable.HeightExaggeration\", openspace.getPropertyValue(\"Scene.CharonProjection.Renderable.HeightExaggeration\") - 5000);"
    },
    {
      "documentation": "Decreases the height map exaggeration on Charon.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "CTRL+L",
      "name": "Charon HeightExaggeration -",
      "script": "openspace.setPropertyValueSingle(\"Scene.CharonProjection.Renderable.HeightExaggeration\", openspace.getPropertyValue(\"Scene.CharonProjection.Renderable.HeightExaggeration\") - 5000);"
    },
    {
      "documentation": "Toggles the visibility of the trail behind Pluto.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "O",
      "name": "Toggle Pluto Trail",
      "script": "openspace.setPropertyValueSingle('Scene.PlutoBarycentricTrail.Renderable.Enabled', not openspace.getPropertyValue('Scene.PlutoBarycentricTrail.Renderable.Enabled'));"
    },
    {
      "documentation": "Toggles the visibility of the text labels of Pluto, Charon, Hydra, Nix, Kerberos, and Styx.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "J",
      "name": "Toggle Pluto Labels",
      "script": "local list = {\"Scene.PlutoText.Enabled\", \"Scene.CharonText.Enabled\", \"Scene.HydraText.Enabled\", \"Scene.NixText.Enabled\", \"Scene.KerberosText.Enabled\", \"Scene.StyxText.Enabled\"}; for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Toggles the visibility of the labels for the New Horizons instruments.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "I",
      "name": "Toggle New Horizons Labels",
      "script": "local v = openspace.getPropertyValue(\"Scene.Labels.Renderable.Opacity\"); if v <= 0.5 then openspace.setPropertyValueSingle(\"Scene.Labels.Renderable.Opacity\",1.0,2.0) else openspace.setPropertyValueSingle(\"Scene.Labels.Renderable.Opacity\",0.0,2.0) end"
    },
    {
      "documentation": "Draws the instrument field of views in a solid color or as lines.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "M",
      "name": "Toggle instrument FOVs",
      "script": "local list = {\"Scene.PlutoText.Enabled\", \"Scene.NH_LORRI.Renderable.SolidDraw\", \"Scene.NH_RALPH_LEISA.Renderable.SolidDraw\", \"Scene.NH_RALPH_MVIC_PAN1.Renderable.SolidDraw\", \"Scene.NH_RALPH_MVIC_PAN2.Renderable.SolidDraw\", \"Scene.NH_RALPH_MVIC_RED.Renderable.SolidDraw\", \"Scene.NH_RALPH_MVIC_BLUE.Renderable.SolidDraw\", \"Scene.NH_RALPH_MVIC_FT.Renderable.SolidDraw\", \"Scene.NH_RALPH_MVIC_METHANE.Renderable.SolidDraw\", \"Scene.NH_RALPH_MVIC_NIR.Renderable.SolidDraw\", \"Scene.NH_ALICE_AIRGLOW.Renderable.SolidDraw\", \"Scene.NH_ALICE_SOC.Renderable.SolidDraw\"}; for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Toggles the visibility of the shadow visualization of Pluto and Charon.",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "SHIFT+T",
      "name": "Toggle Shadows",
      "script": "openspace.setPropertyValueSingle('Scene.PlutoShadow.Renderable.Enabled', not openspace.getPropertyValue('Scene.PlutoShadow.Renderable.Enabled'));openspace.setPropertyValueSingle('Scene.CharonShadow.Renderable.Enabled', not openspace.getPropertyValue('Scene.CharonShadow.Renderable.Enabled'));"
    },
    {
      "documentation": "Toggles the trail of New Horizons",
      "gui_path": "/Missions/New Horizons",
      "is_local": false,
      "key": "T",
      "name": "Toggle NH Trail",
      "script": "openspace.setPropertyValueSingle('Scene.NewHorizonsTrailPluto.Renderable.Enabled', not openspace.getPropertyValue('Scene.NewHorizonsTrailPluto.Renderable.Enabled'));"
    }
  ],
  "mark_nodes": [
    "NewHorizons",
    "CharonProjection",
    "PlutoProjection"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "This profile shows the acquisition of NASA New Horizons’ images of the Plutonian system in July 2015. The profile starts at around 10:00 on July 14th, around 10 minutes before a new image campaign starts. By selecting Pluto as the Origin and moving time faster, you can see the imprint of the instrument’s field-of-view on the planetary surface and see the images being projected. A timer on the top left of the screen shows when the next image is being taken.",
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
    "minor": 0
  }
}
