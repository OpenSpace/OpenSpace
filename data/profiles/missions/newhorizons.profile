{
  "assets": [
    "base",
    "base_keybindings",
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
      "action": "os.newhorizons.FocusNewHorizons",
      "key": "A"
    },
    {
      "action": "os.newhorizons.AimPluto",
      "key": "SHIFT+A"
    },
    {
      "action": "os.newhorizons.FocusPluto",
      "key": "S"
    },
    {
      "action": "os.newhorizons.FocusCharon",
      "key": "D"
    },
    {
      "action": "os.newhorizons.ToggleImageProjection",
      "key": "F7"
    },
    {
      "action": "os.newhorizons.ClearImageProjections",
      "key": "F8"
    },
    {
      "action": "os.newhorizons.Approach",
      "key": "F9"
    },
    {
      "action": "os.newhorizons.IncreaseHeightmapPluto",
      "key": "KP_8"
    },
    {
      "action": "os.newhorizons.IncreaseHeightmapPluto",
      "key": "CTRL+I"
    },
    {
      "action": "os.newhorizons.DecreaseHeightmapPluto",
      "key": "CTRL+K"
    },
    {
      "action": "os.newhorizons.DecreaseHeightmapPluto",
      "key": "KP_2"
    },
    {
      "action": "os.newhorizons.IncreaseHeightmapCharon",
      "key": "KP_9"
    },
    {
      "action": "os.newhorizons.IncreaseHeightmapCharon",
      "key": "CTRL+O"
    },
    {
      "action": "os.newhorizons.DecreaseHeightmapCharon",
      "key": "KP_3"
    },
    {
      "action": "os.newhorizons.DecreaseHeightmapCharon",
      "key": "CTRL+L"
    },
    {
      "action": "os.newhorizons.TogglePlutoTrail",
      "key": "O"
    },
    {
      "action": "os.newhorizons.TogglePlutoLabels",
      "key": "J"
    },
    {
      "action": "os.newhorizons.ToggleNewHorizonsLabels",
      "key": "I"
    },
    {
      "action": "os.newhorizons.ToggleShadows",
      "key": "SHIFT+T"
    },
    {
      "action": "os.newhorizons.ToggleNewHorizonsTrail",
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
    "description": "This profile shows the acquisition of NASA New Horizons' images of the Plutonian system in July 2015. The profile starts at around 10:00 on July 14th, around 10 minutes before a new image campaign starts. By selecting Pluto as the Origin and moving time faster, you can see the imprint of the instrument's field-of-view on the planetary surface and see the images being projected. A timer on the top left of the screen shows when the next image is being taken.",
    "license": "MIT License",
    "name": "New Horizons",
    "url": "https://www.openspaceproject.com",
    "version": "1.1"
  },
  "panel_visibility": {
    "mission": true
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
      "name": "Scene.NixText.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.PlutoText.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.CharonText.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.HydraText.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.KerberosText.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.StyxText.Renderable.Enabled",
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
    "is_paused": false,
    "type": "absolute",
    "value": "2015-07-14T08:00:00"
  },
  "version": {
    "major": 1,
    "minor": 4
  }
}
