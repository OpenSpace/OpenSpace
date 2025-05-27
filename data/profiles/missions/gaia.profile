{
  "assets": [
    "base",
    "base_keybindings",
    "scene/milkyway/gaia/apogee",
    "scene/milkyway/gaia/gaiastars",
    "scene/milkyway/gaia/galah",
    "scene/solarsystem/telescopes/gaia/dashboard",
    "scene/solarsystem/telescopes/gaia/gaia",
    "scene/solarsystem/telescopes/gaia/trail"
  ],
  "camera": {
    "aim": "",
    "anchor": "Earth",
    "frame": "",
    "position": {
      "x": 1000000000000.0,
      "y": 1000000000000.0,
      "z": 1000000000000.0
    },
    "type": "setNavigationState"
  },
  "delta_times": [
    1.0,
    157680000000.0,
    315360000000.0,
    1576800000000.0,
    3153600000000.0
  ],
  "mark_nodes": [
    "Gaia",
    "Earth",
    "Sun"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "This scene contains a new rendering method to show the massive ESA Gaia stars dataset. By default, it loads the few million stars of the Gaia DR2 that contain radial velocities.",
    "license": "MIT License",
    "name": "Gaia",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "modules": [
    {
      "loadedInstruction": "",
      "name": "Gaia",
      "notLoadedInstruction": "openspace.printFatal('Could not load gaia profile due to missing module \"Gaia\"')"
    }
  ],
  "panel_visibility": {
    "mission": true
  },
  "properties": [
    {
      "name": "Scene.Stars.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    }
  ],
  "time": {
    "is_paused": false,
    "type": "absolute",
    "value": "2019-06-10T00:00:00"
  },
  "version": {
    "major": 1,
    "minor": 4
  }
}
