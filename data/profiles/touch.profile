{
  "additional_scripts": [
    "openspace.setPropertyValueSingle(\"Modules.CefWebGui.GuiUrl\", 'http://127.0.0.1:4680/frontend/#/ontouch');"
  ],
  "assets": [
    "base",
    "base_keybindings"
  ],
  "camera": {
    "altitude": 20000000.0,
    "anchor": "Earth",
    "latitude": 58.5877,
    "longitude": 16.1924,
    "type": "goToGeo"
  },
  "meta": {
    "author": "OpenSpace Team",
    "description": "Profile set up to load /touch path from the gui, which contains an alternate gui with stories for a touch screen",
    "license": "MIT License",
    "name": "Touch",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "modules": [
    {
      "loadedInstruction": "",
      "name": "Touch",
      "notLoadedInstruction": "openspace.printFatal('Could not load scene due to missing module \"touch\"');"
    }
  ],
  "properties": [
    {
      "name": "Scene.PlutoBarycenterTrail.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.Pluto.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.Charon.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    }
  ],
  "time": {
    "is_paused": false,
    "type": "relative",
    "value": "-1d"
  },
  "version": {
    "major": 1,
    "minor": 4
  }
}