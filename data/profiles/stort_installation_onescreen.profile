{
  "actions": [],
  "assets": [
    "installationspecific/stort_installation_base"
  ],
  "camera": {
    "altitude": 17000000.0,
    "anchor": "Earth",
    "latitude": 58.5877,
    "longitude": 16.1924,
    "type": "goToGeo"
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
      "action": "gui.scaleup",
      "key": "CTRL+UP"
    },
    {
      "action": "gui.scaledown",
      "key": "CTRL+DOWN"
    }
  ],
  "mark_nodes": [
    "Earth",
    "Mars",
    "Moon",
    "Sun",
    "Venus",
    "ISS"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "Profile specifically created for the STORT exhibition.",
    "license": "MIT License",
    "name": "Default",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "modules": [
    {
      "loadedInstruction": "",
      "name": "Touch",
      "notLoadedInstruction": "openspace.printFatal('Could not load profile due to missing module \"Touch\"');"
    }
  ],
  "properties": [
    {
      "name": "Modules.CefWebGui.Visible",
      "type": "setPropertyValueSingle",
      "value": "true"
    },
    {
      "name": "Modules.CefWebGui.GuiScale",
      "type": "setPropertyValueSingle",
      "value": "1.0"
    },
    {
      "name": "Modules.CefWebGui.GuiUrl",
      "type": "setPropertyValueSingle",
      "value": "http://localhost:4680/stort-installation/homeOneScreen/"
    }
  ],
  "time": {
    "type": "absolute",
    "value": "2021-08-26T03:20:55.505"
  },
  "version": {
    "major": 1,
    "minor": 1
  }
}
