{
  "version": { "major": 1, "minor": 3 },
  "meta": {
    "name": "name",
    "version": "version",
    "description": "description",
    "author": "author",
    "url": "url",
    "license": "license"
  },
  "modules": [
    { "name": "abs-module" },
    {
      "name": "def-module",
      "loadedInstruction": "instr"
    },
    {
      "name": "ghi-module",
      "notLoadedInstruction": "not_instr"
    },
    {
      "name": "jkl-module",
      "loadedInstruction": "instr",
      "notLoadedInstruction": "not_instr"
    }
  ],
  "assets": [
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/satellites/satellites",
    "folder1/folder2/asset",
    "folder3/folder4/asset2",
    "folder5/folder6/asset3"
  ],
  "properties": [
    {
      "type": "setPropertyValue",
      "name": "{earth_satellites}.Renderable.Enabled",
      "value": "false"
    },
    {
      "type": "setPropertyValue",
      "name": "property_name_1",
      "value": "property_value_1"
    },
    {
      "type": "setPropertyValue",
      "name": "property_name_2",
      "value": "property_value_2"
    },
    {
      "type": "setPropertyValue",
      "name": "property_name_3",
      "value": "property_value_3"
    },
    {
      "type": "setPropertyValueSingle",
      "name": "property_name_4",
      "value": "property_value_4"
    },
    {
      "type": "setPropertyValueSingle",
      "name": "property_name_5",
      "value": "property_value_5"
    },
    {
      "type": "setPropertyValueSingle",
      "name": "property_name_6",
      "value": "property_value_6"
    }
  ],
  "actions": [
    {
      "identifier": "profile.keybind.0",
      "documentation": "T documentation",
      "name": "T name",
      "gui_path": "T Gui-Path",
      "is_local": true,
      "script": "T script"
    },
    {
      "identifier": "profile.keybind.1",
      "documentation": "U documentation",
      "name": "U name",
      "gui_path": "U Gui-Path",
      "is_local": false,
      "script": "U script"
    },
    {
      "identifier": "profile.keybind.2",
      "documentation": "CTRL+V documentation",
      "name": "CTRL+V name",
      "gui_path": "CTRL+V Gui-Path",
      "is_local": false,
      "script": "CTRL+V script"
    }
  ],
  "keybindings": [
    {
      "action": "profile.keybind.0",
      "key": "T"
    },
    {
      "action": "profile.keybind.1",
      "key": "U"
    },
    {
      "action": "profile.keybind.2",
      "key": "CTRL+V"
    }
  ],
  "time": {
    "type": "relative",
    "value": "-1d",
    "is_paused": false
  },
  "camera": {
    "type": "goToGeo",
    "anchor": "Earth",
    "latitude": 58.5877,
    "longitude": 16.1924,
    "altitude": 2.0e+07
  },
  "mark_nodes": [
    "Earth", "Mars", "Moon", "Sun"
  ],
  "additional_scripts": [
    "script-1",
    "script-2",
    "script-3"
  ]
}
