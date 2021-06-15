{
  "version": { "major": 22, "minor": 21 },
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
  "keybindings": [
    {
      "key": "T",
      "documentation": "T documentation",
      "name": "T name",
      "gui_path": "T Gui-Path",
      "is_local": true,
      "script": "T script"
    },
    {
      "key": "U",
      "documentation": "U documentation",
      "name": "U name",
      "gui_path": "U Gui-Path",
      "is_local": false,
      "script": "U script"
    },
    {
      "key": "CTRL+V",
      "documentation": "CTRL+V documentation",
      "name": "CTRL+V name",
      "gui_path": "CTRL+V Gui-Path",
      "is_local": false,
      "script": "CTRL+V script"
    }
  ],
  "time": {
    "type": "relative",
    "value": "-1d"
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
