{
  "actions": [
    {
      "documentation": "Retargets the camera on points center",
      "gui_path": "/PointsCenter",
      "identifier": "profile.focus.pointscenter",
      "is_local": false,
      "name": "Focus on Point center",
      "script": "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'CosmicLifePoints')openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    },
     {
      "documentation": "World image enables when human asset is enable",
      "gui_path": "/Cosmic",
      "identifier": "profile.enable.worldimage",
      "is_local": false,
      "name": "Enable world image",
      "script": "openspace.setPropertyValueSingle('Scene.all.Renderable.Enabled', 'true'); openspace.setPropertyValueSingle('ScreenSpace.humans.Enabled', 'true'); "
    },

  ],
  "assets": [
    "base_blank",
    "cosmic_view/consensus",
    "cosmic_view/endangered",
    "cosmic_view/humans",
    "cosmic_view/lineage",
    "cosmic_view/mutations"
  ],
  "camera": {
    "altitude": 2700000000000.0,
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
  "meta": {
    "author": "OpenSpace Team",
    "description": "A Cosmic View of Life Thesis Profile. Visualize relations between species.",
    "license": "MIT License",
    "name": "cosmic_view",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "time": {
    "type": "relative",
    "value": "-1d"
  },
  "version": {
    "major": 1,
    "minor": 1
  }
}