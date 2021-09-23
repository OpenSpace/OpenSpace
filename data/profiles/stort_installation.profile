{
  "actions": [
    {
      "documentation": "Toggle trails on or off for satellites around Earth",
      "gui_path": "/Earth",
      "identifier": "profile.keybind.0",
      "is_local": false,
      "name": "Toggle satellite trails",
      "script": "local list = openspace.getProperty('{earth_satellites}.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end"
    },
    {
      "documentation": "Refocuses the camera on the ISS",
      "gui_path": "/Earth",
      "identifier": "profile.keybind.1",
      "is_local": false,
      "name": "Focus ISS",
      "script": "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'ISS');openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    },
    {
      "documentation": "Retargets the camera on Earth",
      "gui_path": "/Earth",
      "identifier": "profile.keybind.2",
      "is_local": false,
      "name": "Focus on Earth",
      "script": "openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Earth')openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    }
  ],
  "assets": [
    "base",
    "installationspecific/stort_state_machine",
    "installationspecific/others/olympus_mons_node",
    "scene/solarsystem/dwarf_planets/pluto/charon/charon",
    "scene/solarsystem/dwarf_planets/pluto/charon/charon_trail",
    "scene/solarsystem/dwarf_planets/pluto/minor/hydra",
    "scene/solarsystem/dwarf_planets/pluto/minor/kerberos",
    "scene/solarsystem/dwarf_planets/pluto/minor/nix",
    "scene/solarsystem/dwarf_planets/pluto/minor/styx",
    "scene/solarsystem/dwarf_planets/pluto/pluto",
    "scene/solarsystem/missions/voyagerpioneer/voyager1_2__pioneer10_11",
    "scene/solarsystem/planets/earth/satellites/misc/iss",
    "installationspecific/substates/missions/cassini/trail",
    "installationspecific/substates/mars_moons/deimos",
    "installationspecific/substates/mars_moons/phobos",
    "scene/solarsystem/planets/jupiter/major_moons",
    "scene/solarsystem/planets/jupiter/minor_moons",
    "scene/solarsystem/planets/neptune/inner_moons",
    "scene/solarsystem/planets/neptune/irregular_prograde_moons",
    "scene/solarsystem/planets/neptune/irregular_retrograde_moons",
    "scene/solarsystem/planets/neptune/major_moons",
    "scene/solarsystem/planets/neptune/minor_moons",
    "scene/solarsystem/planets/neptune/neptune",
    "scene/solarsystem/planets/neptune/triton",
    "scene/solarsystem/planets/saturn/major_moons",
    "scene/solarsystem/planets/saturn/minor_moons",
    "scene/solarsystem/planets/uranus/major_moons",
    "scene/solarsystem/planets/uranus/minor_moons"
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
      "action": "profile.keybind.0",
      "key": "S"
    },
    {
      "action": "profile.keybind.1",
      "key": "I"
    },
    {
      "action": "profile.keybind.2",
      "key": "HOME"
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
  "properties": [
    {
      "name": "Scene.Phobos.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "Scene.Deimos.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "NavigationHandler.OrbitalNavigator.LinearFlight.FlightDestinationFactor",
      "type": "setPropertyValueSingle",
      "value": "0.05"
    },
    {
      "name": "Scene.Constellations.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "true"
    },
    {
      "name": "Scene.Constellations.Renderable.Opacity",
      "type": "setPropertyValueSingle",
      "value": "0.0"
    },
    {
      "name": "Scene.ConstellationArt*.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "true"
    },
    {
      "name": "Scene.ConstellationArt*.Renderable.Opacity",
      "type": "setPropertyValue",
      "value": "0.0"
    },
    {
      "name": "Scene.Mars.Renderable.Layers.ColorLayers.Themis_IR_Day_Sweden.Enabled",
      "type": "setPropertyValueSingle",
      "value": "true"
    },
    {
      "name": "{solarsystem_labels}.Renderable.FontSize",
      "type": "setPropertyValue",
      "value": "70"
    },
    {
      "name": "{solarsystem_labels}.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "true"
    },
    {
      "name": "Scene.SunLabel.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "false"
    },
    {
      "name": "{solarsystem_labels}.Renderable.Opacity",
      "type": "setPropertyValue",
      "value": "0.0"
    },
    {
      "name": "{moonTrail_minor}.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "{moonTrail_minor}.Renderable.Appearance.Color",
      "type": "setPropertyValue",
      "value": "{0.3, 0.5, 0.5}"
    },
    {
      "name": "Scene.Voyager1.Renderable.Appearance.Color",
      "type": "setPropertyValueSingle",
      "value": "{0.424000, 1.000000, 0.404000}"
    },
    {
      "name": "Scene.Voyager2.Renderable.Appearance.Color",
      "type": "setPropertyValueSingle",
      "value": "{0.424000, 1.000000, 0.404000}"
    },
    {
      "name": "Scene.Pioneer10.Renderable.Appearance.Color",
      "type": "setPropertyValueSingle",
      "value": "{0.910000, 0.478000, 0.976000}"
    },
    {
      "name": "Scene.Pioneer11.Renderable.Appearance.Color",
      "type": "setPropertyValueSingle",
      "value": "{0.910000, 0.478000, 0.976000}"
    },
    {
      "name": "Scene.Planck.Renderable.Opacity",
      "type": "setPropertyValueSingle",
      "value": "0.0"
    },
    {
      "name": "Scene.Exoplanets.Renderable.Enabled",
      "type": "setPropertyValueSingle",
      "value": "true"
    },
    {
      "name": "Scene.Exoplanets.Renderable.Opacity",
      "type": "setPropertyValueSingle",
      "value": "0.0"
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