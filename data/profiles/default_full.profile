{
  "assets": [
    "base",
    "events/toggle_sun",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/satellites/satellites",
    "scene/solarsystem/planets/jupiter/major_moons",
    "scene/solarsystem/planets/jupiter/minor_moons",
    "scene/solarsystem/planets/neptune/major_moons",
    "scene/solarsystem/planets/neptune/minor_moons",
    "scene/solarsystem/planets/saturn/major_moons",
    "scene/solarsystem/planets/saturn/minor_moons",
    "scene/solarsystem/planets/uranus/major_moons",
    "scene/solarsystem/planets/uranus/minor_moons",
    "scene/milkyway/objects/orionnebula/orionnebula"
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
      "action": "os.solarsystem.earth.togglesatellitetrails",
      "key": "S"
    },
    {
      "action": "os.solarsystem.earth.iss.focus",
      "key": "I"
    },
    {
      "action": "os.solarsystem.earth.focus",
      "key": "HOME"
    },
    {
      "action": "os_default.toggle_minormoon_trails",
      "key": "SHIFT+H"
    }
  ],
  "mark_nodes": [
    "Mercury",
    "Venus",
    "Earth",
    "Moon",
    "Moon",
    "Mars",
    "Saturn",
    "Jupiter",
    "Uranus",
    "Neptune",
    "Pluto",
    "Sun"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "Default OpenSpace Profile. Adds Earth satellites not contained in other profiles",
    "license": "MIT License",
    "name": "Default",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "properties": [
    {
      "name": "{earth_satellites}.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    }
  ],
  "time": {
    "type": "relative",
    "value": "-1d"
  },
  "version": {
    "major": 1,
    "minor": 1
  }
}
