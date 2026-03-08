{
  "assets": [
    "base",
    "base_keybindings",
    "events/toggle_sun",
    "scene/solarsystem/planets/earth/earth",
    "scene/solarsystem/planets/earth/satellites/satellites",
    "scene/solarsystem/planets/earth/noaa-sos/overlays/latlon_grid-white"
  ],
  "variants": {
    "minor_moons": {
      "name": "Minor Moons",
      "description": "Includes the minor moons of the outer solar system planets Jupiter, Saturn, Uranus, and Neptune.",
      "assets": [
        "scene/solarsystem/planets/jupiter/minor_moons",
        "scene/solarsystem/planets/saturn/minor_moons",
        "scene/solarsystem/planets/uranus/minor_moons",
        "scene/solarsystem/planets/neptune/minor_moons"
      ]
    },
    "dwarf_planets": {
      "name": "Dwarf Planets",
      "description": "Includes the various dwarf planets such as Ceres, Gonggong, Sedna, Vesta, etc.",
      "assets": [
        "scene/solarsystem/dwarf_planets/ceres/ceres",
        "scene/solarsystem/dwarf_planets/ceres/default_layers",
        "scene/solarsystem/dwarf_planets/eris/eris",
        "scene/solarsystem/dwarf_planets/gonggong/gonggong",
        "scene/solarsystem/dwarf_planets/haumea/haumea",
        "scene/solarsystem/dwarf_planets/makemake/makemake",
        "scene/solarsystem/dwarf_planets/orcus/orcus",
        "scene/solarsystem/dwarf_planets/pluto/pluto",
        "scene/solarsystem/dwarf_planets/pluto/default_layers",
        "scene/solarsystem/dwarf_planets/pluto/charon/charon",
        "scene/solarsystem/dwarf_planets/pluto/charon/default_layers",
        "scene/solarsystem/dwarf_planets/quaoar/quaoar",
        "scene/solarsystem/dwarf_planets/sedna/sedna",
        "scene/solarsystem/dwarf_planets/vesta/vesta"
      ]
    },
    "asteroids": {
      "name": "Asteroids",
      "description": "Includes asteroids from the JPL Horizons Small-Body Database and using their categories.",
      "assets": [
        "scene/solarsystem/sssb/amor_asteroid",
        "scene/solarsystem/sssb/apollo_asteroid",
        "scene/solarsystem/sssb/aten_asteroid",
        "scene/solarsystem/sssb/atira_asteroid",
        "scene/solarsystem/sssb/centaur_asteroid",
        "scene/solarsystem/sssb/chiron-type_comet",
        "scene/solarsystem/sssb/encke-type_comet",
        "scene/solarsystem/sssb/halley-type_comet",
        "scene/solarsystem/sssb/inner_main_belt_asteroid",
        "scene/solarsystem/sssb/jupiter-family_comet",
        "scene/solarsystem/sssb/jupiter_trojan_asteroid",
        "scene/solarsystem/sssb/main_belt_asteroid",
        "scene/solarsystem/sssb/mars-crossing_asteroid",
        "scene/solarsystem/sssb/outer_main_belt_asteroid",
        "scene/solarsystem/sssb/pha",
        "scene/solarsystem/sssb/transneptunian_object_asteroid"
      ]
    },
    "interstellar": {
      "name": "Interstellar",
      "description": "Includes the interstellar objects that passed through the solar system.",
      "assets": [
        "scene/solarsystem/interstellar/c-2019_q4_borisov",
        "scene/solarsystem/interstellar/oumuamua",
        "scene/solarsystem/interstellar/3i_atlas"
      ]
    }
  },
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
    "description": "Default OpenSpace Profile. Adds Earth satellites not contained in other profiles.",
    "license": "MIT License",
    "name": "Default",
    "url": "https://www.openspaceproject.com",
    "version": "1.0"
  },
  "properties": [
    {
      "name": "{earth_satellites~space_stations}.Renderable.Enabled",
      "type": "setPropertyValue",
      "value": "false"
    },
    {
      "name": "Scene.Earth.Renderable.Layers.Overlays.noaa-sos-overlays-latlon_grid-white.Enabled",
      "type": "setPropertyValueSingle",
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
    "minor": 5
  }
}
