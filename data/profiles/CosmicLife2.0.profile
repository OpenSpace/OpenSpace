{
  "actions": [
    {
      "documentation": "turns off all",
      "gui_path": "/Brian",
      "identifier": "turnoffhomobranch",
      "is_local": false,
      "name": "Turn Off HomoSapian Branch",
      "script": "openspace.setPropertyValueSingle(\"Scene.primates_24001_primates.Renderable.Enabled\", false)\nopenspace.setPropertyValueSingle(\"Scene.primates_25001_haplorrhini.Renderable.Enabled\", false)\nopenspace.setPropertyValueSingle(\"Scene.primates_26001_simiiformes.Renderable.Enabled\", false)\nopenspace.setPropertyValueSingle(\"Scene.primates_27001_catarrhini.Renderable.Enabled\", false)\nopenspace.setPropertyValueSingle(\"Scene.primates_28015_hominoidea.Renderable.Enabled\", false)\nopenspace.setPropertyValueSingle(\"Scene.primates_29009_hominidae.Renderable.Enabled\", false)\nopenspace.setPropertyValueSingle(\"Scene.primates_30013_homininae.Renderable.Enabled\", false)\nopenspace.setPropertyValueSingle(\"Scene.primates_31009_homo.Renderable.Enabled\", false)\n"
    }
  ],
  "assets": [
    "base_blank",
    "events/toggle_image_trail",
    "${USER_ASSETS}/cosmic_life/birds/branches_anas",
    "${USER_ASSETS}/cosmic_life/birds/clades",
    "${USER_ASSETS}/cosmic_life/birds/consensus_species",
    "${USER_ASSETS}/cosmic_life/birds/sequence_lineage",
    "${USER_ASSETS}/cosmic_life/birds/sequences",
    "${USER_ASSETS}/cosmic_life/birds/taxon",
    "${USER_ASSETS}/cosmic_life/human_origins/human_origins",
    "${USER_ASSETS}/cosmic_life/human_origins/human_origins_regions",
    "${USER_ASSETS}/cosmic_life/primates/branches_homo_sapiens",
    "${USER_ASSETS}/cosmic_life/primates/clades",
    "${USER_ASSETS}/cosmic_life/primates/consensus_species",
    "${USER_ASSETS}/cosmic_life/primates/sequence_lineage",
    "${USER_ASSETS}/cosmic_life/primates/sequences",
    "${USER_ASSETS}/cosmic_life/primates/takanori_trials",
    "${USER_ASSETS}/cosmic_life/primates/taxon"
  ],
  "camera": {
    "aim": "",
    "anchor": "primates_consensus_species",
    "frame": "",
    "position": {
      "x": 331794.72636622854,
      "y": 514401.2860644698,
      "z": -593040.5097923045
    },
    "type": "setNavigationState",
    "up": {
      "x": -0.9004983278960381,
      "y": 0.40829590634145274,
      "z": -0.14965698888208098
    },
    "yaw": 0.0003844993920354935
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
    "author": "Brian Abbott",
    "description": "Cosmic View of Life",
    "license": "MIT License",
    "name": "Default",
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