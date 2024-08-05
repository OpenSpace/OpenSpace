{
  "assets": [
    "base",
    "base_keybindings",
    "scene/solarsystem/missions/osirisrex/dashboard",
    "scene/solarsystem/missions/osirisrex/model",
    "scene/solarsystem/missions/osirisrex/osirisrex",
    "scene/solarsystem/missions/osirisrex/imageplane"
  ],
  "camera": {
    "aim": "",
    "anchor": "OsirisRex",
    "frame": "",
    "position": {
      "x": 26974590199.661884,
      "y": 76314608558.90802,
      "z": -127086452897.10179
    },
    "type": "setNavigationState"
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
      "action": "os.osirisrex.FocusOsirisRex",
      "key": "A"
    },
    {
      "action": "os.osirisrex.FocusBennu",
      "key": "S"
    },
    {
      "action": "os.osirisrex.SetupBennuApproach",
      "key": "F8"
    },
    {
      "action": "os.osirisrex.SetupBennuSurvey",
      "key": "F9"
    },
    {
      "action": "os.osirisrex.SetupBennuEventB",
      "key": "F10"
    },
    {
      "action": "os.osirisrex.SetupBennuReconEvent",
      "key": "F11"
    },
    {
      "action": "os.osirisrex.ToggleSunMarker",
      "key": "Q"
    }
  ],
  "mark_nodes": [
    "OsirisRex",
    "BennuBarycenter",
    "Earth"
  ],
  "meta": {
    "author": "OpenSpace Team",
    "description": "This profile demonstrates the entire lifetime of the NASA OSIRIS-REx spacecraft on its way to the asteroid Bennu and its subsequent journey back to Earth. The profile starts at Earth around the time of the spacecraft's launch and has information throughout the entire mission until its landing back on Earth in Utah. The models of OSIRIS-REx and Bennu are available, as well as a preliminary instrument timing, which uses the same image projection technique as employed in New Horizons and Rosetta",
    "license": "MIT License",
    "name": "Osiris-Rex",
    "url": "https://www.openspaceproject.com",
    "version": "1.1"
  },
  "properties": [
    {
      "name": "NavigationHandler.OrbitalNavigator.FollowAnchorNodeRotationDistance",
      "type": "setPropertyValue",
      "value": "20.0"
    },
    {
      "name": "NavigationHandler.OrbitalNavigator.LimitZoom.MinimumAllowedDistance",
      "type": "setPropertyValueSingle",
      "value": "3.0"
    }
  ],
  "time": {
    "type": "absolute",
    "value": "2018-10-30T23:00:00"
  },
  "version": {
    "major": 1,
    "minor": 1
  }
}
