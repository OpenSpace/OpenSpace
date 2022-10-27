{
  "actions": [
    {
      "documentation": "Sets the focus of the camera on 'OsirisRex'",
      "gui_path": "/Missions/Osiris Rex",
      "identifier": "profile.focus.osirisrex",
      "is_local": false,
      "name": "Focus on OsirisRex",
      "script": "openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'OsirisRex'); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    },
    {
      "documentation": "Sets the focus of the camera on 'Bennu'",
      "gui_path": "/Missions/Osiris Rex",
      "identifier": "profile.focus.bennu",
      "is_local": false,
      "name": "Focus on Bennu",
      "script": "openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'BennuBarycenter'); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);"
    },
    {
      "documentation": "Sets the time to the approach at Bennu",
      "gui_path": "/Missions/Osiris Rex",
      "identifier": "profile.setup.bennu_approach",
      "is_local": false,
      "name": "Set Bennu approach time",
      "script": "openspace.printInfo('Set time: Approach');openspace.time.setTime('2018-SEP-11 21:31:01.183');"
    },
    {
      "documentation": "Sets the time to the preliminary survey of Bennu",
      "gui_path": "/Missions/Osiris Rex",
      "identifier": "profile.setup.bennu_survey",
      "is_local": false,
      "name": "Set Bennu survey time",
      "script": "openspace.printInfo('Set time: Preliminary Survey'); openspace.time.setTime('2018-NOV-20 01:13:12.183');"
    },
    {
      "documentation": "Sets the time to the orbital B event",
      "gui_path": "/Missions/Osiris Rex",
      "identifier": "profile.setup.orbital_b_event",
      "is_local": false,
      "name": "Set orbital B event time",
      "script": "openspace.printInfo('Set time: Orbital B'); openspace.time.setTime('2019-APR-08 10:35:27.186');"
    },
    {
      "documentation": "Sets the time to the recon event",
      "gui_path": "/Missions/Osiris Rex",
      "identifier": "profile.setup.recon_event",
      "is_local": false,
      "name": "Set recon event time",
      "script": "openspace.printInfo('Set time: Recon'); openspace.time.setTime('2019-MAY-25 03:50:31.195');"
    },
    {
      "documentation": "Toggles the visibility of the text marking the location of the Sun",
      "gui_path": "/Missions/Osiris Rex",
      "identifier": "profile.toggle.sun_marker",
      "is_local": false,
      "name": "Toggle Sun marker",
      "script": "openspace.setPropertyValueSingle('Scene.SunMarker.Renderable.Enabled', not openspace.getPropertyValue('Scene.SunMarker.Renderable.Enabled'));"
    }
  ],
  "assets": [
    "base",
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
      "action": "profile.focus.osirisrex",
      "key": "A"
    },
    {
      "action": "profile.focus.bennu",
      "key": "S"
    },
    {
      "action": "profile.setup.bennu_approach",
      "key": "F8"
    },
    {
      "action": "profile.setup.bennu_survey",
      "key": "F9"
    },
    {
      "action": "profile.setup.orbital_b_event",
      "key": "F10"
    },
    {
      "action": "profile.setup.recon_event",
      "key": "F11"
    },
    {
      "action": "profile.toggle.sun_marker",
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
    "version": "1.0"
  },
  "properties": [
    {
      "name": "NavigationHandler.OrbitalNavigator.FollowAnchorNodeRotationDistance",
      "type": "setPropertyValue",
      "value": "20.0"
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
