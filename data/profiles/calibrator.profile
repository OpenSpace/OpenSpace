{
  "assets": [
    "base_blank",
    "util/calibration"
  ],
  "camera": {
    "aim": "Calibration_Front",
    "anchor": "Calibration",
    "frame": "Calibration",
    "position": {
      "x": 0.5,
      "y": 0.5,
      "z": 0.5
    },
    "type": "setNavigationState"
  },
  "meta": {
    "author": "OpenSpace Team",
    "description": "This profile places the camera in the inside of a calibration cube. This profile can be used to verify that a display environment is set up correctly. If a setup is correct, the different windows/viewports should show the correct parts of the surrounding cube accurately and withou any unwanted distortion.",
    "license": "MIT license",
    "name": "Calibration",
    "url": "https://openspaceproject.com",
    "version": "1.0"
  },
  "properties": [
    {
      "name": "NavigationHandler.OrbitalNavigator.LimitZoom.EnableMinimumAllowedDistance",
      "type": "setPropertyValueSingle",
      "value": "false"
    }
  ],
  "version": {
    "major": 1,
    "minor": 4
  }
}