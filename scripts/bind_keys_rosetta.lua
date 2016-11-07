--[[  OpenSpace keybinding script loaded from the rosetta.scene file ]]--
-- This script sets the default keybindings and is executed at startup

-- Load the common helper functions
dofile(openspace.absPath('${SCRIPTS}/common.lua'))

openspace.clearKeys()
helper.setCommonKeys()

helper.setDeltaTimeKeys({
    1, 5, 10, 20, 40, 90, 360, 720, 2880, 14400,
    28800, 57600, 115200, 230400, 460800, 921600, 1843200, 3686400, 7372800, 14745600
})

openspace.bindKey(
    "a",
    "openspace.setPropertyValue('Interaction.origin', '67P')",
    "Sets the focus of the camera on '67P'."
)
openspace.bindKey(
    "s",
    "openspace.setPropertyValue('Interaction.origin', 'Rosetta')",
    "Sets the focus of the camera on 'Rosetta'."
)

openspace.bindKey(
    "F5",
    "openspace.time.setTime('2014-08-01T03:05:18.101')",
    "Jumps to the time of initial approach of Rosetta to 67P."
)
openspace.bindKey(
    "F6",
    "openspace.time.setTime('2014-11-12T08:20:00.00')",
    "Jumps to the time when the Philae lander is released."
)
openspace.bindKey(
    "F8",
    "openspace.setPropertyValue('67P.renderable.ProjectionComponent.clearAllProjections', true)",
    "Removes all image projections from 67P."
)

openspace.bindKey(
    "i",
    helper.renderable.toggle('ImagePlaneRosetta'),
    "Toggles the visibility of the free floating image plane."
)
openspace.bindKey(
    "q",
    helper.renderable.toggle('SunMarker'),
    "Toggles the visibility of the text marking the location of the Sun."
)
openspace.bindKey(
    "e",
    helper.renderable.toggle('JupiterTrail') .. helper.renderable.toggle('SaturnTrail') ..
    helper.renderable.toggle('UranusTrail') .. helper.renderable.toggle('NeptuneTrail'),
    "Toggles the visibility of all trails further from the Sun than 67P."
)
openspace.bindKey(
    "f",
    helper.renderable.toggle('PhilaeTrail'),
    "Toggles the visibility of Philae's trail."
)

openspace.bindKey(
    "p",
    helper.property.invert('67P.renderable.ProjectionComponent.performProjection'),
    "Enables or disables the image projection on 67P."
)
