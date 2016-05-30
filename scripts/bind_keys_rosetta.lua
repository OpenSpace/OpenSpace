--[[  OpenSpace keybinding script ]]--
-- This script sets the default keybindings and is executed at startup

-- Load the common helper functions
dofile(openspace.absPath('${SCRIPTS}/common.lua'))

openspace.clearKeys()
helper.setCommonKeys()

helper.setDeltaTimeKeys({
    1, 5, 10, 20, 40, 90, 360, 720, 2880, 14400,
    28800, 57600, 115200, 230400, 460800
})

openspace.bindKey("a", "openspace.setPropertyValue('Interaction.origin', '67P')")
openspace.bindKey("s", "openspace.setPropertyValue('Interaction.origin', 'Rosetta')")

openspace.bindKey("F5", "openspace.setPropertyValue('Interaction.coordinateSystem', 'Sun'); openspace.printInfo('Changing Viewpoint to Sun');");
openspace.bindKey("F6", "openspace.setPropertyValue('Interaction.coordinateSystem', '67P'); openspace.printInfo('Changing Viewpoint to 67P');");
openspace.bindKey("F8", "openspace.setPropertyValue('67P.renderable.clearAllProjections', true");

openspace.bindKey("i", helper.renderable.toggle('ImagePlaneRosetta'))
openspace.bindKey("q", helper.renderable.toggle('SunMarker'))
openspace.bindKey("e", helper.renderable.toggle('EarthMarker'))

openspace.bindKey("c", "openspace.parallel.setAddress('130.236.142.51');openspace.parallel.setPassword('newhorizons-20150714');openspace.parallel.connect();")
