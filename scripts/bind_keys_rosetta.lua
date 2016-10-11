--[[  OpenSpace keybinding script ]]--
-- This script sets the default keybindings and is executed at startup

-- Load the common helper functions
dofile(openspace.absPath('${SCRIPTS}/common.lua'))

openspace.clearKeys()
helper.setCommonKeys()

helper.setDeltaTimeKeys({
    1, 5, 10, 20, 40, 90, 360, 720, 2880, 14400,
    28800, 57600, 115200, 230400, 460800, 921600, 1843200, 3686400, 7372800, 14745600
})

openspace.bindKey("a", "openspace.setPropertyValue('Interaction.origin', '67P')")
openspace.bindKey("s", "openspace.setPropertyValue('Interaction.origin', 'Rosetta')")

-- openspace.bindKey("F5", "openspace.setPropertyValue('Interaction.coordinateSystem', 'Sun'); openspace.printInfo('Changing Viewpoint to Sun');");
openspace.bindKey("F6", "openspace.setPropertyValue('Interaction.coordinateSystem', '67P'); openspace.printInfo('Changing Viewpoint to 67P');");
openspace.bindKey("F7", "openspace.time.setTime('2014-08-15T03:05:18.101')");
openspace.bindKey("F8", "openspace.setPropertyValue('67P.renderable.ProjectionComponent.clearAllProjections', true);");

openspace.bindKey("i", helper.renderable.toggle('ImagePlaneRosetta'))
openspace.bindKey("q", helper.renderable.toggle('SunMarker'))
openspace.bindKey("e", helper.renderable.toggle('JupiterTrail') .. helper.renderable.toggle('SaturnTrail') .. helper.renderable.toggle('UranusTrail') .. helper.renderable.toggle('NeptuneTrail'))
openspace.bindKey("f", helper.renderable.toggle('PhilaeTrail'))

openspace.bindKeyLocal("h", "openspace.parallel.setAddress('127.0.0.1');openspace.parallel.setPort('25001');openspace.parallel.setPassword('test');openspace.parallel.connect();openspace.parallel.requestHostship('test');")
openspace.bindKeyLocal("c", "openspace.parallel.setAddress('127.0.0.1');openspace.parallel.setPort('25001');openspace.parallel.setPassword('test');openspace.parallel.connect();")
