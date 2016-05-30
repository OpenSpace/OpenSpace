--[[  OpenSpace keybinding script ]]--
-- This script sets the default keybindings and is executed at startup

openspace.clearKeys()
openspace.bindKey("F1", "openspace.gui.toggle()")
openspace.bindKey("F2", "openspace.setPerformanceMeasurement(true)")
openspace.bindKey("F3", "openspace.setPerformanceMeasurement(false)")
openspace.bindKey("F5", "openspace.setPropertyValue('Interaction.coordinateSystem', 'Sun'); openspace.printInfo('Changing Viewpoint to Sun-in-center');");
openspace.bindKey("F6", "openspace.setPropertyValue('Interaction.coordinateSystem', '67P'); openspace.printInfo('Changing Viewpoint to 67P-in-center');");

openspace.bindKey("PRINT_SCREEN", "openspace.takeScreenshot()")

openspace.bindKey("SPACE", "openspace.time.togglePause()")

-- Bookmarks for the New Horizons encounter
openspace.bindKey("1", "openspace.time.setDeltaTime(1)")
openspace.bindKey("2", "openspace.time.setDeltaTime(5)")
openspace.bindKey("3", "openspace.time.setDeltaTime(10)")
openspace.bindKey("4", "openspace.time.setDeltaTime(20)")
openspace.bindKey("5", "openspace.time.setDeltaTime(40)")
openspace.bindKey("6", "openspace.time.setDeltaTime(90)")
openspace.bindKey("7", "openspace.time.setDeltaTime(360)")
openspace.bindKey("8", "openspace.time.setDeltaTime(720)")
openspace.bindKey("9", "openspace.time.setDeltaTime(2880)")
openspace.bindKey("0", "openspace.time.setDeltaTime(14400)")
openspace.bindKey("SHIFT+1", "openspace.time.setDeltaTime(28800)")
openspace.bindKey("SHIFT+2", "openspace.time.setDeltaTime(57600)")
openspace.bindKey("SHIFT+3", "openspace.time.setDeltaTime(115200)")
openspace.bindKey("SHIFT+4", "openspace.time.setDeltaTime(230400)")
openspace.bindKey("SHIFT+5", "openspace.time.setDeltaTime(460800)")

openspace.bindKey("i", "local b = openspace.getPropertyValue('ImagePlaneRosetta.renderable.enabled'); openspace.setPropertyValue('ImagePlaneRosetta.renderable.enabled', not b)")

openspace.bindKey("F8", "openspace.setPropertyValue('67P.renderable.clearAllProjections', true");

openspace.bindKey("a", "openspace.setPropertyValue('Interaction.origin', '67P')")
openspace.bindKey("s", "openspace.setPropertyValue('Interaction.origin', 'Rosetta')")

openspace.bindKey("q", "local b = openspace.getPropertyValue('SunMarker.renderable.enabled'); openspace.setPropertyValue('SunMarker.renderable.enabled', not b)")
openspace.bindKey("e", "local b = openspace.getPropertyValue('EarthMarker.renderable.enabled'); openspace.setPropertyValue('EarthMarker.renderable.enabled', not b)")

openspace.bindKey("c", "openspace.parallel.setAddress('130.236.142.51');openspace.parallel.setPassword('newhorizons-20150714');openspace.parallel.connect();")

openspace.bindKey("COMMA", "openspace.setRenderer('Framebuffer');")
openspace.bindKey("PERIOD", "openspace.setRenderer('ABuffer');")