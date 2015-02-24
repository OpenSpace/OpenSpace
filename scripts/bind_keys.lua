openspace.clearKeys()

interaction_speed = 2.75

-- Key Bindings
openspace.bindKey("f1", "openspace.gui.toggle()")
openspace.bindKey("f2", "openspace.setPerformanceMeasurement(true)")
openspace.bindKey("f3", "openspace.setPerformanceMeasurement(false)")
openspace.bindKey("f4", "openspace.takeScreenshot()")
openspace.bindKey("f5", "loadKeyBindings()")

openspace.bindKey("f9", "openspace.changeCoordinateSystem('Pluto'); openspace.printInfo('Changing Viewpoint to Pluto-in-center');");
openspace.bindKey("f10", "openspace.changeCoordinateSystem('Sun'); openspace.printInfo('Changing Viewpoint to Sun-in-center');");
openspace.bindKey("f11", "openspace.fadeOut(2);")
openspace.bindKey("f12", "openspace.fadeIn(2);")

openspace.bindKey("T", "openspace.distance(-interaction_speed * openspace.dt(), 6.0)")
openspace.bindKey("G", "openspace.distance(interaction_speed * openspace.dt(), 6.0)")
openspace.bindKey("Y", "openspace.distance(-interaction_speed * openspace.dt(), 10.0)")
openspace.bindKey("H", "openspace.distance(interaction_speed * openspace.dt(), 10.0)")
openspace.bindKey("U", "openspace.distance(-interaction_speed * openspace.dt(), 10.0)")
openspace.bindKey("J", "openspace.distance(interaction_speed * openspace.dt(), 10.0)")

openspace.bindKey("PRINT_SCREEN", "openspace.takeScreenshot()")