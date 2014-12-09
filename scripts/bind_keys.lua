
openspace.clearKeys()

function setEarthAsOrigin()
	openspace.setOrigin('Earth')
end

interaction_speed = 2.75

-- Key Bindings
openspace.bindKey("f1", "openspace.gui.toggle()")
openspace.bindKey("f2", "openspace.setPerformanceMeasurement(true)")
openspace.bindKey("f3", "openspace.setPerformanceMeasurement(false)")
openspace.bindKey("f5", "loadKeyBindings()")


openspace.bindKey("U", "openspace.distance(-interaction_speed * openspace.dt(), 13.0)")
openspace.bindKey("J", "openspace.distance(interaction_speed * openspace.dt(), 13.0)")

openspace.bindKey("PRINT_SCREEN", "openspace.takeScreenshot()")