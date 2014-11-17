
openspace.clearKeys()

function setEarthAsOrigin()
	openspace.setOrigin('Earth')
end

interaction_speed = 2.75

-- Key Bindings
openspace.bindKey("f1", "setEarthAsOrigin()")
openspace.bindKey("f2", "openspace.setOrigin('Mercury')")
openspace.bindKey("f3", "openspace.setPropertyValue('Earth.renderable.enabled', true)")
openspace.bindKey("f4", "openspace.setPropertyValue('Earth.renderable.enabled', false)")
openspace.bindKey("f5", "loadKeyBindings()")


openspace.bindKey("U", "openspace.distance(-interaction_speed * openspace.dt(), 13.0)")
openspace.bindKey("J", "openspace.distance(interaction_speed * openspace.dt(), 13.0)")

openspace.bindKey("PRINT_SCREEN", "openspace.takeScreenshot()")