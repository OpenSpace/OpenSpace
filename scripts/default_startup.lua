--openspace.setPropertyValue('Earth.renderable.colorTexture', '${OPENSPACE_DATA}/modules/mars/textures/mars.png')
openspace.time.setTime("2007-02-26T17:34:00")
--openspace.time.setTime("2006-08-22T20:00:00")

--openspace.time.setDeltaTime(200000.0)
--openspace.time.setDeltaTime(5000.00)
--openspace.time.setDeltaTime(864000)
openspace.time.setDeltaTime(100.0)
-- print(openspace.time.currentTimeUTC())



function loadKeyBindings()
	p = openspace.absPath('${SCRIPTS}/bind_keys.lua')
	dofile(p)
end
loadKeyBindings()