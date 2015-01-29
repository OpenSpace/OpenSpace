--openspace.setPropertyValue('Earth.renderable.colorTexture', '${OPENSPACE_DATA}/modules/mars/textures/mars.png')
openspace.time.setTime("2015-07-14T10:50:00.00") -- pluto

--openspace.time.setDeltaTime(200000.0)
--openspace.time.setDeltaTime(5000.00)
--openspace.time.setDeltaTime(864000)
openspace.time.setDeltaTime(10)
-- print(openspace.time.currentTimeUTC())



function loadKeyBindings()
	p = openspace.absPath('${SCRIPTS}/bind_keys.lua')
	dofile(p)
end
loadKeyBindings()