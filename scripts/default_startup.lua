--openspace.setPropertyValue('Earth.renderable.colorTexture', '${OPENSPACE_DATA}/modules/mars/textures/mars.png')
openspace.time.setTime("2015-07-14T09:13:00.557") -- pluto

--openspace.time.setDeltaTime(200000.0)
--openspace.time.setDeltaTime(5000.00)
--openspace.time.setDeltaTime(864000)
openspace.time.setDeltaTime(0)
-- print(openspace.time.currentTimeUTC())



function loadKeyBindings()
	p = openspace.absPath('${SCRIPTS}/bind_keys.lua')
	dofile(p)
end
loadKeyBindings()