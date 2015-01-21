--openspace.setPropertyValue('Earth.renderable.colorTexture', '${OPENSPACE_DATA}/modules/mars/textures/mars.png')
--openspace.time.setTime("2007-01-08T20:42:01.359") -- far
--openspace.time.setTime("2007-02-10T13:30:01.359") -- 4x4
openspace.time.setTime("2007-02-27T16:30:00.000") -- close

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