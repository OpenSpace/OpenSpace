--openspace.setPropertyValue('Earth.renderable.colorTexture', '${OPENSPACE_DATA}/modules/mars/textures/mars.png')
openspace.time.setTime("2007-11-01T00:00:00")
openspace.time.setDeltaTime(100000.0)

print(openspace.time.currentTimeUTC())
