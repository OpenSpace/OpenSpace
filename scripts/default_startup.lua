--openspace.setPropertyValue('Earth.renderable.colorTexture', '${OPENSPACE_DATA}/modules/mars/textures/mars.png')
openspace.time.setTime("2005-11-01T00:00:00")
openspace.time.setDeltaTime(20000000.0)

print(openspace.time.currentTimeUTC())
