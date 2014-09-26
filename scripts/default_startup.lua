--openspace.setPropertyValue('Earth.renderable.colorTexture', '${OPENSPACE_DATA}/modules/mars/textures/mars.png')
openspace.time.setTime("2000-01-01T00:00:00")
openspace.time.setDeltaTime(1.0)

print(openspace.time.currentTimeUTC())
