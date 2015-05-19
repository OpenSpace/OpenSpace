--[[  OpenSpace settings script ]]--
-- This Lua script get executed once at the start of the application and is used to
-- set settings in the loaded scene graph

openspace.printInfo("Setting default values")
openspace.setPropertyValue("Sun.renderable.enabled", false)
openspace.setPropertyValue("SunMarker.renderable.enabled", true)
openspace.setPropertyValue("EarthMarker.renderable.enabled", true)
openspace.setPropertyValue("Constellation Bounds.renderable.enabled", false)
openspace.setPropertyValue("PlutoTrail.renderable.enabled", false)

openspace.setPropertyValue("MilkyWay.renderable.transparency", 0.75)
openspace.setPropertyValue("MilkyWay.renderable.segments", 50)

openspace.changeCoordinateSystem("Jupiter")

openspace.printInfo("Done setting default values")
