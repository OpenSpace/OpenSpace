--[[  OpenSpace settings script ]]--
-- This Lua script get executed once at the start of the application and is used to
-- set settings in the loaded scene graph

openspace.printInfo("Setting default values")
openspace.setPropertyValue("Constellation Bounds.renderable.enabled", false)

openspace.setPropertyValue("MilkyWay.renderable.transparency", 0.75)
openspace.setPropertyValue("MilkyWay.renderable.segments", 50)

openspace.printInfo("Done setting default values")

--openspace.fadeIn(2)