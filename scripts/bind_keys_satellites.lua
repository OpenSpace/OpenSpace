--[[  OpenSpace keybinding script loaded from the satellites.scene file ]]--

-- Load the common helper functions
dofile(openspace.absPath('${SCRIPTS}/common.lua'))
dofile(openspace.absPath('${SCRIPTS}/bind_common_keys.lua'))

-- Set focuses

openspace.bindKey(
    "p" ,
    "if gpsVis then gpsVis = false; else gpsVis = true; end; openspace.setPropertyValue('gps-ops*.renderable.enabled', not gpsVis)",
    "Toggles visibility of gps satellites."
)
openspace.bindKey(
    "s" ,
    "if stVis then stVis = false; else stVis = true; end; openspace.setPropertyValue('station*.renderable.enabled', not stVis)",
    "Toggles visibility of stations."
)
openspace.bindKey(
    "g" ,
    "if geoVis then geoVis = false; else geoVis = true; end; openspace.setPropertyValue('geo*.renderable.enabled', not geoVis)",
    "Toggles visibility of geostationary."
)