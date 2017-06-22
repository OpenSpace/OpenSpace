--[[  OpenSpace keybinding script loaded from the fieldlinessequence.scene file ]]--

-- Load the common helper functions
dofile(openspace.absPath('${SCRIPTS}/common.lua'))

openspace.clearKeys()
helper.setCommonKeys()
helper.setDeltaTimeKeys({
    -- Fastest update of fieldlines are each 15 secounds for BATSRUS. 1min is also used
    -- Enlil updates every hour

-- Given 15 sec         12fps   24fps  48fps
-- Given 1 min                         12fps   24fps   60fps
-- Given 1hour                                                  12fps   24fps  48fps
-- Btn  1       2       3       4       5       6       7       8       9       0

        1,      2,      5,      10,     30,     60,     120,    300,    600,    1800,
        15,     60,     180,    360,    720,    1440,   3600,   43200,  86400,  172800,
       -15,    -60,    -180,   -360,   -720,   -1440,  -3600,  -43200, -86400, -172800

})
