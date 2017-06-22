-- Load the common helper functions
dofile(openspace.absPath('${SCRIPTS}/common.lua'))

openspace.clearKeys()
helper.setCommonKeys()
helper.setDeltaTimeKeys({
--  1           2           3           4           5           6           7           8           9           0
--------------------------------------------------------------------------------------------------------------------------
--  1s          2s          5s          10s         30s         1m          2m          5m          10m         30m
    1,          2,          5,          10,         30,         60,         120,        300,        600,        1800,

--  1h          2h          3h          6h          12h         1d          2d          4d          1w          2w
    3600,       7200,       10800,      21600,      43200,      86400,      172800,     345600,     604800,     1209600,

--  1mo         2mo         3mo         6mo         1yr         2y          5y          10y         20y         50y
    2592000,    5184000,    7776000,    15552000,   31536000,   63072000,   157680000,  315360000,  630720000,  1576800000
})
--  OBS: One month (1mo) is approximated by 30 days.

openspace.bindKey("a", "openspace.setPropertyValue('Interaction.origin', 'Sun')", "Sets the focus of the camera to the Sun")
openspace.bindKey("s", "openspace.setPropertyValue('Interaction.origin', 'SDO')", "Sets the focus of the camera to SDO")
openspace.bindKey("d", "openspace.setPropertyValue('Interaction.origin', 'Stereo A')", "Sets the focus of the camera to Stereo A")
openspace.bindKey("f", "openspace.setPropertyValue('Interaction.origin', 'SOHO')", "Sets the focus of the camera to SOHO")
--openspace.bindKey("g", "openspace.setPropertyValue('Interaction.origin', 'ISS')", "Sets the focus of the camera to ISS")
-- Time
openspace.bindKey("j", "openspace.time.setTime('2012 JUL 01 00:00:00.000')", "Sets time to 2012 07 01 00:00:00.000")
openspace.bindKey("k", "openspace.time.setTime('2012 JUL 04 00:00:00.000')", "Sets time to 2012 07 04 00:00:00.000")
openspace.bindKey("l", "openspace.time.setTime('2012 JUL 12 00:00:00.000')", "Sets time to 2012 07 12 00:00:00.000")
openspace.bindKey("h", "openspace.time.setDeltaTime(1500)", "Set delta time to 1500")
