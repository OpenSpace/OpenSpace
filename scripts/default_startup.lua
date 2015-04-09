--[[  OpenSpace startup script ]]--
-- This Lua script get executed once at the start of the application

--openspace.setInvertRotation(true)                     -- Uncomment this if you want to invert the rotation
openspace.setInvertRoll(true);
--openspace.setInteractionSensitivity(10)               -- This is the default value for the sensitivity (the higher, the more sensitive)

--openspace.time.setTime("2007 FEB 27 16:30:00")          -- This is the start time for a Jupiter run of New Horizons


openspace.time.setDeltaTime(0)                         -- How many seconds pass per second of realtime, changeable in the GUI

dofile(openspace.absPath('${SCRIPTS}/bind_keys.lua'))   -- Load the default keybindings

-- openspace.time.setDeltaTime(50);

openspace.time.setTime("2015-07-14T11:49:40.00") -- PLUTO
-- NH takes series of images from visible to dark side (across terminator)
-- Sequence lasts ~10 mins, (recommended dt = 10)

--openspace.time.setTime("2015-07-14T11:22:00.00") -- PLUTO
--BEAUTIFUL SEQUENCE -- NH takes series of images (same as previous)
-- Sequence ends at 2015 JUL 14T11:24:00:000, (recommended dt = 1)

--openspace.time.setTime("2015-07-14T11:36:40.00") -- PLUTO
-- NH takes series of images (same as previous)
-- Sequence ends at 2015 JUL 14T11:39:15:000, (recommended dt = 1)

--openspace.time.setTime("2015-07-14T11:48:43.00") -- CHARON
--NH takes imagesequence across  terminator
-- Sequence ends at 2015 JUL 14T11:49:30:000, (recommended dt = 1)

--openspace.time.setTime("2015-07-14T12:04:35.00") -- PLUTO
--NH takes imagesequence across Plutos north pole
-- Sequence ends at 2015 JUL 14T12:05:48:000, (recommended dt = 1)

--openspace.time.setTime("2015-07-14T15:02:46.00") -- PLUTO DARKSIDE
--From this point on NH takes a huge amount of images of the far side of 
-- CHARON and PLUTO - recommend run time from here and just let it run 
-- until audience tires (enough to fill 10-15 mins if run at dt = 100)


-- print(openspace.time.currentTimeUTC())
