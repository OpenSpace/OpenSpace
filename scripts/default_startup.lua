openspace.setInvertRoll(true);

openspace.time.setTime("2015-07-14T10:50:00.00") -- PLUTO
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


openspace.time.setDeltaTime(10)
-- print(openspace.time.currentTimeUTC())



function loadKeyBindings()
	p = openspace.absPath('${SCRIPTS}/bind_keys.lua')
	dofile(p)
end
loadKeyBindings()