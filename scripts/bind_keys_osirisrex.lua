--[[  OpenSpace keybinding script ]]--

-- Load the common helper functions
dofile(openspace.absPath('${SCRIPTS}/common.lua'))

--openspace.clearKeys()
--helper.setCommonKeys()

-- Set focuses
openspace.bindKey("o", "openspace.setPropertyValue('Interaction.origin', 'OsirisRex')")
openspace.bindKey("b", "openspace.setPropertyValue('Interaction.origin', 'Bennu2')")


-- Quickfix backjumps in Osiris rex
openspace.bindKey("F6" , "openspace.printInfo('Set time: Launch');             openspace.time.setTime('2016 SEP 08 23:05:00');")
openspace.bindKey("F7" , "openspace.printInfo('Set time: Gravity Assist');     openspace.time.setTime('2017 SEP 22 15:00:00');")
openspace.bindKey("F8" , "openspace.printInfo('Set time: Approach');           openspace.time.setTime('2018-SEP-11 21:31:01.183');")
openspace.bindKey("F9" , "openspace.printInfo('Set time: Preliminary Survey'); openspace.time.setTime('2018-NOV-20 01:13:12.183');")
openspace.bindKey("F10", "openspace.printInfo('Set time: Orbital B');          openspace.time.setTime('2019-APR-08 10:35:27.186');")
openspace.bindKey("F11", "openspace.printInfo('Set time: Recon');              openspace.time.setTime('2019-MAY-25 03:50:31.195');")
-- OBS!! Avoid key F12
-- Pressing F12 triggers a breakpoint on AMNH Windows machine, with with the following stack trace:
--    ntdll.dll!DbgBreakPoint()
--    ntdll.dll!DbgUiRemoteBreakin()
--    kernel32.dll!BaseThreadInitThunk()
--    ntdll.dll!RtUserThreadStart()

openspace.bindKey("q", helper.property.invert('SunMarker.renderable.enabled'))
openspace.bindKey("e", helper.property.invert('EarthMarker.renderable.enabled'))

openspace.bindKey("c", "openspace.parallel.setAddress('130.236.142.51');openspace.parallel.setPassword('osiris2016');openspace.parallel.connect();")
