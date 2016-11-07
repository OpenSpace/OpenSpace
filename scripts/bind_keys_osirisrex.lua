--[[  OpenSpace keybinding script loaded from the osirisrex.scene file ]]--

-- Load the common helper functions
dofile(openspace.absPath('${SCRIPTS}/common.lua'))
dofile(openspace.absPath('${SCRIPTS}/bind_common_keys.lua'))

-- Set focuses
openspace.bindKey(
    "a",
    "openspace.setPropertyValue('Interaction.origin', 'OsirisRex')",
    "Sets the focus of the camera on 'Osiris Rex'."
)
openspace.bindKey(
    "s",
    "openspace.setPropertyValue('Interaction.origin', 'BennuBarycenter')",
    "Sets the focus of the camera on 'Bennu'."
)

openspace.bindKey(
    "F6" ,
    "openspace.printInfo('Set time: Launch');openspace.time.setTime('2016 SEP 08 23:05:00');",
    "Sets the time to the launch."
)
openspace.bindKey(
    "F7",
    "openspace.printInfo('Set time: Gravity Assist');openspace.time.setTime('2017 SEP 22 15:00:00');",
    "Sets the time to the Earth gravity assist."
)
openspace.bindKey(
    "F8",
    "openspace.printInfo('Set time: Approach');openspace.time.setTime('2018-SEP-11 21:31:01.183');",
    "Sets the time to the approach at Bennu."
)
openspace.bindKey(
    "F9",
    "openspace.printInfo('Set time: Preliminary Survey');openspace.time.setTime('2018-NOV-20 01:13:12.183');",
    "Sets the time to the preliminary survey of Bennu."
)
openspace.bindKey(
    "F10",
    "openspace.printInfo('Set time: Orbital B');openspace.time.setTime('2019-APR-08 10:35:27.186');",
    "Sets the time to the orbital B event."
)
openspace.bindKey(
    "F11",
    "openspace.printInfo('Set time: Recon');openspace.time.setTime('2019-MAY-25 03:50:31.195');",
    "Sets the time to the recon event."
)

openspace.bindKey(
    "q",
    helper.property.invert('SunMarker.renderable.enabled'),
    "Toggles the visibility of the text marking the location of the Sun."
)
openspace.bindKey(
    "e",
    helper.property.invert('EarthMarker.renderable.enabled'),
    "Toggles the visibility of the text marking the location of the Earth."
)
