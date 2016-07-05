--[[  OpenSpace keybinding script ]]--

-- Load the common helper functions
dofile(openspace.absPath('${SCRIPTS}/common.lua'))

openspace.clearKeys()
helper.setCommonKeys()

helper.setDeltaTimeKeys({
    1, 5, 10, 20, 40, 60, 120, 360, 540, 1080,
    2160, 4320, 8640
})

openspace.bindKey("a", "openspace.setPropertyValue('Interaction.origin', 'NewHorizons')")
openspace.bindKey("s", "openspace.setPropertyValue('Interaction.origin', 'PlutoProjection')")
openspace.bindKey("d", "openspace.setPropertyValue('Interaction.origin', 'Charon')")
openspace.bindKey("z", "openspace.setPropertyValue('Interaction.origin', 'JupiterProjection')")
openspace.bindKey("x", "openspace.setPropertyValue('Interaction.origin', 'Europa')")

openspace.bindKey("F5", "openspace.setPropertyValue('Interaction.coordinateSystem', 'Sun'); openspace.printInfo('Changing Viewpoint to Sun');");
openspace.bindKey("F6", "openspace.setPropertyValue('Interaction.coordinateSystem', 'Jupiter'); openspace.printInfo('Changing Viewpoint to Jupiter');");
openspace.bindKey("F7", "openspace.setPropertyValue('Interaction.coordinateSystem', 'Pluto'); openspace.printInfo('Changing Viewpoint to Pluto');");

openspace.bindKey("F8", "openspace.setPropertyValue('PlutoProjection.renderable.ProjectionComponent.clearAllProjections', true); openspace.setPropertyValue('Charon.renderable.ProjectionComponent.clearAllProjections', true);")

-- Quickfix backjumps in pluto sequence
openspace.bindKey("F9", "openspace.time.setTime('2015-07-14T09:00:00.00'); openspace.setPropertyValue('PlutoProjection.renderable.clearAllProjections', true); openspace.setPropertyValue('Charon.renderable.clearAllProjections', true);")
openspace.bindKey("F10", "openspace.time.setTime('2015-07-14T10:00:00.00'); openspace.setPropertyValue('PlutoProjection.renderable.clearAllProjections', true); openspace.setPropertyValue('Charon.renderable.clearAllProjections', true);")
openspace.bindKey("F11", "openspace.time.setTime('2015-07-14T11:17:00.00'); openspace.setPropertyValue('PlutoProjection.renderable.clearAllProjections', true); openspace.setPropertyValue('Charon.renderable.clearAllProjections', true);")
openspace.bindKey("F12", "openspace.time.setTime('2015-07-14T12:45:00.00'); openspace.setPropertyValue('PlutoProjection.renderable.clearAllProjections', true); openspace.setPropertyValue('Charon.renderable.clearAllProjections', true);")

openspace.bindKey("r", helper.property.invert('PlutoProjection.renderable.fk'))

openspace.bindKey("KP_8", helper.property.increment('PlutoProjection.renderable.heightExaggeration', 2))
openspace.bindKey("KP_2", helper.property.decrement('PlutoProjection.renderable.heightExaggeration', 2))

openspace.bindKey("KP_9", helper.property.increment('Charon.renderable.heightExaggeration', 2))
openspace.bindKey("KP_3", helper.property.decrement('Charon.renderable.heightExaggeration', 2))


openspace.bindKey("g", "openspace.time.setTime('2007-02-28T11:40:00.00'); openspace.time.setDeltaTime(1);")

openspace.bindKey("h", "openspace.time.setTime('2015-07-14T10:00:00.00'); openspace.time.setDeltaTime(1); openspace.setPropertyValue('Interaction.coordinateSystem', 'Pluto');openspace.setPropertyValue('Interaction.origin', 'PlutoProjection'); openspace.printInfo('Changing Viewpoint to Pluto-in-center');")

openspace.bindKey("q", helper.property.invert('SunMarker.renderable.enabled'))
openspace.bindKey("e", helper.property.invert('EarthMarker.renderable.enabled'))
openspace.bindKey("o", helper.property.invert('PlutoTrail.renderable.enabled'))

openspace.bindKey("k",
    helper.renderable.toggle('HydraText') .. helper.renderable.toggle('NixText') .. helper.renderable.toggle('KerberosText') .. helper.renderable.toggle('StyxText')
)
openspace.bindKey("j", helper.renderable.toggle('PlutoText'))

openspace.bindKey("l", helper.property.invert('Labels.renderable.performFading'))

openspace.bindKey("m",
    helper.property.invert('NH_LORRI.renderable.solidDraw') .. helper.property.invert('NH_RALPH_LEISA.renderable.solidDraw') ..
    helper.property.invert('NH_RALPH_MVIC_PAN1.renderable.solidDraw') .. helper.property.invert('NH_RALPH_MVIC_PAN2.renderable.solidDraw') ..
    helper.property.invert('NH_RALPH_MVIC_RED.renderable.solidDraw') .. helper.property.invert('NH_RALPH_MVIC_BLUE.renderable.solidDraw') ..
    helper.property.invert('NH_RALPH_MVIC_FT.renderable.solidDraw') .. helper.property.invert('NH_RALPH_MVIC_METHANE.renderable.solidDraw') ..
    helper.property.invert('NH_RALPH_MVIC_NIR.renderable.solidDraw') .. helper.property.invert('NH_ALICE_AIRGLOW.renderable.solidDraw') ..
    helper.property.invert('NH_ALICE_SOC.renderable.solidDraw')
)

openspace.bindKey("t", helper.renderable.toggle('PlutoShadow') .. helper.renderable.toggle('CharonShadow'))

openspace.bindKey("p",
    helper.property.invert('JupiterProjection.renderable.performProjection') .. helper.property.invert('Io.renderable.performProjection') .. 
    helper.property.invert('Ganymede.renderable.performProjection') .. helper.property.invert('Europa.renderable.performProjection') .. 
    helper.property.invert('Callisto.renderable.performProjection') .. helper.property.invert('PlutoProjection.renderable.performProjection') .. 
    helper.property.invert('Charon.renderable.performProjection')
)

openspace.bindKey("c", "openspace.parallel.setAddress('130.236.142.51');openspace.parallel.setPassword('newhorizons-20150714');openspace.parallel.connect();")
