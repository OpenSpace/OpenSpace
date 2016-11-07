--[[  OpenSpace keybinding script loaded from the newhorizons.scene file ]]--

-- Load the common helper functions
dofile(openspace.absPath('${SCRIPTS}/common.lua'))

openspace.clearKeys()
helper.setCommonKeys()
helper.setDeltaTimeKeys({
    1, 5, 10, 20, 40, 60, 120, 360, 540, 1080,
    2160, 4320, 8640
})

openspace.bindKey(
    "a",
    "openspace.setPropertyValue('Interaction.origin', 'NewHorizons')",
    "Sets the focus of the camera on 'NewHorizons'."
)
openspace.bindKey(
    "s",
    "openspace.setPropertyValue('Interaction.origin', 'PlutoProjection')",
    "Sets the focus of the camera on 'Pluto'"
)
openspace.bindKey(
    "d",
    "openspace.setPropertyValue('Interaction.origin', 'Charon')",
    "Sets the focus of the camera on 'Charon'."
)
openspace.bindKey(
    "z",
    "openspace.setPropertyValue('Interaction.origin', 'JupiterProjection')",
    "Sets the focus of the camera on 'Jupiter'."
)
openspace.bindKey(
    "x",
    "openspace.setPropertyValue('Interaction.origin', 'Europa')",
    "Sets the focus of the camera on 'Europa'."
)

openspace.bindKey(
    "F8",
    "openspace.setPropertyValue('PlutoProjection.renderable.ProjectionComponent.clearAllProjections', true);" ..
    "openspace.setPropertyValue('Charon.renderable.ProjectionComponent.clearAllProjections', true);",
    "Removes all image projections from Pluto and Charon."
)

openspace.bindKey(
    "F9",
    "openspace.time.setTime('2015-07-14T09:00:00.00');" ..
    "openspace.setPropertyValue('PlutoProjection.renderable.clearAllProjections', true);" ..
    "openspace.setPropertyValue('Charon.renderable.clearAllProjections', true);",
    "Jumps to the 14th of July 2015 at 0900 UTC and clears all projections."
)

openspace.bindKey(
    "KP_8",
    helper.property.increment('PlutoProjection.renderable.heightExaggeration', 2),
    "Increases the height map exaggeration on Pluto."
)
openspace.bindKey(
    "KP_2",
    helper.property.decrement('PlutoProjection.renderable.heightExaggeration', 2),
    "Decreases the height map exaggeration on Pluto."
)
openspace.bindKey(
    "KP_9",
    helper.property.increment('Charon.renderable.heightExaggeration', 2),
    "Increases the height map exaggeration on Charon."
)
openspace.bindKey(
    "KP_3",
    helper.property.decrement('Charon.renderable.heightExaggeration', 2),
    "Decreases the height map exaggeration on Charon."
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
openspace.bindKey(
    "o",
    helper.property.invert('PlutoTrail.renderable.enabled'),
    "Toggles the visibility of the trail behind Pluto."
)

openspace.bindKey(
    "j",
    helper.renderable.toggle('PlutoText') .. helper.renderable.toggle('CharonText') ..
    helper.renderable.toggle('HydraText') .. helper.renderable.toggle('NixText') ..
    helper.renderable.toggle('KerberosText') .. helper.renderable.toggle('StyxText'),
    "Toggles the visibility of the text labels of Pluto, Charon, Hydra, Nix, Kerberos, and Styx."
)

openspace.bindKey(
    "l",
    helper.property.invert('Labels.renderable.performFading'),
    "Toggles the visibility of the labels for the New Horizons instruments."
)

openspace.bindKey("m",
    helper.property.invert('NH_LORRI.renderable.solidDraw') ..
    helper.property.invert('NH_RALPH_LEISA.renderable.solidDraw') ..
    helper.property.invert('NH_RALPH_MVIC_PAN1.renderable.solidDraw') ..
    helper.property.invert('NH_RALPH_MVIC_PAN2.renderable.solidDraw') ..
    helper.property.invert('NH_RALPH_MVIC_RED.renderable.solidDraw') ..
    helper.property.invert('NH_RALPH_MVIC_BLUE.renderable.solidDraw') ..
    helper.property.invert('NH_RALPH_MVIC_FT.renderable.solidDraw') ..
    helper.property.invert('NH_RALPH_MVIC_METHANE.renderable.solidDraw') ..
    helper.property.invert('NH_RALPH_MVIC_NIR.renderable.solidDraw') ..
    helper.property.invert('NH_ALICE_AIRGLOW.renderable.solidDraw') ..
    helper.property.invert('NH_ALICE_SOC.renderable.solidDraw'),
    "Draws the instrument field of views in a solid color or as lines."
)

openspace.bindKey(
    "t",
    helper.renderable.toggle('PlutoShadow') .. helper.renderable.toggle('CharonShadow'),
    "Toggles the visibility of the shadow visualization of Pluto and Charon."
)

openspace.bindKey("p",
    helper.property.invert('JupiterProjection.renderable.performProjection') ..
    helper.property.invert('Io.renderable.performProjection') .. 
    helper.property.invert('Ganymede.renderable.performProjection') ..
    helper.property.invert('Europa.renderable.performProjection') .. 
    helper.property.invert('Callisto.renderable.performProjection') ..
    helper.property.invert('PlutoProjection.renderable.performProjection') .. 
    helper.property.invert('Charon.renderable.performProjection'),
    "Enables or disables the image projection on the different available objects."
)
