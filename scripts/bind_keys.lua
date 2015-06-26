--[[  OpenSpace keybinding script ]]--
-- This script sets the default keybindings and is executed at startup

openspace.clearKeys()
openspace.bindKey("F1", "openspace.gui.toggle()")
openspace.bindKey("F2", "openspace.setPerformanceMeasurement(true)")
openspace.bindKey("F3", "openspace.setPerformanceMeasurement(false)")
openspace.bindKey("F5", "openspace.changeCoordinateSystem('Sun'); openspace.printInfo('Changing Viewpoint to Sun-in-center');");
openspace.bindKey("F6", "openspace.changeCoordinateSystem('Jupiter'); openspace.printInfo('Changing Viewpoint to Jupiter-in-center');");
openspace.bindKey("F7", "openspace.changeCoordinateSystem('Pluto'); openspace.printInfo('Changing Viewpoint to Pluto-in-center');");

openspace.bindKey("PRINT_SCREEN", "openspace.takeScreenshot()")

openspace.bindKey("SPACE", "openspace.time.togglePause()")

-- Bookmarks for the New Horizons encounter
openspace.bindKey("1", "openspace.time.setDeltaTime(1)")
openspace.bindKey("2", "openspace.time.setDeltaTime(5)")
openspace.bindKey("3", "openspace.time.setDeltaTime(10)")
openspace.bindKey("4", "openspace.time.setDeltaTime(30)")
openspace.bindKey("5", "openspace.time.setDeltaTime(60)")
openspace.bindKey("6", "openspace.time.setDeltaTime(120)")
openspace.bindKey("7", "openspace.time.setDeltaTime(360)")
openspace.bindKey("8", "openspace.time.setDeltaTime(540)")
openspace.bindKey("9", "openspace.time.setDeltaTime(720)")

openspace.bindKey("F8", "openspace.time.setTime('2015-07-14T09:00:00.00'); openspace.setPropertyValue('PlutoProjection.renderable.clearAllProjections', true); openspace.setPropertyValue('Charon.renderable.clearAllProjections', true);")

-- Quickfix backjumps in pluto sequence
openspace.bindKey("F9", "openspace.time.setTime('2015-07-14T09:00:00.00'); openspace.setPropertyValue('PlutoProjection.renderable.clearAllProjections', true); openspace.setPropertyValue('Charon.renderable.clearAllProjections', true);")
openspace.bindKey("F10", "openspace.time.setTime('2015-07-14T10:00:00.00'); openspace.setPropertyValue('PlutoProjection.renderable.clearAllProjections', true); openspace.setPropertyValue('Charon.renderable.clearAllProjections', true);")
openspace.bindKey("F11", "openspace.time.setTime('2015-07-14T11:17:00.00'); openspace.setPropertyValue('PlutoProjection.renderable.clearAllProjections', true); openspace.setPropertyValue('Charon.renderable.clearAllProjections', true);")
openspace.bindKey("F12", "openspace.time.setTime('2015-07-14T12:45:00.00'); openspace.setPropertyValue('PlutoProjection.renderable.clearAllProjections', true); openspace.setPropertyValue('Charon.renderable.clearAllProjections', true);")

openspace.bindKey("r", "local b = openspace.getPropertyValue('PlutoProjection.renderable.fk'); openspace.setPropertyValue('PlutoProjection.renderable.fk', not b)")

openspace.bindKey("a", "openspace.setOrigin('NewHorizons')")
openspace.bindKey("s", "openspace.setOrigin('PlutoProjection')")
openspace.bindKey("d", "openspace.setOrigin('Charon')")
openspace.bindKey("z", "openspace.setOrigin('JupiterProjection')")
openspace.bindKey("x", "openspace.setOrigin('Europa')")


openspace.bindKey("g", "openspace.time.setTime('2007-02-28T11:40:00.00'); openspace.time.setDeltaTime(1);")

openspace.bindKey("h", "openspace.time.setTime('2015-07-14T09:00:00.00'); openspace.time.setDeltaTime(1); openspace.changeCoordinateSystem('Pluto'); openspace.setOrigin('PlutoProjection'); openspace.printInfo('Changing Viewpoint to Pluto-in-center');")

--[[
openspace.bindKey("1", "openspace.time.setTime('2007-02-27T16:40:00.00'); openspace.time.setDeltaTime(10)")
openspace.bindKey("2", "openspace.time.setTime('2015-07-14T10:50:00.00'); openspace.time.setDeltaTime(10)")
openspace.bindKey("3", "openspace.time.setTime('2015-07-14T11:22:00.00'); openspace.time.setDeltaTime(1)")
openspace.bindKey("4", "openspace.time.setTime('2015-07-14T11:36:40.00'); openspace.time.setDeltaTime(1)")
openspace.bindKey("5", "openspace.time.setTime('2015-07-14T11:48:43.00'); openspace.time.setDeltaTime(1)")
openspace.bindKey("6", "openspace.time.setTime('2015-07-14T12:04:35.00'); openspace.time.setDeltaTime(1)")
openspace.bindKey("7", "openspace.time.setTime('2015-07-14T15:02:46.00'); openspace.time.setDeltaTime(100)")
]]--

openspace.bindKey("i", "local b = openspace.getPropertyValue('PlutoTexture.renderable.enabled'); openspace.setPropertyValue('PlutoTexture.renderable.enabled', not b)")

openspace.bindKey("q", "local b = openspace.getPropertyValue('SunMarker.renderable.enabled'); openspace.setPropertyValue('SunMarker.renderable.enabled', not b)")
openspace.bindKey("e", "local b = openspace.getPropertyValue('EarthMarker.renderable.enabled'); openspace.setPropertyValue('EarthMarker.renderable.enabled', not b)")

openspace.bindKey("o", "local b = openspace.getPropertyValue('PlutoTrail.renderable.enabled'); openspace.setPropertyValue('PlutoTrail.renderable.enabled', not b)")

openspace.bindKey("k", "local b = openspace.getPropertyValue('HydraText.renderable.enabled'); openspace.setPropertyValue('HydraText.renderable.enabled', not b)")
openspace.bindKey("k", "local b = openspace.getPropertyValue('CharonText.renderable.enabled'); openspace.setPropertyValue('CharonText.renderable.enabled', not b)")
openspace.bindKey("k", "local b = openspace.getPropertyValue('NixText.renderable.enabled'); openspace.setPropertyValue('NixText.renderable.enabled', not b)")
openspace.bindKey("k", "local b = openspace.getPropertyValue('KerberosText.renderable.enabled'); openspace.setPropertyValue('KerberosText.renderable.enabled', not b)")
openspace.bindKey("k", "local b = openspace.getPropertyValue('StyxText.renderable.enabled'); openspace.setPropertyValue('StyxText.renderable.enabled', not b)")
openspace.bindKey("j", "local b = openspace.getPropertyValue('PlutoText.renderable.enabled'); openspace.setPropertyValue('PlutoText.renderable.enabled', not b)")


openspace.bindKey("l", "local b = openspace.getPropertyValue('Labels.renderable.performFading'); openspace.setPropertyValue('Labels.renderable.performFading', not b)")

openspace.bindKey("m", "local b = openspace.getPropertyValue('NH_LORRI.renderable.solidDraw'); openspace.setPropertyValue('NH_LORRI.renderable.solidDraw', not b)")
openspace.bindKey("m", "local b = openspace.getPropertyValue('NH_RALPH_LEISA.renderable.solidDraw'); openspace.setPropertyValue('NH_RALPH_LEISA.renderable.solidDraw', not b)")
openspace.bindKey("m", "local b = openspace.getPropertyValue('NH_RALPH_MVIC_PAN1.renderable.solidDraw'); openspace.setPropertyValue('NH_RALPH_MVIC_PAN1.renderable.solidDraw', not b)")
openspace.bindKey("m", "local b = openspace.getPropertyValue('NH_RALPH_MVIC_PAN2.renderable.solidDraw'); openspace.setPropertyValue('NH_RALPH_MVIC_PAN2.renderable.solidDraw', not b)")
openspace.bindKey("m", "local b = openspace.getPropertyValue('NH_RALPH_MVIC_RED.renderable.solidDraw'); openspace.setPropertyValue('NH_RALPH_MVIC_RED.renderable.solidDraw', not b)")
openspace.bindKey("m", "local b = openspace.getPropertyValue('NH_RALPH_MVIC_BLUE.renderable.solidDraw'); openspace.setPropertyValue('NH_RALPH_MVIC_BLUE.renderable.solidDraw', not b)")
openspace.bindKey("m", "local b = openspace.getPropertyValue('NH_RALPH_MVIC_FT.renderable.solidDraw'); openspace.setPropertyValue('NH_RALPH_MVIC_FT.renderable.solidDraw', not b)")
openspace.bindKey("m", "local b = openspace.getPropertyValue('NH_RALPH_MVIC_METHANE.renderable.solidDraw'); openspace.setPropertyValue('NH_RALPH_MVIC_METHANE.renderable.solidDraw', not b)")
openspace.bindKey("m", "local b = openspace.getPropertyValue('NH_RALPH_MVIC_NIR.renderable.solidDraw'); openspace.setPropertyValue('NH_RALPH_MVIC_NIR.renderable.solidDraw', not b)")
openspace.bindKey("m", "local b = openspace.getPropertyValue('NH_ALICE_AIRGLOW.renderable.solidDraw'); openspace.setPropertyValue('NH_ALICE_AIRGLOW.renderable.solidDraw', not b)")
openspace.bindKey("m", "local b = openspace.getPropertyValue('NH_ALICE_SOC.renderable.solidDraw'); openspace.setPropertyValue('NH_ALICE_SOC.renderable.solidDraw', not b)")

openspace.bindKey("t", "local b = openspace.getPropertyValue('PlutoShadow.renderable.enabled'); openspace.setPropertyValue('PlutoShadow.renderable.enabled', not b)")
openspace.bindKey("t", "local b = openspace.getPropertyValue('CharonShadow.renderable.enabled'); openspace.setPropertyValue('CharonShadow.renderable.enabled', not b)")
openspace.bindKey("p", "local b = openspace.getPropertyValue('JupiterProjection.renderable.performProjection'); openspace.setPropertyValue('JupiterProjection.renderable.performProjection', not b)")
openspace.bindKey("p", "local b = openspace.getPropertyValue('Io.renderable.performProjection'); openspace.setPropertyValue('Io.renderable.performProjection', not b)")
openspace.bindKey("p", "local b = openspace.getPropertyValue('Ganymede.renderable.performProjection'); openspace.setPropertyValue('Ganymede.renderable.performProjection', not b)")
openspace.bindKey("p", "local b = openspace.getPropertyValue('Europa.renderable.performProjection'); openspace.setPropertyValue('Europa.renderable.performProjection', not b)")
openspace.bindKey("p", "local b = openspace.getPropertyValue('Callisto.renderable.performProjection'); openspace.setPropertyValue('Callisto.renderable.performProjection', not b)")


openspace.bindKey("PAUSE", "openspace.printInfo('F5: Toogle to Sun, F6: Toggle to Jupiter, F7: Toggle to Pluto'); openspace.printInfo('Bookmarks: 1: Jupter, 2-7: Pluto');")
