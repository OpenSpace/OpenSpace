/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#ifndef __OPENSPACE_TEST___PROFILE_COMMON___H__
#define __OPENSPACE_TEST___PROFILE_COMMON___H__

//#include "catch2/catch.hpp"
//#include "openspace/scene/profile.h"
//#include <ghoul/filesystem/filesystem.h>
//#include <ghoul/misc/exception.h>
//#include <iostream>
//#include <sstream>
//#include <iomanip>
//
//using namespace openspace;
//
//namespace {
//}
//
//struct testProfileFormat {
//    std::vector<std::string> tsv;
//    std::vector<std::string> tsm;
//    std::vector<std::string> tsa;
//    std::vector<std::string> tsp;
//    std::vector<std::string> tsk;
//    std::vector<std::string> tst;
//    std::vector<std::string> tsc;
//    std::vector<std::string> tsn;
//};
//
//const std::string newHorizonsProfileInput ="\
//#Version\n\
//1.0\n\
//\n\
//#Asset\n\
//scene/solarsystem/missions/newhorizons/newhorizons\trequired\n\
//scene/solarsystem/missions/newhorizons/model\trequired\n\
//\n\
//#Property\n\
//setPropertyValueSingle\tNavigationHandler.OrbitalNavigator.FollowAnchorNodeRotationDistance\t20.000000\n\
//setPropertyValueSingle\tScene.Pluto.Renderable.Enabled\tfalse\n\
//setPropertyValueSingle\tScene.Charon.Renderable.Enabled\tfalse\n\
//setPropertyValueSingle\tScene.PlutoBarycenterTrail.Renderable.Enabled\tfalse\n\
//\n\
//#Keybinding\n\
//a\tSets the focus of the camera on 'NewHorizons'.\tFocus on New Horizons\t/New Horizons\tfalse\t\"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'NewHorizons');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)\"\n\
//SHIFT+a\tSets the focus of the camera on 'NewHorizons'.\tAnchor at New Horizons, Aim at Pluto\t/New Horizons\tfalse\t\"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'NewHorizons');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', 'Pluto');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)\"\n\
//s\tSets the focus of the camera on 'Pluto'\tFocus on Pluto\t/New Horizons\tfalse\t\"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'Pluto') ;openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)\"\n\
//d\tSets the focus of the camera on 'Charon'.\tFocus on New Charon\t/New Horizons\tfalse\t\"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'Charon');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)\"\n\
//F7\tToggles New Horizons image projection.\tToggle NH Image Projection\t/New Horizons\tfalse\t[[local enabled = openspace.getPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.PerformProjection'); openspace.setPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.PerformProjection', not enabled); openspace.setPropertyValue('Scene.CharonProjection.Renderable.ProjectionComponent.PerformProjection', not enabled)]]\n\
//F8\tRemoves all image projections from Pluto and Charon.\tClear image projections\t/New Horizons\tfalse\t\"openspace.setPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.ClearAllProjections', true); openspace.setPropertyValue('Scene.CharonProjection.Renderable.ProjectionComponent.ClearAllProjections', true)\"\n\
//F9\tJumps to the 14th of July 2015 at 0900 UTC and clears all projections.\tReset time and projections\t/New Horizons\tfalse\t\"openspace.time.setTime('2015-07-14T09:00:00.00');openspace.setPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.ClearAllProjections', true);openspace.setPropertyValue('Scene.CharonProjection.Renderable.ProjectionComponent.ClearAllProjections', true)\"\n\
//KP_8\tIncreases the height map exaggeration on Pluto.\tPluto HeightExaggeration +\t/New Horizons\tfalse\tpropertyHelper.increment('Scene.PlutoProjection.Renderable.HeightExaggeration', 5000)\n\
//CTRL+I\tIncreases the height map exaggeration on Pluto.\tPluto HeightExaggeration +\t/New Horizons\tfalse\tpropertyHelper.increment('Scene.PlutoProjection.Renderable.HeightExaggeration', 5000)\n\
//KP_2\tDecreases the height map exaggeration on Pluto.\tPluto HeightExaggeration -\t/New Horizons\tfalse\tpropertyHelper.decrement('Scene.PlutoProjection.Renderable.HeightExaggeration', 5000)\n\
//CTRL+K\tDecreases the height map exaggeration on Pluto.\tPluto HeightExaggeration -\t/New Horizons\tfalse\tpropertyHelper.decrement('Scene.PlutoProjection.Renderable.HeightExaggeration', 5000)\n\
//KP_9\tIncreases the height map exaggeration on Charon.\tCharon HeightExaggeration +\t/New Horizons\tfalse\tpropertyHelper.increment('Scene.CharonProjection.Renderable.HeightExaggeration', 5000)\n\
//CTRL+O\tIncreases the height map exaggeration on Charon.\tCharon HeightExaggeration +\t/New Horizons\tfalse\tpropertyHelper.increment('Scene.CharonProjection.Renderable.HeightExaggeration', 5000)\n\
//KP_3\tDecreases the height map exaggeration on Charon.\tCharon HeightExaggeration -\t/New Horizons\tfalse\tpropertyHelper.decrement('Scene.CharonProjection.Renderable.HeightExaggeration', 5000)\n\
//CTRL+L\tDecreases the height map exaggeration on Charon.\tCharon HeightExaggeration -\t/New Horizons\tfalse\tpropertyHelper.decrement('Scene.CharonProjection.Renderable.HeightExaggeration', 5000)\n\
//o\tToggles the visibility of the trail behind Pluto.\tToggle Pluto Trail\t/New Horizons\tfalse\tpropertyHelper.invert('Scene.PlutoBarycentricTrail.Renderable.Enabled')\n\
//j\tToggles the visibility of the text labels of Pluto, Charon, Hydra, Nix, Kerberos, and Styx.\tToggle Pluto Labels\t/New Horizons\tfalse\trenderableHelper.toggle('Scene.PlutoText') .. renderableHelper.toggle('Scene.CharonText') .. renderableHelper.toggle('Scene.HydraText') .. renderableHelper.toggle('Scene.NixText') .. renderableHelper.toggle('Scene.KerberosText') .. renderableHelper.toggle('Scene.StyxText')\n\
//l\tToggles the visibility of the labels for the New Horizons instruments.\tToggle New Horizons Labels\t/New Horizons\tfalse\tpropertyHelper.fadeInOut('Scene.Labels.Renderable.Opacity', 2.0)\n\
//m\tDraws the instrument field of views in a solid color or as lines.\tToggle instrument FOVs\t/New Horizons\tfalse\tpropertyHelper.invert('Scene.NH_LORRI.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_LEISA.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_PAN1.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_PAN2.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_RED.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_BLUE.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_FT.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_METHANE.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_NIR.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_ALICE_AIRGLOW.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_ALICE_SOC.Renderable.SolidDraw')\n\
//Shift+t\tToggles the visibility of the shadow visualization of Pluto and Charon.\tToggle Shadows\t/New Horizons\tfalse\trenderableHelper.toggle('Scene.PlutoShadow') .. renderableHelper.toggle('Scene.CharonShadow')\n\
//t\tToggles the trail of New Horizons.\tToggle NH Trail\t/New Horizons\tfalse\trenderableHelper.toggle('Scene.NewHorizonsTrailPluto')\n\
//h\tDisables visibility of the trails\tHide Trails\t/Rendering\tfalse\t\"local list = openspace.getProperty('*Trail.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end\"\n\
//1\tSetting the simulation speed to 1 seconds per realtime second\tSet sim speed 1\t/Simulation Speed\tfalse\t\"openspace.time.interpolateDeltaTime(1)\"\n\
//2\tSetting the simulation speed to 5 seconds per realtime second\tSet sim speed 5\t/Simulation Speed\tfalse\t\"openspace.time.interpolateDeltaTime(5)\"\n\
//3\tSetting the simulation speed to 10 seconds per realtime second\tSet sim speed 10\t/Simulation Speed\tfalse\t\"openspace.time.interpolateDeltaTime(10)\"\n\
//4\tSetting the simulation speed to 20 seconds per realtime second\tSet sim speed 20\t/Simulation Speed\tfalse\t\"openspace.time.interpolateDeltaTime(20)\"\n\
//5\tSetting the simulation speed to 40 seconds per realtime second\tSet sim speed 40\t/Simulation Speed\tfalse\t\"openspace.time.interpolateDeltaTime(40)\"\n\
//6\tSetting the simulation speed to 60 seconds per realtime second\tSet sim speed 60\t/Simulation Speed\tfalse\t\"openspace.time.interpolateDeltaTime(60)\"\n\
//7\tSetting the simulation speed to 120 seconds per realtime second\tSet sim speed 120\t/Simulation Speed\tfalse\t\"openspace.time.interpolateDeltaTime(120)\"\n\
//8\tSetting the simulation speed to 360 seconds per realtime second\tSet sim speed 360\t/Simulation Speed\tfalse\t\"openspace.time.interpolateDeltaTime(360)\"\n\
//9\tSetting the simulation speed to 540 seconds per realtime second\tSet sim speed 540\t/Simulation Speed\tfalse\t\"openspace.time.interpolateDeltaTime(540)\"\n\
//0\tSetting the simulation speed to 1080 seconds per realtime second\tSet sim speed 1080\t/Simulation Speed\tfalse\t\"openspace.time.interpolateDeltaTime(1080)\"\n\
//Shift+1\tSetting the simulation speed to 2160 seconds per realtime second\tSet sim speed 2160\t/Simulation Speed\tfalse\t\"openspace.time.interpolateDeltaTime(2160)\"\n\
//Shift+2\tSetting the simulation speed to 4320 seconds per realtime second\tSet sim speed 4320\t/Simulation Speed\tfalse\t\"openspace.time.interpolateDeltaTime(4320)\"\n\
//Shift+3\tSetting the simulation speed to 8640 seconds per realtime second\tSet sim speed 8640\t/Simulation Speed\tfalse\t\"openspace.time.interpolateDeltaTime(8640)\"\n\
//\n\
//#Time\n\
//absolute\t2015-07-14T08:00:00.00\n\
//\n\
//#Camera\n\
//setNavigationState\t\"NewHorizons\"\t\t\"Root\"\t-6.572656E1, -7.239404E1, -2.111890E1\t0.102164, -0.362945, 0.926193\t\t\n\
//\n\
//#MarkNodes\n\
//Pluto\n\
//NewHorizons\n\
//Charon";
//
//const std::string newHorizonsExpectedSceneOutput = "\
//\n\
//asset.require(\"base\");\n\
//local assetHelper = asset.require(\"util/asset_helper\")\n\
//local propertyHelper = asset.require(\"util/property_helper\")\n\
//local sceneHelper = asset.require(\"util/scene_helper\")\n\
//local renderableHelper = asset.require(\"util/renderable_helper\")\n\
//asset.require(\"scene/solarsystem/missions/newhorizons/newhorizons\")\n\
//asset.require(\"scene/solarsystem/missions/newhorizons/model\")\n\
//\n\
//local Keybindings = {\n\
//  {\n\
//    Key = \"a\",\n\
//    Documentation = \"Sets the focus of the camera on 'NewHorizons'.\",\n\
//    Name = \"Focus on New Horizons\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = \"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'NewHorizons');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)\"\n\
//  },\n\
//  {\n\
//    Key = \"SHIFT+a\",\n\
//    Documentation = \"Sets the focus of the camera on 'NewHorizons'.\",\n\
//    Name = \"Anchor at New Horizons, Aim at Pluto\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = \"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'NewHorizons');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', 'Pluto');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)\"\n\
//  },\n\
//  {\n\
//    Key = \"s\",\n\
//    Documentation = \"Sets the focus of the camera on 'Pluto'\",\n\
//    Name = \"Focus on Pluto\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = \"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'Pluto') ;openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', ''); openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)\"\n\
//  },\n\
//  {\n\
//    Key = \"d\",\n\
//    Documentation = \"Sets the focus of the camera on 'Charon'.\",\n\
//    Name = \"Focus on New Charon\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = \"openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Anchor', 'Charon');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValue('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil)\"\n\
//  },\n\
//  {\n\
//    Key = \"F7\",\n\
//    Documentation = \"Toggles New Horizons image projection.\",\n\
//    Name = \"Toggle NH Image Projection\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = [[local enabled = openspace.getPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.PerformProjection'); openspace.setPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.PerformProjection', not enabled); openspace.setPropertyValue('Scene.CharonProjection.Renderable.ProjectionComponent.PerformProjection', not enabled)]]\n\
//  },\n\
//  {\n\
//    Key = \"F8\",\n\
//    Documentation = \"Removes all image projections from Pluto and Charon.\",\n\
//    Name = \"Clear image projections\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = \"openspace.setPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.ClearAllProjections', true); openspace.setPropertyValue('Scene.CharonProjection.Renderable.ProjectionComponent.ClearAllProjections', true)\"\n\
//  },\n\
//  {\n\
//    Key = \"F9\",\n\
//    Documentation = \"Jumps to the 14th of July 2015 at 0900 UTC and clears all projections.\",\n\
//    Name = \"Reset time and projections\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = \"openspace.time.setTime('2015-07-14T09:00:00.00');openspace.setPropertyValue('Scene.PlutoProjection.Renderable.ProjectionComponent.ClearAllProjections', true);openspace.setPropertyValue('Scene.CharonProjection.Renderable.ProjectionComponent.ClearAllProjections', true)\"\n\
//  },\n\
//  {\n\
//    Key = \"KP_8\",\n\
//    Documentation = \"Increases the height map exaggeration on Pluto.\",\n\
//    Name = \"Pluto HeightExaggeration +\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = propertyHelper.increment('Scene.PlutoProjection.Renderable.HeightExaggeration', 5000)\n\
//  },\n\
//  {\n\
//    Key = \"CTRL+I\",\n\
//    Documentation = \"Increases the height map exaggeration on Pluto.\",\n\
//    Name = \"Pluto HeightExaggeration +\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = propertyHelper.increment('Scene.PlutoProjection.Renderable.HeightExaggeration', 5000)\n\
//  },\n\
//  {\n\
//    Key = \"KP_2\",\n\
//    Documentation = \"Decreases the height map exaggeration on Pluto.\",\n\
//    Name = \"Pluto HeightExaggeration -\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = propertyHelper.decrement('Scene.PlutoProjection.Renderable.HeightExaggeration', 5000)\n\
//  },\n\
//  {\n\
//    Key = \"CTRL+K\",\n\
//    Documentation = \"Decreases the height map exaggeration on Pluto.\",\n\
//    Name = \"Pluto HeightExaggeration -\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = propertyHelper.decrement('Scene.PlutoProjection.Renderable.HeightExaggeration', 5000)\n\
//  },\n\
//  {\n\
//    Key = \"KP_9\",\n\
//    Documentation = \"Increases the height map exaggeration on Charon.\",\n\
//    Name = \"Charon HeightExaggeration +\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = propertyHelper.increment('Scene.CharonProjection.Renderable.HeightExaggeration', 5000)\n\
//  },\n\
//  {\n\
//    Key = \"CTRL+O\",\n\
//    Documentation = \"Increases the height map exaggeration on Charon.\",\n\
//    Name = \"Charon HeightExaggeration +\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = propertyHelper.increment('Scene.CharonProjection.Renderable.HeightExaggeration', 5000)\n\
//  },\n\
//  {\n\
//    Key = \"KP_3\",\n\
//    Documentation = \"Decreases the height map exaggeration on Charon.\",\n\
//    Name = \"Charon HeightExaggeration -\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = propertyHelper.decrement('Scene.CharonProjection.Renderable.HeightExaggeration', 5000)\n\
//  },\n\
//  {\n\
//    Key = \"CTRL+L\",\n\
//    Documentation = \"Decreases the height map exaggeration on Charon.\",\n\
//    Name = \"Charon HeightExaggeration -\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = propertyHelper.decrement('Scene.CharonProjection.Renderable.HeightExaggeration', 5000)\n\
//  },\n\
//  {\n\
//    Key = \"o\",\n\
//    Documentation = \"Toggles the visibility of the trail behind Pluto.\",\n\
//    Name = \"Toggle Pluto Trail\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = propertyHelper.invert('Scene.PlutoBarycentricTrail.Renderable.Enabled')\n\
//  },\n\
//  {\n\
//    Key = \"j\",\n\
//    Documentation = \"Toggles the visibility of the text labels of Pluto, Charon, Hydra, Nix, Kerberos, and Styx.\",\n\
//    Name = \"Toggle Pluto Labels\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = renderableHelper.toggle('Scene.PlutoText') .. renderableHelper.toggle('Scene.CharonText') .. renderableHelper.toggle('Scene.HydraText') .. renderableHelper.toggle('Scene.NixText') .. renderableHelper.toggle('Scene.KerberosText') .. renderableHelper.toggle('Scene.StyxText')\n\
//  },\n\
//  {\n\
//    Key = \"l\",\n\
//    Documentation = \"Toggles the visibility of the labels for the New Horizons instruments.\",\n\
//    Name = \"Toggle New Horizons Labels\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = propertyHelper.fadeInOut('Scene.Labels.Renderable.Opacity', 2.0)\n\
//  },\n\
//  {\n\
//    Key = \"m\",\n\
//    Documentation = \"Draws the instrument field of views in a solid color or as lines.\",\n\
//    Name = \"Toggle instrument FOVs\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = propertyHelper.invert('Scene.NH_LORRI.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_LEISA.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_PAN1.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_PAN2.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_RED.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_BLUE.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_FT.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_METHANE.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_RALPH_MVIC_NIR.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_ALICE_AIRGLOW.Renderable.SolidDraw') .. propertyHelper.invert('Scene.NH_ALICE_SOC.Renderable.SolidDraw')\n\
//  },\n\
//  {\n\
//    Key = \"Shift+t\",\n\
//    Documentation = \"Toggles the visibility of the shadow visualization of Pluto and Charon.\",\n\
//    Name = \"Toggle Shadows\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = renderableHelper.toggle('Scene.PlutoShadow') .. renderableHelper.toggle('Scene.CharonShadow')\n\
//  },\n\
//  {\n\
//    Key = \"t\",\n\
//    Documentation = \"Toggles the trail of New Horizons.\",\n\
//    Name = \"Toggle NH Trail\",\n\
//    GuiPath = \"/New Horizons\",\n\
//    Local = false,\n\
//    Command = renderableHelper.toggle('Scene.NewHorizonsTrailPluto')\n\
//  },\n\
//  {\n\
//    Key = \"h\",\n\
//    Documentation = \"Disables visibility of the trails\",\n\
//    Name = \"Hide Trails\",\n\
//    GuiPath = \"/Rendering\",\n\
//    Local = false,\n\
//    Command = \"local list = openspace.getProperty('*Trail.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end\"\n\
//  },\n\
//  {\n\
//    Key = \"1\",\n\
//    Documentation = \"Setting the simulation speed to 1 seconds per realtime second\",\n\
//    Name = \"Set sim speed 1\",\n\
//    GuiPath = \"/Simulation Speed\",\n\
//    Local = false,\n\
//    Command = \"openspace.time.interpolateDeltaTime(1)\"\n\
//  },\n\
//  {\n\
//    Key = \"2\",\n\
//    Documentation = \"Setting the simulation speed to 5 seconds per realtime second\",\n\
//    Name = \"Set sim speed 5\",\n\
//    GuiPath = \"/Simulation Speed\",\n\
//    Local = false,\n\
//    Command = \"openspace.time.interpolateDeltaTime(5)\"\n\
//  },\n\
//  {\n\
//    Key = \"3\",\n\
//    Documentation = \"Setting the simulation speed to 10 seconds per realtime second\",\n\
//    Name = \"Set sim speed 10\",\n\
//    GuiPath = \"/Simulation Speed\",\n\
//    Local = false,\n\
//    Command = \"openspace.time.interpolateDeltaTime(10)\"\n\
//  },\n\
//  {\n\
//    Key = \"4\",\n\
//    Documentation = \"Setting the simulation speed to 20 seconds per realtime second\",\n\
//    Name = \"Set sim speed 20\",\n\
//    GuiPath = \"/Simulation Speed\",\n\
//    Local = false,\n\
//    Command = \"openspace.time.interpolateDeltaTime(20)\"\n\
//  },\n\
//  {\n\
//    Key = \"5\",\n\
//    Documentation = \"Setting the simulation speed to 40 seconds per realtime second\",\n\
//    Name = \"Set sim speed 40\",\n\
//    GuiPath = \"/Simulation Speed\",\n\
//    Local = false,\n\
//    Command = \"openspace.time.interpolateDeltaTime(40)\"\n\
//  },\n\
//  {\n\
//    Key = \"6\",\n\
//    Documentation = \"Setting the simulation speed to 60 seconds per realtime second\",\n\
//    Name = \"Set sim speed 60\",\n\
//    GuiPath = \"/Simulation Speed\",\n\
//    Local = false,\n\
//    Command = \"openspace.time.interpolateDeltaTime(60)\"\n\
//  },\n\
//  {\n\
//    Key = \"7\",\n\
//    Documentation = \"Setting the simulation speed to 120 seconds per realtime second\",\n\
//    Name = \"Set sim speed 120\",\n\
//    GuiPath = \"/Simulation Speed\",\n\
//    Local = false,\n\
//    Command = \"openspace.time.interpolateDeltaTime(120)\"\n\
//  },\n\
//  {\n\
//    Key = \"8\",\n\
//    Documentation = \"Setting the simulation speed to 360 seconds per realtime second\",\n\
//    Name = \"Set sim speed 360\",\n\
//    GuiPath = \"/Simulation Speed\",\n\
//    Local = false,\n\
//    Command = \"openspace.time.interpolateDeltaTime(360)\"\n\
//  },\n\
//  {\n\
//    Key = \"9\",\n\
//    Documentation = \"Setting the simulation speed to 540 seconds per realtime second\",\n\
//    Name = \"Set sim speed 540\",\n\
//    GuiPath = \"/Simulation Speed\",\n\
//    Local = false,\n\
//    Command = \"openspace.time.interpolateDeltaTime(540)\"\n\
//  },\n\
//  {\n\
//    Key = \"0\",\n\
//    Documentation = \"Setting the simulation speed to 1080 seconds per realtime second\",\n\
//    Name = \"Set sim speed 1080\",\n\
//    GuiPath = \"/Simulation Speed\",\n\
//    Local = false,\n\
//    Command = \"openspace.time.interpolateDeltaTime(1080)\"\n\
//  },\n\
//  {\n\
//    Key = \"Shift+1\",\n\
//    Documentation = \"Setting the simulation speed to 2160 seconds per realtime second\",\n\
//    Name = \"Set sim speed 2160\",\n\
//    GuiPath = \"/Simulation Speed\",\n\
//    Local = false,\n\
//    Command = \"openspace.time.interpolateDeltaTime(2160)\"\n\
//  },\n\
//  {\n\
//    Key = \"Shift+2\",\n\
//    Documentation = \"Setting the simulation speed to 4320 seconds per realtime second\",\n\
//    Name = \"Set sim speed 4320\",\n\
//    GuiPath = \"/Simulation Speed\",\n\
//    Local = false,\n\
//    Command = \"openspace.time.interpolateDeltaTime(4320)\"\n\
//  },\n\
//  {\n\
//    Key = \"Shift+3\",\n\
//    Documentation = \"Setting the simulation speed to 8640 seconds per realtime second\",\n\
//    Name = \"Set sim speed 8640\",\n\
//    GuiPath = \"/Simulation Speed\",\n\
//    Local = false,\n\
//    Command = \"openspace.time.interpolateDeltaTime(8640)\"\n\
//  },\n\
//}\n\
//\n\
//asset.onInitialize(function ()\n\
//  openspace.time.setTime(\"2015-07-14T08:00:00.00\")\n\
//\n\
//  sceneHelper.bindKeys(Keybindings)\n\
//\n\
//  openspace.markInterestingNodes({\"Pluto\", \"NewHorizons\", \"Charon\", })\n\
//\n\
//  openspace.setPropertyValueSingle(\"NavigationHandler.OrbitalNavigator.FollowAnchorNodeRotationDistance\", 20.000000)\n\
//  openspace.setPropertyValueSingle(\"Scene.Pluto.Renderable.Enabled\", false)\n\
//  openspace.setPropertyValueSingle(\"Scene.Charon.Renderable.Enabled\", false)\n\
//  openspace.setPropertyValueSingle(\"Scene.PlutoBarycenterTrail.Renderable.Enabled\", false)\n\
//\n\
//  openspace.navigation.setNavigationState({Anchor = \"NewHorizons\", ReferenceFrame = \"Root\", Position = {-6.572656E1, -7.239404E1, -2.111890E1}, Up = {0.102164, -0.362945, 0.926193}, })\n\
//end)";
//
//const std::string detectChangedPropsResult_1 = "\
//#Version\n\
//" + std::string(Profile::FormatVersion) + "\n\
//\n\
//#Module\n\
//\n\
//#Asset\n\
//scene/solarsystem/planets/earth/earth\trequired\n\
//scene/solarsystem/planets/earth/satellites/satellites\trequired\n\
//\n\
//#Property\n\
//setPropertyValue\t{earth_satellites}.Renderable.Enabled\tfalse\n\
//setPropertyValueSingle\tScene.Pluto.Renderable.Enabled\tfalse\n\
//setPropertyValueSingle\tScene.Charon.Renderable.Enabled\tfalse\n\
//initialized 1st\t123\n\
//initialized 2nd\t3.14159\n\
//initialized 3rd\ttested.\n\
//initialized fourth\tfalse.\n\
//\n\
//#Keybinding\n\
//\n\
//#Time\n\
//absolute\t2020-02-29T01:23:45.00\n\
//\n\
//#Camera\n\
//setNavigationState\t\"Earth\"\t\"Sun\"\t\"root\"\t-1.000000,-2.000000,-3.000000\t0.000000,0.000000,1.000000\t0.000000\t0.000000\n\
//\n\
//#MarkNodes\n\
//Earth\n\
//Mars\n\
//Moon\n\
//Sun\n\
//";
//
//const std::string detectChangedAssetsResult_1 = "\
//#Version\n\
//" + std::string(Profile::FormatVersion) + "\n\
//\n\
//#Module\n\
//\n\
//#Asset\n\
//scene/solarsystem/planets/earth/earth\trequired\n\
//scene/solarsystem/planets/earth/satellites/satellites\trequired\n\
//initialization\trequested\n\
//test2\trequested\n\
//test3\trequested\n\
//\n\
//#Property\n\
//setPropertyValue\t{earth_satellites}.Renderable.Enabled\tfalse\n\
//setPropertyValueSingle\tScene.Pluto.Renderable.Enabled\tfalse\n\
//setPropertyValueSingle\tScene.Charon.Renderable.Enabled\tfalse\n\
//\n\
//#Keybinding\n\
//\n\
//#Time\n\
//absolute\t2020-02-29T01:23:45.00\n\
//\n\
//#Camera\n\
//setNavigationState\t\"Earth\"\t\"Sun\"\t\"root\"\t-1.000000,-2.000000,-3.000000\t0.000000,0.000000,1.000000\t0.000000\t0.000000\n\
//\n\
//#MarkNodes\n\
//Earth\n\
//Mars\n\
//Moon\n\
//Sun\n\
//";
//
//const std::string detectChangedAssetsResult_2 = "\
//#Version\n\
//" + std::string(Profile::FormatVersion) + "\n\
//\n\
//#Module\n\
//\n\
//#Asset\n\
//scene/solarsystem/planets/earth/earth\trequired\n\
//scene/solarsystem/planets/earth/satellites/satellites\trequired\n\
//test3\trequested\n\
//test4\trequested\n\
//\n\
//#Property\n\
//setPropertyValue\t{earth_satellites}.Renderable.Enabled\tfalse\n\
//setPropertyValueSingle\tScene.Pluto.Renderable.Enabled\tfalse\n\
//setPropertyValueSingle\tScene.Charon.Renderable.Enabled\tfalse\n\
//\n\
//#Keybinding\n\
//\n\
//#Time\n\
//absolute\t2020-02-29T01:23:45.00\n\
//\n\
//#Camera\n\
//setNavigationState\t\"Earth\"\t\"Sun\"\t\"root\"\t-1.000000,-2.000000,-3.000000\t0.000000,0.000000,1.000000\t0.000000\t0.000000\n\
//\n\
//#MarkNodes\n\
//Earth\n\
//Mars\n\
//Moon\n\
//Sun\n\
//";
//
//const std::string detectChangedAssetsResult_3 = "\
//#Version\n\
//" + std::string(Profile::FormatVersion) + "\n\
//\n\
//#Module\n\
//\n\
//#Asset\n\
//scene/solarsystem/planets/earth/earth\trequired\n\
//scene/solarsystem/planets/earth/satellites/satellites\trequired\n\
//test2\trequested\n\
//test4\trequested\n\
//\n\
//#Property\n\
//setPropertyValue\t{earth_satellites}.Renderable.Enabled\tfalse\n\
//setPropertyValueSingle\tScene.Pluto.Renderable.Enabled\tfalse\n\
//setPropertyValueSingle\tScene.Charon.Renderable.Enabled\tfalse\n\
//\n\
//#Keybinding\n\
//\n\
//#Time\n\
//absolute\t2020-02-29T01:23:45.00\n\
//\n\
//#Camera\n\
//setNavigationState\t\"Earth\"\t\"Sun\"\t\"root\"\t-1.000000,-2.000000,-3.000000\t0.000000,0.000000,1.000000\t0.000000\t0.000000\n\
//\n\
//#MarkNodes\n\
//Earth\n\
//Mars\n\
//Moon\n\
//Sun\n\
//";
//
//const std::string detectChangedAssetsResult_4 = "\
//#Version\n\
//" + std::string(Profile::FormatVersion) + "\n\
//\n\
//#Module\n\
//\n\
//#Asset\n\
//scene/solarsystem/planets/earth/earth\trequired\n\
//scene/solarsystem/planets/earth/satellites/satellites\trequired\n\
//test2\trequested\n\
//test4\trequested\n\
//\n\
//#Property\n\
//setPropertyValue\t{earth_satellites}.Renderable.Enabled\tfalse\n\
//setPropertyValueSingle\tScene.Pluto.Renderable.Enabled\tfalse\n\
//setPropertyValueSingle\tScene.Charon.Renderable.Enabled\tfalse\n\
//\n\
//#Keybinding\n\
//\n\
//#Time\n\
//absolute\t2020-02-29T01:23:45.00\n\
//\n\
//#Camera\n\
//setNavigationState\t\"Earth\"\t\"Sun\"\t\"root\"\t-1.000000,-2.000000,-3.000000\t0.000000,0.000000,1.000000\t0.000000\t0.000000\n\
//\n\
//#MarkNodes\n\
//Earth\n\
//Mars\n\
//Moon\n\
//Sun\n\
//";
//
//const std::string detectChangedAssetsResult_5 = "\
//#Version\n\
//" + std::string(Profile::FormatVersion) + "\n\
//\n\
//#Module\n\
//\n\
//#Asset\n\
//scene/solarsystem/planets/earth/earth\trequired\n\
//scene/solarsystem/planets/earth/satellites/satellites\trequired\n\
//test2\trequested\n\
//test4\trequested\n\
//test5\tremoved\n\
//\n\
//#Property\n\
//setPropertyValue\t{earth_satellites}.Renderable.Enabled\tfalse\n\
//setPropertyValueSingle\tScene.Pluto.Renderable.Enabled\tfalse\n\
//setPropertyValueSingle\tScene.Charon.Renderable.Enabled\tfalse\n\
//\n\
//#Keybinding\n\
//\n\
//#Time\n\
//absolute\t2020-02-29T01:23:45.00\n\
//\n\
//#Camera\n\
//setNavigationState\t\"Earth\"\t\"Sun\"\t\"root\"\t-1.000000,-2.000000,-3.000000\t0.000000,0.000000,1.000000\t0.000000\t0.000000\n\
//\n\
//#MarkNodes\n\
//Earth\n\
//Mars\n\
//Moon\n\
//Sun\n\
//";
//
//const std::string detectChangedAssetsResult_6 = "\
//#Version\n\
//" + std::string(Profile::FormatVersion) + "\n\
//\n\
//#Module\n\
//\n\
//#Asset\n\
//scene/solarsystem/planets/earth/earth\trequired\n\
//scene/solarsystem/planets/earth/satellites/satellites\trequired\n\
//test2\trequested\n\
//test4\trequested\n\
//scene/solarsystem/planets/earth/earth\tremoved\n\
//scene/solarsystem/planets/earth/satellites/satellites\tremoved\n\
//\n\
//#Property\n\
//setPropertyValue\t{earth_satellites}.Renderable.Enabled\tfalse\n\
//setPropertyValueSingle\tScene.Pluto.Renderable.Enabled\tfalse\n\
//setPropertyValueSingle\tScene.Charon.Renderable.Enabled\tfalse\n\
//\n\
//#Keybinding\n\
//\n\
//#Time\n\
//absolute\t2020-02-29T01:23:45.00\n\
//\n\
//#Camera\n\
//setNavigationState\t\"Earth\"\t\"Sun\"\t\"root\"\t-1.000000,-2.000000,-3.000000\t0.000000,0.000000,1.000000\t0.000000\t0.000000\n\
//\n\
//#MarkNodes\n\
//Earth\n\
//Mars\n\
//Moon\n\
//Sun\n\
//";
//
//testProfileFormat buildTestProfile1();
//std::string stringFromSingleProfileSection(std::vector<std::string>& section,
//                                                     bool blankLineSeparator);
//std::string stringFromTestProfileFormat(testProfileFormat& tpf);
//ProfileFile makeProfileFromString(std::string s);
//
//class StringPerLineReader {
//public:
//    StringPerLineReader(std::string s);
//    bool getNextLine(std::string& line);
//
//private:
//    std::istringstream _iss;
//};
//
#endif //__OPENSPACE_TEST___PROFILE_COMMON___H__
