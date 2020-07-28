#Version
1.0

#Asset
util/scene_helper	sceneHelper
base	
scene/solarsystem/missions/perseverance/perseverance	
scene/solarsystem/missions/perseverance/shortcuts	perseveranceShortcuts
scene/solarsystem/missions/insight/edl	insightAsset
scene/solarsystem/missions/insight/shortcuts	insightShortcuts

#Camera
goToGeo	"Mars"	58.5877	16.1924	8000000

#MarkNodes
Mars
Insight
Perseverance

#AdditionalScripts
local insightEDLShortcuts = sceneHelper.extractShortcuts({"Insight Height Offset", "Enable HiRISE", "Insight EDL Time", "Insight EDL NavigationState"}, insightShortcuts.Shortcuts)
local insightDisableShortcuts = sceneHelper.extractShortcuts({"Default Height Offset", "Disable HiRISE"}, insightShortcuts.Shortcuts)
local PerseverenceLandedShortcuts = sceneHelper.extractShortcuts({"Perseverance Height Offset", "Perseverance landed time", "Enable HiRISE"}, perseveranceShortcuts.Shortcuts)
local Keybindings = { sceneHelper.createKeyBindFromShortcuts("i", insightEDLShortcuts, "/Missions/Insight", "Set and goto Insight Landing", "Setup scene for insight EDL" ), sceneHelper.createKeyBindFromShortcuts("SHIFT+i", insightDisableShortcuts, "/Missions/Insight", "Unset Insight Landing", "Disable Mars layer settings used for insight EDL" ), sceneHelper.createKeyBindFromShortcuts("p", PerseverenceLandedShortcuts, "/Missions/Perseverance" )}
sceneHelper.bindKeys(Keybindings)
