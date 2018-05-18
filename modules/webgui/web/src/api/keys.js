// eslint-disable import/prefer-default-export

// key for setting/getting the Origin (focus) node
export const OriginKey = 'NavigationHandler.Origin';
// key to get all scene graph nodes
// export const SceneGraphKey = '__allNodes';
// key to get all properties in openspace engine
// export const AllPropertiesKey = '__allProperties';
export const rootOwnerKey = '__rootOwner';
// export const AllScreenSpaceRenderablesKey = '__screenSpaceRenderables';
export const VersionInfoKey = 'VersionInfo';
export const SCMInfoKey = 'SCMInfo';
// script to toggle pause
export const TogglePauseScript = 'openspace.time.togglePause()';
// key for getting current time subscription
export const TimeKey = 'special:currentTime';
// script to toggle shutdown
export const ShutdownScript = 'openspace.toggleShutdown()';
// toggle console
export const ToggleConsoleScript = 'openspace.setPropertyValueSingle("LuaConsole.IsVisible", ' +
                                   'not openspace.getPropertyValue("LuaConsole.IsVisible"))';
export const ToggleNativeGuiScript = 'local b = openspace.getPropertyValue("Modules.ImGUI.Main.Enabled");' + 
                                     'openspace.setPropertyValueSingle("Modules.ImGUI.Main.Enabled", not b);' +
                                     'openspace.setPropertyValueSingle("Modules.ImGUI.Main.IsHidden", b);';
// key for getting current simulation time
export const CurrenTimeKey = 'currentTime';
// key for getting the delta time
export const DeltaTime = 'deltaTime';
// value placeholder in scripts with parameter
export const ValuePlaceholder = '___value___';
// script for setting deltatime
export const SetDeltaTimeScript = `openspace.time.setDeltaTime(${ValuePlaceholder})`;
// key for setting/getting the solarSystemOverview property
export const ApplyOverviewKey = 'NavigationHandler.OrbitalNavigator.ApplyOverview';
// key  for apply fly to trigger property
export const ApplyFlyToKey = 'NavigationHandler.OrbitalNavigator.ApplyFlyTo';
// key for json file with info for icons
export const infoIconKey = 'info_icons';
// key for endpoint for json files
export const dataEndpointKey = 'https://openspace.github.io/sci/mastertheses/2018/info/solarsystem/';
// script for setting goToGeo
export const SetGoToGeoScript = `openspace.globebrowsing.goToGeo(${ValuePlaceholder})`;
// script for setting time
export const SetTimeScript = `openspace.time.setTime(${ValuePlaceholder})`;
// key for getting story identifier property
export const StoryIdentifierKey = 'Modules.WebGui.StoryHandler.StoryIdentifier';
// key for apply remove tag trigger property
export const ApplyRemoveTagKey = 'Modules.WebGui.StoryHandler.ApplyRemoveTag';
// key for apply add tag trigger property
export const ApplyAddTagKey = 'Modules.WebGui.StoryHandler.ApplyAddTag';
// key for focus nodes list property
export const FocusNodesListKey = 'Modules.WebGui.StoryHandler.FocusNodesList';
// key for default story
export const defaultStory = 'story_default';
// key for overview limit
export const OverlimitKey = 'Modules.WebGui.StoryHandler.OverviewLimit';
// key for scale property
export const ScaleKey = `Scene.${ValuePlaceholder}.Scale.Scale`;
// keys for timePlayerController
export const FastRewind = 'fast_rewind';
export const Rewind = 'rewind';
export const Play = 'play';
export const Forward = 'forward';
export const FastForward = 'fast_forward';
