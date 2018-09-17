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
export const InterpolateTogglePauseScript = 'openspace.time.interpolateTogglePause()';
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
export const InterpolateDeltaTimeScript = `openspace.time.interpolateDeltaTime(${ValuePlaceholder})`;
