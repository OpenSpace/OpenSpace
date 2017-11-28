// eslint-disable import/prefer-default-export

// key for setting/getting the Origin (focus) node
export const OriginKey = 'NavigationHandler.Origin';
// key for setting/getting the envelopes (focus) node
export const TransferFunctionKey = 'Enlil Sequence.renderable.TransferFunctionHandler.TransferFunction';
// key for setting/getting the histogram (focus) node
export const HistogramKey = 'Enlil Sequence.renderable.TransferFunctionHandler.Histogram';
// key to get all scene graph nodes
export const SceneGraphKey = '__allNodes';
// key to get all properties in openspace engine
export const AllPropertiesKey = '__allProperties';
export const AllScreenSpaceRenderablesKey = '__screenSpaceRenderables';
export const VersionInfoKey = 'VersionInfo';
export const SCMInfoKey = 'SCMInfo';
// script to toggle pause
export const TogglePauseScript = 'openspace.time.togglePause()';
// key for getting current time subscription
export const TimeKey = 'special:currentTime';
// script to toggle shutdown
export const ShutdownScript = 'openspace.toggleShutdown()';
// toggle console
export const ToggleConsoleScript = 'openspace.setPropertyValue("Global Properties.ImGUI.Main.Enabled", ' +
                                   'not openspace.getPropertyValue("Global Properties.ImGUI.Main.Enabled"))';
// key for getting current simulation time
export const CurrenTimeKey = 'currentTime';
// key for getting the delta time
export const DeltaTime = 'deltaTime';
// value placeholder in scripts with parameter
export const ValuePlaceholder = '___value___';
// script for setting deltatime
export const SetDeltaTimeScript = `openspace.time.setDeltaTime(${ValuePlaceholder})`;
