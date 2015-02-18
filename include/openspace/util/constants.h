/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#ifndef __CONSTANTS_H__
#define __CONSTANTS_H__

#include <string>

namespace openspace {
namespace constants {

namespace fonts {
	const std::string keySGCT = "SGCTFont";
	const std::string keyMono = "Mono";
	const std::string keyLight = "Light";
} // namespace fonts

namespace configurationmanager {
    const std::string keyPaths = "Paths";
	const std::string keyCache = "CACHE";
	const std::string keyCachePath = keyPaths + "." + keyCache;
	const std::string keyFonts = "Fonts";
    const std::string keyConfigSgct = "SGCTConfig";
	const std::string keyLuaDocumentationType = "LuaDocumentationFile.Type";
	const std::string keyLuaDocumentationFile = "LuaDocumentationFile.File";
    const std::string keyConfigScene = "Scene";
	const std::string keyEnableGui = "EnableGUI";
    const std::string keyStartupScript = "StartupScripts";
    const std::string keySettingsScript = "SettingsScripts";
	const std::string keySpiceTimeKernel = "SpiceKernel.Time";
	const std::string keySpiceLeapsecondKernel = "SpiceKernel.LeapSecond";
	const std::string keyLogLevel = "Logging.LogLevel";
	const std::string keyLogImmediateFlush = "Logging.ImmediateFlush";
	const std::string keyLogs = "Logging.Logs";
} // namespace configurationmanager

namespace scenegraph {
    const std::string keyPathScene = "ScenePath";
	const std::string keyCommonFolder = "CommonFolder";
    const std::string keyModules = "Modules";
    const std::string keyCamera = "Camera";
    const std::string keyFocusObject = "Focus";
    const std::string keyPositionObject = "Position";
    const std::string keyViewOffset = "Offset";
    const std::string keyPathModule = "ModulePath";
}  // namespace scenegraph

namespace scenegraphnode {
    const std::string keyName = "Name";
    const std::string keyParentName = "Parent";
    const std::string keyRenderable = "Renderable";
    const std::string keyEphemeris = "Ephemeris";
} // namespace scenegraphnode

namespace renderable {
    const std::string keyType = "Type";
} // namespace renderable

namespace planetgeometry {
	const std::string keyType = "Type";
} // namespace planetgeometry

namespace renderablemodel {
	const std::string keyGeometry = "Geometry";
} // namespace renderablemodel

namespace modelgeometry {
	const std::string keyType = "Type";
	const std::string keyObjFile = "ObjFile";
} // namespace modelgeometry

namespace renderablestars {
	const std::string keyFile = "File";
	const std::string keyTexture = "Texture";
	const std::string keyColorMap = "ColorMap";
} // namespace renderablestars

namespace renderablevolumegl {
	const std::string keyVolume = "Volume";
	const std::string keyHints = "Hints";
	const std::string keyTransferFunction = "TransferFunction";
	const std::string keySampler = "Sampler";
	const std::string keyBoxScaling = "BoxScaling";
	const std::string keyVolumeName = "VolumeName";
	const std::string keyTransferFunctionName = "TransferFunctionName";
} // namespace renderablevolumegl

namespace renderablesphericalgrid {
	const std::string gridType = "GridType";
	const std::string gridColor = "GridColor";
	const std::string gridMatrix = "GridMatrix";
	const std::string gridSegments = "GridSegments";
	const std::string gridRadius = "GridRadius";
	const std::string gridPatentsRotiation = "ParentsRotation";
} // namespace renderablesphericalgrid

namespace ephemeris {
    const std::string keyType = "Type";
} // namespace ephemeris

namespace staticephemeris {
    const std::string keyPosition = "Position";
} // namespace staticephemeris

namespace spiceephemeris {
    const std::string keyBody = "Body";
    const std::string keyOrigin = "Observer";
	const std::string keyKernels = "Kernels";
} // namespace spiceephemeris

}  // namespace constants
}  // namespace openspace


#endif // __CONSTANTS_H__