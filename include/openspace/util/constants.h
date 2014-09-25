/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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
namespace openspaceengine {
    const std::string keyPaths = "Paths";
    const std::string keyPathScene = "Paths.SCENEPATH";
    const std::string keyConfigSgct = "SGCTConfig";
    const std::string keyConfigScene = "Scene";
    const std::string keyStartupScript = "StartupScripts";
	const std::string keyConfigTimekernel = "SpiceTimeKernel";
} // namespace openspaceengine

namespace scenegraph {
    const std::string keyModules = "Modules";
    const std::string keyCamera = "Camera";
    const std::string keyFocusObject = "Focus";
    const std::string keyPositionObject = "Position";
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

namespace renderableplanet {
    const std::string keyGeometry = "Geometry";
} // namespace renderableplanet

namespace renderablestars {
	const std::string keySpeckFile = "SpeckFile";
	const std::string keyPathModule = "ModulePath";
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

namespace planetgeometry {
    const std::string keyType = "Type";
} // namespace planetgeometry

namespace renderablesphericalgrid {
	const std::string gridType = "GridType";
	const std::string gridColor = "GridColor";
	const std::string gridMatrix = "GridMatrix";
	const std::string gridSegments = "GridSegments";
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
} // namespace spiceephemeris

}  // namespace constants
}  // namespace openspace


#endif // __CONSTANTS_H__