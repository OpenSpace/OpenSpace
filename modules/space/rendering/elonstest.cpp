/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/space/rendering/elonstest.h>

#include <ghoul/filesystem/filesystem.h>
#include <modules/space/spacemodule.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>


namespace {
    constexpr const char* ProgramName = "ElonsTest";
    
    static const openspace::properties::Property::PropertyInfo PathInfo = {
        "Path",
        "Path",
        "The file path to the CSV file to read"
    };
    
    static const openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Segments",
        "The number of segments to use for each orbit ellipse"
    };
    
    static const openspace::properties::Property::PropertyInfo EccentricityColumnInfo = {
        "EccentricityColumn",
        "EccentricityColumn",
        "The header of the column where the eccentricity is stored"
    };
    
    static const openspace::properties::Property::PropertyInfo SemiMajorAxisColumnInfo = {
        "SemiMajorAxisColumn",
        "SemiMajorAxisColumn",
        "The header of the column where the semi-major axis is stored"
    };
    
    static const openspace::properties::Property::PropertyInfo SemiMajorAxisUnitInfo = {
        "SemiMajorAxisUnit",
        "SemiMajorAxisUnit",
        "The unit of the semi major axis. For example: If specified in km, set this to 1000."
    };
    
    static const openspace::properties::Property::PropertyInfo InclinationColumnInfo = {
        "InclinationColumn",
        "InclinationColumn",
        "The header of the column where the inclination is stored"
    };
    
    static const openspace::properties::Property::PropertyInfo AscendingNodeColumnInfo = {
        "AscendingNodeColumn",
        "AscendingNodeColumn",
        "The header of the column where the ascending node is stored"
    };
    
    static const openspace::properties::Property::PropertyInfo ArgumentOfPeriapsisColumnInfo = {
        "ArgumentOfPeriapsisColumn",
        "ArgumentOfPeriapsisColumn",
        "The header of the column where the argument of periapsis is stored"
    };
    
    static const openspace::properties::Property::PropertyInfo MeanAnomalyAtEpochColumnInfo = {
        "MeanAnomalyAtEpochColumn",
        "MeanAnomalyAtEpochColumn",
        "The header of the column where the mean anomaly at epoch is stored"
    };
    
    static const openspace::properties::Property::PropertyInfo EpochColumnInfo = {
        "EpochColumn",
        "EpochColumn",
        "The header of the column where the epoch is stored"
    };
}

namespace openspace {

ElonsTest::ElonsTest(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _path(PathInfo)
    , _nSegments(SegmentsInfo)
    , _eccentricityColumnName(EccentricityColumnInfo)
    , _semiMajorAxisColumnName(SemiMajorAxisColumnInfo)
    , _semiMajorAxisUnit(SemiMajorAxisUnitInfo)
    , _inclinationColumnName(InclinationColumnInfo)
    , _ascendingNodeColumnName(AscendingNodeColumnInfo)
    , _argumentOfPeriapsisColumnName(ArgumentOfPeriapsisColumnInfo)
    , _meanAnomalyAtEpochColumnName(MeanAnomalyAtEpochColumnInfo)
    , _epochColumnName(EpochColumnInfo)
{
    _path =
        dictionary.value<std::string>(PathInfo.identifier);
    _nSegments =
        static_cast<int>(dictionary.value<double>(SegmentsInfo.identifier));
    _eccentricityColumnName =
        dictionary.value<std::string>(EccentricityColumnInfo.identifier);
    _semiMajorAxisColumnName =
        dictionary.value<std::string>(SemiMajorAxisColumnInfo.identifier);
    _semiMajorAxisUnit =
        dictionary.value<double>(SemiMajorAxisUnitInfo.identifier);
    _inclinationColumnName =
        dictionary.value<std::string>(InclinationColumnInfo.identifier);
    _ascendingNodeColumnName =
        dictionary.value<std::string>(AscendingNodeColumnInfo.identifier);
    _argumentOfPeriapsisColumnName =
        dictionary.value<std::string>(ArgumentOfPeriapsisColumnInfo.identifier);
    _meanAnomalyAtEpochColumnName =
        dictionary.value<std::string>(MeanAnomalyAtEpochColumnInfo.identifier);
    _epochColumnName =
        dictionary.value<std::string>(EpochColumnInfo.identifier);

    addProperty(_path);
    addProperty(_nSegments);
    addProperty(_semiMajorAxisUnit);
}
// uses Renderables destructor?

void ElonsTest::initialize(){
    // note to self, se vad Gene skrev. Fyll _vertexArray i init och 
        // rendera bara orbits, inga rörliga delar.
}

void ElonsTest::initializeGL() {
    _programObject = SpaceModule::ProgramObjectManager.request(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine.buildRenderProgram(
                ProgramName,
                absPath("${MODULE_SPACE}/shaders/renderablekeplerorbits_vs.glsl"),
                absPath("${MODULE_SPACE}/shaders/renderablekeplerorbits_fs.glsl")
            );
        }
    );

}

void ElonsTest::render(const RenderData& data, RendererTasks& rendererTask)  {

}

void ElonsTest::update(const UpdateData& data) {}

}
