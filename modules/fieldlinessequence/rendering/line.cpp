#include "line.h"

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/translation.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/programobject.h>
#include <cmath>
#include <optional>


namespace {
    constexpr const char* ProgramName = "EphemerisProgram";
}

namespace openspace {


Line::Line(const ghoul::Dictionary& dictionary) 
    : Renderable(dictionary)
{

}

void Line::initializeGL() {
    //ZoneScoped

    //_programObject = BaseModule::ProgramObjectManager.request(
    //    ProgramName,
    //    []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
    //        return global::renderEngine->buildRenderProgram(
    //            ProgramName,
    //            absPath("${MODULE_BASE}/shaders/renderabletrail_vs.glsl"),
    //            absPath("${MODULE_BASE}/shaders/renderabletrail_fs.glsl")
    //        );
    //    }
    //);

   //ghoul::opengl::updateUniformLocations(*_programObject, _uniformCache, UniformNames);
}

void Line::deinitializeGL()
{
}

bool Line::isReady() const
{
    return false;
}

void Line::render(const RenderData& data, RendererTasks& rendererTask)
{
}

void Line::update(const UpdateData& data)
{
}

}


