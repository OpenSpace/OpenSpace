/****************************************************************************************
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

#ifndef __OPENSPACE_MODULE_SPACE___RENDERABLESMALLBODY___H__
#define __OPENSPACE_MODULE_SPACE___RENDERABLESMALLBODY___H__

#include <openspace/rendering/renderable.h>

#include <modules/base/rendering/renderabletrail.h>
#include <modules/space/translation/keplertranslation.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/uintproperty.h>
#include <ghoul/glm.h>
#include <ghoul/misc/objectmanager.h>
#include <ghoul/opengl/programobject.h>

namespace openspace {

class RenderableSmallBody : public Renderable {
public:
    RenderableSmallBody(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;
    void render(const RenderData& data, RendererTasks& rendererTask) override;

    static documentation::Documentation Documentation();
    /**
        * Reads the provided file downloaded from the JPL Small Body Database and calls
        * the KeplerTranslation::setKeplerElments method with the correct values.
        * If \p filename is a valid JPL SBDB file but contains
        * disallowed values (see KeplerTranslation::setKeplerElements), a
        * KeplerTranslation::RangeError is thrown.
        *
        * \param filename The path to the file that contains the file.
        *
        * \throw ghoul::RuntimeError if the file does not exist or there is a
        *        problem with its format.
        * \pre The \p filename must exist
        */
    void readJplSbDb(const std::string& filename);

private:
    struct Vertex {
        glm::vec3 position;
        glm::vec3 color;
        glm::vec2 texcoord;
    };

    struct KeplerParameters {
        double inclination = 0.0;
        double semiMajorAxis = 0.0;
        double ascendingNode = 0.0;
        double eccentricity = 0.0;
        double argumentOfPeriapsis = 0.0;
        double meanAnomaly = 0.0;
        double meanMotion = 0.0;
        double epoch = 0.0;
        double period = 0.0;
    };

    /// The layout of the VBOs
    struct TrailVBOLayout {
        float x, y, z, time;
        double epoch, period; 
    };

    void readOrbitalParamsFromThisLine(int& fieldCount, std::streamoff& csvLine,
        std::ifstream& file);

    KeplerTranslation _keplerTranslator;
    std::vector<KeplerParameters> _sbData;
    std::vector<std::string> _sbNames;

    /// The backend storage for the vertex buffer object containing all points for this
    /// trail.
    std::vector<TrailVBOLayout> _vertexBufferData;

    /// The index array that is potentially used in the draw call. If this is empty, no
    /// element draw call is used.
    std::vector<unsigned int> _indexBufferData;

    GLuint _vertexArray;
    GLuint _vertexBuffer;
    GLuint _indexBuffer;

    //GLuint _vaoTest; // vertexArrayObject
    //GLuint _vboTest; // vertextBufferObject
    //GLuint _eboTest; // elementBufferObject/ indexBufferObject       

    void updateBuffers();

    ghoul::opengl::ProgramObject* _programObject;

    properties::StringProperty _path;
    properties::UIntProperty _nSegments;
    properties::UIntProperty _upperLimit;

    RenderableTrail::Appearance _appearance;

    glm::vec3 _position;

    UniformCache(modelView, projection, lineFade, inGameTime, color, opacity,
        numberOfSegments) _uniformCache;

    const double convertAuToKm = 1.496e8;
    const double convertDaysToSecs = 86400.;
};

static double importAngleValue(const std::string& angle);

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___RENDERABLESMALLBODY___H__

