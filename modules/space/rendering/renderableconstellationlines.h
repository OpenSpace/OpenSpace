/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLECONSTELLATIONLINES___H__
#define __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLECONSTELLATIONLINES___H__

#include <modules/space/rendering/renderableconstellation.h>

#include <ghoul/opengl/uniformcache.h>
#include <unordered_map>

namespace ghoul::filesystem { class File; }
namespace ghoul::fontrendering { class Font; }
namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {

namespace documentation { struct Documentation; }

class RenderableConstellationLines : public RenderableConstellation {
public:
    explicit RenderableConstellationLines(const ghoul::Dictionary& dictionary);
    ~RenderableConstellationLines() override = default;

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    struct ConstellationLine {
        bool isEnabled = true;
        std::string identifier;
        int lineIndex;
        int colorIndex;
        // From: Partiview User's Guide
        // Brian Abbott
        // Hayden Planetarium American Museum of Natural History New York, USA
        // "Specifies the dimensions of the mesh"
        // "If you wish to draw a line between points, then numU will be 1 while
        // numV will equal the number of points to connect.
        // If you want a square, 4000×4000 grid with lines every 200 units,
        // then numU numV will both equal 21
        int numV;
        GLuint vaoArray;
        GLuint vboArray;
        std::vector<GLfloat> vertices;
    };

    void createConstellations();
    void renderConstellations(const RenderData& data, const glm::dmat4& modelViewMatrix,
        const glm::dmat4& projectionMatrix);

    bool loadData();
    bool readSpeckFile();

    /**
     * Callback method that gets triggered when <code>_constellationSelection</code>
     * changes
     */
    void selectionPropertyHasChanged() override;

    bool _hasSpeckFile = false;
    bool _dataIsDirty = true;

    properties::BoolProperty _drawElements;

    std::unique_ptr<ghoul::opengl::ProgramObject> _program = nullptr;
    UniformCache(modelViewTransform, projectionTransform, alphaValue,
        color) _uniformCache;

    std::string _speckFile;

    DistanceUnit _constellationUnit = DistanceUnit::Parsec;

    std::vector<float> _fullData;

    std::unordered_map<int, glm::vec3> _constellationColorMap;
    std::unordered_map<int, ConstellationLine> _renderingConstellationsMap;
};
} // namespace openspace

#endif // __OPENSPACE_MODULE_DIGITALUNIVERSE___RENDERABLECONSTELLATIONLINES___H__
