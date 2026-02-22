/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_MODULE_MOLECULE___RENDERABLESIMULATIONBOX___H__
#define __OPENSPACE_MODULE_MOLECULE___RENDERABLESIMULATIONBOX___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/misc/listproperty.h>
#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/misc/selectionproperty.h>
#include <openspace/properties/vector/dvec3property.h>
#include <openspace/properties/vector/vec4property.h>
#include <ghoul/opengl/uniformcache.h>
#include <md_gl.h>
#include <md_molecule.h>
#include <md_trajectory.h>

namespace openspace {

class RenderableSimulationBox : public Renderable {
public:
    explicit RenderableSimulationBox(const ghoul::Dictionary& dictionary);
    ~RenderableSimulationBox() override;

    void initializeGL() override;
    void deinitializeGL() override;
    bool isReady() const override;
    void update(const UpdateData& data) override;
    void render(const RenderData& data, RendererTasks& tasks) override;

    static openspace::Documentation Documentation();

private:
    struct Molecules {
        std::filesystem::path moleculeFile;
        std::optional<std::filesystem::path> trajectoryFile;
        std::optional<int> count;

        struct Data {
            struct State {
                glm::dvec3 position;
                double angle;
                // moving direction where magnitude is linear velocity
                glm::dvec3 direction;
                // rotation axis where magnitude is angular velocity
                glm::dvec3 rotationAxis;
            };

            std::vector<State> states;
            md_molecule_t molecule = {};
            const md_trajectory_i* trajectory = nullptr;
            md_gl_representation_t drawRep = {};
            md_gl_molecule_t drawMol = {};
        };
        Data data;
    };

    void updateSimulation(Molecules::Data& mol, double dt);

    void initMolecule(Molecules::Data& mol, std::filesystem::path molFile,
        std::filesystem::path trajFile);
    void freeMolecule(Molecules::Data& mol);

    bool _renderableInView = true;

    struct {
        std::unique_ptr<ghoul::opengl::ProgramObject> program;
        UniformCache(transform, strokeWidth, strokeFalloffExp, fragDepth,
            strokeColor, opacity) uniforms;
        GLuint vao = 0;
    } _billboard;


    std::vector<Molecules> _molecules;

    OptionProperty _representation;
    OptionProperty _coloring;
    FloatProperty _repScale;
    FloatProperty _animationSpeed;
    FloatProperty _simulationSpeed;
    FloatProperty _linearVelocity;
    FloatProperty _angularVelocity;
    DVec3Property _simulationBox;
    FloatProperty _collisionRadius;
    StringProperty _filter;

    Vec4Property  _circleColor;
    FloatProperty _circleWidth;
    FloatProperty _circleFalloff;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_MOLECULE___RENDERABLESIMULATIONBOX___H__
