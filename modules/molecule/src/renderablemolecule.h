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

#ifndef __OPENSPACE_MODULE_MOLECULE___RENDERABLEMOLECULE___H__
#define __OPENSPACE_MODULE_MOLECULE___RENDERABLEMOLECULE___H__

#include <openspace/rendering/renderable.h>

#include <modules/molecule/src/def.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <core/md_bitfield.h>
#include <md_gl.h>
#include <md_molecule.h>
#include <md_trajectory.h>

namespace openspace {

struct RenderData;

class RenderableMolecule : public Renderable {
public:
    explicit RenderableMolecule(const ghoul::Dictionary& dictionary);
    virtual ~RenderableMolecule();

    void initializeGL() override;
    void deinitializeGL() override;
    
    bool isReady() const override;
    void render(const RenderData& data, RendererTasks& tasks) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    void addRepresentation(bool enabled, mol::rep::Type type = mol::rep::Type::SpaceFill,
        mol::rep::Color color = mol::rep::Color::Cpk, std::string filter = "all",
        float scale = 1.f, glm::vec4 uniform_color = glm::vec4(1.f));
    
    void updateTrajectoryFrame(const UpdateData& data);

    void initMolecule(std::string_view mol_file, std::string_view traj_file = "");
    void freeMolecule();
    void updateRepresentations();

    // indicates whether the molecule is in view in any camera's viewpoint
    bool _renderableInView = true;

    // This represents the epoch which we derive our local frame time from
    double _localEpoch;
    // This is the current frame of the trajectory
    double _frame;

    md_molecule_t _molecule;
    const md_trajectory_i* _trajectory;
    md_gl_molecule_t _gl_molecule;

    glm::dvec3 _center;
    double _radius;

    struct RepData {
        RepData();
        ~RepData();
        md_gl_representation_t gl_rep;
        md_bitfield_t mask;
        bool dynamic;
        bool enabled = true;
    };

    std::vector<RepData> _repData;
    properties::PropertyOwner _repProps;
    
    properties::StringProperty _moleculeFile;
    properties::StringProperty _trajectoryFile;
    properties::BoolProperty _coarseGrained;
    properties::BoolProperty _applyPbcOnLoad;
    properties::BoolProperty _applyPbcPerFrame;
    properties::DoubleProperty _animationBaseScale;
    properties::DoubleProperty _animationSpeed;
    properties::OptionProperty _animationRepeatMode;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_MOLECULE___RENDERABLEMOLECULE___H__
