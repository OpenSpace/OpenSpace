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
#include <openspace/properties/vector/vec4property.h>
#include <core/md_bitfield.h>
#include <md_gl.h>
#include <md_molecule.h>
#include <md_trajectory.h>

namespace openspace {

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
    void updateTrajectoryFrame(const UpdateData& data);

    void initMolecule(std::string_view molFile, std::string_view trajFile);

    // Indicates whether the molecule is in view in any camera's viewpoint
    bool _renderableInView = true;

    // This represents the epoch which we derive our local frame time from
    double _localEpoch = 0.0;
    // This is the current frame of the trajectory
    double _frame = 0.0;

    md_molecule_t _molecule = {};
    const md_trajectory_i* _trajectory = nullptr;
    md_gl_molecule_t _glMolecule = {};
    double _radius = 0.0;

    struct Representation : public properties::PropertyOwner {
        explicit Representation(size_t number, const md_molecule_t& molecule_,
            bool enabled_, molecule::rep::Type type_, molecule::rep::Color color_,
            std::string filter_, float scale_, glm::vec4 uniformColor_);
        ~Representation();

        md_gl_representation_t glRep = {};
        md_bitfield_t mask;
        const md_molecule_t& molecule;
        bool isDynamic = false;

        properties::BoolProperty enabled;
        properties::OptionProperty type;
        properties::OptionProperty color;
        properties::StringProperty filter;
        properties::FloatProperty scale;
        properties::Vec4Property uniformColor;
    };

    std::vector<std::unique_ptr<Representation>> _repData;
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
