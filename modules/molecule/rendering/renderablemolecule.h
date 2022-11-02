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

#pragma once

#include "openspace/properties/listproperty.h"
#include "openspace/properties/optionproperty.h"
#include "openspace/properties/selectionproperty.h"
#include <openspace/rendering/renderable.h>

#include <openspace/properties/list/stringlistproperty.h>
#include <openspace/properties/list/intlistproperty.h>
#include <openspace/properties/list/doublelistproperty.h>
#include <openspace/properties/vector/dvec3property.h>

#include <md_gl.h>
#include <md_molecule.h>
#include <md_trajectory.h>
#include <core/md_bitfield.h>
//#include "viamd/gfx/conetracing_utils.h"

namespace openspace {

struct RenderData;
class HttpMemoryDownload;

class RenderableMolecule : public Renderable {
public:
    explicit RenderableMolecule(const ghoul::Dictionary& dictionary);
    virtual ~RenderableMolecule();

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;
    bool isReady() const override;
    void render(const RenderData& data, RendererTasks& tasks) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:

    struct molecule_data_t {
        glm::vec3 extent;
        glm::vec3 center;
        float radius;
        //md_molecule_api* moleculeApi;
        //md_trajectory_api* trajectoryApi;

        md_molecule_t molecule;
        const md_trajectory_i* trajectory;

        md_gl_representation_t drawRep;
        md_gl_molecule_t drawMol;
        md_bitfield_t visibilityMask;
        //cone_trace::GPUVolume occupancyVolume;
    };
    
    void computeAABB(molecule_data_t& mol);
    void updateAnimation(molecule_data_t& mol, double t);
    void updateRepresentation(molecule_data_t& mol);
    void applyViamdFilter(molecule_data_t& mol, std::string_view filter);
    void applyViamdScript(molecule_data_t& mol, std::string_view script);
    
    void initMolecule(molecule_data_t& mol, std::string_view mol_file, std::string_view traj_file = {});
    void freeMolecule(molecule_data_t& mol);

    bool _renderableInView; // indicates whether the molecule is in view in any camera's viewpoint

    double _frame;
    
    molecule_data_t _molecule;
    
    properties::StringProperty _moleculeFile;
    properties::StringProperty _trajectoryFile;
    properties::OptionProperty _repType;
    properties::OptionProperty _coloring;
    properties::FloatProperty _repScale;
    properties::FloatProperty _animationSpeed;
    properties::StringProperty _viamdFilter;
    properties::BoolProperty _ssaoEnabled;
    properties::FloatProperty _ssaoIntensity;
    properties::FloatProperty _ssaoRadius;
    properties::FloatProperty _ssaoBias;
};

} // namespace openspace
