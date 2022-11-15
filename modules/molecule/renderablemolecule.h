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

#include <openspace/rendering/renderable.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>

#include <md_gl.h>
#include <md_molecule.h>
#include <md_trajectory.h>
#include "mol/def.h"

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
    
    void addRepresentation(mol::rep::Type type = mol::rep::Type::SpaceFill, mol::rep::Color color = mol::rep::Color::Cpk, std::string filter = "all", float scale = 1.0f);
    void initMolecule(std::string_view mol_file, std::string_view traj_file = {});
    void freeMolecule();
    void updateRepresentationsGL();

    bool _renderableInView; // indicates whether the molecule is in view in any camera's viewpoint

    double _frame;

    md_molecule_t _molecule;
    const md_trajectory_i* _trajectory;
    md_gl_molecule_t _gl_molecule;

    glm::dvec3  _center;
    double      _radius;

    std::vector<md_gl_representation_t> _gl_representations;
    properties::PropertyOwner  _representations;
    
    properties::StringProperty _moleculeFile;
    properties::StringProperty _trajectoryFile;
    properties::FloatProperty _animationSpeed;
    properties::BoolProperty _ssaoEnabled;
    properties::FloatProperty _ssaoIntensity;
    properties::FloatProperty _ssaoRadius;
    properties::FloatProperty _ssaoBias;
    properties::FloatProperty _exposure;
};

} // namespace openspace
