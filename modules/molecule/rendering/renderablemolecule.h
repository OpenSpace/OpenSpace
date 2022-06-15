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

#ifndef __OPENSPACE_MODULE_MOLECULE___RENDERABLEMOLECULE___H__
#define __OPENSPACE_MODULE_MOLECULE___RENDERABLEMOLECULE___H__

#include <openspace/rendering/renderable.h>

//#include <openspace/properties/scalar/floatproperty.h>
//#include <openspace/properties/vector/vec3property.h>
//#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
//#include <ghoul/opengl/ghoul_gl.h>
//#include <ghoul/opengl/uniformcache.h>


#include <md_gl.h>
#include <md_molecule.h>
#include <md_trajectory.h>

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
    enum class MoleculeType {
        Unknown,
        Pdb,
        Gro,
    };

    bool loadProteinPDB(std::string_view pdb_id);
    bool loadMoleculeFile(std::string_view filename);
    bool loadTrajectoryFile(std::string_view filename);

    void initMolecule(std::string_view data, MoleculeType type);
    void freeMolecule();

    void initTrajectory(std::string_view filename);
    void freeTrajectory();

    void updateRepresentation(md_gl_representation_type_t rep_type);

    std::unique_ptr<HttpMemoryDownload> _pdb_download;
    double _pdb_download_progress;
    double _frame;
    md_gl_shaders_t _shaders;
    md_gl_representation_t _draw_rep;
    md_gl_molecule_t _draw_mol;

    md_molecule_t _molecule;
    md_trajectory_i _trajectory;
    MoleculeType _molecule_type;

    glm::vec3 _center;
    glm::vec3 _extent;

    properties::StringProperty _pdb_id;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_MOLECULE___RENDERABLEMOLECULE___H__
