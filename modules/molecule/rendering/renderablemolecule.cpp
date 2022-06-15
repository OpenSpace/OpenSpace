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

#include "glbinding/gl/bitfield.h"
#include "glbinding/gl/functions.h"
#include "md_util.h"
#include <modules/molecule/rendering/renderablemolecule.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/httprequest.h>

//#include <openspace/rendering/renderengine.h>
//#include <openspace/util/boxgeometry.h>
//#include <openspace/util/distanceconstants.h>
#include <openspace/util/updatestructures.h>
//#include <openspace/engine/downloadmanager.h>

#include <md_pdb.h>
#include <md_gro.h>
#include <md_frame_cache.h>
#include <core/md_allocator.h>

constexpr const char* shader_output_snippet = R"(
layout(location = 0) out vec4 out_color;

void write_fragment(vec3 view_coord, vec3 view_vel, vec3 view_normal, vec4 color, uint atom_index) {
   out_color  = color;
}
)";

namespace {
    openspace::properties::Property::PropertyInfo pdb_id_info = {
        "pdb_id",
        "pdb_id",
        "This is the Protein Data Bank ID which is attempted to be loaded"
    };
}

namespace openspace {

documentation::Documentation RenderableMolecule::Documentation() {
    //return codegen::doc<Parameters>("galaxy_renderablegalaxy");
    return {};
}

RenderableMolecule::RenderableMolecule(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary),
    _pdb_id(pdb_id_info)
{
    _molecule = {};
    _trajectory = {};
    _draw_mol = {};
    _draw_rep = {};

    _pdb_id.onChange([this]() {
        loadProteinPDB(_pdb_id.value());
    });

    if (dictionary.hasKey("PdbId")) {
        _pdb_id.setValue(dictionary.value<std::string>("PdbId"));
    }
}

RenderableMolecule::~RenderableMolecule() {
    freeMolecule();
    freeTrajectory();
}

void RenderableMolecule::initialize() {
    ZoneScoped
}

void RenderableMolecule::initializeGL() {
    ZoneScoped
    md_gl_initialize();
    md_gl_shaders_init(&_shaders, shader_output_snippet);
}

void RenderableMolecule::deinitializeGL() {
    md_gl_shaders_free(&_shaders);
    md_gl_shutdown();
}

bool RenderableMolecule::isReady() const {
    return true;
}

void RenderableMolecule::update(const UpdateData& data) {
    if (_pdb_download) {
        if (_pdb_download->hasSucceeded()) {
            auto& blob = _pdb_download->downloadedData();
            initMolecule(std::string_view(blob.data(), blob.size()), MoleculeType::Pdb);
            _pdb_download.release();
        } else if (_pdb_download->hasFailed()) {
            _pdb_download.release();
        }
    }
}

void RenderableMolecule::render(const RenderData& data, RendererTasks& tasks) {
    using namespace glm;
    const dmat4 I(1.0);

    if (_molecule.atom.count) {

        // zoom out and move closer the camera
        dmat4 camView =
            translate(I, {0.f, 0.f, -100.f}) *
            scale(I, dvec3(1.0 / 215.0)) *
            translate(I, { 0, 0, 21500 }) *
            data.camera.combinedViewMatrix();

        mat4 model_matrix =
            scale(I, data.modelTransform.scale) *
            dmat4(data.modelTransform.rotation) *
            translate(I, data.modelTransform.translation - dvec3(_center));
        
        mat4 view_matrix =
            camView;

        mat4 proj_matrix = data.camera.projectionMatrix();

        md_gl_draw_op_t draw_op = {};

        draw_op.rep = &_draw_rep;
        draw_op.model_matrix = value_ptr(model_matrix);

        md_gl_draw_args_t args = {};
        args.shaders = &_shaders;
        args.draw_operations = {
            1,
            &draw_op,
        };
        args.view_transform = {
            value_ptr(view_matrix),
            value_ptr(proj_matrix),
            nullptr, nullptr
        };
        args.atom_mask = 0;
        args.options = 0;
        
        // gl::glClear(gl::GL_DEPTH_BUFFER_BIT);
        md_gl_draw(&args);
    }
}

void RenderableMolecule::initMolecule(std::string_view data, MoleculeType type) {
    freeMolecule();

    // MOL
    md_pdb_molecule_api()->init_from_str(&_molecule, { data.data(), static_cast<int64_t>(data.size()) }, default_allocator);

    if (_molecule.atom.count > 0) {
        glm::vec3 min_aabb {FLT_MAX};
        glm::vec3 max_aabb {-FLT_MAX};

        for (int64_t i = 0; i < _molecule.atom.count; ++i) {
            glm::vec3 p {_molecule.atom.x[i], _molecule.atom.y[i], _molecule.atom.z[i]};
            min_aabb = glm::min(min_aabb, p);
            max_aabb = glm::max(max_aabb, p);
        }

        _extent = max_aabb - min_aabb;
        _center = (min_aabb + max_aabb) * 0.5f;
        float radius = glm::compMax(_extent) * 0.5f;
    
        setBoundingSphere(radius);
        setInteractionSphere(radius);
    }

    // GL MOL
    md_gl_molecule_init(&_draw_mol, &_molecule);

    // REP
    md_gl_representation_init(&_draw_rep, &_draw_mol);
    updateRepresentation(MD_GL_REP_SPACE_FILL);

    uint32_t* colors = (uint32_t*)md_alloc(default_temp_allocator, sizeof(uint32_t) * _molecule.atom.count);
    for(int64_t i = 0; i < _molecule.atom.count; i++) {
        md_element_t elt = _molecule.atom.element[i];
        colors[i] = md_util_element_cpk_color(elt);
    }
    md_gl_representation_set_color(&_draw_rep, 0, static_cast<uint32_t>(_molecule.atom.count), colors, 0);
}

void RenderableMolecule::updateRepresentation(md_gl_representation_type_t rep_type) {
    md_gl_representation_args_t rep_args{};
    
    // float radius = data.modelTransform.scale.x * 1;
    float radius = 1.f;
    
    switch (rep_type) {
        case MD_GL_REP_SPACE_FILL:
            rep_args.space_fill.radius_scale = radius;
            break;
        case MD_GL_REP_LICORICE:
            rep_args.licorice.radius = radius;
            break;
    }

    md_gl_representation_set_type_and_args(&_draw_rep, rep_type, rep_args);
}

void RenderableMolecule::freeMolecule() {
    switch (_molecule_type) {
    case MoleculeType::Pdb:
        md_pdb_molecule_free(&_molecule, default_allocator);
    case MoleculeType::Gro:
        md_gro_molecule_free(&_molecule, default_allocator);
    default:
        break;
    }
    _molecule = {};
    md_gl_representation_free(&_draw_rep);
}

void RenderableMolecule::freeTrajectory() {
}

bool RenderableMolecule::loadProteinPDB(std::string_view pdb_id) {
    std::string url = "https://files.rcsb.org/download/" + std::string(pdb_id) + ".pdb";

    /*
    if (_pdb_download) {
        _pdb_download->cancel();
        _pdb_download->wait();
    }
    */

    _pdb_download = std::make_unique<HttpMemoryDownload>(url);

    _pdb_download->onProgress([this](size_t downloadedBytes, std::optional<size_t> totalBytes) -> bool {
        if (totalBytes.has_value()) {
            this->_pdb_download_progress = downloadedBytes / static_cast<double>(totalBytes.value());
        }
        return true;
    });

    _pdb_download->start();

    return true;
}

bool RenderableMolecule::loadMoleculeFile(std::string_view filename) {
    return true;
}

bool RenderableMolecule::loadTrajectoryFile(std::string_view filename) {
    return true;
}

} // namespace openspace
