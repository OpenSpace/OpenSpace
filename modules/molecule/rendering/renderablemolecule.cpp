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
#include "viamd/coloring.h"
#include <modules/molecule/rendering/renderablemolecule.h>

#include <ghoul/logging/logmanager.h>

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

// COMBAK: because my ide complains
#ifndef ZoneScoped
#define ZoneScoped
#endif


constexpr const char* shader_output_snippet = R"(
layout(location = 0) out vec4 out_color;

void write_fragment(vec3 view_coord, vec3 view_vel, vec3 view_normal, vec4 color, uint atom_index) {
   out_color  = color;
}
)";

namespace {
    constexpr const char* _loggerCat = "RenderableMolecule";

    enum class MoleculeType {
        Pdb,
        Gro,
    };

    enum class RepresentationType {
        SpaceFill = MD_GL_REP_SPACE_FILL,
        Licorice = MD_GL_REP_LICORICE,
    };

    enum class Coloring {
        // Uniform,
        Cpk,
        AtomIndex,
        ResId,
        ResIndex,
        ChainId,
        ChainIndex,
        SecondaryStructure,
        // Property
    };


    constexpr openspace::properties::Property::PropertyInfo FileInfo = {
        "file",
        "Molecule File",
        "File path or URL to a molecule file"
    };

    constexpr openspace::properties::Property::PropertyInfo MoleculeTypeInfo = {
        "moleculeType",
        "Molecule Type",
        "Type of molecule file"
    };

    constexpr openspace::properties::Property::PropertyInfo IsUrlInfo = {
        "isUrl",
        "Is URL",
        "Whether the Molecule File is a URL or a local path"
    };

    constexpr openspace::properties::Property::PropertyInfo RepTypeInfo = {
        "RepType",
        "Representation Type",
        "How to draw the molecule"
    };

    constexpr openspace::properties::Property::PropertyInfo ColoringInfo = {
        "Coloring",
        "Coloring",
        "Select a color mapping for the atoms"
    };

    constexpr openspace::properties::Property::PropertyInfo RepScaleInfo = {
        "RepScale",
        "Representation Scale",
        "Thickness of the atoms in Space Fill or Licorice representation"
    };

    struct [[codegen::Dictionary(RenderableMolecule)]] Parameters {
        // [[codegen::verbatim(FileInfo.description)]]
        std::string fileOrUrl;

        enum class [[codegen::map(MoleculeType)]] MoleculeType {
            Pdb,
            Gro,
        };

        // [[codegen::verbatim(MoleculeTypeInfo.description)]]
        std::optional<MoleculeType> moleculeType;

        // [[codegen::verbatim(IsUrlInfo.description)]]
        std::optional<bool> isUrl;

        enum class [[codegen::map(RepresentationType)]] RepresentationType {
            SpaceFill,
            Licorice,
        };

        // [[codegen::verbatim(RepTypeInfo.description)]]
        std::optional<RepresentationType> repType;

        enum class [[codegen::map(Coloring)]] Coloring {
            // Uniform,
            Cpk,
            AtomIndex,
            ResId,
            ResIndex,
            ChainId,
            ChainIndex,
            SecondaryStructure,
            // Property
        };

        // [[codegen::verbatim(ColoringInfo.description)]]
        std::optional<Coloring> coloring;

        // [[codegen::verbatim(RepScaleInfo.description)]]
        std::optional<float> repScale;
    };

#include "renderablemolecule_codegen.cpp"
}

namespace openspace {

documentation::Documentation RenderableMolecule::Documentation() {
    return codegen::doc<Parameters>("molecule_renderablemolecule");
}

RenderableMolecule::RenderableMolecule(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary),
    _fileOrUrl(FileInfo),
    _moleculeType(MoleculeTypeInfo),
    _isUrl(IsUrlInfo),
    _repType(RepTypeInfo),
    _coloring(ColoringInfo),
    _repScale(RepScaleInfo, 1.f, 0.1f, 100.f), 
    _isMoleculeLoaded(false)
{
    _molecule = {};
    _trajectory = {};
    _drawMol = {};
    _drawRep = {};
    

    const Parameters p = codegen::bake<Parameters>(dictionary);

    // TODO: find if it is url automatically based on fileOrUrl when property is unset.
    _isUrl = p.isUrl.value_or(true);

    _fileOrUrl.onChange([this]() {
        if (_isUrl) {
            loadMoleculeFromUrl(_fileOrUrl.value());
        } else {
            loadMoleculeFromFile(_fileOrUrl.value());
        }
    });

    _fileOrUrl = p.fileOrUrl;

    _moleculeType.addOptions({
        { static_cast<int>(MoleculeType::Pdb), "PDB" },
        { static_cast<int>(MoleculeType::Gro), "GRO" },
    });
    
    if (p.moleculeType.has_value()) {
        _moleculeType = static_cast<int>(codegen::map<MoleculeType>(*p.moleculeType));
    } else {
        // TODO: find molecule type automatically based on file extension when property
        // is unset.
        _moleculeType = static_cast<int>(MoleculeType::Pdb);
    }

    _repType.addOptions({
        { static_cast<int>(RepresentationType::SpaceFill), "Space Fill" },
        { static_cast<int>(RepresentationType::Licorice), "Licorice" },
    });
    
    if (p.repType.has_value()) {
        _repType = static_cast<int>(codegen::map<RepresentationType>(*p.repType));
    } else {
        _repType = static_cast<int>(RepresentationType::SpaceFill);
    }
    
    _coloring.addOptions({
        { static_cast<int>(Coloring::Cpk), "CPK" },
        { static_cast<int>(Coloring::AtomIndex), "Atom Index" },
        { static_cast<int>(Coloring::ResId), "Residue ID" },
        { static_cast<int>(Coloring::ResIndex), "Residue Index" },
        { static_cast<int>(Coloring::ChainId), "Chain ID" },
        { static_cast<int>(Coloring::ChainIndex), "Chain Index" },
        { static_cast<int>(Coloring::SecondaryStructure), "Secondary Structure" },
    });
    
    if (p.coloring.has_value()) {
        _coloring = static_cast<int>(codegen::map<Coloring>(*p.coloring));
    } else {
        _coloring = static_cast<int>(Coloring::Cpk);
    }
    
    _repScale = p.repScale.value_or(1.f);
    
    addProperty(_fileOrUrl);
    addProperty(_moleculeType);
    addProperty(_isUrl);
    addProperty(_repType);
    addProperty(_coloring);
    addProperty(_repScale);
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
    if (_fileDownload) {
        if (_fileDownload->hasSucceeded()) {
            auto& blob = _fileDownload->downloadedData();
            initMolecule(std::string_view(blob.data(), blob.size()));
            _fileDownload.release();
        } else if (_fileDownload->hasFailed()) {
            _fileDownload.release();
        }
    }
}

void RenderableMolecule::render(const RenderData& data, RendererTasks& tasks) {
    using namespace glm;
    const dmat4 I(1.0);

    if (_molecule.atom.count) {
        
        // _repScale = data.modelTransform.scale.x;

        // zoom out and move closer the camera
        dmat4 camView =
            translate(I, {0.f, 0.f, -100.f}) *
            scale(I, dvec3(1.0 / 215.0)) *
            translate(I, { 0, 0, 21500 }) *
            data.camera.combinedViewMatrix() *
            I;

        mat4 model_matrix =
            scale(I, data.modelTransform.scale) *
            dmat4(data.modelTransform.rotation) *
            translate(I, data.modelTransform.translation - dvec3(_center)) *
            I;
        
        mat4 view_matrix =
            camView *
            I;

        mat4 proj_matrix =
            dmat4(data.camera.projectionMatrix()) *
            I;

        md_gl_draw_op_t draw_op = {};

        draw_op.rep = &_drawRep;
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
        
        md_gl_draw(&args);
    }
}

void RenderableMolecule::initMolecule(std::string_view data) {
    if(_isMoleculeLoaded) {
        freeMolecule();
    }

    md_molecule_api* api;

    switch (static_cast<MoleculeType>(_moleculeType.value())) {
    case MoleculeType::Pdb:
        api = md_pdb_molecule_api();
        break;
    case MoleculeType::Gro:
        api = md_gro_molecule_api();
        break;
    default:
        ghoul_assert(false, "unexpected molecule type");
        break;
    }

    api->init_from_str(&_molecule, { data.data(), static_cast<int64_t>(data.size()) }, default_allocator);

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
    
        // setBoundingSphere(radius);
        // setInteractionSphere(radius);
    }

    // GL MOL
    md_gl_molecule_init(&_drawMol, &_molecule);

    // REP
    md_gl_representation_init(&_drawRep, &_drawMol);
    updateRepresentation();
    
    // set the updateRepresentation hook once, when the molecule is first initialized
    if (!_isMoleculeLoaded) {
        _repType.onChange([this]() { updateRepresentation(); });
        _repScale.onChange([this]() { updateRepresentation(); });
        _coloring.onChange([this]() { updateRepresentation(); });
    }
    _isMoleculeLoaded = true;
}

void RenderableMolecule::updateRepresentation() {
    { // REPRESENTATION TYPE
        md_gl_representation_args_t rep_args{};
    
        switch (static_cast<RepresentationType>(_repType.value())) {
            case RepresentationType::SpaceFill:
                rep_args.space_fill.radius_scale = _repScale;
                break;
            case RepresentationType::Licorice:
                rep_args.licorice.radius = _repScale * .5f;
                break;
            default:
                ghoul_assert(false, "unexpected molecule representation type");
                break;
        }
        // rep_args.cartoon.thickness_scale = 1;
        // rep_args.cartoon.width_scale = 1;
        
        // _repType = MD_GL_REP_CARTOON;

        md_gl_representation_set_type_and_args(&_drawRep, _repType, rep_args);
    }
    { // COLORING
        uint32_t* colors = (uint32_t*)md_alloc(default_temp_allocator, sizeof(uint32_t) * _molecule.atom.count);
        uint32_t count = static_cast<uint32_t>(_molecule.atom.count);

        switch (static_cast<Coloring>(_coloring.value())) {
            case Coloring::Cpk:
                color_atoms_cpk(colors, count, _molecule);
                break;
            case Coloring::AtomIndex:
                color_atoms_idx(colors, count, _molecule);
                break;
            case Coloring::ResId:
                color_atoms_residue_id(colors, count, _molecule);
                break;
            case Coloring::ResIndex:
                color_atoms_residue_index(colors, count, _molecule);
                break;
            case Coloring::ChainId:
                color_atoms_chain_id(colors, count, _molecule);
                break;
            case Coloring::ChainIndex:
                color_atoms_chain_index(colors, count, _molecule);
                break;
            case Coloring::SecondaryStructure:
                color_atoms_secondary_structure(colors, count, _molecule);
                break;
            default:
                ghoul_assert(false, "unexpected molecule coloring");
                break;
        }

        md_gl_representation_set_color(&_drawRep, 0, static_cast<uint32_t>(_molecule.atom.count), colors, 0);
    }
}

void RenderableMolecule::freeMolecule() {
    switch (static_cast<MoleculeType>(_moleculeType.value())) {
    case MoleculeType::Pdb:
        md_pdb_molecule_free(&_molecule, default_allocator);
        break;
    case MoleculeType::Gro:
        md_gro_molecule_free(&_molecule, default_allocator);
        break;
    }
    _molecule = {};
    md_gl_representation_free(&_drawRep);
}

void RenderableMolecule::freeTrajectory() {
}

bool RenderableMolecule::loadMoleculeFromUrl(std::string_view url) {
    // TODO: handle error

    if (_fileDownload) {
        _fileDownload->cancel();
        _fileDownload->wait();
    }
    _downloadProgress = 0.0;

    _fileDownload = std::make_unique<HttpMemoryDownload>(url.data());

    _fileDownload->onProgress([this](size_t downloadedBytes, std::optional<size_t> totalBytes) -> bool {
        if (totalBytes.has_value()) {
            _downloadProgress = downloadedBytes / static_cast<double>(totalBytes.value());
        }
        return true;
    });

    _fileDownload->start();

    return true;
}

bool RenderableMolecule::loadMoleculeFromFile(std::string_view filepath) {
    // TODO: handle error

    if (_fileDownload) {
        _fileDownload->cancel();
        _fileDownload->wait();
    }
    _downloadProgress = 0.0;

    std::ifstream fs;
    std::stringstream ss;
    fs.open(filepath.data());
    if (!fs.is_open()) {
        std::string err = "failed to open file: " + std::string(filepath);
        LERROR(err);
        return false;
    }
    ss << fs.rdbuf();
    initMolecule(ss.str());

    return true;
}

} // namespace openspace
