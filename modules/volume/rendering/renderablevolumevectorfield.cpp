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

#include <modules/volume/rendering/renderablevolumevectorfield.h>

#include <openspace/documentation/documentation.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>


namespace {
    constexpr std::string_view _loggerCat = "RenderableVectorField";

    constexpr openspace::properties::Property::PropertyInfo StrideInfo = {
        "Stride",
        "Stride",
        "The stride to use when downsampling the volume data"
    };

    constexpr openspace::properties::Property::PropertyInfo VolumeDataInfo = {
        "VolumeFile",
        "Volume file",
        "The path to the file containing the volume data"
    };

    constexpr openspace::properties::Property::PropertyInfo MinDomainInfo = {
        "MinDomain",
        "Min Domain",
        "The min domain values that the volume should be mapped to"
    };

    constexpr openspace::properties::Property::PropertyInfo MaxDomainInfo = {
        "MaxDomain",
        "Max Domain",
        "The max domain values that the volume should be mapped to"
    };

    constexpr openspace::properties::Property::PropertyInfo DimensionsInfo = {
        "Dimensions",
        "Dimensions",
        "The dimensions of the volume data"
    };

    struct [[codegen::Dictionary(RenderableVectorField)]] Parameters {
        // [[codegen::verbatim(VolumeDataInfo.description)]]
        std::filesystem::path volumeFile;
        // [[codegen::verbatim(StrideInfo.description)]]
        int stride;
        // [[codegen::verbatim(MinDomainInfo.description)]]
        glm::dvec3 minDomain;
        // [[codegen::verbatim(MaxDomainInfo.description)]]
        glm::dvec3 maxDomain;
        // [[codegen::verbatim(DimensionsInfo.description)]]
        glm::dvec3 dimensions;

    };

#include "renderablevolumevectorfield_codegen.cpp"
} // namespace

namespace openspace::volume {

documentation::Documentation RenderableVectorField::Documentation() {
    return codegen::doc<Parameters>("volume_renderable_vectorfield");

}

RenderableVectorField::RenderableVectorField(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _stride( StrideInfo, 0, 0, 16 )
    , _sourceFile(VolumeDataInfo)
    , _minDomain(MinDomainInfo)
    , _maxDomain(MaxDomainInfo)
    , _dimensions(DimensionsInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _sourceFile = p.volumeFile.string();
    _minDomain = p.minDomain;
    _maxDomain = p.maxDomain;
    _dimensions = p.dimensions;
    _stride = p.stride;

    addProperty(_stride);
    addProperty(_minDomain);
    addProperty(_maxDomain);
    addProperty(_dimensions);
}

void RenderableVectorField::initializeGL()
{
    std::filesystem::path dataFile = absPath(_sourceFile);
    if (!std::filesystem::is_regular_file(dataFile)) {
        throw ghoul::RuntimeError(std::format("Could not load data file '{}'", dataFile));
    }
}

void RenderableVectorField::deinitializeGL()
{
}

bool RenderableVectorField::isReady() const
{
    return true;
}

void RenderableVectorField::render(const RenderData& data, RendererTasks& renderTask)
{
}

void RenderableVectorField::update(const UpdateData& data)
{
}

} // namespace openspace::volume
