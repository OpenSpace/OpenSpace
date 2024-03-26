/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/rendering/transferfunction.h>

#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/opengl/texture.h>
#include <iterator>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>

namespace {
    constexpr std::string_view _loggerCat = "TransferFunction";
} // namespace

namespace openspace {

TransferFunction::TransferFunction(const std::string& filepath,
                                   TfChangedCallback tfChangedCallback)
    : _filepath(filepath)
{
    setPath(filepath);
    setCallback(std::move(tfChangedCallback));
}

TransferFunction::~TransferFunction() {}

void TransferFunction::setPath(const std::string& filepath) {
    if (_file) {
        _file = nullptr;
    }
    const std::filesystem::path f = absPath(filepath);
    if (!std::filesystem::is_regular_file(f)) {
        LERROR("Could not find transfer function file");
        _file = nullptr;
        return;
    }
    _filepath = f;
    _file = std::make_unique<ghoul::filesystem::File>(_filepath);
    _needsUpdate = true;
    _file->setCallback([this]() { _needsUpdate = true; });
}

ghoul::opengl::Texture& TransferFunction::texture() {
    ghoul_assert(_texture != nullptr, "Transfer function is null");
    update();
    return *_texture;
}

void TransferFunction::update() {
    if (_needsUpdate) {
        if (_filepath.extension() == ".txt") {
            setTextureFromTxt();
        }
        else {
            setTextureFromImage();
        }
        _texture->uploadTexture();
        _needsUpdate = false;
        if (_tfChangedCallback) {
            _tfChangedCallback(*this);
        }
    }
}

void TransferFunction::setCallback(TfChangedCallback callback) {
    _tfChangedCallback = std::move(callback);
}

void TransferFunction::setTextureFromTxt() {
    std::ifstream in;
    in.open(_filepath);

    if (!in.is_open()) {
        throw ghoul::FileNotFoundError(_filepath.string());
    }

    int width = 512;
    float lower = 0.f;
    float upper = 1.f;

    std::vector<MappingKey> mappingKeys;

    std::string line;

    while (ghoul::getline(in, line)) {
        std::istringstream iss(line);
        std::string key;
        iss >> key;

        if (key == "width") {
            iss >> width;
        }
        else if (key == "lower") {
            iss >> lower;
            lower = glm::clamp(lower, 0.f, 1.f);
        }
        else if (key == "upper") {
            iss >> upper;
            upper = glm::clamp(upper, lower, 1.f);
        }
        else if (key == "mappingkey") {
            float intensity = 0.f;
            glm::vec4 rgba;
            iss >> intensity;
            for(int i = 0; i < 4; i++) {
                iss >> rgba[i];
            }
            mappingKeys.emplace_back(intensity, rgba);
        }
    }
    in.close();

    if (mappingKeys.empty()) {
        return;
    }

    if (mappingKeys.front().position > lower) {
        mappingKeys.insert(mappingKeys.begin(), {lower, mappingKeys.front().color});
    }

    if (mappingKeys.back().position < upper) {
        mappingKeys.emplace_back(upper, mappingKeys.back().color);
    }

    // allocate new float array with zeros
    float* transferFunction = new float[width * 4];
    for (int i = 0; i < 4 * width; i++) {
        transferFunction[i] = 0.f;
    }

    const size_t lowerIndex = static_cast<size_t>(
        std::floor(lower * static_cast<float>(width - 1))
    );
    const size_t upperIndex = static_cast<size_t>(
        std::floor(upper * static_cast<float>(width - 1))
    );

    auto prevKey = mappingKeys.begin();
    auto currentKey = prevKey + 1;
    auto lastKey = mappingKeys.end() -1;

    for (size_t i = lowerIndex; i <= upperIndex; i++) {
        const float fpos = static_cast<float>(i) / static_cast<float>(width-1);
        if (fpos > currentKey->position) {
            prevKey = currentKey;
            currentKey++;
            if (currentKey == mappingKeys.end()) {
                currentKey = lastKey;
            }
        }

        const float dist = fpos - prevKey->position;
        const float weight = dist / (currentKey->position - prevKey->position);

        for (int channel = 0; channel < 4; ++channel) {
            const size_t position = 4 * i + channel;
            // Interpolate linearly between prev and next mapping key
            float value = (prevKey->color[channel] * (1.f - weight) +
                          currentKey->color[channel] * weight) / 255.f;
            if (channel < 3) {
                // Premultiply with alpha
                value *= (prevKey->color[3] * (1.f - weight) +
                         currentKey->color[3] * weight) / 255.f;
            }
            transferFunction[position] = value;
        }
    }

    // no need to deallocate transferFunction. Ownership is transferred to the Texture.

    _texture = std::make_unique<ghoul::opengl::Texture>(
        transferFunction,
        glm::size3_t(width, 1, 1),
        GL_TEXTURE_1D,
        ghoul::opengl::Texture::Format::RGBA,
        GL_RGBA,
        GL_FLOAT,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge
    );
}

void TransferFunction::setTextureFromImage() {
    _texture = ghoul::io::TextureReader::ref().loadTexture(_filepath.string(), 1);
    _texture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToEdge);
}

glm::vec4 TransferFunction::sample(size_t offset) {
    if (!_texture) {
        return glm::vec4(0.f);
    }

    const int nPixels = _texture->width();

    // Clamp to range.
    if (offset >= static_cast<size_t>(nPixels)) {
        offset = nPixels - 1;
    }

    return _texture->texelAsFloat(static_cast<unsigned int>(offset));
}

size_t TransferFunction::width() {
    update();
    return _texture->width();
}

void TransferFunction::bind() {
    update();
    _texture->bind();
}

} // namespace openspace
