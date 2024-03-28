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

#include <modules/iswa/rendering/datacygnet.h>

#include <modules/iswa/rendering/iswadatagroup.h>
#include <modules/iswa/util/dataprocessor.h>
#include <modules/iswa/util/iswamanager.h>
#include <openspace/rendering/transferfunction.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <fstream>

namespace {
    constexpr std::string_view _loggerCat = "DataCygnet";
    constexpr int MaxTextures = 6;

    constexpr openspace::properties::Property::PropertyInfo DataOptionsInfo = {
        "DataOptions",
        "Data Options",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo UseLogInfo = {
        "UseLog",
        "Use Logarithm",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UseHistogramInfo = {
        "UseHistogram",
        "Auto Contrast",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AutoFilterInfo = {
        "AutoFilter",
        "Auto Filter",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo NormalizeValuesInfo = {
        "NormValues",
        "Normalize Values",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo BackgroundInfo = {
        "BackgroundValues",
        "Background Values",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo TransferFunctionsFile = {
        "Transferfunctions",
        "Transfer Functions",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::Developer
    };
} // namespace

namespace openspace {

DataCygnet::DataCygnet(const ghoul::Dictionary& dictionary)
    : IswaCygnet(dictionary)
    , _dataOptions(DataOptionsInfo)
    , _transferFunctionsFile(TransferFunctionsFile, "${SCENE}/iswa/tfs/default.tf")
    , _backgroundValues(BackgroundInfo, glm::vec2(0.f), glm::vec2(0.f), glm::vec2(1.f))
    , _normValues(NormalizeValuesInfo, glm::vec2(1.f), glm::vec2(0.f), glm::vec2(5.f))
    , _useLog(UseLogInfo, false)
    , _useHistogram(UseHistogramInfo, false)
    , _autoFilter(AutoFilterInfo, true)
{
    addProperty(_dataOptions);
    addProperty(_useLog);
    addProperty(_useHistogram);
    addProperty(_autoFilter);
    addProperty(_normValues);
    addProperty(_backgroundValues);
    addProperty(_transferFunctionsFile);
    registerProperties();
}

DataCygnet::~DataCygnet() {}

bool DataCygnet::updateTexture() {
    const std::vector<float*>& data = textureData();

    if (data.empty()) {
        return false;
    }

    bool texturesReady = false;
    const std::set<std::string>& selectedOptions = _dataOptions;
    const std::vector<std::string>& options = _dataOptions.options();
    std::vector<int> selectedOptionsIndices;
    for (const std::string& option : selectedOptions) {
        auto it = std::find(options.begin(), options.end(), option);
        ghoul_assert(it != options.end(), "Selected option must be in all options");
        int idx = static_cast<int>(std::distance(options.begin(), it));
        selectedOptionsIndices.push_back(idx);
    }

    for (int option : selectedOptionsIndices) {
        float* values = data[option];
        if (!values) {
            continue;
        }

        if (!_textures[option]) {
            auto texture = std::make_unique<ghoul::opengl::Texture>(
                values,
                _textureDimensions,
                GL_TEXTURE_2D,
                ghoul::opengl::Texture::Format::Red,
                GL_RED,
                GL_FLOAT,
                ghoul::opengl::Texture::FilterMode::Linear,
                ghoul::opengl::Texture::WrappingMode::ClampToEdge
            );

            texture->uploadTexture();
            texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
            _textures[option] = std::move(texture);
        }
        else {
            _textures[option]->setPixelData(values);
            _textures[option]->uploadTexture();
        }
        texturesReady = true;
    }
    return texturesReady;
}

bool DataCygnet::downloadTextureResource(double timestamp) {
    if (_futureObject.valid()) {
        return false;
    }

    std::future<DownloadManager::MemoryFile> future = IswaManager::ref().fetchDataCygnet(
        _data.id,
        timestamp
    );

    if (future.valid()) {
        _futureObject = std::move(future);
        return true;
    }

    return false;
}

bool DataCygnet::updateTextureResource() {
    DownloadManager::MemoryFile dataFile = _futureObject.get();

    if (dataFile.corrupted) {
        return false;
    }

    _dataBuffer = std::string(dataFile.buffer, dataFile.size);
    delete[] dataFile.buffer;

    return true;
}

bool DataCygnet::readyToRender() const {
    return (!_textures.empty() && !_transferFunctions.empty());
}

/**
 * Set both transfer function textures and data textures in same function so that they
 * bind to the right texture units. If separate in to two functions a list of
 * ghoul::TextureUnit needs to be passed as an argument to both.
 */
void DataCygnet::setTextureUniforms() {
    const std::set<std::string>& selectedOptions = _dataOptions;
    const std::vector<std::string>& options = _dataOptions.options();
    std::vector<int> selectedOptionsIndices;
    for (const std::string& option : selectedOptions) {
        auto it = std::find(options.begin(), options.end(), option);
        ghoul_assert(it != options.end(), "Selected option must be in all options");
        int idx = static_cast<int>(std::distance(options.begin(), it));
        selectedOptionsIndices.push_back(idx);
    }

    int activeTextures = std::min(static_cast<int>(selectedOptions.size()), MaxTextures);
    int activeTransferfunctions = std::min(
        static_cast<int>(_transferFunctions.size()),
        MaxTextures
    );

    // Set Textures
    ghoul::opengl::TextureUnit txUnits[MaxTextures];
    int j = 0;
    for (int option : selectedOptionsIndices) {
        if (_textures[option]) {
            txUnits[j].activate();
            _textures[option]->bind();
            _shader->setUniform("textures[" + std::to_string(j) + "]", txUnits[j]);

            j++;
            if (j >= MaxTextures) {
                break;
            }
        }
    }

    if (activeTextures > 0 &&
        selectedOptionsIndices.back() >= static_cast<int>(_transferFunctions.size()))
        {
            activeTransferfunctions = 1;
        }

    // This array + txUnits will use up 12 Texture Units, which is alot
    ghoul::opengl::TextureUnit tfUnits[MaxTextures];
    j = 0;

    if (activeTransferfunctions == 1) {
        tfUnits[0].activate();
        _transferFunctions[0].bind();
        _shader->setUniform("transferFunctions[0]", tfUnits[0]);
    }
    else {
        for (int option : selectedOptionsIndices) {
            if (static_cast<int>(_transferFunctions.size()) >= option) {
                tfUnits[j].activate();
                _transferFunctions[option].bind();
                _shader->setUniform(
                    "transferFunctions[" + std::to_string(j) + "]",
                    tfUnits[j]
                );

                j++;
                if (j >= MaxTextures) {
                    break;
                }
            }
        }
    }

    _shader->setUniform("numTransferFunctions", activeTransferfunctions);
    _shader->setUniform("numTextures", activeTextures);
}

void DataCygnet::readTransferFunctions(std::string tfPath) {
    std::ifstream tfFile(absPath(std::move(tfPath)));

    std::vector<TransferFunction> tfs;

    if (tfFile.is_open()) {
        std::string line;
        while (ghoul::getline(tfFile, line)) {
            tfs.emplace_back(absPath(line).string());
        }

        tfFile.close();
    }

    if (!tfs.empty()) {
        _transferFunctions = std::move(tfs);
    }
}

void DataCygnet::fillOptions(const std::string& source) {
    std::vector<std::string> options = _dataProcessor->readMetadata(
        source,
        _textureDimensions
    );

    for (int i = 0; i < static_cast<int>(options.size()); i++) {
        _dataOptions.addOption(options[i]);
        _textures.push_back(nullptr);
    }

    if (_group) {
        IswaDataGroup* g = dynamic_cast<IswaDataGroup*>(_group);

        g->registerOptions(_dataOptions.options());
        _dataOptions.setValue(g->dataOptionsValue());
    }
    else {
        _dataOptions.setValue({ options.front() });
    }
}

void DataCygnet::setPropertyCallbacks() {
    _normValues.onChange([this]() {
        _dataProcessor->normValues(_normValues);
        updateTexture();
    });

    _useLog.onChange([this]() {
        _dataProcessor->useLog(_useLog);
        updateTexture();
    });

    _useHistogram.onChange([this]() {
        _dataProcessor->useHistogram(_useHistogram);
        updateTexture();
        if (_autoFilter) {
            _backgroundValues = _dataProcessor->filterValues();
        }
    });

    _dataOptions.onChange([this]() {
        if (_dataOptions.value().size() > MaxTextures) {
            LWARNING("Too many options chosen, max is " + std::to_string(MaxTextures));
        }
        updateTexture();
    });

    _transferFunctionsFile.onChange([this]() {
        readTransferFunctions(_transferFunctionsFile);
    });
}

void DataCygnet::subscribeToGroup() {
    ghoul::Event<ghoul::Dictionary>& groupEvent = _group->groupEvent();

    groupEvent.subscribe(
        identifier(),
        "dataOptionsChanged",
        [this](const ghoul::Dictionary& dict) {
            LDEBUG(identifier() + " Event dataOptionsChanged");
            if (dict.hasValue<std::vector<int>>("dataOptions")) {
                std::vector<int> idx = dict.value<std::vector<int>>("dataOptions");
                std::vector<std::string> opts = _dataOptions.options();
                std::set<std::string> selected;
                for (int i : idx) {
                    selected.insert(opts[i]);
                }
                _dataOptions = selected;
            }
        }
    );

    groupEvent.subscribe(
        identifier(),
        "normValuesChanged",
        [this](const ghoul::Dictionary& dict) {
            LDEBUG(identifier() + " Event normValuesChanged");
            if (dict.hasValue<glm::dvec2>("normValues")) {
                _normValues = dict.value<glm::dvec2>("normValues");
            }
        }
    );

    groupEvent.subscribe(
        identifier(),
        "backgroundValuesChanged",
        [this](const ghoul::Dictionary& dict) {
            LDEBUG(identifier() + " Event backgroundValuesChanged");
            if (dict.hasValue<glm::dvec2>("backgroundValues")) {
                _backgroundValues = dict.value<glm::dvec2>("backgroundValues");
            }
        }
    );

    groupEvent.subscribe(
        identifier(),
        "transferFunctionsChanged",
        [this](const ghoul::Dictionary& dict) {
            LDEBUG(identifier() + " Event transferFunctionsChanged");
            _transferFunctionsFile = dict.value<std::string>("transferFunctions");
        }
    );

    groupEvent.subscribe(
        identifier(),
        "useLogChanged",
        [this](const ghoul::Dictionary& dict) {
            LDEBUG(identifier() + " Event useLogChanged");
            _useLog = dict.value<bool>("useLog");
        }
    );

    groupEvent.subscribe(
        identifier(),
        "useHistogramChanged",
        [this](const ghoul::Dictionary& dict) {
            LDEBUG(identifier() + " Event useHistogramChanged");
            _useHistogram = dict.value<bool>("useHistogram");
        }
    );

    groupEvent.subscribe(
        identifier(),
        "autoFilterChanged",
        [this](const ghoul::Dictionary& dict) {
            LDEBUG(identifier() + " Event autoFilterChanged");
            _autoFilter = dict.value<bool>("autoFilter");
        }
    );

    groupEvent.subscribe(
        identifier(),
        "updateGroup",
        [this](const ghoul::Dictionary&) {
            LDEBUG(identifier() + " Event updateGroup");
            if (_autoFilter) {
                _backgroundValues = _dataProcessor->filterValues();
            }
            updateTexture();
        }
    );
}

} //namespace openspace
