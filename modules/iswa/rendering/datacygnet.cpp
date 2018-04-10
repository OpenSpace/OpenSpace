/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <fstream>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>
#include <modules/iswa/util/dataprocessortext.h>

#include <ghoul/opengl/programobject.h>

namespace {
    constexpr const char* _loggerCat = "DataCygnet";

    static const openspace::properties::Property::PropertyInfo DataOptionsInfo = {
        "DataOptions",
        "Data Options",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo UseLogInfo = {
        "UseLog",
        "Use Logarithm",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo UseHistogramInfo = {
        "UseHistogram",
        "Auto Contrast",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo AutoFilterInfo = {
        "AutoFilter",
        "Auto Filter",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo NormalizeValuesInfo = {
        "NormValues",
        "Normalize Values",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo BackgroundInfo = {
        "BackgroundValues",
        "Background Values",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo TransferFunctionsFile = {
        "Transferfunctions",
        "Transfer Functions",
        "" // @TODO Missing documentation
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

    , _dataProcessor(nullptr)
    //FOR TESTING
    , _numOfBenchmarks(0)
    , _avgBenchmarkTime(0.0f)
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
    std::vector<float*> data = textureData();

    if (data.empty()) {
        return false;
    }

    bool texturesReady = false;
    std::vector<int> selectedOptions = _dataOptions.value();

    for (int option : selectedOptions) {
        float* values = data[option];
        if (!values) {
            continue;
        }

        if (!_textures[option]) {
            auto texture =  std::make_unique<ghoul::opengl::Texture>(
                values,
                _textureDimensions,
                ghoul::opengl::Texture::Format::Red,
                GL_RED,
                GL_FLOAT,
                ghoul::opengl::Texture::FilterMode::Linear,
                ghoul::opengl::Texture::WrappingMode::ClampToEdge
            );

            if (texture) {
                texture->uploadTexture();
                texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
                _textures[option] = std::move(texture);
            }
        } else {
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
        _data->id,
        timestamp
    );

    if (future.valid()) {
        _futureObject = std::move(future);
        return true;
    }

    return false;
}

bool DataCygnet::updateTextureResource(){
    DownloadManager::MemoryFile dataFile = _futureObject.get();

     if (dataFile.corrupted) {
        return false;
    }

    _dataBuffer = std::string(dataFile.buffer, dataFile.size);
    delete[] dataFile.buffer;

    return true;
}

bool DataCygnet::readyToRender() const{
    return (!_textures.empty() && !_transferFunctions.empty());
}

/**
 * Set both transfer function textures and data textures in same function so that they
 * bind to the right texture units. If separate in to two functions a list of
 * ghoul::TextureUnit needs to be passed as an argument to both.
 */
void DataCygnet::setTextureUniforms(){
    std::vector<int> selectedOptions = _dataOptions.value();
    int activeTextures = std::min(static_cast<int>(selectedOptions.size()), MAX_TEXTURES);
    int activeTransferfunctions = std::min(
        static_cast<int>(_transferFunctions.size()),
        MAX_TEXTURES
    );

    // Set Textures
    ghoul::opengl::TextureUnit txUnits[MAX_TEXTURES];
    int j = 0;
    for (int option : selectedOptions) {
        if (_textures[option]) {
            txUnits[j].activate();
            _textures[option]->bind();
            _shader->setUniform(
                "textures[" + std::to_string(j) + "]",
                txUnits[j]
            );

            j++;
            if (j >= MAX_TEXTURES) {
                break;
            }
        }
    }

    if (activeTextures > 0 &&
        selectedOptions.back() >= static_cast<int>(_transferFunctions.size()))
        {
            activeTransferfunctions = 1;
        }

    ghoul::opengl::TextureUnit tfUnits[MAX_TEXTURES];
    j = 0;

    if (activeTransferfunctions == 1) {
        tfUnits[0].activate();
        _transferFunctions[0]->bind();
        _shader->setUniform(
            "transferFunctions[0]",
            tfUnits[0]
        );
    } else {
        for (int option : selectedOptions) {
            if (_transferFunctions[option]) {
                tfUnits[j].activate();
                _transferFunctions[option]->bind();
                _shader->setUniform(
                "transferFunctions[" + std::to_string(j) + "]",
                tfUnits[j]
                );

                j++;
                if(j >= MAX_TEXTURES) break;
            }
        }
    }

    _shader->setUniform("numTransferFunctions", activeTransferfunctions);
    _shader->setUniform("numTextures", activeTextures);
}

void DataCygnet::readTransferFunctions(std::string tfPath){
    std::string line;
    std::ifstream tfFile(absPath(tfPath));

    std::vector<std::shared_ptr<TransferFunction>> tfs;

    if (tfFile.is_open()) {
        while (getline(tfFile, line)) {
            std::shared_ptr<TransferFunction> tf = std::make_shared<TransferFunction>(
                absPath(line)
            );
            if (tf) {
                tfs.push_back(tf);
            }
        }

        tfFile.close();
    }

    if (!tfs.empty()) {
        _transferFunctions.clear();
        _transferFunctions = tfs;
    }
}

void DataCygnet::fillOptions(std::string& source) {
    std::vector<std::string> options = _dataProcessor->readMetadata(
        source,
        _textureDimensions
    );

    for (int i = 0; i < static_cast<int>(options.size()); i++) {
        _dataOptions.addOption({i, options[i]});
        _textures.push_back(nullptr);
    }

    if (_group) {
        std::dynamic_pointer_cast<IswaDataGroup>(_group)->registerOptions(
            _dataOptions.options()
        );
        _dataOptions.setValue(
            std::dynamic_pointer_cast<IswaDataGroup>(_group)->dataOptionsValue()
        );
    } else {
        _dataOptions.setValue(std::vector<int>(1,0));
    }
}

void DataCygnet::setPropertyCallbacks() {
    _normValues.onChange([this]() {
        _dataProcessor->normValues(_normValues.value());
        updateTexture();
    });

    _useLog.onChange([this]() {
        _dataProcessor->useLog(_useLog.value());
        updateTexture();
    });

    _useHistogram.onChange([this]() {
        _dataProcessor->useHistogram(_useHistogram.value());
        updateTexture();
        if (_autoFilter.value()) {
            _backgroundValues.setValue(_dataProcessor->filterValues());
        }
    });

    _dataOptions.onChange([this]() {
        if (_dataOptions.value().size() > MAX_TEXTURES) {
            LWARNING("Too many options chosen, max is " + std::to_string(MAX_TEXTURES));
        }
        updateTexture();
    });

    _transferFunctionsFile.onChange([this]() {
        readTransferFunctions(_transferFunctionsFile.value());
    });
}

void DataCygnet::subscribeToGroup() {
    auto groupEvent = _group->groupEvent();

    groupEvent->subscribe(
        identifier(),
        "dataOptionsChanged",
        [&](ghoul::Dictionary dict) {
            LDEBUG(identifier() + " Event dataOptionsChanged");
            std::vector<int> values;
            bool success = dict.getValue<std::vector<int> >("dataOptions", values);
            if (success) {
                _dataOptions.setValue(values);
            }
        }
    );

    groupEvent->subscribe(
        identifier(),
        "normValuesChanged",
        [&](ghoul::Dictionary dict) {
            LDEBUG(identifier() + " Event normValuesChanged");
            glm::vec2 values;
            bool success = dict.getValue("normValues", values);
            if (success) {
                _normValues.setValue(values);
            }
        }
    );

    groupEvent->subscribe(
        identifier(),
        "backgroundValuesChanged",
        [&](ghoul::Dictionary dict) {
            LDEBUG(identifier() + " Event backgroundValuesChanged");
            glm::vec2 values;
            bool success = dict.getValue("backgroundValues", values);
            if (success) {
                _backgroundValues.setValue(values);
            }
        }
    );

    groupEvent->subscribe(
        identifier(),
        "transferFunctionsChanged",
        [&](ghoul::Dictionary dict) {
            LDEBUG(identifier() + " Event transferFunctionsChanged");
            _transferFunctionsFile.setValue(dict.value<std::string>("transferFunctions"));
        }
    );

    groupEvent->subscribe(
        identifier(),
        "useLogChanged",
        [&](const ghoul::Dictionary& dict) {
            LDEBUG(identifier() + " Event useLogChanged");
            _useLog.setValue(dict.value<bool>("useLog"));
        }
    );

    groupEvent->subscribe(
        identifier(),
        "useHistogramChanged",
        [&](ghoul::Dictionary dict) {
            LDEBUG(identifier() + " Event useHistogramChanged");
            _useHistogram.setValue(dict.value<bool>("useHistogram"));
        }
    );

    groupEvent->subscribe(
        identifier(),
        "autoFilterChanged",
        [&](ghoul::Dictionary dict) {
            LDEBUG(identifier() + " Event autoFilterChanged");
            _autoFilter.setValue(dict.value<bool>("autoFilter"));
        }
    );

    groupEvent->subscribe(
        identifier(),
        "updateGroup",
        [&](ghoul::Dictionary) {
            LDEBUG(identifier() + " Event updateGroup");
            if (_autoFilter.value()) {
                _backgroundValues.setValue(_dataProcessor->filterValues());
            }
            updateTexture();
        }
    );
}

} //namespace openspace
