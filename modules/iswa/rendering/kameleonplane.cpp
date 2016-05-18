// /*****************************************************************************************
//  *                                                                                       *
//  * OpenSpace                                                                             *
//  *                                                                                       *
//  * Copyright (c) 2014-2016                                                               *
//  *                                                                                       *
//  * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
//  * software and associated documentation files (the "Software"), to deal in the Software *
//  * without restriction, including without limitation the rights to use, copy, modify,    *
//  * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
//  * permit persons to whom the Software is furnished to do so, subject to the following   *
//  * conditions:                                                                           *
//  *                                                                                       *
//  * The above copyright notice and this permission notice shall be included in all copies *
//  * or substantial portions of the Software.                                              *
//  *                                                                                       *
//  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
//  * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
//  * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
//  * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
//  * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
//  * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
//  ****************************************************************************************/

#include <modules/iswa/rendering/kameleonplane.h>
#include <ghoul/filesystem/filesystem>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <fstream>


namespace {
    const std::string _loggerCat = "KameleonPlane";
}

namespace openspace {

KameleonPlane::KameleonPlane(const ghoul::Dictionary& dictionary)
    :CygnetPlane(dictionary)
    ,_useLog("useLog","Use Logarithm", false)
    ,_useHistogram("useHistogram", "Use Histogram", true)
    ,_normValues("normValues", "Normalize Values", glm::vec2(1.0,1.0), glm::vec2(0), glm::vec2(5.0))
    ,_backgroundValues("backgroundValues", "Background Values", glm::vec2(0.0), glm::vec2(0), glm::vec2(1.0))
    ,_transferFunctionsFile("transferfunctions", "Transfer Functions", "${SCENE}/iswa/tfs/hot.tf")
    ,_dataOptions("dataOptions", "Data Options")
{       
    std::string name;
    dictionary.getValue("Name", name);
    setName(name);

    registerProperties();

    addProperty(_useLog);
    addProperty(_useHistogram);
    addProperty(_normValues);
    addProperty(_backgroundValues);
    addProperty(_transferFunctionsFile);
    addProperty(_dataOptions);

    if(_data->groupName.empty()){
        OsEng.gui()._iswa.registerProperty(&_useLog);
        OsEng.gui()._iswa.registerProperty(&_useHistogram);
        OsEng.gui()._iswa.registerProperty(&_normValues);
        OsEng.gui()._iswa.registerProperty(&_backgroundValues);
        OsEng.gui()._iswa.registerProperty(&_transferFunctionsFile);
        OsEng.gui()._iswa.registerProperty(&_dataOptions);
    }

    setTransferFunctions(_transferFunctionsFile.value());

    _dataProcessor = std::make_shared<DataProcessor>(
        _useLog.value(),
        _useHistogram.value(),
        _normValues
    );

    _dataOptions.onChange([this](){updateTexture();});

    dictionary.getValue("kwPath", _kwPath);

    std::string axis;
    dictionary.getValue("axisCut", axis);

    if(axis == "x"){
        _data->scale.x = 0;
    }else if(axis == "y"){
        _data->scale.y = 0;
    }else{
        _data->scale.z = 0;
    }

    _dimensions = glm::size3_t(500,500,1);
}

KameleonPlane::~KameleonPlane(){
    _kw = nullptr;
}


// bool KameleonPlane::initialize(){
//  _textures.push_back(nullptr);

//  std::cout << "initialize kameleonplane" << std::endl;
//  // std::string kwPath;
//  // dictionary.getValue("KW", _kw);

//     createPlane();

//     if (_shader == nullptr) {
//     // DatePlane Program
//     RenderEngine& renderEngine = OsEng.renderEngine();
//     _shader = renderEngine.buildRenderProgram("PlaneProgram",
//         "${MODULE_ISWA}/shaders/dataplane_vs.glsl",
//         "${MODULE_ISWA}/shaders/dataplane_fs.glsl"
//         );
//     if (!_shader)
//         return false;
//     }



//     loadTexture();P

//     return isReady();
// }

// bool KameleonPlane::deinitialize(){
//     unregisterProperties();
//     destroyPlane();
//     destroyShader();
    
//  _kw = nullptr;
//  _memorybuffer = "";
    
//  return true;
// }


bool KameleonPlane::loadTexture() {    
    std::cout << "load kameleonplane texture" << std::endl;
    ghoul::opengl::Texture::FilterMode filtermode = ghoul::opengl::Texture::FilterMode::Linear;
    ghoul::opengl::Texture::WrappingMode wrappingmode = ghoul::opengl::Texture::WrappingMode::ClampToEdge;

    std::vector<int> selectedOptions = _dataOptions.value();
    auto options = _dataOptions.options();

    for(int selected : selectedOptions){
        if(_dataSlices[selected]){
            if(!_textures[selected]){
                std::unique_ptr<ghoul::opengl::Texture> texture = 
                    std::make_unique<ghoul::opengl::Texture>(_dataSlices[selected], _dimensions, ghoul::opengl::Texture::Format::Red, GL_RED, GL_FLOAT, filtermode, wrappingmode);

                if (!texture){
                    std::cout << "Could not create texture" << std::endl;
                    return false;
                }

                texture->uploadTexture();
                texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
               _textures[selected] = std::move(texture);
            }else{
                // _textures[selected]->setPixelData(values);
                // _textures[selected]->uploadTexture();
            }
        }
    }

    return true;    
}

bool KameleonPlane::updateTexture(){
    if(!_kw){
        _kw = std::make_shared<KameleonWrapper>(absPath(_kwPath));
    }

    if(!_dataOptions.options().size()){
        std::vector<std::string> options = _kw->getVariables();
        int numOptions = 0;

        for(std::string option : options){
            if(option.size() < 4 && option != "x" && option != "y" && option != "z"){
                _dataOptions.addOption({numOptions, option});
                _dataSlices.push_back(nullptr);
                _textures.push_back(nullptr);
                numOptions++;
            }
        }
        _dataOptions.setValue(std::vector<int>(1,0));

        if(!_data->groupName.empty())
            IswaManager::ref().registerOptionsToGroup(_data->groupName, _dataOptions.options());
    }

    std::vector<int> selectedOptions = _dataOptions.value();
    auto options = _dataOptions.options();
    float zSlice = 0.5f;

    for(int selected : selectedOptions){
        if(!_dataSlices[selected]){
            std::cout << options[selected].description << std::endl;
            _dataSlices[selected] = _kw->getUniformSliceValues(options[selected].description, _dimensions, zSlice);
        }
    }

    loadTexture();

    return true;
}

bool KameleonPlane::readyToRender(){
    return (_textures[0] != nullptr && !_transferFunctions.empty());
}

void KameleonPlane::setUniformAndTextures(){
    std::vector<int> selectedOptions = _dataOptions.value();
    int activeTextures = selectedOptions.size();
    int activeTransferfunctions = _transferFunctions.size();

    ghoul::opengl::TextureUnit txUnits[10];
    int j = 0;
    for(int option : selectedOptions){
        if(_textures[option]){
            txUnits[j].activate();
            _textures[option]->bind();
            _shader->setUniform(
                "textures[" + std::to_string(j) + "]",
                txUnits[j]
            );

            j++;
        }
    }

    if(activeTextures > 0){
        if(selectedOptions.back()>=activeTransferfunctions)
            activeTransferfunctions = 1;
    }

    ghoul::opengl::TextureUnit tfUnits[10];
    j = 0;

    if((activeTransferfunctions == 1)){
        tfUnits[0].activate();
        _transferFunctions[0]->bind();
        _shader->setUniform(
            "transferFunctions[0]",
            tfUnits[0]
        );
    }else{
        for(int option : selectedOptions){
            // std::cout << option << std::endl;
            // if(option >= activeTransferfunctions){
            //     // LWARNING("No transfer function for this value.");
            //     break;
            // }

            if(_transferFunctions[option]){
                tfUnits[j].activate();
                _transferFunctions[option]->bind();
                _shader->setUniform(
                "transferFunctions[" + std::to_string(j) + "]",
                tfUnits[j]
                );

                j++;
            }
        }
    }

    _shader->setUniform("numTextures", activeTextures);
    _shader->setUniform("numTransferFunctions", activeTransferfunctions);
    _shader->setUniform("backgroundValues", _backgroundValues.value());
}

bool KameleonPlane::createShader(){
    if (_shader == nullptr) {
        // DatePlane Program
        RenderEngine& renderEngine = OsEng.renderEngine();
        _shader = renderEngine.buildRenderProgram("DataPlaneProgram",
            "${MODULE_ISWA}/shaders/dataplane_vs.glsl",
            "${MODULE_ISWA}/shaders/dataplane_fs.glsl"
            );
        if (!_shader) return false;
    }
}

void KameleonPlane::setTransferFunctions(std::string tfPath){
    std::string line;
    std::ifstream tfFile(absPath(tfPath));

    std::vector<std::shared_ptr<TransferFunction>> tfs;

    if(tfFile.is_open()){
        while(getline(tfFile, line)){
            std::shared_ptr<TransferFunction> tf = std::make_shared<TransferFunction>(absPath(line));
            if(tf){
                tfs.push_back(tf);
            }
        }
    }

    if(!tfs.empty()){
        _transferFunctions.clear();
        _transferFunctions = tfs;
    }

}

}// namespace openspace