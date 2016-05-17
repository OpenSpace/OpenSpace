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
	,_backgroundValues("backgroundValues", "Background Values", glm::vec2(0.0), glm::vec2(0), glm::vec2(1.0))
    ,_transferFunctionsFile("transferfunctions", "Transfer Functions", "${SCENE}/iswa/tfs/hot.tf")
{		
	std::string name;
    dictionary.getValue("Name", name);
    setName(name);

	registerProperties();


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
	_kw = std::make_shared<KameleonWrapper>(absPath(_kwPath));
		KameleonWrapper::Model model = _kw->model();
	if(	model == KameleonWrapper::Model::BATSRUS)
		_var = "p";
	else
		_var = "rho";

	_dimensions = glm::size3_t(500,500,1);
	float zSlice = 0.5f;
	_dataSlice = _kw->getUniformSliceValues(std::string(_var), _dimensions, zSlice);
}

KameleonPlane::~KameleonPlane(){}


// bool KameleonPlane::initialize(){
// 	_textures.push_back(nullptr);

// 	std::cout << "initialize kameleonplane" << std::endl;
// 	// std::string kwPath;
// 	// dictionary.getValue("KW", _kw);

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



//     loadTexture();

//     return isReady();
// }

// bool KameleonPlane::deinitialize(){
//     unregisterProperties();
//     destroyPlane();
//     destroyShader();
	
// 	_kw = nullptr;
// 	_memorybuffer = "";
	
// 	return true;
// }


bool KameleonPlane::loadTexture() {
		std::cout << "load kameleonplane texture" << std::endl;
		ghoul::opengl::Texture::FilterMode filtermode = ghoul::opengl::Texture::FilterMode::Linear;
		ghoul::opengl::Texture::WrappingMode wrappingmode = ghoul::opengl::Texture::WrappingMode::ClampToEdge;
		std::unique_ptr<ghoul::opengl::Texture> texture = 
			std::make_unique<ghoul::opengl::Texture>(_dataSlice, _dimensions, ghoul::opengl::Texture::Format::Red, GL_RED, GL_FLOAT, filtermode, wrappingmode);

		if (!texture)
			return false;
			// LDEBUG("Loaded texture from '" << absPath(_path) << "'");

		texture->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
		texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

       _textures[0] = std::move(texture);
		
		return true;	
}

bool KameleonPlane::updateTexture(){
	if(!_textures[0]){
		loadTexture();
	}
	return true;
}

bool KameleonPlane::readyToRender(){
    return (_textures[0] != nullptr);
}

void KameleonPlane::setUniformAndTextures(){
    ghoul::opengl::TextureUnit unit;

    ghoul::opengl::TextureUnit txUnits[10];
    txUnits[0].activate();
    _textures[0]->bind();
    _shader->setUniform("textures[0]", txUnits[0]);

    ghoul::opengl::TextureUnit tfUnits[10];
    tfUnits[0].activate();
    _transferFunctions[0]->bind();
    _shader->setUniform(
       	"transferFunctions[0]",
        tfUnits[0]
    );

    _shader->setUniform("numTextures", 1);
    _shader->setUniform("numTransferFunctions", 1);
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