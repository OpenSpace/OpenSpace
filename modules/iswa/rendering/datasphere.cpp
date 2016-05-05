/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2015                                                               *
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

#include <modules/iswa/rendering/datasphere.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem.h>
#include <modules/base/rendering/planetgeometry.h>

namespace openspace {

DataSphere::DataSphere(const ghoul::Dictionary& dictionary)
    :CygnetSphere(dictionary)
{
	std::string name;
    dictionary.getValue("Name", name);
    setName(name);

    registerProperties();
}

DataSphere::~DataSphere(){}


bool DataSphere::loadTexture(){
	_textures[0] = nullptr;
	std::string texturepath = "${OPENSPACE_DATA}/scene/mars/textures/mars.jpg";
	 
	auto texture = ghoul::io::TextureReader::ref().loadTexture(absPath(texturepath));
	 if(texture){
	 	texture->uploadTexture();
	 	texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
	 	_textures[0] = std::move(texture);
	 }

    return true;
}

bool DataSphere::updateTexture(){
	if(_textures.empty())
        _textures.push_back(nullptr);

	loadTexture();
    return true; 
}


bool DataSphere::readyToRender(){
	return (isReady() && ((!_textures.empty()) && (_textures[0] != nullptr)));
}


bool DataSphere::setUniformAndTextures(){
    _shader->setUniform("transparency",0.5f);
    
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _textures[0]->bind();
    _shader->setUniform("texture1", unit);
}


bool DataSphere::createShader(){
	if (_shader == nullptr) {
    // Plane Program
    RenderEngine& renderEngine = OsEng.renderEngine();
    _shader = renderEngine.buildRenderProgram(
            "DataSphereProgram",
            "${MODULE_ISWA}/shaders/datasphere_vs.glsl",
            "${MODULE_ISWA}/shaders/datasphere_fs.glsl");
    if (!_shader)
        return false;
    }
    return true;
}


} //namespace openspace