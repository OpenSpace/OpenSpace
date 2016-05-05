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

#include <modules/iswa/rendering/cygnetsphere.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <modules/base/rendering/planetgeometry.h>
#include <modules/base/rendering/simplespheregeometry.h>

namespace openspace{

CygnetSphere::CygnetSphere(const ghoul::Dictionary& dictionary)
    :ISWACygnet(dictionary)
    ,_futureObject(nullptr)
    ,_geometry(nullptr)
{
	//read segments from dictronary
	_segments = 20;
}

CygnetSphere::~CygnetSphere(){}

bool CygnetSphere::isReady() const{
	bool ready = true;
	if(!_shader)
		ready &= false;
	if(!_geometry)
		ready &= false;

    return ready;
}

void CygnetSphere::render(const RenderData& data){
	if ((!_textures.empty()) && (_textures[0] != nullptr)) return;
	if (!_geometry) return;

	psc position = data.position;
    glm::mat4 transform = glm::mat4(1.0);

    glm::mat4 rot = glm::mat4(1.0);
    for (int i = 0; i < 3; i++){
        for (int j = 0; j < 3; j++){
            transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
        }
    }

    position += transform*glm::vec4(_data->spatialScale.x*_data->offset, _data->spatialScale.w);
    
    _shader->activate();
    glEnable(GL_ALPHA_TEST);
    glDisable(GL_CULL_FACE);


    _shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _shader->setUniform("ModelTransform", transform);

    setPscUniforms(*_shader.get(), data.camera, position);

    ghoul::opengl::TextureUnit unit;

    unit.activate();
    _textures[0]->bind();
    _shader->setUniform("texture1", unit);

    _geometry->render();
    // setUniforms();

    // glBindVertexArray(_quad);
    // glDrawArrays(GL_TRIANGLES, 0, 6);
    glEnable(GL_CULL_FACE);
    _shader->deactivate();
}

void CygnetSphere::update(const UpdateData& data){
	_openSpaceTime = Time::ref().currentTime();
    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch());


     _stateMatrix = ISWAManager::ref().getTransform(_data->frame, "GALACTIC", _openSpaceTime);
    // glm::dmat3 spiceMatrix    = SpiceManager::ref().positionTransformMatrix("J2000", "GALACTIC", _openSpaceTime);
     // = spiceMatrix*kameleonMatrix;

    bool timeToUpdate = (fabs(_openSpaceTime-_lastUpdateOpenSpaceTime) >= _data->updateTime &&
                        (_realTime.count()-_lastUpdateRealTime.count()) > _minRealTimeUpdateInterval);
    if( _data->updateTime != 0 && (Time::ref().timeJumped() || timeToUpdate )){
        updateTexture();

        _lastUpdateRealTime = _realTime;
        _lastUpdateOpenSpaceTime = _openSpaceTime;
    }

    if(_futureObject && _futureObject->isFinished){
        if(loadTexture())
            _futureObject = nullptr;
    }

    if(!_transferFunctions.empty())
        for(auto tf : _transferFunctions)
            tf->update();
}

void CygnetSphere::createSphere(){
	glm::vec4 radius(6.371f, 6.371f, 6.371f, 6); //use scale in _data

	ghoul::Dictionary geometryDictionary;
	geometryDictionary.setValue(SceneGraphNode::KeyName, name());
	geometryDictionary.setValue("Radius", radius);
	geometryDictionary.setValue("Segments", _segments);

	_geometry = std::make_shared<planetgeometry::SimpleSphereGeometry>(geometryDictionary);
}

void CygnetSphere::destroySphere(){
	if(_geometry)
        _geometry->deinitialize();

	_geometry = nullptr;
}

bool CygnetSphere::createShader(){
    if (_shader == nullptr) {
    // Plane Program
    RenderEngine& renderEngine = OsEng.renderEngine();
    _shader = renderEngine.buildRenderProgram(
            "pscstandard",
            "${MODULE_BASE}/shaders/pscstandard_vs.glsl",
            "${MODULE_BASE}/shaders/pscstandard_fs.glsl");
    if (!_shader)
        return false;
    }
    return true;
}

void CygnetSphere::destroyShader(){
    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }
}

} //namespace openspace