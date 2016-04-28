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

#include <modules/iswa/rendering/cygnetplane.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>

namespace openspace{

CygnetPlane::CygnetPlane(const ghoul::Dictionary& dictionary)
    :ISWACygnet(dictionary)
    ,_quad(0)
    ,_vertexPositionBuffer(0)
    ,_futureObject(nullptr)
{}

CygnetPlane::~CygnetPlane(){}

bool CygnetPlane::isReady() const{
    bool ready = true;
    if (!_shader)
        ready &= false;
    return ready;
}

void CygnetPlane::render(const RenderData& data){
    if(!textureReady()) return;
    
    psc position = data.position;
    glm::mat4 transform = glm::mat4(1.0);

    glm::mat4 rotx = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(1, 0, 0));
    glm::mat4 roty = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(0, -1, 0));
    glm::mat4 rotz = glm::rotate(transform, static_cast<float>(M_PI_2), glm::vec3(0, 0, 1));

    glm::mat4 rot = glm::mat4(1.0);
    for (int i = 0; i < 3; i++){
        for (int j = 0; j < 3; j++){
            transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
        }
    }


    // Correct for the small error of x-axis not pointing directly at the sun
    if(_data->frame == "GSM"){

        transform = transform * rotz * roty; //BATSRUS

        glm::vec4 v(1,0,0,1);
        glm::vec3 xVec = glm::vec3(transform*v);
        xVec = glm::normalize(xVec);

        double  lt;
        glm::vec3 sunVec =
        SpiceManager::ref().targetPosition("Sun", "Earth", "GALACTIC", {}, _openSpaceTime, lt);
        sunVec = glm::normalize(sunVec);

        float angle = acos(glm::dot(xVec, sunVec));
        glm::vec3 ref =  glm::cross(xVec, sunVec);

        glm::mat4 rotation = glm::rotate(glm::mat4(1.0f), angle, ref); 
        transform = rotation * transform;
    }

    position += transform*glm::vec4(_data->spatialScale.x*_data->offset, _data->spatialScale.w);
    

    // Activate shader
    _shader->activate();
    glEnable(GL_ALPHA_TEST);
    glDisable(GL_CULL_FACE);


    _shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _shader->setUniform("ModelTransform", transform);

    setPscUniforms(*_shader.get(), data.camera, position);

    setUniforms();

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glEnable(GL_CULL_FACE);
    _shader->deactivate();

}

void CygnetPlane::update(const UpdateData& data){
    _openSpaceTime = Time::ref().currentTime();
    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch());
    _stateMatrix = SpiceManager::ref().positionTransformMatrix("GALACTIC", _data->frame, _openSpaceTime);


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

    if(!_transferFunctions.empty() && _transferFunctions[0])
        _transferFunctions[0]->update();
}

bool CygnetPlane::textureReady(){
    return ((!_textures.empty()) && (_textures[0] != nullptr));
}

void CygnetPlane::createPlane(){
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    
    // ============================
    //         GEOMETRY (quad)
    // ============================
    // GLfloat x,y, z;
    float s = _data->spatialScale.x;
    const GLfloat x = s*_data->scale.x/2.0;
    const GLfloat y = s*_data->scale.y/2.0;
    const GLfloat z = s*_data->scale.z/2.0;
    const GLfloat w = _data->spatialScale.w;

    const GLfloat vertex_data[] = { // square of two triangles (sigh)
        //      x      y     z     w     s     t
        -x, -z,             -y,  w, 0, 1,
         x,  z,              y,  w, 1, 0,
        -x,  ((x>0)?z:-z),   y,  w, 0, 0,
        -x, -z,             -y,  w, 0, 1,
         x,  ((x>0)?-z:z),  -y,  w, 1, 1,
         x,  z,              y,  w, 1, 0,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));
}

void CygnetPlane::destroyPlane(){
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;
}

bool CygnetPlane::createShader(){
    if (_shader == nullptr) {
    // Plane Program
    RenderEngine& renderEngine = OsEng.renderEngine();
    _shader = renderEngine.buildRenderProgram("PlaneProgram",
        "${MODULE_ISWA}/shaders/cygnetplane_vs.glsl",
        "${MODULE_ISWA}/shaders/cygnetplane_fs.glsl"
        );
    if (!_shader)
        return false;
    }
    return true;
}

void CygnetPlane::destroyShader(){
    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }
}

} //namespace openspace