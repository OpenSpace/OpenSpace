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

namespace openspace{

// CygnetPlane::CygnetPlane(int cygnetId, std::string path)
// 	:ISWACygnet(cygnetId, path)
// 	,_quad(0)
// 	,_vertexPositionBuffer(0)
//     ,_planeIsDirty(true)
// {}

CygnetPlane::CygnetPlane(std::shared_ptr<Metadata> data)
    :ISWACygnet(data)
    ,_quad(0)
    ,_vertexPositionBuffer(0)
    ,_planeIsDirty(true)
{}

CygnetPlane::CygnetPlane(const ghoul::Dictionary& dictionary)
    :ISWACygnet(dictionary)
    ,_quad(0)
    ,_vertexPositionBuffer(0)
    ,_planeIsDirty(true)
{}

CygnetPlane::~CygnetPlane(){}

bool CygnetPlane::isReady(){
    bool ready = true;
    if (!_shader)
        ready &= false;
    if(!_texture)
        ready &= false;
    return ready;
}

void CygnetPlane::createPlane(){
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    
	// ============================
    // 		GEOMETRY (quad)
    // ============================
    const GLfloat x = _data->scale->x/2.0;
    const GLfloat y = _data->scale->z/2.0;
    const GLfloat w = _data->scale->w;

    const GLfloat vertex_data[] = { // square of two triangles (sigh)
        //	  x      y     z     w     s     t
        -x, -y, 0, w, 0, 1,
         x,  y, 0, w, 1, 0,
        -x,  y, 0, w, 0, 0,
        -x, -y, 0, w, 0, 1,
         x, -y, 0, w, 1, 1,
         x,  y, 0, w, 1, 0,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));

    _planeIsDirty = false;
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
}

void CygnetPlane::destroyShader(){
    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }
}

} //namespace openspace