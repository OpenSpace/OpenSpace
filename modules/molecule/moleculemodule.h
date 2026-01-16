/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_MOLECULE___MOLECULEMODULE___H__
#define __OPENSPACE_MODULE_MOLECULE___MOLECULEMODULE___H__

#include <openspace/util/openspacemodule.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/util/threadpool.h>
#include <glm/mat4x4.hpp>

struct md_gl_shaders_t;

namespace openspace {

class MoleculeModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "Molecule";

    MoleculeModule();
    ~MoleculeModule();

    GLuint fbo() const { return _fbo; }
    GLuint colorTexture()  const { return _colorTex; }
    GLuint normalTexture() const { return _normalTex; }
    GLuint depthTexture()  const { return _depthTex; }
    
    const md_gl_shaders_t& shaders() const { return *_shaders; }

    ThreadPool& threadPool() { return _threadPool; }

    void internalInitializeGL() final;
    void internalDeinitializeGL() final;

    void setViewMatrix(const glm::mat4& V) {
        _V = V;
    }
    
    void setProjectionMatrix(const glm::mat4& P) {
        _P = P;
    }

    std::vector<documentation::Documentation> documentations() const override;

private:
    void internalInitialize(const ghoul::Dictionary&) override;
    void preDraw();
    void postDraw();
    
    GLuint _fbo = 0;
    GLuint _colorTex  = 0;
    GLuint _normalTex = 0;
    GLuint _depthTex  = 0;
    int _width = 0;
    int _height = 0;
    
    std::unique_ptr<md_gl_shaders_t> _shaders = nullptr;
    glm::mat4 _V;
    glm::mat4 _P;

    properties::BoolProperty    _ssaoEnabled;
    properties::FloatProperty   _ssaoIntensity;
    properties::FloatProperty   _ssaoRadius;
    properties::FloatProperty   _ssaoHorizonBias;
    properties::FloatProperty   _ssaoNormalBias;

    properties::BoolProperty    _ssao2Enabled;
    properties::FloatProperty   _ssao2Intensity;
    properties::FloatProperty   _ssao2Radius;
    properties::FloatProperty   _ssao2HorizonBias;
    properties::FloatProperty   _ssao2NormalBias;
    
    properties::FloatProperty   _exposure;

    properties::BoolProperty    _dofEnabled;
    properties::FloatProperty   _dofFocusDistance;
    properties::FloatProperty   _dofFocusRange;

    ThreadPool _threadPool;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_MOLECULE___MOLECULEMODULE___H__
