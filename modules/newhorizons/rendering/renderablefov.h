/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __RenderableFov_H__
#define __RenderableFov_H__

// open space includes
#include <openspace/rendering/renderable.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/query/query.h>

// ghoul includes
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
//#include <openspace/util/runtimedata.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/spicemanager.h>

namespace openspace {
class RenderableFov : public Renderable{
public:
    RenderableFov(const ghoul::Dictionary& dictionary);
    ~RenderableFov();

    bool initialize() override;
    bool deinitialize() override;

    bool isReady() const override;

    void render(const RenderData& data) override;
    void update(const UpdateData& data) override;

 private:
    // properties
    properties::FloatProperty _lineWidth;
    properties::BoolProperty _drawSolid;
    std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;
    ghoul::opengl::Texture* _texture;
    openspace::SceneGraphNode* _targetNode;

    // class methods
    void loadTexture();
    void allocateData();
    void insertPoint(std::vector<float>& arr, glm::vec4 p, glm::vec4 c);
    void fovSurfaceIntercept(bool H[], std::vector<glm::dvec3> bounds);
    void determineTarget();
    void updateGPU();
    void sendToGPU();


    // helper methods
    void computeColors();
    void computeIntercepts(const RenderData& data);
    psc orthogonalProjection(glm::dvec3 camvec);
    psc checkForIntercept(glm::dvec3 ray);
    psc pscInterpolate(psc p0, psc p1, float t);
    glm::dvec3 interpolate(glm::dvec3 p0, glm::dvec3 p1, float t);
    glm::dvec3 bisection(glm::dvec3 p1, glm::dvec3 p2, double tolerance);

    // instance variables
    int _nrInserted = 0;
    int _isteps;
    bool _rebuild = false;
    bool _interceptTag[8];
    bool _withinFOV;
    std::vector<psc> _projectionBounds;
    psc _interceptVector;

    std::vector<float> _fovBounds;
    std::vector<float> _fovPlane;

    // spice
    std::string _spacecraft;
    std::string _observer;
    std::string _frame;
    std::string _instrumentID;
    SpiceManager::AberrationCorrection _aberrationCorrection;
    std::string _fovTarget;
    glm::dvec3 ipoint, ivec;
    glm::dvec3 _previousHalf;
    glm::dvec3 _boresight;
    glm::dmat3 _stateMatrix;
    glm::mat4 _spacecraftRotation;
    std::vector<glm::dvec3> _bounds;
    std::vector<std::string> _potentialTargets;
    bool _drawFOV;
    double _lt;

    // GPU 
    GLuint _fovBoundsVAO;
    GLuint _fovBoundsVBO;
    unsigned int _vBoundsSize;
    GLuint _fovPlaneVAO;
    GLuint _fovPlaneVBO;
    unsigned int _vPlaneSize;
    GLenum _mode;
    unsigned int _stride;

    // time
    double _time = 0;
    double _oldTime = 0;

    // colors
    glm::vec4 col_sq;      // orthogonal white square
    glm::vec4 col_project; // color when projections occur
    glm::vec4 col_start;   // intersection start color
    glm::vec4 col_end;     // intersection end color
    glm::vec4 col_blue;    // withinFov color
    glm::vec4 col_gray;    // no intersection color

};
}
#endif
