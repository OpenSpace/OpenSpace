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

#include <modules/newhorizons/rendering/renderablefov.h>

#include <modules/newhorizons/util/imagesequencer.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>

#include <ghoul/opengl/programobject.h>

#include <glm/gtx/projection.hpp>

#include <openspace/performance/performancemeasurement.h>

namespace {
    const std::string _loggerCat              = "RenderableFov";

    const std::string keyBody                 = "Body";
    const std::string keyFrame                = "Frame";
    const std::string keyPathModule           = "ModulePath";
    const std::string keyColor                = "RGB";
    const std::string keyInstrument           = "Instrument.Name";
    const std::string keyInstrumentMethod     = "Instrument.Method";
    const std::string keyInstrumentAberration = "Instrument.Aberration";
    const std::string keyPotentialTargets     = "PotentialTargets";

    const int InterpolationSteps = 10;
    const int Stride = 8;
}

namespace openspace {

RenderableFov::RenderableFov(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _lineWidth("lineWidth", "Line Width", 1.f, 1.f, 20.f)
    , _drawSolid("solidDraw", "Draw as Quads", false)
    , _programObject(nullptr)
    , _texture(nullptr)
    , _drawFOV(false)
    , _mode(GL_LINES)
    //, _interceptTag{false, false, false, false, false, false, false, false}
    , _withinFOV(false)
    , _vBoundsSize(0)
    , _vPlaneSize(40)
{
    bool success = dictionary.getValue(keyBody, _spacecraft);
    ghoul_assert(success, "");

    success = dictionary.getValue(keyFrame, _frame);
    ghoul_assert(success, "");

    success = dictionary.getValue(keyInstrument, _instrumentID);
    ghoul_assert(success, "");

//    success = dictionary.getValue(keyInstrumentMethod, _method);
//    ghoul_assert(success, "");

    std::string a = "NONE";
    success = dictionary.getValue(keyInstrumentAberration, a);
    a = SpiceManager::AberrationCorrection(a);
    ghoul_assert(success, "");

    ghoul::Dictionary potentialTargets;
    success = dictionary.getValue(keyPotentialTargets, potentialTargets);
    ghoul_assert(success, "");

    _potentialTargets.resize(potentialTargets.size());
    for (int i = 0; i < potentialTargets.size(); ++i) {
        std::string target;
        potentialTargets.getValue(std::to_string(i + 1), target);
        _potentialTargets[i] = target;
    }

    addProperty(_lineWidth);
    addProperty(_drawSolid);
}

void RenderableFov::allocateData() { 
    // fetch data for specific instrument (shape, boresight, bounds etc)
    try {
        SpiceManager::FieldOfViewResult res = SpiceManager::ref().fieldOfView(_instrumentID);
        
        _bounds = std::move(res.bounds);
        _boresight = std::move(res.boresightVector);

        _projectionBounds.resize(_bounds.size());
        int initBoundPoints = 2 * (_bounds.size() + 1);
        _fovBounds.resize(initBoundPoints * Stride);
        _vBoundsSize = static_cast<unsigned int>(_fovBounds.size());
        // allocate second vbo data
        _fovPlane.resize(_vPlaneSize);

    }
    catch (const SpiceManager::SpiceException& e) {
        LERROR(e.what());
    }
}

bool RenderableFov::initialize() {
    bool completeSuccess = true;
    if (_programObject == nullptr) {

        RenderEngine& renderEngine = OsEng.renderEngine();
        _programObject = renderEngine.buildRenderProgram("FovProgram",
            "${MODULE_NEWHORIZONS}/shaders/fov_vs.glsl",
            "${MODULE_NEWHORIZONS}/shaders/fov_fs.glsl");

        if (!_programObject)
            return false;
    }
    allocateData();
    sendToGPU();
    return completeSuccess;
}

bool RenderableFov::deinitialize() {
    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_programObject) {
        renderEngine.removeRenderProgram(_programObject);
        _programObject = nullptr;
    }

    return true;
}

bool RenderableFov::isReady() const {
    return _programObject != nullptr;
}

void RenderableFov::sendToGPU() {
    // Initialize and upload to graphics card

    // FOV lines
    glGenVertexArrays(1, &_fovBoundsVAO);
    glGenBuffers(1, &_fovBoundsVBO);
    glBindVertexArray(_fovBoundsVAO);
    glBindBuffer(GL_ARRAY_BUFFER, _fovBoundsVBO);
    glBufferData(GL_ARRAY_BUFFER, _vBoundsSize * sizeof(GLfloat), NULL, GL_STATIC_DRAW); // orphaning the buffer, sending NULL data.
    glBufferSubData(GL_ARRAY_BUFFER, 0, _vBoundsSize * sizeof(GLfloat), _fovBounds.data());

    GLsizei st = sizeof(GLfloat) * Stride;

    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, st, (void*)0);
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, st, (void*)(4 * sizeof(GLfloat)));

    glBindVertexArray(0);

    // Orthogonal Plane
    glGenVertexArrays(1, &_fovPlaneVAO);
    glGenBuffers(1, &_fovPlaneVBO);

    glBindVertexArray(_fovPlaneVAO);
    glBindBuffer(GL_ARRAY_BUFFER, _fovPlaneVBO);
    glBufferData(GL_ARRAY_BUFFER, _vPlaneSize * sizeof(GLfloat), NULL, GL_STATIC_DRAW); // orphaning the buffer, sending NULL data.
    glBufferSubData(GL_ARRAY_BUFFER, 0, _vPlaneSize * sizeof(GLfloat), _fovPlane.data());

    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, st, (void*)0);
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, st, (void*)(4 * sizeof(GLfloat)));

    glBindVertexArray(0);
}

void RenderableFov::updateGPU() {
    PerfMeasure("updateGPU");
    glBindBuffer(GL_ARRAY_BUFFER, _fovBoundsVBO);
    glBufferSubData(GL_ARRAY_BUFFER, 0, _vBoundsSize * sizeof(GLfloat), _fovBounds.data());
    if (!_rebuild) {
        // no new points
        glBindBuffer(GL_ARRAY_BUFFER, _fovPlaneVBO);
        glBufferSubData(GL_ARRAY_BUFFER, 0, _vPlaneSize * sizeof(GLfloat), _fovPlane.data());
    }
    else {
        // new points - memory change 
        glBindVertexArray(_fovPlaneVAO);
        glBindBuffer(GL_ARRAY_BUFFER, _fovPlaneVBO);
        glBufferData(GL_ARRAY_BUFFER, _vPlaneSize * sizeof(GLfloat), NULL, GL_STATIC_DRAW); // orphaning the buffer, sending NULL data.
        glBufferSubData(GL_ARRAY_BUFFER, 0, _vPlaneSize * sizeof(GLfloat), _fovPlane.data());

        GLsizei st = sizeof(GLfloat) * Stride;
        glEnableVertexAttribArray(0);
        glEnableVertexAttribArray(1);
        glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, st, (void*)0);
        glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, st, (void*)(4 * sizeof(GLfloat)));
    }

    glBindVertexArray(0);
}

// various helper methods
void RenderableFov::insertPoint(std::vector<float>& arr, glm::vec4 p, glm::vec4 c) {
    for (int i = 0; i < 4; i++){
        arr.push_back(p[i]);
    }
    for (int i = 0; i < 4; i++){
        arr.push_back(c[i]);
    }
    _nrInserted++;
}

glm::dvec3 RenderableFov::interpolate(glm::dvec3 p0, glm::dvec3 p1, float t) {
    assert(t >= 0 && t <= 1);
    float t2 = (1.f - t);
    return glm::dvec3(p0.x*t2 + p1.x*t, p0.y*t2 + p1.y*t, p0.z*t2 + p1.z*t);
}


// This method is the current bottleneck.
psc RenderableFov::checkForIntercept(glm::dvec3 ray) {
    std::string bodyfixed = "IAU_";
    bool convert = (_frame.find(bodyfixed) == std::string::npos);
    if (convert)
        bodyfixed = SpiceManager::ref().frameFromBody(_fovTarget);
    else
        bodyfixed = _frame;
    
    SpiceManager::SurfaceInterceptResult result = SpiceManager::ref().surfaceIntercept(
       _fovTarget, _spacecraft, _instrumentID, bodyfixed, _aberrationCorrection, _time, ray);
    
    if (convert) {
        result.surfaceVector = SpiceManager::ref().frameTransformationMatrix(bodyfixed, _frame, _time) * result.surfaceVector;
    }
    
    ipoint = result.surfaceIntercept;
    ivec = result.surfaceVector;
//    bool intercepted = result.interceptFound;
    
    ivec *= 0.9999;// because fov lands exactly on top of surface we need to move it out slightly
    _interceptVector = PowerScaledCoordinate::CreatePowerScaledCoordinate(ivec[0], ivec[1], ivec[2]);
    _interceptVector[3] += 3;

    return _interceptVector;
}
// Orthogonal projection next to planets surface
psc RenderableFov::orthogonalProjection(glm::dvec3 vecFov) {
    double lt;
    glm::dvec3 vecToTarget =
        SpiceManager::ref().targetPosition(_fovTarget, _spacecraft, _frame, _aberrationCorrection, _time, lt);
    vecFov = SpiceManager::ref().frameTransformationMatrix(_instrumentID, _frame, _time) * vecFov;
    glm::dvec3 p = glm::proj(vecToTarget, vecFov);

    psc projection = PowerScaledCoordinate::CreatePowerScaledCoordinate(p[0], p[1], p[2]);
    projection[3] += 3;

    return projection;
}
// Bisection method, simple recurtion
glm::dvec3 RenderableFov::bisection(glm::dvec3 p1, glm::dvec3 p2) {
    const double Tolerance = 0.000000001; // very low tolerance factor
                                          
    //check if point is on surface
    glm::dvec3 half = interpolate(p1, p2, 0.5f);
    
    std::string bodyfixed = "IAU_";
    bool convert = (_frame.find(bodyfixed) == std::string::npos);
    if (convert)
        bodyfixed = SpiceManager::ref().frameFromBody(_fovTarget);
    else
        bodyfixed = _frame;

    
    SpiceManager::SurfaceInterceptResult result = SpiceManager::ref().surfaceIntercept(
      _fovTarget, _spacecraft, _instrumentID, bodyfixed, _aberrationCorrection, _time, half);
    
    if (convert) {
        result.surfaceVector = SpiceManager::ref().frameTransformationMatrix(bodyfixed, _frame, _time) * result.surfaceVector;
    }
    
    ipoint = result.surfaceIntercept;
    ivec = result.surfaceVector;
    bool intercepted = result.interceptFound;
    
    if (glm::distance(_previousHalf, half) < Tolerance) {
        _previousHalf = glm::dvec3(0);
        return half;
    }
    _previousHalf = half;
    //recursive search
    if (!intercepted) {
        return bisection(p1, half);
    }
    else {
        return bisection(half, p2);
    }
}


void RenderableFov::fovSurfaceIntercept(bool H[], std::vector<glm::dvec3> bounds) {
    _nrInserted = 0;
    _fovPlane.clear(); // empty the array

    glm::dvec3 mid;
    glm::dvec3 interpolated;
    glm::dvec3 current;
    glm::dvec3 next;
    glm::vec4 tmp(1);
    if (bounds.size() > 1) {
        for (int i = 0; i < bounds.size(); ++i) {
            int k = (i + 1 > bounds.size() - 1) ? 0 : i + 1;

            current = bounds[i];
            next = bounds[k];

            if (H[i] == false) { // If point is non-interceptive, project it.
                insertPoint(_fovPlane, orthogonalProjection(current).vec4(), tmp);
                if (H[i + 1] == false && _withinFOV) {
                    // IFF incident point is also non-interceptive BUT something is within FOV
                    // we need then to check if this segment makes contact with surface
                    glm::dvec3 half = interpolate(current, next, 0.5f);
                    
                    std::string bodyfixed = "IAU_";
                    bool convert = (_frame.find(bodyfixed) == std::string::npos);
                    if (convert) {
                        bodyfixed = SpiceManager::ref().frameFromBody(_fovTarget);
                    }
                    else {
                        bodyfixed = _frame;
                    }
                    
                    SpiceManager::SurfaceInterceptResult res =
                        SpiceManager::ref().surfaceIntercept(_fovTarget, _spacecraft,
                            _instrumentID, bodyfixed, _aberrationCorrection, _time, half);
                    
                    if (convert) {
                        res.surfaceVector = SpiceManager::ref().frameTransformationMatrix(bodyfixed, _frame, _time) * res.surfaceVector;
                    }

                    ipoint = res.surfaceIntercept;
                    ivec = res.surfaceVector;
                    bool intercepted = res.interceptFound;

                    if (intercepted) {
                        // find the two outer most points of intersection 
                        glm::dvec3 root1 = bisection(half, current);
                        glm::dvec3 root2 = bisection(half, next);

                        insertPoint(_fovPlane, orthogonalProjection(root1).vec4(), col_sq);
                        for (int j = 1; j < InterpolationSteps; ++j) {
                            float t = (static_cast<float>(j) / InterpolationSteps);
                            interpolated = interpolate(root1, root2, t);
                            _interceptVector = checkForIntercept(interpolated);
                            insertPoint(_fovPlane, _interceptVector.vec4(), col_sq);
                        }
                        insertPoint(_fovPlane, orthogonalProjection(root2).vec4(), col_sq);
                    }
                }
            }
            if (H[i] == true && H[i + 1] == false) { // current point is interceptive, next is not
                // find outer most point for interpolation
                mid = bisection(current, next);
                for (int j = 1; j <= InterpolationSteps; ++j) {
                    float t = (static_cast<float>(j) / InterpolationSteps);
                    interpolated = interpolate(current, mid, t);
                    _interceptVector = (j < InterpolationSteps) ? checkForIntercept(interpolated) : orthogonalProjection(interpolated);
                    insertPoint(_fovPlane, _interceptVector.vec4(), col_sq);
                }
            }
            if (H[i] == false && H[i + 1] == true){ // current point is non-interceptive, next is
                mid = bisection(next, current);
                for (int j = 1; j <= InterpolationSteps; ++j) {
                    float t = (static_cast<float>(j) / InterpolationSteps);
                    interpolated = interpolate(mid, next, t);
                    _interceptVector = (j > 1) ? checkForIntercept(interpolated) : orthogonalProjection(interpolated);
                    insertPoint(_fovPlane, _interceptVector.vec4(), col_sq);
                }
            }
            if (H[i] == true && H[i + 1] == true){ // both points intercept
                for (int j = 0; j <= InterpolationSteps; ++j) {
                    float t = (static_cast<float>(j) / InterpolationSteps);
                    interpolated = interpolate(current, next, t);
                    _interceptVector = checkForIntercept(interpolated);
                    insertPoint(_fovPlane, _interceptVector.vec4(), col_sq);
                }
            }
        }
     }
    if (_nrInserted == 0) {
        _rebuild = false;
    } 
    else {
        _rebuild = true;
        //update size etc;
        _vPlaneSize  = static_cast<unsigned int>(_fovPlane.size());
    }
}

// This method is purely cosmetics, can very well be removed 
// but be sure to set colors somewhere. 
void RenderableFov::computeColors() {
    double t2 = (openspace::ImageSequencer::ref().getNextCaptureTime());
    double diff = (t2 - _time);
    float t = 0.0;
    float interpolationStart = 7.0; //seconds before
    if (diff <= interpolationStart)
        t = static_cast<float>(1.0 - (diff / interpolationStart));

    if (diff < 0.0)
        t = 0.f;

    // This is a bit hardcoded - either we go for color tables
    // or make these properties.
    col_gray    = glm::vec4(0.7);
    col_project = glm::vec4(0.0, 1.0, 0.00, 1); 
    col_start   = glm::vec4(1.00, 0.89, 0.00, 1);
    col_end     = glm::vec4(1.00, 0.29, 0.00, 1);
    col_blue    = glm::vec4(0, 0.5, 0.7, 1);
    col_sq      = glm::vec4(1.00, 0.29, 0.00, 1);
    
    col_end  = col_project*t + col_end*(1 - t);
    col_blue = col_project*t + col_blue*(1 - t);
    col_sq   = col_project*t + col_sq*(1 - t);
    
    float alpha;
    alpha = _drawSolid ? 0.5f : 0.8f;

    col_blue.w = alpha;
    col_project.w = alpha;
    col_end.w = alpha;
}

void RenderableFov::determineTarget() {
    PerfMeasure("determineTarget");
    _fovTarget = _potentialTargets[0]; //default;
    for (int i = 0; i < _potentialTargets.size(); ++i) {
        _withinFOV = openspace::SpiceManager::ref().isTargetInFieldOfView(
            _potentialTargets[i],
            _spacecraft,                          
            _instrumentID,
            SpiceManager::FieldOfViewMethod::Ellipsoid,
            _aberrationCorrection,
            _time
        );
        if (_withinFOV) {
            _fovTarget = _potentialTargets[i];
            break;
        }
    }
}

void RenderableFov::computeIntercepts(const RenderData& data) {
    PerfMeasure("computeIntercepts");
    // for each FOV vector
    _fovBounds.clear();
    for (int i = 0; i <= _bounds.size(); ++i) {
        int r = (i == _bounds.size()) ? 0 : i;
        std::string bodyfixed = "IAU_";
        bool convert = (_frame.find(bodyfixed) == std::string::npos);
        if (convert) {
            bodyfixed = SpiceManager::ref().frameFromBody(_fovTarget);
        }
        else {
            bodyfixed = _frame;
        }
        
        SpiceManager::SurfaceInterceptResult res =
            SpiceManager::ref().surfaceIntercept(_fovTarget, _spacecraft,
                _instrumentID, bodyfixed, _aberrationCorrection, _time, _bounds[r]);
        
        if (convert) {
            res.surfaceVector = SpiceManager::ref().frameTransformationMatrix(bodyfixed, _frame, _time) * res.surfaceVector;
        }
        
        ipoint = res.surfaceIntercept;
        ivec = res.surfaceVector;
        _interceptTag[r] = res.interceptFound;
        
        // if not found, use the orthogonal projected point
        if (!_interceptTag[r]) {
            _projectionBounds[r] = orthogonalProjection(_bounds[r]);
        }

        glm::vec4 fovOrigin = glm::vec4(0); //This will have to be fixed once spacecraft is 1:1!

        if (_interceptTag[r]) {
            _interceptVector = PowerScaledCoordinate::CreatePowerScaledCoordinate(ivec[0], ivec[1], ivec[2]);
            _interceptVector[3] += 3;
            // INTERCEPTIONS
            insertPoint(_fovBounds, fovOrigin, col_start);
            insertPoint(_fovBounds, _interceptVector.vec4(), col_end);
        }
        else if (_withinFOV) {
            // OBJECT IN FOV, NO INTERCEPT FOR THIS FOV-RAY
            insertPoint(_fovBounds, fovOrigin, glm::vec4(0, 0, 1, 1));
            insertPoint(_fovBounds, _projectionBounds[r].vec4(), col_blue);
        }
        else {
            glm::vec4 corner(_bounds[r][0], _bounds[r][1], _bounds[r][2], data.position[3] + 2);
            corner = _spacecraftRotation*corner;
            // NONE OF THE FOV-RAYS INTERCEPT AND NO OBJECT IN FOV
            insertPoint(_fovBounds, fovOrigin, col_gray);
            insertPoint(_fovBounds, corner, glm::vec4(0));
        }
    }
    _interceptTag[_bounds.size()] = _interceptTag[0];
    fovSurfaceIntercept(_interceptTag, _bounds);

    glm::vec3 aim = (_spacecraftRotation * glm::vec4(_boresight, 1)).xyz();
    double lt;
    glm::dvec3 position =
    SpiceManager::ref().targetPosition(
        _fovTarget,
        _spacecraft,
        _frame,
        _aberrationCorrection,
        _time,
        lt
    );
    psc p = PowerScaledCoordinate::CreatePowerScaledCoordinate(position.x, position.y, position.z);
    pss length = p.length();
    if (length[0] < DBL_EPSILON) {
        _drawFOV = false;
        return;
    }
    //if aimed 80 deg away from target, dont draw white square
    if (glm::dot(glm::normalize(aim), glm::normalize(p.vec3())) < 0.2) {
        _drawFOV = false;
    }
}

void RenderableFov::render(const RenderData& data) {
    assert(_programObject);
    _programObject->activate();

    _drawFOV = false;
    // setup the data to the shader
    //_programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    //_programObject->setUniform("ModelTransform", glm::translate(glm::mat4(1), glm::vec3(data.positionVec3)) );
    //setPscUniforms(*_programObject.get(), data.camera, data.position);
    
    // Model transform and view transform needs to be in double precision
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.positionVec3) * // Translation
        glm::dmat4(data.rotation);
    glm::mat4 modelViewProjectionTransform =
        data.camera.projectionMatrix() *
        glm::mat4(data.camera.combinedViewMatrix() *
        modelTransform);

    _programObject->setUniform("modelViewProjectionTransform", modelViewProjectionTransform);

    if (openspace::ImageSequencer::ref().isReady()) {
        _drawFOV = ImageSequencer::ref().instrumentActive(_instrumentID);
    }

    if (_drawFOV) {
        // update only when time progresses.
        if (_oldTime != _time) {
            PerfMeasure("Total");
            determineTarget();
            computeColors();
            computeIntercepts(data);
            updateGPU();
        }
        _oldTime = _time;
        _mode = _drawSolid ? GL_TRIANGLE_STRIP : GL_LINES;

        glLineWidth(_lineWidth);
        glBindVertexArray(_fovBoundsVAO);
        glDrawArrays(_mode, 0, static_cast<int>(_vBoundsSize / Stride));
        glBindVertexArray(0);

        if (_drawFOV) {
            glLineWidth(2.f);
            glBindVertexArray(_fovPlaneVAO);
            glDrawArrays(GL_LINE_LOOP, 0, static_cast<int>(_vPlaneSize / Stride));
            glBindVertexArray(0);
        }
        glLineWidth(1.f);
    }
    _programObject->deactivate();
}

void RenderableFov::update(const UpdateData& data) {
    _time  = data.time;
    _stateMatrix = SpiceManager::ref().positionTransformMatrix(_instrumentID, _frame, data.time);
    _spacecraftRotation = glm::mat4(_stateMatrix);
}

} // namespace openspace
