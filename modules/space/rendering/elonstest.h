/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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
/*
#ifndef __OPENSPACE_MODULE_SPACE___ELONSTEST___H__
#define __OPENSPACE_MODULE_SPACE___ELONSTEST___H__

#include <openspace/rendering/renderable.h>
#include <modules/space/translation/keplertranslation.h>
#include <modules/space/translation/tletranslation.h>

#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/uintproperty.h>

#include <ghoul/glm.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/misc/exception.h>



namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace {
        // The layout of the VBOs
    struct TrailVBOLayout {
        float x, y, z, time;
    };
        // All of the Kepler element information
    struct KeplerParameters{
        double inclination = 0.0;
        double semiMajorAxis = 0.0;
        double ascendingNode = 0.0;
        double eccentricity = 0.0;
        double argumentOfPeriapsis = 0.0;
        double meanAnomaly = 0.0;
        double meanMotion = 0.0;
        double epoch = 0.0;
    };

namespace documentation { struct Documentation; }

class ElonsTest : public Renderable { 
public:
    // constructors & destructor
    ElonsTest(const ghoul::Dictionary& dictionary);
    
    // override?
    void initialize() override;
    void initializeGL() override;
    // void deinitialize();
    void deinitializeGL() override;
    //
    bool isReady() const;


    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

protected:
private:
  
    
    // TLETranslation _tleTranslator;
    // std::vector<KeplerTranslation::KeplerOrbit> _orbits;
    ghoul::opengl::ProgramObject* _programObject;

    KeplerTranslation _keplerTranslator;

    std::vector<KeplerParameters> _TLEData;


    /// The backend storage for the vertex buffer object containing all points for this
    /// trail.
    std::vector<TrailVBOLayout> _vertexBufferData;
    /// The index array that is potentially used in the draw call. If this is empty, no
    /// element draw call is used.
    std::vector<unsigned int> _indexBufferData;

    GLuint _vertexArray;
    GLuint _vertexBuffer;
    GLuint _indexBuffer;
    
    properties::StringProperty _path;
    properties::UIntProperty _nSegments;
    
    properties::StringProperty _eccentricityColumnName;
    properties::StringProperty _semiMajorAxisColumnName;
    properties::DoubleProperty _semiMajorAxisUnit;
    properties::StringProperty _inclinationColumnName;
    properties::StringProperty _ascendingNodeColumnName;
    properties::StringProperty _argumentOfPeriapsisColumnName;
    properties::StringProperty _meanAnomalyAtEpochColumnName;
    properties::StringProperty _epochColumnName;


    void readTLEFile(const std::string& filename);
    void updateBuffers();

    /// Dirty flag for the _orbitPlaneRotation parameters
    mutable bool _orbitPlaneDirty = true;
    // glm::dvec3 calculatePosition(const Time& time, double epoch) const; 

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___ELONSTEST___H__

*/
