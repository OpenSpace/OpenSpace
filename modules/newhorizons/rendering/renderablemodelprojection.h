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

#ifndef __RENDERABLEMODELPROJECTION_H__
#define __RENDERABLEMODELPROJECTION_H__

#include <openspace/rendering/renderable.h>

#include <modules/newhorizons/util/imagesequencer.h>
#include <modules/newhorizons/util/labelparser.h>

#include <openspace/properties/numericalproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/util/updatestructures.h>
#include <modules/base/rendering/modelgeometry.h>
#include <openspace/util/spicemanager.h>

#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

namespace openspace {

    namespace modelgeometry {
        class ModelGeometry;
    }

    class RenderableModelProjection : public Renderable {
    public:
        RenderableModelProjection(const ghoul::Dictionary& dictionary);

        bool initialize() override;
        bool deinitialize() override;

        bool isReady() const override;

        void render(const RenderData& data) override;
        void update(const UpdateData& data) override;


    protected:
        void loadTextures();
        std::unique_ptr<ghoul::opengl::Texture> loadProjectionTexture(const std::string& texturePath);

    private:
        bool auxiliaryRendertarget();
        glm::mat4 computeProjectorMatrix(const glm::vec3 loc, glm::dvec3 aim, const glm::vec3 up);
        void attitudeParameters(double time);
        void imageProjectGPU(std::unique_ptr<ghoul::opengl::Texture> projectionTexture);

        void project();
        void clearAllProjections();

        properties::StringProperty _colorTexturePath;
        properties::BoolProperty _performProjection;
        properties::BoolProperty _clearAllProjections;

        properties::IntProperty _rotationX;
        properties::IntProperty _rotationY;
        properties::IntProperty _rotationZ;

        std::unique_ptr<ghoul::opengl::ProgramObject> _programObject;
        std::unique_ptr<ghoul::opengl::ProgramObject> _fboProgramObject;

        std::unique_ptr<ghoul::opengl::Texture> _baseTexture;
        std::unique_ptr<ghoul::opengl::Texture> _projectionTexture;

        properties::FloatProperty _projectionFading;

        modelgeometry::ModelGeometry* _geometry;

        float _alpha;
        glm::dmat3 _stateMatrix;
        glm::dmat3 _instrumentMatrix;

        std::string _defaultProjImage;
        std::string _source;
        std::string _destination;
        std::string _target;

        // sequence loading
        std::string _sequenceSource;
        std::string _sequenceType;

        // projection mod info
        std::string _instrumentID;
        std::string _projectorID;
        std::string _projecteeID;
        SpiceManager::AberrationCorrection _aberration;
        std::vector<std::string> _potentialTargets;
        float _fovy;
        float _aspectRatio;
        float _nearPlane;
        float _farPlane;

        // uniforms
        glm::vec2  _camScaling;
        glm::vec3  _up;
        glm::mat4  _transform;
        glm::mat4  _projectorMatrix;
        glm::vec3  _boresight;

        // FBO stuff
        GLuint _fboID;
        GLuint _quad;
        GLuint _vertexPositionBuffer;

        GLuint _vbo;
        GLuint _ibo;
        GLuint _vaoID;
        std::vector<modelgeometry::ModelGeometry::Vertex> _geometryVertecies;
        std::vector<int> _geometryIndeces;

        std::vector<Image> _imageTimes;
        int _frameCount;
        double _time;

        bool _capture;
        
        std::string _clearingImage;

        psc _sunPosition;

        properties::BoolProperty _performShading;
        bool _programIsDirty;
    };

}  // namespace openspace

#endif  // __RENDERABLEMODELPROJECTION_H__