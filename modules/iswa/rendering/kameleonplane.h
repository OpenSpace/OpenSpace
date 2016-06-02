/****************************************************************************************
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

#ifndef __KAMELEONPLANE_H__
#define __KAMELEONPLANE_H__

#include <modules/iswa/rendering/datacygnet.h>
#include <openspace/properties/vectorproperty.h>
#include <openspace/properties/selectionproperty.h>

namespace openspace{

/**
 * KameleonPlane is a concrete IswaCygnet with volume data files from 
 * disk as input source. An object of this class will provide a cutplane
 * through a volume of space weather data. It will also give the option
 * to create fieldlines around and intersecting the planes. Interaction
 * with the planes is possible through sliders that will move the planes
 * through the data volume.
 */
class KameleonPlane : public DataCygnet {
public:
 	KameleonPlane(const ghoul::Dictionary& dictionary);
 	~KameleonPlane();

     bool initialize() override;
     bool deinitialize() override;

private:

    /**
     * Creates a plane geometry
     */
    bool createGeometry() override;
    bool destroyGeometry() override;
    bool updateTextureResource() override; 
    void renderGeometry() const override;
    void setUniforms() override;
    std::vector<float*> textureData() override;

    /**
     * Given a path to the json index of seedpoints file, this 
     * method reads, parses and adds them as checkbox options
     * in the _fieldlines SelectionProperty
     * 
     * @param indexFile Path to json index file
     */
    void readFieldlinePaths(std::string indexFile);

    /**
     * Updates the _fieldlineState map to match the _fieldlines 
     * SelectionProperty and creates or removes fieldlines in the scene.
     */
    void updateFieldlineSeeds();
    void subscribeToGroup();

	static int id();

    properties::IntProperty _resolution;
    properties::FloatProperty _slice;
    properties::StringProperty _transferFunctionsFile;

    properties::SelectionProperty _fieldlines;

    properties::Vec2Property _backgroundValues;
    properties::Vec2Property _normValues;

    properties::BoolProperty _useLog;
    properties::BoolProperty _useHistogram;
    properties::BoolProperty _autoFilter;


	std::shared_ptr<KameleonWrapper> _kw;
    std::string _kwPath;

    glm::size3_t _dimensions;
    float* _dataSlice;
    std::string _var;

    std::vector<float*> _dataSlices;
    float _scale;
    
    /**
     * _fieldlineState maps the checkbox value of each fieldline seedpoint file to a tuple 
     * containing information that is needed to either add or remove a fieldline from the scenegraph.
     * this is the name, path to seedpoints file and a boolean to determine if it is active or inactive.
     */
    std::map<int, std::tuple<std::string, std::string, bool> > _fieldlineState;
    std::string _fieldlineIndexFile;

    GLuint _quad;
    GLuint _vertexPositionBuffer;

};
 
} // namespace openspace

#endif //__KAMELEONPLANE_H__