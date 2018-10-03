/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2018                                                               *
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

#include <modules/dsn/rendering/communicationlines.h>
#include <openspace/scene/scene.h>

namespace openspace {
    constexpr const char* _loggerCat = "CommunicationLine";

    // Keys to get values from dictionary
    constexpr const char* KeyDataFolder = "DataFolder";
    constexpr const char* KeyDataFileType = "DataFileType";

    //Filetypes
    const std::string dataFileTypeStringXml = "xml";

    enum class DataFileType : int {
        Xml = 0,
        Invalid
    };

    //Member functions
    CommunicationLines::CommunicationLines(const ghoul::Dictionary& dictionary)
        : RenderableTrail(dictionary){
           _dictionary = std::make_unique<ghoul::Dictionary>(dictionary);
           extractData();
        
    }

    void CommunicationLines::extractData() {
       
        if (!DsnManager::extractMandatoryInfoFromDictionary(_identifier, _dictionary)){
            LERROR(fmt::format("{}: Did not manage to extract data.", _identifier));
        }
        else {
            LDEBUG(fmt::format("{}: Successfully read data.", _identifier));
        }
    }
    
    void CommunicationLines::initializeGL() {
        RenderableTrail::initializeGL();

        // We don't need an index buffer, so we keep it at the default value of 0
        glGenVertexArrays(1, &_primaryRenderInformation._vaoID);
        glGenBuffers(1, &_primaryRenderInformation._vBufferID);
    }
    void CommunicationLines::deinitializeGL(){
        glDeleteVertexArrays(1, &_primaryRenderInformation._vaoID);
        glDeleteBuffers(1, &_primaryRenderInformation._vBufferID);


        RenderableTrail::deinitializeGL();
    }
    void CommunicationLines::update(const UpdateData& data){

        if (_needsFullSweep) {

            const int nValues = 2;

            // Make space for the vertices
            _vertexArray.clear();
            _vertexArray.resize(nValues);

            // ... fill all of the values, dummy values for now, should load from  _translation->position()
            //    const glm::vec3 p = _translation->position()
            _vertexArray[0] = { 0, 0, 0 };
            _vertexArray[1] = { static_cast<float>(58.5877), static_cast<float>(16.1924), static_cast<float>(20000000) }; // go to geo?

            // ... and upload them to the GPU
            glBindVertexArray(_primaryRenderInformation._vaoID);
            glBindBuffer(GL_ARRAY_BUFFER, _primaryRenderInformation._vBufferID);
            glBufferData(
                GL_ARRAY_BUFFER,
                _vertexArray.size() * sizeof(TrailVBOLayout),
                _vertexArray.data(),
                GL_STATIC_DRAW
            );

            glEnableVertexAttribArray(0);
            glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

            // We clear the indexArray just in case. The base class will take care not to use
            // it if it is empty
            _indexArray.clear();

            _needsFullSweep = false;
        }

        // If the full trail should be rendered at all times, we can directly render the
        // entire set
        _primaryRenderInformation.first = 0;
        _primaryRenderInformation.count = static_cast<GLsizei>(_vertexArray.size());
     
        glBindVertexArray(0);
    }

} // namespace openspace


