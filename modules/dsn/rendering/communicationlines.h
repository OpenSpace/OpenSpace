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

#ifndef __OPENSPACE_MODULE_DSN___COMMUNICATIONLINES___H__
#define __OPENSPACE_MODULE_DSN___COMMUNICATIONLINES___H__

#include <modules/dsn/rendering/renderablecommunicationpackage.h>
#include <modules/dsn/dsnmodule.h>
#include <modules/dsn/dsnmanager/dsnmanager.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/globals.h>
#include <fstream>


namespace openspace {

    class CommunicationLines : public RenderableCommunicationPackage {
    public:

        CommunicationLines(const ghoul::Dictionary& dictionary);
        void extractData();
        void initializeGL() override;
        void deinitializeGL() override;
        void update(const UpdateData& data) override;
        
        struct Signal {
            const char* station;
            const char* spacecraft;
            glm::vec3 color;
        };

        struct DsnData {
            std::vector<Signal> signals;
        };

        RenderableCommunicationPackage::LineVBOLayout getSuitablePrecisionPositionForSceneGraphNode(std::string id);
        RenderableCommunicationPackage::LineVBOLayout getPositionForGeocentricSceneGraphNode(const char* id);
        glm::dvec3 GetCoordinatePosFromFocusNode(SceneGraphNode* node);

    private:

         const char* _identifier = "CommunicationLines";
         std::unique_ptr<ghoul::Dictionary> _dictionary;
    
         SceneGraphNode* _focusNode;

    };

}
#endif 
