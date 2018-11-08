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

#include <modules/dsn/rendering/renderablesignals.h>

#include <modules/dsn/dsnmodule.h>
#include <modules/dsn/dsnmanager/dsnmanager.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/globals.h>
#include <fstream>
#include <openspace/util/updatestructures.h>
#include <openspace/util/spicemanager.h>
//#include "SpiceUsr.h"

namespace openspace {

    class CommunicationLines : public RenderableSignals {
    public:

        CommunicationLines(const ghoul::Dictionary& dictionary);
        void extractData();
        void initializeGL() override;
        void deinitializeGL() override;
        void update(const UpdateData& data) override;

        /*Returns a position for a spacecraft*/
        RenderableSignals::PositionVBOLayout getSuitablePrecisionPositionForSceneGraphNode(std::string id);
        /*Returns a position for a station that has Earth as parent*/
        RenderableSignals::PositionVBOLayout getPositionForGeocentricSceneGraphNode(const char* id);
        /*Returns a position calculated where the focusNode position is origin(0,0,0) */
        glm::dvec3 getCoordinatePosFromFocusNode(SceneGraphNode* node);
        /*Returns an index for our filenames */
        int findFileIndexForCurrentTime(double time, std::vector<double> vec);
        /*Adds the signaldata to _vertexArray*/
        void pushSignalDataToVertexArray(DsnManager::Signal signal);
        /* Converts the Ra Dec range coordinates into cartesian coordinates*/
        glm::vec3 convertRaDecRangeToCartesian();
        glm::dvec3 getEstimatedCoordinatePosFromFocusNode(glm::vec3 pos);

        void pushRaDecDataToVertexArray(glm::vec3 posMoon);
    private:
        /*Checks if the current time is within a signal's start and endtime*/
         bool isSignalActive(double currentTime, std::string signalStartTime, std::string signalEndTime);
        
         const char* _identifier = "CommunicationLines";
         std::unique_ptr<ghoul::Dictionary> _dictionary;
         SceneGraphNode* _focusNode;

         int _signalVectorStartIndex;
         /* Rotation matrix to transform into equatorial sphere coordinate system*/
         glm::dmat4 _rotEquatorialSphere = { -0.05487554,  0.4941095, -0.8676661, 0.0,
             -0.8734371 , -0.4448296, -0.1980764, 0.0,
             -0.483835  ,  0.7469823,  0.4559838, 0.0,
             0.0       ,  0.0      ,  0.0      , 1.0 };
    };

}
#endif 
