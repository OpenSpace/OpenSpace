/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/touch/touchmodule.h>
#include <modules/touch/include/TuioEar.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/settingsengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>

#include <ghoul/logging/logmanager.h>
#include <glm/ext.hpp>

#include <sstream>
#include <string>
#include <iostream>
#include <thread>         // std::this_thread::sleep_for
#include <chrono>         // std::chrono::seconds

namespace {
	const std::string _loggerCat = "TouchModule";
}

namespace openspace {

	TuioEar TouchModule::*ear;

TouchModule::TouchModule()
    : OpenSpaceModule("Touch")
{

	OsEng.registerModuleCallback(
		OpenSpaceEngine::CallbackOption::Initialize,
		[&]() {
		LDEBUGC("TouchModule", "Initializing TuioEar");
		ear = new TuioEar();
	}
	);
	
	OsEng.registerModuleCallback(
		OpenSpaceEngine::CallbackOption::Deinitialize,
		[&]() {
		LDEBUGC("TouchModule", "Deinitialize TuioEar");
		delete ear;
	}
	);
	
	OsEng.registerModuleCallback( // maybe call ear->clearInput() here rather than postdraw
		OpenSpaceEngine::CallbackOption::PreSync,
		[&]() {
		//std::this_thread::sleep_for(std::chrono::seconds(1));
		list = ear->getInput();
		ear->clearInput();

		Camera* cam = OsEng.interactionHandler().camera();
		glm::vec3 pos = cam->positionVec3();
		glm::vec3 focusDir = glm::normalize(glm::vec3(cam->focusPositionVec3()) - pos);

		glm::vec2 centroid;
		float distance = 0.0f;
		float lastDistance = 0.0f;
		float zoomFactor = 0.0f;

		if (list.size() > 0) { // sanity check, no need to process if no input
			if (list.size() > 1 && list.size() == lastList.size()) { // calculate centroid if we have multiple IDs
				centroid.x = std::accumulate(list.begin(), list.end(), 0.0f, [](float x, const TuioCursor& c) { return x + c.getX(); }) / list.size();
				centroid.y = std::accumulate(list.begin(), list.end(), 0.0f, [](float y, const TuioCursor& c) { return y + c.getY(); }) / list.size();




				// ------- testing, should use more than just one point in lastList later on
				distance = std::accumulate(list.begin(), list.end(), 0.0f, [&centroid](float d, const TuioCursor& c) { 
					return d + sqrt(pow(c.getX() - centroid.x,2) + pow(c.getY() - centroid.y,2)); 
				});
				lastDistance = std::accumulate(lastList.begin(), lastList.end(), 0.0f, [&centroid](float d, const TuioCursor& c) {
					return d + sqrt(pow(c.getX() - centroid.x, 2) + pow(c.getY() - centroid.y, 2));
				});
				zoomFactor = distance - lastDistance; // should be dependant on screen size, distance from focusNode
				zoomFactor *= glm::distance(pos, glm::vec3(cam->focusPositionVec3()));

				std::cout << "Distance: " << distance << ", Last Distance: " << lastDistance << ", zoomFactor: " << zoomFactor 
					<< ", pos: " << glm::to_string(pos) << ", focusDir: " << glm::to_string(focusDir) << "\n";

				glm::vec3 newPos = pos + focusDir*zoomFactor;
				cam->setPosition(newPos);
			}
			else if (lastList.size() > 0) {
				// do new rotation
				float x = list.at(0).getX() - lastList.at(0).getX();
				float y = list.at(0).getY() - lastList.at(0).getY();

				glm::quat rot;
			
				//cam->rotate(rot);
			}
			// ----------------


			std::ostringstream os; // for debugging

			for (const TuioCursor &j : list) { // go through each item
				std::list<TuioPoint> path = j.getPath();
				std::vector<TuioCursor>::iterator it = find_if(
					lastList.begin(),
					lastList.end(),
					[&j](const TuioCursor& c) { return c.getSessionID() == j.getSessionID(); }
				);
				TuioTime lastTime;
				if (it != lastList.end()) // sanity check, if first element id wont be found in lastList
					lastTime = it->getPath().back().getTuioTime();

				std::list<TuioPoint>::iterator lastPoint = find_if(
					path.begin(),
					path.end(),
					[&lastTime](const TuioPoint& c) { return lastTime == c.getTuioTime();  });

				int count = 0;
				for (; lastPoint != path.end(); ++lastPoint) // here we can access all elements that are to be processed
					count++;

				os << ", Id: " << j.getCursorID() << ", path size: " << j.getPath().size() << ", (" << j.getX() << "," << j.getY() << "), To Process: " << count;
			}
			//LINFO("List size: " << list.size() << os.str() << "\n");
			os.clear();

			glm::mat4 t;
			
		}
		lastList = list;
	}
	);

	OsEng.registerModuleCallback(
		OpenSpaceEngine::CallbackOption::PostDraw,
		[&]() {
		WindowWrapper& wrapper = OsEng.windowWrapper();
		if (OsEng.isMaster() && wrapper.isRegularRendering()) {
			
		}
	}
	);
	
}

} // namespace openspace
