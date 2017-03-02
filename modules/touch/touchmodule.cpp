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



bool TouchModule::gotNewInput() {
	// Get new input from listener
	list = ear->getInput();
	ear->clearInput();

	// Erase old input id's that no longer exists
	lastProcessed.erase(
		std::remove_if(
			lastProcessed.begin(),
			lastProcessed.end(),
			[this](const Point& point) {
		return std::find_if(
			list.begin(),
			list.end(),
			[&point](const TuioCursor& c) {
			return point.first == c.getSessionID();
		}
		) == list.end(); }),
		lastProcessed.end()
	);

	// Return true if we got new input
	if (list.size() == lastProcessed.size() && list.size() > 0) {
		for (Point& p : lastProcessed) {
			std::vector<TuioCursor>::iterator foundID = find_if(list.begin(), list.end(), [&p](const TuioCursor& c) { return c.getSessionID() == p.first; });
			if (p.second.getTuioTime() == foundID->getPath().back().getTuioTime())
				return false;
		}
		return true;
	}		
	else
		return false;
}

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
		if (OsEng.isMaster() && gotNewInput()) {
			//std::this_thread::sleep_for(std::chrono::seconds(1));

			Camera* cam = OsEng.interactionHandler().camera();
			glm::vec3 pos = cam->positionVec3();
			glm::vec3 focusDir = glm::normalize(glm::vec3(cam->focusPositionVec3()) - pos);

			glm::vec2 centroid;
			float distance = 0.0f;
			float lastDistance = 0.0f;
			float zoomFactor = 0.0f;

			if (list.size() > 1) { // calculate centroid if we have multiple IDs
				centroid.x = std::accumulate(list.begin(), list.end(), 0.0f, [](float x, const TuioCursor& c) { return x + c.getX(); }) / list.size();
				centroid.y = std::accumulate(list.begin(), list.end(), 0.0f, [](float y, const TuioCursor& c) { return y + c.getY(); }) / list.size();




				// ------- testing, should use more than just one point later on
				distance = std::accumulate(list.begin(), list.end(), 0.0f, [&centroid](float d, const TuioCursor& c) {
					return d + sqrt(pow(c.getX() - centroid.x, 2) + pow(c.getY() - centroid.y, 2));
				});
				lastDistance = std::accumulate(lastProcessed.begin(), lastProcessed.end(), 0.0f, [&centroid](float d, const Point& p) {
					return d + sqrt(pow(p.second.getX() - centroid.x, 2) + pow(p.second.getY() - centroid.y, 2));
				});
				zoomFactor = distance - lastDistance; // should be dependant on screen size, distance from focusNode
				zoomFactor *= glm::distance(pos, glm::vec3(cam->focusPositionVec3()));

				std::cout << "Distance: " << distance << ", Last Distance: " << lastDistance << ", zoomFactor: " << zoomFactor
					<< "\n";

				glm::vec3 newPos = pos + focusDir*zoomFactor;
				cam->setPosition(newPos);
			}
			else { // do new rotation, work with spherical coordinates. Orbit it both position and rotation, check OrbitInteractionMode in interactionmode
				//cam->rotate(rot);
			}
			// ----------------


			std::ostringstream os; // for debugging

			for (const TuioCursor &j : list) { // go through each item
				std::list<TuioPoint> path = j.getPath();
				
				TuioTime lastTime = find_if(
				lastProcessed.begin(),
				lastProcessed.end(),
				[&j](const Point& p) { return p.first == j.getSessionID(); }
				)->second.getTuioTime();
				
				std::list<TuioPoint>::iterator lastPoint = find_if(
				path.begin(),
				path.end(),
				[&lastTime](const TuioPoint& c) { return lastTime == c.getTuioTime();  });
				
				int count = 0;
				for (; lastPoint != path.end(); ++lastPoint) // here we can access all elements that are to be processed
					count++;

				os << ", Id: " << j.getCursorID() << ", path size: " << j.getPath().size() << ", (" << j.getX() << "," << j.getY() << "), To Process: " << count;

			}
			LINFO("List size: " << list.size() << os.str() << "\n");
			os.clear();

		}

		// update lastProcessed
		lastProcessed.clear();
		for (const TuioCursor& c : list) {
			lastProcessed.push_back(std::make_pair(c.getSessionID(), c.getPath().back()));
		}
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
