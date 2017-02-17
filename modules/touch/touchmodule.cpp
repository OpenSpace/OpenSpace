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
#include <vector>
#include <glm/glm.hpp>

namespace openspace {

	TuioEar TouchModule::*ear;

TouchModule::TouchModule()
    : OpenSpaceModule("Touch")
{

	OsEng.registerModuleCallback(
		OpenSpaceEngine::CallbackOption::Initialize,
		[]() {
		LDEBUGC("TouchModule", "Initializing TuioEar");
		ear = new TuioEar();
	}
	);

	OsEng.registerModuleCallback(
		OpenSpaceEngine::CallbackOption::Deinitialize,
		[]() {
		LDEBUGC("TouchModule", "Deinitialize TuioEar");
		delete ear;
	}
	);
	
	OsEng.registerModuleCallback(
		OpenSpaceEngine::CallbackOption::PostSyncPreDraw,
		[]() {
		WindowWrapper& wrapper = OsEng.windowWrapper();
		if (OsEng.isMaster() && wrapper.isRegularRendering()) {
			std::vector<TuioObject*> list = ear->getInput();
			std::vector<TuioObject*>::iterator it = list.begin();

			// step through the list (from the start) and find each unique id tuioobject
			std::vector<TuioObject*> group;
			
			for (auto &&i : list) {
				bool sameId = false;
				glm::vec2 centroid = glm::vec2(0.0f, 0.0f);
				if (i->containsTuioPointer()) {
					int id = i->getSessionID();
					for (auto &&j : group) // change to lambda/find function
						if (j->getSessionID() == id)
							sameId = true; // step out of for
					if (sameId) { // calculate a centroid
						for (auto &&j : group) {
							centroid.x += j->getTuioPointer()->getX();
							centroid.y += j->getTuioPointer()->getY();
						}
						centroid.x /= group.size();
						centroid.y /= group.size();
					}
					else
						group.push_back(i);


				}
			}

			// group 
		}
	}
	);

	OsEng.registerModuleCallback(
		OpenSpaceEngine::CallbackOption::PostDraw,
		[]() {
		WindowWrapper& wrapper = OsEng.windowWrapper();
		if (OsEng.isMaster() && wrapper.isRegularRendering()) {
			ear->clearInput();
		}
	}
	);
}

} // namespace openspace
