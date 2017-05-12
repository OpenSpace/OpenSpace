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

using namespace TUIO;

namespace {
	const std::string _loggerCat = "TouchModule";
}

namespace openspace {

    // @COMMENT  What does this do?
	TuioEar TouchModule::*ear;

    // @COMMENT  Definining this globally makes it potentially dangerous.  Is it necessary or better to put into the class?
	TouchInteraction* touch;

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
		lastProcessed.end());

	// Tap
	if (list.size() == 0 && lastProcessed.size() == 0 && ear->tap()) {
		TuioCursor c = ear->getTap();
		list.push_back(c);
        // @COMMENT  You can use:
        // lastProcessed.emplace_back(c.getSessionID(), c.getPath().back());
		lastProcessed.push_back(std::make_pair(c.getSessionID(), c.getPath().back()));
		touch->tap();
		return true;
	}
	
	// Return true if we got new input
    // @COMMENT  You can use !list.empty()
	if (list.size() == lastProcessed.size() && list.size() > 0) {
		bool newInput = true;
        // @COMMENT  Why can you use for_each without std:: ?  It seems like there is a using namespace std somewhere
		for_each(lastProcessed.begin(), lastProcessed.end(), [this, &newInput](Point& p) {
			std::vector<TuioCursor>::iterator cursor = find_if(list.begin(), list.end(), [&p](const TuioCursor& c) { return c.getSessionID() == p.first; });
			double now = cursor->getPath().back().getTuioTime().getTotalMilliseconds();
			if (!cursor->isMoving())
				newInput = true;
			else if (p.second.getTuioTime().getTotalMilliseconds() == now)
				newInput = false;
		});
		return newInput;
	}		
	else
		return false;
}

TouchModule::TouchModule()
    : OpenSpaceModule("Touch")
{
    // @COMMENT  If TuioEar and TouchInteraction don't have any dependencies and if they
    // can be initialized immediately and you don't need to register a callback
    // Even better would be to not even use pointers but direct member instances instead
	OsEng.registerModuleCallback(
		OpenSpaceEngine::CallbackOption::Initialize,
		[&]() {
		LDEBUGC("TouchModule", "Initializing TuioEar");
		ear = new TuioEar();
		touch = new TouchInteraction();
		addPropertySubOwner(touch);
	}
	);

	OsEng.registerModuleCallback(
		OpenSpaceEngine::CallbackOption::Deinitialize,
		[&]() {
		LDEBUGC("TouchModule", "Deinitialize TuioEar");
		delete ear;
		delete touch;
	}
	);
	
	OsEng.registerModuleCallback(
		OpenSpaceEngine::CallbackOption::PreSync,
		[&]() {
		touch->setCamera(OsEng.interactionHandler().camera());
		touch->setFocusNode(OsEng.interactionHandler().focusNode());

		if (gotNewInput() && OsEng.windowWrapper().isMaster()) {
			touch->update(list, lastProcessed);
		}
		else if (list.size() == 0) {
			touch->clear();
		}

		// update lastProcessed
		lastProcessed.clear();
		for (const TuioCursor& c : list) {
            // @COMMENT  You can use:
            // lastProcessed.emplace_back(c.getSessionID(), c.getPath().back());
			lastProcessed.push_back(std::make_pair(c.getSessionID(), c.getPath().back()));
		}
		touch->unitTest();
		touch->step(OsEng.windowWrapper().deltaTime());
	}
	);
	
}

} // namespace openspace
