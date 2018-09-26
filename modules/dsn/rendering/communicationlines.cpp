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


namespace openspace {
    constexpr const char* _loggerCat = "CommunicationLine";

	CommunicationLines::CommunicationLines(const ghoul::Dictionary& dictionary) 
		: Renderable(dictionary) {
	//lines between space and earth
		_dictionary = std::make_unique<ghoul::Dictionary>(dictionary);
		CommunicationLines::PrintSomething();
		
	}

	void CommunicationLines::PrintSomething() {

		std::ofstream file{ "dsnmodule.txt" };
		file << _myMessage << std::endl;
		file.close();
        LDEBUG(_myMessage);

	}

	void CommunicationLines::render(const RenderData& data, RendererTasks&) {

	}
	void CommunicationLines::initializeGL() {

	}
	void CommunicationLines::deinitializeGL() {

	}

	bool CommunicationLines::isReady() const {
		return true;
	}

	void CommunicationLines::update(const UpdateData& data) {

	}
} // namespace openspace


