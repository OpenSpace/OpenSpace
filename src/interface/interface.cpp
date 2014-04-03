/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#include <interface/interface.h>

#include <sgct.h>

#include <boost/property_tree/json_parser.hpp>
#include <boost/foreach.hpp>

#include <iterator>

namespace openspace {

Interface::Interface() {}
Interface::~Interface() {}

void Interface::callback(const char* receivedChars) {
	boost::property_tree::ptree pt;
	std::stringstream input(receivedChars);
	boost::property_tree::json_parser::read_json(input, pt);

//	handleNodes(pt);

	auto it = pt.begin();
	while (it != pt.end()) {

		std::string key = (*it).first;
		auto data = (*it).second;

		if (data.size() == 0) { // JSON Single value
			if (strcmp(key.c_str(), "stats") == 0)
				sgct::Engine::instance()->setDisplayInfoVisibility(atoi(pt.get<std::string>(key).c_str()));
			else if (strcmp(key.c_str(), "graph") == 0)
				sgct::Engine::instance()->setStatsGraphVisibility(atoi(pt.get<std::string>(key).c_str()));
			else if (strcmp(key.c_str(), "renderer") == 0){
				if (strcmp(pt.get<std::string>(key).c_str(), "volumeraycaster") == 0) {
//					_useVolumeRaycaster = true;
//					_useFlare = false;
				} else if (strcmp(pt.get<std::string>(key).c_str(), "flare") == 0) {
//					_useVolumeRaycaster = false;
//					_useFlare = true;
				}
			}


		} else { // JSON Array
			std::cout << key << " = { " << std::flush;
			BOOST_FOREACH(boost::property_tree::ptree::value_type &v, pt.get_child(key)) {
				assert(v.first.empty()); // array elements have no names
				std::cout << v.second.data() << " " << std::flush;
			}
			std::cout << "}" << std::endl;
		}

		it++;
	}

}

// Workd in progress on recursive node handler
void Interface::handleNodes(boost::property_tree::ptree pt, std::string key) {
	auto it = pt.begin();
	while (it != pt.end()) {
		std::string name = (*it).first;
		auto data = (*it).second;
		if (!name.empty())
			key = name;

		std::cout << key << " " << data.size() << " " << std::flush;

		if (data.size() == 0) { // leaf node
			std::cout << key << " : "<< data.data() << std::endl;
		} else {
			handleNodes(pt.get_child(key), key);
		}
		++it;
	}
}


} // namespace openspace

