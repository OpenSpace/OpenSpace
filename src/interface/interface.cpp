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

#include <algorithm>

namespace openspace {

Interface::Interface(OpenSpaceEngine* engine) : _engine(engine) {}
Interface::~Interface() {}

void Interface::callback(const char* receivedChars) {
	std::cout << receivedChars;

	boost::property_tree::ptree pt;
	std::stringstream input(receivedChars);
	boost::property_tree::json_parser::read_json(input, pt);
	_nodes = std::vector<Node>();

	loadIntoNodes(pt);
	handleNodes(); // Issue commands
	_nodes.clear(); // Clean up after commands are issued
}

void Interface::handleNodes() {
	for (int i = 0; i < _nodes.size(); ++i) {
		Node node = _nodes.at(i);
		if (node == "stats") {
			sgct::Engine::instance()->setDisplayInfoVisibility(atoi(node._value.c_str()));
		} else if (node == "graph") {
			sgct::Engine::instance()->setStatsGraphVisibility(atoi(node._value.c_str()));
		} else if (node == "renderer") {
			if (strcmp(node._value.c_str(), "volumeraycaster") == 0)
				_engine->setRenderer(OpenSpaceEngine::Renderers::VolumeRaycaster);
			else if (strcmp(node._value.c_str(), "flare") == 0)
				_engine->setRenderer(OpenSpaceEngine::Renderers::Flare);
		}
	}
}

// http://duck-wrath.blogspot.com/2012/02/how-to-recursive-parse.html
void Interface::loadIntoNodes(const boost::property_tree::ptree& tree, std::string parent, const int depth) {
	BOOST_FOREACH( boost::property_tree::ptree::value_type const&v, tree.get_child("") ) {
		boost::property_tree::ptree subtree = v.second;
		std::string value = v.second.data();
		std::string key = v.first;

		// classify and store nodes
		if ( key.length() > 0 ) { // value
			_nodes.push_back(Node(key, value));
		} else { // array
			// Find parent and add to its children vector
			std::vector<Node>::iterator it = std::find(_nodes.begin(), _nodes.end(), Node(parent));
			if (it != _nodes.end()) {
				(*it)._children.push_back(Node(parent, value));
			} else {
				std::cout << "Parent not found" << std::endl;
			}
		}

		// recursive go down the hierarchy
		loadIntoNodes(subtree,key,depth+1);
	}
}

} // namespace openspace

