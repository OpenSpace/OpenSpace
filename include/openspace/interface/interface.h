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

#ifndef INTERFACE_H_
#define INTERFACE_H_
#include <openspace/engine/openspaceengine.h>

#include <boost/property_tree/ptree.hpp>
#include <openspace/engine/openspaceengine.h>
#include <vector>

namespace openspace {
class Interface {
	struct Node {
		std::string _key;
		std::string _value;
		std::vector<Node> _children;
		Node(std::string key, std::string value) {
			_key = key;
			_value = value;
			_children = std::vector<Node>();
		}
		Node(std::string key) {
			_key = key;
			_value = "";
			_children = std::vector<Node>();
		}

		inline bool operator==(const Node& rhs){
			return (strcmp(_key.c_str(), rhs._key.c_str()) == 0);
		}
		inline bool operator==(const std::string& rhs){
			return (strcmp(_key.c_str(), rhs.c_str()) == 0);
		}
	};

public:
	Interface(OpenSpaceEngine* engine);
	~Interface();

	void callback(const char * receivedChars);

private:

	void handleNodes();
	void loadIntoNodes(const boost::property_tree::ptree& tree, std::string parent = "", const int depth = 0);

    //	OpenSpaceEngine* _engine;
	std::vector<Node> _nodes;
};

} // namespace openspace

#endif /* INTERFACE_H_ */
