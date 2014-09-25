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

#ifndef __SHADERCREATOR_H__
#define __SHADERCREATOR_H__


#include <ghoul/opengl/programobject.h>

#include <string>

namespace openspace {
class ShaderCreator {

public:
	ShaderCreator();
	~ShaderCreator();

	void createSourceFile(bool b);
	void sourceFileExtension(const std::string& extension);
	void sourceFileHeader(const std::string& header);

	ghoul::opengl::ProgramObject* buildShader(const std::string& name, const std::string& vpath, const std::string& fpath, const std::string& gpath = "");

private:

	void _generateSource(const std::string& filename);
	std::string _loadSource(const std::string& filename, unsigned int depth = 0);
	std::string _generateFilename(const std::string& filename);

	bool _createSourceFile;
	std::string _sourceFileExtension;
	std::string _sourceFileHeader;
	unsigned int _maxDepth;

};
}

#endif