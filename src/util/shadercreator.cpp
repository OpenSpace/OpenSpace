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

#include <openspace/util/shadercreator.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/shaderobject.h>
#include <ghoul/filesystem/filesystem.h>

#include <boost/regex.hpp>
#include <iostream>
#include <fstream>
// #include <ifstream>
// #include <ofstream>

using ghoul::opengl::ProgramObject;
using ghoul::opengl::ShaderObject;

namespace {
	const std::string _loggerCat = "ShaderCreator";
	const std::string defaultSourceFileExtension = "OpenSpaceGenerated.glsl";
	const std::string separator = "//=====================================================================\n";
    const ShaderObject::ShaderType vsType = ShaderObject::ShaderType::ShaderTypeVertex;
    const ShaderObject::ShaderType fsType = ShaderObject::ShaderType::ShaderTypeFragment;
}

namespace openspace {
ShaderCreator::ShaderCreator(): 
	_createSourceFile(false), 
	_sourceFileExtension(defaultSourceFileExtension),
	_sourceFileHeader(""),
	_maxDepth(2)
{

}

ShaderCreator::~ShaderCreator() {

}

void ShaderCreator::createSourceFile(bool b) {
	_createSourceFile = b;
}

void ShaderCreator::sourceFileExtension(const std::string& extension) {
	if(extension != "") {
		_sourceFileExtension = extension;
	}
}

void ShaderCreator::sourceFileHeader(const std::string& header) {
	_sourceFileHeader = header;
}

ghoul::opengl::ProgramObject* ShaderCreator::buildShader(
	const std::string& name, const std::string& vpath, const std::string& fpath) 
{
	std::string vsPath = absPath(vpath);
    std::string fsPath = absPath(fpath);

    if( ! FileSys.fileExists(vsPath))
    	return nullptr;
    if( ! FileSys.fileExists(fsPath))
    	return nullptr;

    if(_createSourceFile) {
    	_generateSource(vsPath);
    	_generateSource(fsPath);

    	vsPath = _generateFilename(vsPath);
    	fsPath = _generateFilename(fsPath);
    }

    ProgramObject* po = new ProgramObject(name);
    ShaderObject*  vs = new ShaderObject(vsType, vsPath, name + " Vertex");
    ShaderObject*  fs = new ShaderObject(fsType, fsPath, name + " Fragment");
    po->attachObject(vs);
    po->attachObject(fs);
    if ( po->compileShaderObjects() && po->linkProgramObject())
        return po;

    // unsuccessful compilation, cleanup and return nullptr
    delete po;
    po = nullptr;
    return po;
}

void ShaderCreator::_generateSource(const std::string& filename) {
	std::string generatedSource = "";
	if(_sourceFileHeader != "") 
		generatedSource += separator + "// HEADER\n" + separator + _sourceFileHeader + "\n";
	generatedSource += _loadSource(filename);

	std::ofstream of(_generateFilename(filename));
	of << generatedSource;
	of.close();
}

std::string ShaderCreator::_loadSource(const std::string& filename, unsigned int depth) {
	std::string contents = "", line;
	std::ifstream f(filename);
	
	// Pre-allocate memory so the return string doesn't have to resize too often
	// f.seekg( 0, std::ios::end );
	// unsigned int fsize = f.tellg();
	// f.seekg( 0);
	// contents.reserve(fsize*3);
	// line.reserve(fsize*3);
	
	contents += "\n";
	contents += separator;
	contents += "// Filename: " + filename + "\n";
	// contents += "// Size: " + std::to_string(fsize) + "\n";
	contents += separator;
	if(depth > _maxDepth) {
		contents += "// TOO DEEP";
		return contents;
	}

	if( ! FileSys.fileExists(filename)){
		contents += "// FILE NOT FOUND\n";
		return contents;
	}

	boost::regex e1(R"(^\s*#include \"(.+)\"\s*)");
	boost::regex e2(R"(^\s*#include <(.+)>\s*)");
	while(std::getline(f, line)) {
		boost::smatch m;
		if(boost::regex_search(line, m, e1)) {
			std::string includeFilename = m[1];
			includeFilename = filename.substr(0, filename.find_last_of("/")+1) + includeFilename;
			line = _loadSource(includeFilename, depth + 1);
		} else if(boost::regex_search(line, m, e2)) {
			std::string includeFilename = m[1];
			line = _loadSource(absPath(includeFilename), depth + 1);
		}

		contents += line + "\n";
	}
	f.close();

	return contents;
}

std::string ShaderCreator::_generateFilename(const std::string& filename) {
	// If way of confirming and creating a directory this could be a good solution
	// to avoid cluttering in folders
	// if(_cacheFolder != "") {
	// 	size_t delimiter = filename.find_last_of("/");
	// 	std::string file = filename.substr(delimiter+1, filename.length() - delimiter);
	// 	std::string path = filename.substr(0, filename.find_last_of("/"));

	// 	return path + "/" + _cacheFolder + "/" + file + "." + _sourceFileExtension;
	// }

	return filename + "." + _sourceFileExtension;
}

}