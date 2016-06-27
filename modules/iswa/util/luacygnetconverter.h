/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __LUACYGNETCONVERTER_H__
#define __LUACYGNETCONVERTER_H__

/**
 * LuaCygnetConverter transforms the metadata given by the user (through openspace)
 * and the metadata downloaded from iswa (MetaDataFuture) to lua tables that will be 
 * used to add a cygnet to the scene.
 */
#include <memory> // for shared_pointer

namespace openspace {
class MetadataFuture;
class CdfInfo;

class LuaCygnetConverter {
public:
    LuaCygnetConverter();
    ~LuaCygnetConverter();

	std::string toLuaTable(std::shared_ptr<MetadataFuture> metadata) const;
	std::string toLuaTable(CdfInfo info, std::string cut) const;
	std::string toLuaTable(std::string name, std::string cdfPath, std::string seedPath) const;


private:

	std::string planeToLuaTable(std::shared_ptr<MetadataFuture> data) const;
	std::string sphereToLuaTable(std::shared_ptr<MetadataFuture> data) const;
	std::string kameleonToLuaTable(CdfInfo info, std::string cut) const;
	std::string fieldlineToLuaTable(std::string name, std::string cdfPath, std::string seedPath) const;
};

} //namespace openspace
#endif //__LUACYGNETCONVERTER_H__