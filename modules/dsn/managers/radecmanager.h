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
#ifndef __OPENSPACE_MODULE_DSN___RADECMANAGER___H__
#define __OPENSPACE_MODULE_DSN___RADECMANAGER___H__

#include <ghoul/misc/dictionary.h>
#include <ghoul/logging/logmanager.h>
#include <modules/dsn/managers/datafilehelper.h>
#include <modules/dsn/rendering/renderablesignals.h>


namespace openspace {

    class RadecManager {

    public:
       static double _ra;
       static double _dec;
       static double _range;

       /* A vector with all our datafile paths*/
       static std::vector<std::string> _dataFiles;
       /* Extracts all the mandatory information we need from our asset file */
       static bool extractMandatoryInfoFromDictionary(const char* identifier, std::unique_ptr<ghoul::Dictionary> &dictionary);
       /*gets the correct datafile, that matches the current time in open space*/
       static glm::vec3 GetPosForTime(double time);
       /* parses positioningdata from a file given an index in our preloaded filename vector */
       static bool radecParser(int index);
    };
}


#endif 
