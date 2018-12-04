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

#ifndef __OPENSPACE_MODULE_ISWA___DATAPROCESSORTEXT___H__
#define __OPENSPACE_MODULE_ISWA___DATAPROCESSORTEXT___H__

#include <modules/iswa/util/dataprocessor.h>

namespace openspace {

class DataProcessorText : public DataProcessor {
public:
    DataProcessorText();
    virtual ~DataProcessorText();

    virtual std::vector<std::string> readMetadata(const std::string& data,
        glm::size3_t& dimensions) override;

    virtual void addDataValues(const std::string& data,
        properties::SelectionProperty& dataOptions) override;

    virtual std::vector<float*> processData(const std::string& data,
        properties::SelectionProperty& dataOptions, glm::size3_t& dimensions) override;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_ISWA___DATAPROCESSORTEXT___H__
