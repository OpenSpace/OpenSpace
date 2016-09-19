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

#ifndef __DOCUMENTATIONENGINE_H__
#define __DOCUMENTATIONENGINE_H__

#include <openspace/documentation/documentation.h>

#include <ghoul/designpattern/singleton.h>
#include <ghoul/misc/exception.h>

namespace openspace {
namespace documentation {

/**
 * The DocumentationEngine has the ability to collect all Documentation%s that are
 * produced in the application an write them out as a documentation file for human
 * consumption.
 */
class DocumentationEngine : public ghoul::Singleton<DocumentationEngine> {
public:
    struct DuplicateDocumentationException : public ghoul::RuntimeError {
        DuplicateDocumentationException(Documentation documentation);

        Documentation documentation;
    };

    /**
     * Write the collected Documentation%s to disk at the \p filename in the specified
     * \p type. A new file is created and silently overwritten in the location that
     * \p filename is pointed to.
     * \param filename The file that is to be created containing all the Documentation 
     * information.
     * \param type The type of documentation that is written. Currently allowed values are
     * \c text and \c html
     */
    void writeDocumentation(const std::string& filename, const std::string& type);

    /**
     * Adds the \p documentation to the list of Documentation%s that are written to a 
     * documentation file with the writeDocumentation method.
     * \param documentation The Documentation object that is to be stored for later use
     * \throws DuplicateDocumentationException If the \p documentation did not have a
     * unique identifier
     */
    void addDocumentation(Documentation documentation);

    static DocumentationEngine& ref();

private:
    /// The list of all Documentation%s that are stored by the DocumentationEngine
    std::vector<Documentation> _documentations;
};

} // namespace documentation
} // namespace openspace

#define DocEng (openspace::documentation::DocumentationEngine::ref())

#endif // __DOCUMENTATIONENGINE_H__
