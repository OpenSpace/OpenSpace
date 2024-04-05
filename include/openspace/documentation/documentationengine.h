/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_CORE___DOCUMENTATIONENGINE___H__
#define __OPENSPACE_CORE___DOCUMENTATIONENGINE___H__

#include <openspace/documentation/documentation.h>
#include <openspace/json.h>
#include <openspace/properties/propertyowner.h>
#include <ghoul/misc/exception.h>

namespace openspace::documentation {

/**
 * The DocumentationEngine has the ability to collect all Documentation%s that are
 * produced in the application an write them out as a documentation file for human
 * consumption.
 */
class DocumentationEngine {
public:
    /**
     * This exception is thrown by the addDocumentation method if a provided Documentation
     * has an identifier, but the identifier was registered previously.
     */
    struct DuplicateDocumentationException : public ghoul::RuntimeError {
        /**
         * Constructor of a DuplicateDocumentationException storing the offending
         * Documentation for later use.
         *
         * \param doc The Documentation whose identifier was previously registered
         */
        DuplicateDocumentationException(Documentation doc);

        /// The offending Documentation whose identifier was previously registered
        Documentation documentation;
    };

    DocumentationEngine();

    /**
     * Adds the \p documentation to the list of Documentation%s that are written to a
     * documentation file with the writeDocumentation method.
     *
     * \param documentation The Documentation object that is to be stored for later use
     *
     * \throw DuplicateDocumentationException If the \p documentation has a non-empty
     *        identifier and it was not unique
     */
    void addDocumentation(Documentation documentation);

    /**
     * Returns a list of all registered Documentation%s.
     *
     * \return A list of all registered Documentation%s
     */
    std::vector<Documentation> documentations() const;

    static void initialize();
    static void deinitialize();
    static bool isInitialized();

    /**
     * Returns a static reference to the main singleton DocumentationEngine.
     *
     * \return A static reference to the main singleton DocumentationEngine
     */
    static DocumentationEngine& ref();

    void writeDocumentation() const;

    nlohmann::json generateScriptEngineJson() const;
    nlohmann::json generateFactoryManagerJson() const;
    nlohmann::json generateKeybindingsJson() const;
    nlohmann::json generatePropertyOwnerJson(properties::PropertyOwner* owner) const;
    nlohmann::json generateLicenseGroupsJson() const;
    nlohmann::json generateLicenseListJson() const;
    nlohmann::json generateActionJson() const;
    nlohmann::json generateEventJson() const;

private:
    /// The list of all Documentation%s that are stored by the DocumentationEngine
    std::vector<Documentation> _documentations;

    static DocumentationEngine* _instance;
};
} // namespace openspace::documentation

#define DocEng (openspace::documentation::DocumentationEngine::ref())

#endif // __OPENSPACE_CORE___DOCUMENTATIONENGINE___H__
