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

#ifndef __DOCUMENTATION_H__
#define __DOCUMENTATION_H__

#include <ghoul/misc/boolean.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>

#include <memory>
#include <string>
#include <vector>

namespace openspace {
namespace documentation {

struct Verifier;

using Optional = ghoul::Boolean;

struct TestResult {
    bool success;
    std::vector<std::string> offenders;
};

struct SpecificationError : public ghoul::RuntimeError {
    SpecificationError(TestResult result, std::string component);

    TestResult result;
};

struct DocumentationEntry {
    DocumentationEntry(std::string key, Verifier* t, std::string doc = "",
        Optional optional = Optional::No);

    std::string key;
    std::shared_ptr<Verifier> tester;
    bool optional;
    std::string documentation;
};

using DocumentationEntries = std::vector<documentation::DocumentationEntry>;

struct Documentation {
    Documentation(std::string name = "", DocumentationEntries entries = {});
    Documentation(DocumentationEntries entries);

    std::string name;
    DocumentationEntries entries;
};


TestResult testSpecification(const Documentation& d, const ghoul::Dictionary& dictionary);
void testSpecificationAndThrow(const Documentation& doc,
    const ghoul::Dictionary& dictionary, std::string component);

std::string generateDocumentation(const Documentation& d);

} // namespace documentation

using documentation::Documentation;

} // namespace openspace

#endif // __DOCUMENTATION_H__
