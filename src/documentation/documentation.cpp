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

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

#include <set>

namespace {
    const std::string Wildcard = "*";

    // Structure used to make offences unique
    struct OffenceCompare {
        using Offence = openspace::documentation::TestResult::Offence;
        bool operator()(const Offence& lhs, const Offence& rhs) const
        {
            if (lhs.offender != rhs.offender) {
                return lhs.offender < rhs.offender;
            }
            else {
                return std::underlying_type_t<Offence::Reason>(lhs.reason) <
                    std::underlying_type_t<Offence::Reason>(rhs.reason);
            }
        }

    };
} // namespace

namespace std {
std::string to_string(std::string value) {
    return value; 
}
} // namespace std

namespace openspace {
namespace documentation {

SpecificationError::SpecificationError(TestResult result, std::string component)
    : ghoul::RuntimeError("Error in specification", std::move(component))
    , result(std::move(result)) {}


DocumentationEntry::DocumentationEntry(std::string key, Verifier* t, std::string doc,
                                       Optional optional)
    : key(std::move(key))
    , verifier(std::move(t))
    , documentation(std::move(doc))
    , optional(optional) {}

Documentation::Documentation(std::string name, DocumentationEntries entries, Exhaustive exhaustive)
    : name(std::move(name))
    , entries(std::move(entries))
    , exhaustive(std::move(exhaustive))
{}

Documentation::Documentation(DocumentationEntries entries)
    : Documentation("", std::move(entries)) {}

TestResult testSpecification(const Documentation& d, const ghoul::Dictionary& dictionary) {
    TestResult result;
    result.success = true;

    for (const auto& p : d.entries) {
        if (p.key == Wildcard) {
            for (const std::string& key : dictionary.keys()) {
                Verifier& verifier = *(p.verifier);
                TestResult res = verifier(dictionary, key);
                if (!res.success) {
                    result.success = false;
                    result.offenders.insert(
                        result.offenders.end(),
                        res.offenders.begin(),
                        res.offenders.end()
                    );
                }
            }
        }
        else {
            if (p.optional && !dictionary.hasKey(p.key)) {
                // If the key is optional and it doesn't exist, we don't need to check it
                // if the key exists, it has to be correct, however
                continue;
            }
            Verifier& verifier = *(p.verifier);
            TestResult res = verifier(dictionary, p.key);
            if (!res.success) {
                result.success = false;
                result.offenders.insert(
                    result.offenders.end(),
                    res.offenders.begin(),
                    res.offenders.end()
                );
            }
        }
    }

    if (d.exhaustive) {
        // If the documentation is exhaustive, we have to check if there are extra values
        // in the table that are not covered by the Documentation

        for (const std::string& key : dictionary.keys()) {
            auto it = std::find_if(
                d.entries.begin(),
                d.entries.end(),
                [&key](const DocumentationEntry& entry) {
                    if (entry.key == Wildcard) {
                        return true;
                    }
                    else {
                        return entry.key == key;
                    }
                }
            );

            if (it == d.entries.end()) {
                result.success = false;
                result.offenders.push_back(
                    { key, TestResult::Offence::Reason::ExtraKey }
                );
            }
        }
    }

    // Remove duplicate offenders that might occur if multiple rules apply to a single
    // key and more than one of these rules are broken
    std::set<TestResult::Offence, OffenceCompare> uniqueOffenders(
        result.offenders.begin(), result.offenders.end()
    );
    result.offenders = std::vector<TestResult::Offence>(
        uniqueOffenders.begin(), uniqueOffenders.end()
    );

    return result;
}

void testSpecificationAndThrow(const Documentation& doc,
    const ghoul::Dictionary& dictionary, std::string component)

{
    // Perform testing against the documentation/specification
    using namespace openspace::documentation;
    TestResult testResult = testSpecification(
        doc,
        dictionary
    );
    if (!testResult.success) {
        throw SpecificationError(std::move(testResult), std::move(component));
    }
}

} // namespace documentation
} // namespace openspace
