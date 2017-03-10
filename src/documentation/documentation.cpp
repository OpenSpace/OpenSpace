/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <ghoul/misc/dictionary.h>

#include <set>

namespace {
    // Structure used to make offenses unique
    struct OffenseCompare {
        using Offense = openspace::documentation::TestResult::Offense;
        bool operator()(const Offense& lhs, const Offense& rhs) const {
            if (lhs.offender != rhs.offender) {
                return lhs.offender < rhs.offender;
            }
            else {
                return std::underlying_type_t<Offense::Reason>(lhs.reason) <
                    std::underlying_type_t<Offense::Reason>(rhs.reason);
            }
        }
    };

    struct WarningCompare {
        using Warning = openspace::documentation::TestResult::Warning;
        bool operator()(const Warning& lhs, const Warning& rhs) const {
            if (lhs.offender != rhs.offender) {
                return lhs.offender < rhs.offender;
            }
            else {
                return std::underlying_type_t<Warning::Reason>(lhs.reason) <
                    std::underlying_type_t<Warning::Reason>(rhs.reason);
            }
        }
    };

} // namespace

// Unfortunately, the standard library does not contain a no-op for the to_string method
// so we have to include one ourselves
namespace std {
std::string to_string(std::string value) {
    return value; 
}

std::string to_string(openspace::documentation::TestResult::Offense::Reason reason) {
    switch (reason) {
        case openspace::documentation::TestResult::Offense::Reason::ExtraKey:
            return "Extra key";
        case openspace::documentation::TestResult::Offense::Reason::MissingKey:
            return "Missing key";
        case openspace::documentation::TestResult::Offense::Reason::UnknownIdentifier:
            return "Unknown identifier";
        case openspace::documentation::TestResult::Offense::Reason::Verification:
            return "Verification failed";
        case openspace::documentation::TestResult::Offense::Reason::WrongType:
            return "Wrong type";
        default:
            ghoul_assert(false, "Missing case label");
    }
}
    
std::string to_string(openspace::documentation::TestResult::Warning::Reason reason) {
    switch (reason) {
        case openspace::documentation::TestResult::Warning::Reason::Deprecated:
            return "Deprecated";
        default:
            ghoul_assert(false, "Missing case label");
    }
}

} // namespace std

namespace openspace {
namespace documentation {

const std::string DocumentationEntry::Wildcard = "*";

SpecificationError::SpecificationError(TestResult res, std::string component)
    : ghoul::RuntimeError("Error in specification", std::move(component))
    , result(std::move(res))
{
    ghoul_assert(!result.success, "Result's success must be false");
    
    message += " (";
    for (const TestResult::Offense& o : result.offenses) {
        message += o.offender + ',';
    }
    message.back() = ')';
}

DocumentationEntry::DocumentationEntry(std::string k, std::shared_ptr<Verifier> v,
                                       std::string doc, Optional opt)
    : key(std::move(k))
    , verifier(std::move(v))
    , documentation(std::move(doc))
    , optional(opt)
{
    ghoul_assert(!key.empty(), "Key must not be empty");
    ghoul_assert(verifier, "Verifier must not be nullptr");
}

DocumentationEntry::DocumentationEntry(std::string key, Verifier* v, std::string doc,
                                       Optional optional)
    : DocumentationEntry(std::move(key), std::shared_ptr<Verifier>(v), std::move(doc),
                         optional)
{}

Documentation::Documentation(std::string n, std::string id, DocumentationEntries entries,
                             Exhaustive exh)
    : name(std::move(n))
    , id(std::move(id))
    , entries(std::move(entries))
    , exhaustive(std::move(exh))
{}

Documentation::Documentation(std::string n, DocumentationEntries entries, Exhaustive exh)
    : Documentation(n, "", entries, exh)
{}

Documentation::Documentation(DocumentationEntries entries, Exhaustive exh)
    : Documentation("", "", entries, exh)
{}

TestResult testSpecification(const Documentation& d, const ghoul::Dictionary& dict) {
    TestResult result;
    result.success = true;

    auto applyVerifier = [dict, &result](Verifier& verifier, const std::string& key) {
        TestResult res = verifier(dict, key);
        if (!res.success) {
            result.success = false;
            result.offenses.insert(
                result.offenses.end(),
                res.offenses.begin(),
                res.offenses.end()
            );
        }
        result.warnings.insert(
            result.warnings.end(),
            res.warnings.begin(),
            res.warnings.end()
        );
    };

    for (const auto& p : d.entries) {
        if (p.key == DocumentationEntry::Wildcard) {
            for (const std::string& key : dict.keys()) {
                applyVerifier(*(p.verifier), key);
            }
        }
        else {
            if (p.optional && !dict.hasKey(p.key)) {
                // If the key is optional and it doesn't exist, we don't need to check it
                // if the key exists, it has to be correct, however
                continue;
            }
            applyVerifier(*(p.verifier), p.key);
        }
    }

    if (d.exhaustive) {
        // If the documentation is exhaustive, we have to check if there are extra values
        // in the table that are not covered by the Documentation

        for (const std::string& key : dict.keys()) {
            auto it = std::find_if(
                d.entries.begin(),
                d.entries.end(),
                [&key](const DocumentationEntry& entry) {
                    if (entry.key == DocumentationEntry::Wildcard) {
                        return true;
                    }
                    else {
                        return entry.key == key;
                    }
                }
            );

            if (it == d.entries.end()) {
                result.success = false;
                result.offenses.push_back(
                    { key, TestResult::Offense::Reason::ExtraKey }
                );
            }
        }
    }

    // Remove duplicate offenders that might occur if multiple rules apply to a single
    // key and more than one of these rules are broken
    std::set<TestResult::Offense, OffenseCompare> uniqueOffenders(
        result.offenses.begin(), result.offenses.end()
    );
    result.offenses = std::vector<TestResult::Offense>(
        uniqueOffenders.begin(), uniqueOffenders.end()
    );
    // Remove duplicate warnings. This should normally not happen, but we want to be sure
    std::set<TestResult::Warning, WarningCompare> uniqueWarnings(
        result.warnings.begin(), result.warnings.end()
    );
    result.warnings = std::vector<TestResult::Warning>(
        uniqueWarnings.begin(), uniqueWarnings.end()
    );

    std::sort(
        result.offenses.begin(),
        result.offenses.end(),
        [](const TestResult::Offense& lhs, const TestResult::Offense& rhs) {
            return OffenseCompare()(lhs, rhs);
        }
    );
    std::sort(
        result.warnings.begin(),
        result.warnings.end(),
        [](const TestResult::Warning& lhs, const TestResult::Warning& rhs) {
            return WarningCompare()(lhs, rhs);
        }
    );

    return result;
}

void testSpecificationAndThrow(const Documentation& doc, const ghoul::Dictionary& dict,
                               std::string component)
{
    // Perform testing against the documentation/specification
    TestResult testResult = testSpecification(doc, dict);
    if (!testResult.success) {
        throw SpecificationError(std::move(testResult), std::move(component));
    }
}

} // namespace documentation
} // namespace openspace
