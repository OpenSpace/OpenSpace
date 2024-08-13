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

#include <openspace/documentation/documentation.h>

#include <openspace/documentation/verifier.h>
#include <ghoul/misc/dictionary.h>
#include <algorithm>
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

namespace ghoul {

template <>
std::string to_string(const openspace::documentation::TestResult& value) {
    using namespace openspace::documentation;

    if (value.success) {
        return "Success";
    }
    else {
        std::stringstream stream;
        stream << "Specification Failure. ";

        for (const TestResult::Offense& offense : value.offenses) {
            stream << std::format(" {}", ghoul::to_string(offense));
            if (!offense.explanation.empty()) {
                stream << std::format(" ({})", offense.explanation);
            }
            stream << '\n';
        }

        for (const TestResult::Warning& warning : value.warnings) {
            stream << std::format(" {}\n", ghoul::to_string(warning));
        }

        return stream.str();
    }
}

template <>
std::string to_string(const openspace::documentation::TestResult::Offense& value) {
    std::stringstream stream;
    stream << value.offender + ": " + ghoul::to_string(value.reason);

    if (!value.explanation.empty()) {
        stream << std::format(" ({})", value.explanation);
    }

    return stream.str();
}

template <>
std::string to_string(const openspace::documentation::TestResult::Offense::Reason& value)
{
    switch (value) {
        case openspace::documentation::TestResult::Offense::Reason::Unknown:
            return "Unknown";
        case openspace::documentation::TestResult::Offense::Reason::MissingKey:
            return "Missing key";
        case openspace::documentation::TestResult::Offense::Reason::UnknownIdentifier:
            return "Unknown identifier";
        case openspace::documentation::TestResult::Offense::Reason::Verification:
            return "Verification failed";
        case openspace::documentation::TestResult::Offense::Reason::WrongType:
            return "Wrong type";
        default:
            throw ghoul::MissingCaseException();
    }
}

template <>
std::string to_string(const openspace::documentation::TestResult::Warning& value) {
    return value.offender + ": " + ghoul::to_string(value.reason);
}

template <>
std::string to_string(const openspace::documentation::TestResult::Warning::Reason& value)
{
    switch (value) {
        case openspace::documentation::TestResult::Warning::Reason::Deprecated:
            return "Deprecated";
        default:
            throw ghoul::MissingCaseException();
    }
}

} // namespace ghoul

namespace openspace::documentation {

const std::string DocumentationEntry::Wildcard = "*";

SpecificationError::SpecificationError(TestResult res, std::string comp)
    : ghoul::RuntimeError("Error in specification", std::move(comp))
    , result(std::move(res))
{
    ghoul_assert(!result.success, "Result's success must be false");
}

void logError(const SpecificationError& error, std::string component) {
    if (error.result.success) {
        // Nothing to print if we succeeded
        return;
    }

    if (component.empty()) {
        LERRORC(error.component, error.message);
    }
    else {
        LERRORC(std::format("{}: {}", component, error.component), error.message);
    }
    for (const documentation::TestResult::Offense& o : error.result.offenses) {
        if (o.explanation.empty()) {
            LERRORC(ghoul::to_string(o.reason), o.offender);
        }
        else {
            LERRORC(
                ghoul::to_string(o.reason),
                std::format("{}: {}", o.offender, o.explanation)
            );
        }
    }
    for (const documentation::TestResult::Warning& w : error.result.warnings) {
        LWARNINGC(ghoul::to_string(w.reason), w.offender);
    }
}

DocumentationEntry::DocumentationEntry(std::string k, std::shared_ptr<Verifier> v,
                                       Optional opt, std::string doc)
    : key(std::move(k))
    , verifier(std::move(v))
    , optional(opt)
    , documentation(std::move(doc))
{
    ghoul_assert(!key.empty(), "Key must not be empty");
    ghoul_assert(verifier, "Verifier must not be nullptr");
}

DocumentationEntry::DocumentationEntry(std::string k, Verifier* v, Optional opt,
                                       std::string doc)
    : DocumentationEntry(std::move(k), std::shared_ptr<Verifier>(v), opt,
                         std::move(doc))
{}

TestResult testSpecification(const Documentation& documentation,
                             const ghoul::Dictionary& dictionary)
{
    TestResult result;
    result.success = true;

    auto applyVerifier = [dictionary, &result](const Verifier& verifier,
                                               const std::string& key)
    {
        TestResult res = verifier(dictionary, key);
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

    for (const auto& p : documentation.entries) {
        if (p.key == DocumentationEntry::Wildcard) {
            for (const std::string_view key : dictionary.keys()) {
                applyVerifier(*(p.verifier), std::string(key));
            }
        }
        else {
            if (p.optional && !dictionary.hasKey(p.key)) {
                // If the key is optional and it doesn't exist, we don't need to check it
                // if the key exists, it has to be correct, however
                continue;
            }
            if (!p.optional && !dictionary.hasKey(p.key)) {
                result.success = false;
                result.offenses.emplace_back(
                    p.key,
                    TestResult::Offense::Reason::MissingKey
                );
                continue;
            }
            applyVerifier(*(p.verifier), p.key);
        }
    }

    // Remove duplicate offenders that might occur if multiple rules apply to a single
    // key and more than one of these rules are broken
    const std::set<TestResult::Offense, OffenseCompare> uniqueOffenders(
        result.offenses.begin(), result.offenses.end()
    );
    result.offenses = std::vector<TestResult::Offense>(
        uniqueOffenders.begin(), uniqueOffenders.end()
    );
    // Remove duplicate warnings. This should normally not happen, but we want to be sure
    const std::set<TestResult::Warning, WarningCompare> uniqueWarnings(
        result.warnings.begin(), result.warnings.end()
    );
    result.warnings = std::vector<TestResult::Warning>(
        uniqueWarnings.begin(), uniqueWarnings.end()
    );

    return result;
}

void testSpecificationAndThrow(const Documentation& documentation,
                               const ghoul::Dictionary& dictionary, std::string component)
{
    // Perform testing against the documentation/specification
    const TestResult testResult = testSpecification(documentation, dictionary);
    if (!testResult.success) {
        throw SpecificationError(testResult, std::move(component));
    }
}

} // namespace openspace::documentation
