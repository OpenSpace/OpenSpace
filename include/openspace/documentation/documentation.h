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

#ifndef __OPENSPACE_CORE___DOCUMENTATION___H__
#define __OPENSPACE_CORE___DOCUMENTATION___H__

#include <ghoul/misc/boolean.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>

#include <memory>
#include <string>
#include <vector>

namespace openspace {
namespace documentation {

using Optional = ghoul::Boolean;
using Exhaustive = ghoul::Boolean;

/**
 * The TestResult structure returns the information from the #testSpecification method. It
 * contains the information whether test specification test was successful
 * (TestResult::success) and a list of TestResult::Offense%s (TestResult::offenses). If
 * TestResult::success is true, TestResult::offenses is guaranteed to be empty.
 */
struct TestResult {
    /**
     * An Offense is a violation against a specific verifier. The Offense::offender is the
     * key that caused the offense (in the case of nested tables, it will be fully 
     * qualified identifier) and the Offense::Reason is the reason that caused the
     * offense.
     */
    struct Offense {
        /**
         * The Reason for the offense
         */
        enum class Reason {
            MissingKey,       ///< The offending key that was requested was not found
            ExtraKey,         ///< The exhaustive documentation contained an extra key
            WrongType,        ///< The key's value was not of the expected type
            Verification,     ///< The value did not pass a necessary non-type verifier
            UnknownIdentifier ///< If the identifier for a ReferencingVerifier did not exist
        };
        /// The offending key that caused the Offense. In the case of a nested table,
        /// this value will be the fully qualified name of the key
        std::string offender;
        /// The Reason that caused this offense
        Reason reason;
    };
    
    /**
     * A warning is some value that that does not exactly adhere to the specification, but
     * that also does not violate so badly to warrant an Offense. This, for example, could
     * be that a value is marked deprecated and should not be used anymore as the value
     * might be removed in a latter version.
     */
    struct Warning {
        /**
         * The reason for the warning
         */
        enum class Reason {
            Deprecated          ///< The value is marked as deprecated and should not used
        };

        /// The offending key that caused the Warning. In the case of a nested table,
        /// this value will be the fully qualified name of the key
        std::string offender;
        /// The Reason that caused this Warning
        Reason reason;
    };


    /// Is \c true if the TestResult is positive, \c false otherwise
    bool success;
    /// Contains a list of offenses that were found in the test. Is empty if
    /// TestResult::Success is \c true
    std::vector<Offense> offenses;
    /// Contains a list of warnings that were found in the test
    std::vector<Warning> warnings;
};

/**
 * This exception is thrown by the #testSpecificationAndThrow method if the test detected
 * a specification violation. This class contains the TestResult that would have otherwise
 * be returned in a call to #testSpecification.
 */
struct SpecificationError : public ghoul::RuntimeError {
    /**
     * Creates the SpecificationError exception instance.
     * \param result The offending TestResult that is passed on
     * \param component The component that initiated the specification test
     * \pre \p result%'s TestResult::success must be \c false
     */
    SpecificationError(TestResult result, std::string component);

    /// The TestResult that caused the SpecificationError to be thrown
    TestResult result;
};

struct Verifier;

/**
 * A DocumentationEntry provides the specification for a single key, which is tested using
 * the provided Verifier. Each DocumentationEntry can contain a textual documentation that
 * describes the entry and is printed when the documentation for a Documentation is
 * requested. Lastly, each DocumentationEntry can be Optional. If the provided key is the
 * DocumentationEntry::Wildcard, any key in the containing Documentation will be tested
 * against the provided verifier. The most convenient way of creating DocumentationEntry%s
 * is by using an inline initializer list such as:
 *\verbatim
DocumentationEntry e = { "key", new IntVerifier, "Documentation text", Optional::Yes };
\endverbatim

 * Furthermore, these initializer lists can be crated all at once for a Documentation.
 * Even if the Verifier%s are specified using the \c new operators, they will not leak
 * memory as the DocumentationEntry takes ownership of them in the constructor.
 */
struct DocumentationEntry {
    /// The wildcard character that will match against every key in a Documentation
    static const std::string Wildcard;

    /**
     * The constructor for a DocumentationEntry describing a \p key in a Documentation.
     * The value for the key (or each value in the case of the 
     * DocumentationEntry::Wildcard) is tested using the \p verifier, that specifies the 
     * conditions that the \p key%'s value has to fulfill. The textual documentation
     * \p doc shall describe the usage of the key-value pair and will be printed for human
     * consumption for example in the DocumentationEngine. Each DocumentationEntry can
     * further be \p optional.
     * \param key The key for which this DocumentationEntry is valid. If this valid is
     * equal to DocumentationEntry::Wildcard, each entry in the Documentation that
     * contains this DocumentationEntry will be matched
     * \param verifier The Verifier that is used to test the \p key%'s value to determine
     * if it is a valid value
     * \param doc The textual documentation that describes the DocumentationEntry in a
     * human readable format
     * \param optional Determines whether the Documentation containing this
     * DocumentationEntry must have a key \p key, or whether it is optional
     * \pre \p key must not be empty
     * \pre \p verifier must not be nullptr
     */
    DocumentationEntry(std::string key, std::shared_ptr<Verifier> verifier,
                       std::string doc = "", Optional optional = Optional::No);

    /**
    * The constructor for a DocumentationEntry describing a \p key in a Documentation.
    * The value for the key (or each value in the case of the
    * DocumentationEntry::Wildcard) is tested using the \p verifier, that specifies the
    * conditions that the \p key%'s value has to fulfill. The textual documentation
    * \p doc shall describe the usage of the key-value pair and will be printed for human
    * consumption for example in the DocumentationEngine. Each DocumentationEntry can
    * further be \p optional.
    * \param key The key for which this DocumentationEntry is valid. If this valid is
    * equal to DocumentationEntry::Wildcard, each entry in the Documentation that
    * contains this DocumentationEntry will be matched
    * \param verifier The Verifier that is used to test the \p key%'s value to determine
    * if it is a valid value. The DocumentationEntry will take ownership of the passed
    * object
    * \param doc The textual documentation that describes the DocumentationEntry in a
    * human readable format
    * \param optional Determines whether the Documentation containing this
    * DocumentationEntry must have a key \p key, or whether it is optional
    * \pre \p key must not be empty
    * \pre \p verifier must not be nullptr
    */
    DocumentationEntry(std::string key, Verifier* verifier, std::string doc = "",
        Optional optional = Optional::No);

    /// The key that is described by this DocumentationEntry
    std::string key;
    /// The Verifier that is used to test the key's value
    std::shared_ptr<Verifier> verifier;
    /// Determines whether the described DocumentationEntry is optional or not
    Optional optional;
    /// The textual description of this DocumentationEntry
    std::string documentation;
};


/**
 * This struct contains the documentation and specification for a ghoul::Dictionary. It is
 * used to impose restrictions on keys and values and determine whether a given
 * ghoul::Dictionary adheres to these specifications (see #testSpecification and
 * #testSpecificationAndThrow methods). Each Documentation consists of a human-readable
 * \c name, a list of DocumentationEntry%s that each describe a single key value, and a
 * flag whether these entries are Exhaustive or not. If a Documentation is Exhaustive, a
 * ghoul::Dictionary that contains additional keys will fail the specification, whereas a
 * non-exhaustive Documentation allow for other (potentially non used) keys. The most
 * convenient way of creating a Documentation is by using nested initializer lists:
 *\verbatim
Documentation doc = {
    "Documentation for an arbitrary dictionary",
    { // A list of DocumentationEntry%s; also specified using initializer lists
        { "key1", new IntVerifier, "Documentation key1", Optional::Yes },
        { "key2", new FloatVerifier, "Documentation key2" },
        { "key3", new StringVerifier } 
    },
    Exhaustive::Yes
+;
\endverbatim
 *
 * If multiple DocumentationEntries cover the same key, they are all evaluated for that
 * specific key. The same holds true if there is a DocumentationEntry with a
 * DocumentationEntry::Wildcard and a more specialized DocumentationEntry. In this case,
 * both the wildcard and the specialized entry will be evaluated.
 */
struct Documentation {
    using DocumentationEntries = std::vector<documentation::DocumentationEntry>;

    /**
     * Creates a Documentation with a human-readable \p name and a list of \p entries.
     * \param name The human-readable name of this Documentation
     * \param id A unique identifier which can be used by applications (or other
     * Documentation%s to reference this entry
     * \param entries A list of DocumentationEntry%s that describe the individual keys for
     * this entrie Documentation
     * \param exhaustive Determines whether the \p entries are an exhaustive specification
     * of the object or whether additional, potentially unused, keys are allowed
     */
    Documentation(std::string name, std::string id, DocumentationEntries entries = {},
        Exhaustive exhaustive = Exhaustive::No);

    /**
    * Creates a Documentation with a human-readable \p name.
    * \param name The human-readable name of this Documentation
    * \param entries A list of DocumentationEntry%s that describe the individual keys for
    * this entrie Documentation
    * \param exhaustive Determines whether the \p entries are an exhaustive specification
    * of the object or whether additional, potentially unused, keys are allowed
    */
    Documentation(std::string name, DocumentationEntries entries = {},
        Exhaustive exhaustive = Exhaustive::No);

    /**
    * Creates a Documentation.
    * \param entries A list of DocumentationEntry%s that describe the individual keys for
    * this entrie Documentation
    * \param exhaustive Determines whether the \p entries are an exhaustive specification
    * of the object or whether additional, potentially unused, keys are allowed
    */
    Documentation(DocumentationEntries entries = {}, Exhaustive exhaustive = Exhaustive::No);

    /// The human-readable name of the Documentation
    std::string name;
    /// A unique identifier which can be used to reference this Documentation
    std::string id;
    /// A list of specifications that are describing this Documentation
    DocumentationEntries entries;
    /// A flag to say wheter the DocumentationEntries are an exhaustive description
    Exhaustive exhaustive;
};

/**
 * This method tests whether a provided ghoul::Dictionary \p dictionary adheres to the
 * specification \p documentation and returns its result as a TestResult. The TestResult
 * will contain whether the \p dictionary adheres to the \p documentation and, in
 * addition, the list of all offending keys together with the reason why they are
 * offending.
 * \param documentation The Documentation that the \p dictionary is tested against
 * \param dictionary The ghoul::Dictionary that is to be tested against the
 * \p documentation
 * \return A TestResult that contains the results of the specification testing
 */
TestResult testSpecification(const Documentation& documentation,
    const ghoul::Dictionary& dictionary);

/**
* This method tests whether a provided ghoul::Dictionary \p dictionary adheres to the
* specification \p documentation. If the \p dictionary does not adhere to the
* specification a SpecificationError is thrown, and the exception contains the TestResult
* that contains more information about the offending keys. If the \p dictionary adheres to
* the \p documentation, the method returns normally.
* \param documentation The Documentation that the \p dictionary is tested against
* \param dictionary The ghoul::Dictionary that is to be tested against the
* \p documentation
* \param component The component that is using this method; this argument is passed to the
* SpecificationError that is thrown in case of not adhering to the \p documentation
* \throw SpecificationError If the \p dictionary does not adhere to the \p documentation
*/
void testSpecificationAndThrow(const Documentation& documentation,
    const ghoul::Dictionary& dictionary, std::string component);

} // namespace documentation

// We want to make it easier for people to use it, so we pull the Documentation class into
// the openspace namespace
using documentation::Documentation;

} // namespace openspace

// Make the overload for std::to_string available for the Offense::Reason for easier
// error logging

namespace std {

std::string to_string(openspace::documentation::TestResult::Offense::Reason reason);
std::string to_string(openspace::documentation::TestResult::Warning::Reason reason);

} // namespace std

#endif // __OPENSPACE_CORE___DOCUMENTATION___H__
