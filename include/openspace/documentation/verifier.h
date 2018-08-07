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

#ifndef __OPENSPACE_CORE___VERIFIER___H__
#define __OPENSPACE_CORE___VERIFIER___H__

#include <openspace/documentation/documentation.h>
#include <ghoul/glm.h>
#include <functional>
#include <type_traits>

namespace openspace::documentation {

/**
 * The base class of all Verifier%s. Each object must have an Verifier::operator()
 * overload, that performs the actual testing of the key inside the passed
 * ghoul::Dictionary and return a TestResult. The Verifier::type method returns a
 * human-readable representation of the type that is expected by the concret subclass of
 * Verifier. Furthermore, the Verifier::documentation method returns a human-readable
 * description of the Verifier subclass and what it tests for.
 */
struct Verifier {
    virtual ~Verifier() = default;

    /**
     * This method tests whether the \p key contained in the \p dictionary adheres to
     * whatever the concrete Verifer needs to test. The actual testing depends on the
     * concrete subclass and can range from type testing (for example IntVerifier or
     * StringVerifier) to more complex testing (for example DoubleInRangeVerifier or
     * TableVerifier).
     *
     * \param dictionary The dictionary that contains the \p key which is to be tested by
     *        this Verifier
     * \param key The key inside the \p dictionary that is to be tested
     * \return A TestResult struct that contains information about whether the key adheres
     *         to the demands of the specific Verifier. If it does not,
     *         TestResult::offenders will either contain \p key or, in the case of a
     *         TableVerifier, a list of all offending subkeys as fully qualified names.
     *
     * \post If the return values' TestResult::success is \c true, its
     *      TestResult::offenders is empty
     */
    virtual TestResult operator()(const ghoul::Dictionary& dictionary,
        const std::string& key) const = 0;

    /**
     * This method returns a human-readable string describing the type of object that is
     * handled by the Verifier subclass. This is only used for generating a human-readable
     * documentation and description of a Documenation object.
     *
     * \return A human-readable string describing the type of object for the Verifier
     *
     * \post The return value is not empty
     */
    virtual std::string type() const = 0;

    /**
     * This method returns a human-readable string describing the tests that the concrete
     * Verifier subclass implements. This is only used for generating a human-readable
     * documentation and description of a Documentation object.
     *
     * \return A human-readable string describing the tests that are performed by the
     *         Verifier
     *
     * \post The return value is not empty
     */
    virtual std::string documentation() const = 0;
};

//----------------------------------------------------------------------------------------
// General verifiers
//----------------------------------------------------------------------------------------

/**
 * The base class Verifier for all Verifier%s that have to test against a specific value
 * type. This Verifier tests whether a given key exists and whether it has the same type
 * as the template parameter \c T.
 * \tparam T The type against which the key's value is tested
 */
template <typename T>
struct TemplateVerifier : public Verifier {
    using Type = T;

    /**
     * Tests whether the \p key contained in the ghoul::Dictionary \p dictionary exists
     * and has the same type as \c T.
     *
     * \param dictionary The ghoul::Dictionary that contains the \p key to be tested
     * \param key The key inside the \p dictinoary that is to be tested
     * \return A TestResult that contains the information whether the \p key exists in the
     *         \p dictionary and whether the key's value's type agrees with \c T.
     *
     * \post The return values' TestResult::success is either \c true and
     *       TestResult::offenders is empty, or it is \c false and TestResult::offenders
     *       contains \p key
     */
    TestResult operator()(const ghoul::Dictionary& dictionary,
        const std::string& key) const override;

    std::string documentation() const override;
};

/**
 * A Verifier that checks whether a given key inside a ghoul::Dictionary is of type
 * \c bool. No implicit conversion is considered in this testing.
 */
struct BoolVerifier : public TemplateVerifier<bool> {
    std::string type() const override;
};

/**
 * A Verifier that checks whether a given key inside a ghoul::Dictionary is of type
 * \c double. No implicit conversion is considered in this testing.
 */
struct DoubleVerifier : public TemplateVerifier<double> {
    std::string type() const override;
};

/**
 * A Verifier that checks whether a given key inside a ghoul::Dictionary is of type
 * \c int. It will also return \c true if the key's value is of type \c double, but is a
 * integer value (for example, <code>0.0</code>, <code>12.0</code>, but not
 * <code>0.5</code>).
 */
struct IntVerifier : public TemplateVerifier<int> {
    TestResult operator()(const ghoul::Dictionary& dict,
        const std::string& key) const override;

    std::string type() const override;
};

/**
 * A Verifier that checks whether a given key inside a ghoul::Dictionary is of type
 * <code>std::string</code>. No implicit conversion is considered in this testing.
 */
struct StringVerifier : public TemplateVerifier<std::string> {
    std::string type() const override;
};

/**
 * A Verifier that checks whether a given key inside a ghoul::Dictionary is another
 * ghoul::Dictionary. The constructor takes a list of DocumentationEntry%s, which are used
 * recursively to check the contained table. If this list is empty, a simple type testing
 * is performed instead. If the testing finds any offending keys, it will return those
 * keys with fully qualified names, that is, the name of the table will be prepended to
 * the offending keys. Example: If the key \c Table is tested and a passed
 * DocumentationEntry checks for a nested key \c a and this does not comply, this Verifier
 * will return <code>Table.a</code> as an offender.
 */
struct TableVerifier : public TemplateVerifier<ghoul::Dictionary> {
    /**
     * This constructor takes a list of DocumentationEntry%s that are used recursively to
     * check the table (= ghoul::Dictionary) contained in the key's value. Similar to the
     * Documentation, these DocumentationEntry%s can be Exhaustive or not.
     *
     * \param documentationEntries The DocumentationEntry%s that are used to recursively
     *        test the ghoul::Dictionary that is contained inside. If this list is empty,
     *        only a type check is performed
     */
    TableVerifier(std::vector<DocumentationEntry> documentationEntries = {});

    /**
     * Checks whether the \p key%'s value is a table (= ghoul::Dictionary) and (if
     * provided) recursively checks whether the table adheres to the DocumentationEntry%s
     * provided in the constructor. If the testing finds any offending keys, it will
     * return those keys with fully qualified names, that is, the name of the table will
     * be prepended to the offending keys.
     *
     * \param dictionary The ghoul::Dictionary that is to be tested for the \p key
     * \param key The key for which the \p dictionary is tested
     * \return A TestResult containing the results of the testing. If DocumentationEntry%s
     *         were specified in the constructor and one of those values find an offending
     *         key inside the table, it's name will be returned with a fully qualified
     *         name by prepending the name (= \key) of the table.
     */
    TestResult operator()(const ghoul::Dictionary& dictionary,
        const std::string& key) const override;

    std::string type() const override;

    /// The documentations passed in the constructor
    std::vector<DocumentationEntry> documentations;
};

/**
 * A Verifier that checks whether all values contained in a Table are of type \c string.
 */
struct StringListVerifier : public TableVerifier {
    /**
     * Constructor for a StringListVerifier.
     *
     * \param elementDocumentation The documentation for each string in the list
     */
    StringListVerifier(std::string elementDocumentation = "");

    std::string type() const override;
};

/**
 * A Verifier that checks whether all values contained in a Table are of type \c int.
 */
struct IntListVerifier : public TableVerifier {
    /**
     * Constructor for a IntListVerifier.
     *
     * \param elementDocumentation The documentation for each string in the list
     */
    IntListVerifier(std::string elementDocumentation = "");

    std::string type() const override;
};

//----------------------------------------------------------------------------------------
// Vector verifiers
//----------------------------------------------------------------------------------------

/**
 * This struct is the base class for all Verifier%s that check for \c glm vector types.
 * The template parameter for the subclasses is the containing type, not the full vector
 * type. For example to check for <code>glm::dvec3</code>, one would create a
 * <code>Vector3Verifier<double></code>.
 */
struct VectorVerifier {};

/**
 * This Verifier checks whether the value is of type <code>glm::tvec2<T></code>
 */
template <typename T>
struct Vector2Verifier : public TemplateVerifier<glm::tvec2<T>>, public VectorVerifier {
    std::string type() const override;
};

/**
 * This Verifier checks whether the value is of type <code>glm::tvec3<T></code>
 */
template <typename T>
struct Vector3Verifier : public TemplateVerifier<glm::tvec3<T>>, public VectorVerifier {
    std::string type() const override;
};

/**
 * This Verifier checks whether the value is of type <code>glm::tvec4<T></code>
 */
template <typename T>
struct Vector4Verifier : public TemplateVerifier<glm::tvec4<T>>, public VectorVerifier {
    std::string type() const override;
};

/**
 * A Verifier that checks whether all values contained in a Table are of
 * type <code>glm::tvec2<T></code>
 */
template <typename T>
struct Vector2ListVerifier : public TableVerifier {
    Vector2ListVerifier(std::string elementDocumentation = "") : TableVerifier({
            { "*", new Vector2Verifier<T>, Optional::No, std::move(elementDocumentation) }
    })
    {}

    std::string type() const override {
        return "List of ints";
    }
};

/**
 * A Verifier that checks whether all values contained in a Table are of
 * type <code>glm::tvec3<T></code>
 */
template <typename T>
struct Vector3ListVerifier : public TableVerifier {
    Vector3ListVerifier(std::string elementDocumentation = "") : TableVerifier({
        { "*", new Vector3Verifier<T>, Optional::No, std::move(elementDocumentation) }
    })
    {}

    std::string type() const override {
        return "List of ints";
    }
};

/**
 * A Verifier that checks whether all values contained in a Table are of
 * type <code>glm::tvec4<T></code>
 */
template <typename T>
struct Vector4ListVerifier : public TableVerifier {
    Vector4ListVerifier(std::string elementDocumentation = "") : TableVerifier({
        { "*", new Vector4Verifier<T>, Optional::No, std::move(elementDocumentation) }
    })
    {}

    std::string type() const override {
        return "List of ints";
    }
};
//----------------------------------------------------------------------------------------
// Matrix verifiers
//----------------------------------------------------------------------------------------

/**
 * This struct is the base class for all Verifier%s that check for \c glm matrix types.
 * The template parameter for the subclasses is the containing type, not the full matrix
 * type. For example to check for <code>glm::dmat4x3</code>, one would create a
 * <code>Matrix4x3Verifier<double></code>.
 */
struct MatrixVerifier {};

/**
 * This Verifier checks whether the value is of type <code>glm::mat2x2<T></code>
 */
template <typename T>
struct Matrix2x2Verifier :
    public TemplateVerifier<glm::tmat2x2<T>>, public MatrixVerifier
{
    std::string type() const override;
};

/**
 * This Verifier checks whether the value is of type <code>glm::mat2x3<T></code>
 */
template <typename T>
struct Matrix2x3Verifier :
    public TemplateVerifier<glm::tmat2x3<T>>, public MatrixVerifier {
    std::string type() const override;
};

/**
 * This Verifier checks whether the value is of type <code>glm::mat2x4<T></code>
 */
template <typename T>
struct Matrix2x4Verifier :
    public TemplateVerifier<glm::tmat2x4<T>>, public MatrixVerifier {
    std::string type() const override;
};

/**
 * This Verifier checks whether the value is of type <code>glm::mat3x2<T></code>
 */
template <typename T>
struct Matrix3x2Verifier :
    public TemplateVerifier<glm::tmat3x2<T>>, public MatrixVerifier {
    std::string type() const override;
};

/**
 * This Verifier checks whether the value is of type <code>glm::mat3x3<T></code>
 */
template <typename T>
struct Matrix3x3Verifier :
    public TemplateVerifier<glm::tmat3x3<T>>, public MatrixVerifier {
    std::string type() const override;
};

/**
 * This Verifier checks whether the value is of type <code>glm::mat3x4<T></code>
 */
template <typename T>
struct Matrix3x4Verifier :
    public TemplateVerifier<glm::tmat3x4<T>>, public MatrixVerifier {
    std::string type() const override;
};

/**
 * This Verifier checks whether the value is of type <code>glm::mat4x2<T></code>
 */
template <typename T>
struct Matrix4x2Verifier :
    public TemplateVerifier<glm::tmat4x2<T>>, public MatrixVerifier {
    std::string type() const override;
};

/**
 * This Verifier checks whether the value is of type <code>glm::mat4x3<T></code>
 */
template <typename T>
struct Matrix4x3Verifier :
    public TemplateVerifier<glm::tmat4x3<T>>, public MatrixVerifier {
    std::string type() const override;
};

/**
 * This Verifier checks whether the value is of type <code>glm::mat4x4<T></code>
 */
template <typename T>
struct Matrix4x4Verifier :
    public TemplateVerifier<glm::tmat4x4<T>>, public MatrixVerifier {
    std::string type() const override;
};

//----------------------------------------------------------------------------------------
// Operator verifiers
//----------------------------------------------------------------------------------------

/**
 * This is the abstract base class of all binary operator-based verifiers. This class
 * takes two template parameters. The first is the Verifier that one would use to only
 * check for the type of the object, for example IntVerifier. The second argument is a
 * function object that has its <code>operator()</code> function overloaded and returns a
 * boolean value. In these cases, the \c std function objects <code>std::less</code>,
 * <code>std::equal_to</code>, etc are used.
 *
 * This verifier will apply the \c Operator to the stored value and the incoming value
 * (after type checking) and will check if the \c Operator returns \c true or \c false.
 * The incoming value is used as the first argument and the stored value as the second
 * argument to the \c Operator. If the type checking fails, the offense reason
 * TestResult::Offense::Reason::WrongType is returned. If the \c Operator fails, the
 * reason TestResult::Offense::Verification is returned instead.
 */
template <typename T, typename Operator>
struct OperatorVerifier : public T {
    /**
     * Constructor for an OperatorVerifier. As all operators need to compare the incoming
     * value to a stored value, we require the comparison \p value to be passed in here.
     * \param value The value against which the tested value is compared using the
     * \c Operator
     */
    OperatorVerifier(typename T::Type value);

    /**
     * First checks whether the \p dictionary contains the passed \p key and whether the
     * \p key%'s value is correct using the template paramater \c T as a verifier. Then,
     * the \p key%'s value is checked against the stored OperatorVerifier::value using the
     * \c Operator.
     *
     * \param dictionary The ghoul::Dictionary that contains the \p key to be tested
     * \param key The key inside the \p dictinoary that is to be tested
     * \return A TestResult containing the results of the specification testing. If the
     *         \p key%'s value has the wrong type, it will be added to the TestResult's
     *         offense list with the reason TestResult::Offense::Reason::WrongType; if the
     *         \c Operator returns false, it will be added with the reason
     *         TestResult::Offense::Verification instead.
     */
    TestResult operator()(const ghoul::Dictionary& dictionary,
        const std::string& key) const override;

    /// The stored value which is passed to the \c Operator as a second argument
    typename T::Type value;
};

/**
 * This Verifier checks whether the incoming value is strictly smaller than the stored
 * value. Due to the operator type restrictions, \c T cannot be a subclass of (or the same
 * as) BoolVerifier, StringVerifier, TableVerifier, or VectorVerifier.
 */
template <typename T>
struct LessVerifier : public OperatorVerifier<T, std::less<typename T::Type>> {
    static_assert(
        !std::is_base_of<BoolVerifier, T>::value,
        "T cannot be BoolVerifier"
    );
    static_assert(
        !std::is_base_of<StringVerifier, T>::value,
        "T cannot be StringVerifier"
    );
    static_assert(
        !std::is_base_of<TableVerifier, T>::value,
        "T cannot be TableVerifier"
    );
    static_assert(
        !std::is_base_of<VectorVerifier, T>::value,
        "T cannot be VectorVerifier"
    );

    using OperatorVerifier<T, std::less<typename T::Type>>::OperatorVerifier;

    std::string documentation() const override;

    using OperatorVerifier<T, std::less<typename T::Type>>::value;
};

/**
 * This Verifier checks whether the incoming value is smaller than or equal to the stored
 * value. Due to the operator type restrictions, \c T cannot be a subclass of (or the same
 * as) BoolVerifier, StringVerifier, TableVerifier, or VectorVerifier.
 */
template <typename T>
struct LessEqualVerifier : public OperatorVerifier<T, std::less_equal<typename T::Type>> {
    static_assert(
        !std::is_base_of<BoolVerifier, T>::value,
        "T cannot be BoolVerifier"
    );
    static_assert(
        !std::is_base_of<StringVerifier, T>::value,
        "T cannot be StringVerifier"
    );
    static_assert(
        !std::is_base_of<TableVerifier, T>::value,
        "T cannot be TableVerifier"
    );
    static_assert(
        !std::is_base_of<VectorVerifier, T>::value,
        "T cannot be VectorVerifier"
    );

    using OperatorVerifier<T, std::less_equal<typename T::Type>>::OperatorVerifier;

    std::string documentation() const override;

    using OperatorVerifier<T, std::less_equal<typename T::Type>>::value;
};

/**
 * This Verifier checks whether the incoming value is strictly greater than the stored
 * value. Due to the operator type restrictions, \c T cannot be a subclass of (or the same
 * as) BoolVerifier, StringVerifier, TableVerifier, or VectorVerifier.
 */
template <typename T>
struct GreaterVerifier : public OperatorVerifier<T, std::greater<typename T::Type>> {
    static_assert(
        !std::is_base_of<BoolVerifier, T>::value,
        "T cannot be BoolVerifier"
        );
    static_assert(
        !std::is_base_of<StringVerifier, T>::value,
        "T cannot be StringVerifier"
        );
    static_assert(
        !std::is_base_of<TableVerifier, T>::value,
        "T cannot be TableVerifier"
        );
    static_assert(
        !std::is_base_of<VectorVerifier, T>::value,
        "T cannot be VectorVerifier"
        );

    using OperatorVerifier<T, std::greater<typename T::Type>>::OperatorVerifier;

    std::string documentation() const override;

    using OperatorVerifier<T, std::greater<typename T::Type>>::value;
};

/**
 * This Verifier checks whether the incoming value is greater than or equal to the stored
 * value. Due to the operator type restrictions, \c T cannot be a subclass of (or the same
 * as) BoolVerifier, StringVerifier, TableVerifier, or VectorVerifier.
 */
template <typename T>
struct GreaterEqualVerifier : public OperatorVerifier<T,
                                                     std::greater_equal<typename T::Type>>
{
    static_assert(
        !std::is_base_of<BoolVerifier, T>::value,
        "T cannot be BoolVerifier"
    );
    static_assert(
        !std::is_base_of<StringVerifier, T>::value,
        "T cannot be StringVerifier"
    );
    static_assert(
        !std::is_base_of<TableVerifier, T>::value,
        "T cannot be TableVerifier"
    );
    static_assert(
        !std::is_base_of<VectorVerifier, T>::value,
        "T cannot be VectorVerifier"
    );

    using OperatorVerifier<T, std::greater_equal<typename T::Type>>::OperatorVerifier;

    std::string documentation() const override;

    using OperatorVerifier<T, std::greater_equal<typename T::Type>>::value;
};

/**
 * This Verifier checks whether the incoming value is equal to the stored value. Due to
 * the operator type restrictions, \c T cannot be a subclass of (or the same as)
 * TableVerifier.
 */
template <typename T>
struct EqualVerifier : public OperatorVerifier<T, std::equal_to<typename T::Type>> {
    static_assert(!std::is_base_of<TableVerifier, T>::value, "T cannot be TableVerifier");

    using OperatorVerifier<T, std::equal_to<typename T::Type>>::OperatorVerifier;

    std::string documentation() const override;

    using OperatorVerifier<T, std::equal_to<typename T::Type>>::value;
};

/**
 * This Verifier checks whether the incoming value is unequal to the store value. Due to
 * the operator type restrictions, \c T cannot be a subclass of (or the same as)
 * TableVerifier.
 */
template <typename T>
struct UnequalVerifier : public OperatorVerifier<T, std::not_equal_to<typename T::Type>> {
    static_assert(!std::is_base_of<TableVerifier, T>::value, "T cannot be TableVerifier");

    using OperatorVerifier<T, std::not_equal_to<typename T::Type>>::OperatorVerifier;

    std::string documentation() const override;

    using OperatorVerifier<T, std::not_equal_to<typename T::Type>>::value;
};

//----------------------------------------------------------------------------------------
// List verifiers
//----------------------------------------------------------------------------------------

/**
 * This Verifier checks whether the incoming value is of the correct type, using the
 * Verifier passed as a template parameter \c T and then checks whether it is part of a
 * list that is passed to the constructor. To the missing equality operator, \c T cannot
 * be a subclass of (or the same as) TableVerifier.
 */
template <typename T>
struct InListVerifier : public T {
    static_assert(!std::is_base_of<TableVerifier, T>::value, "T cannot be TableVerifier");

    /**
     * Constructs an InListVerifier that checks whether the incoming value is of the
     * correct type and whether the value is part of the list passed as \p values.
     *
     * \param values The list of values against which the incoming value is tested
     */
    InListVerifier(std::vector<typename T::Type> values);

    /**
     * Tests whether the \p key exists in the \p dictionary, whether it has the correct
     * type by invoking the template parameter \c T, and then tests if the \p key's value
     * is part of the list passed to the constructor.
     *
     * \param dictionary The ghoul::Dictionary that contains the \p key
     * \param key The key that is contained in the \p dictionary and whose value is tested
     * \return A TestResult containing the results of the specification testing. If the
     *         \p key%'s value has the wrong type, it will be added to the TestResult's
     *         offense list with the reason TestResult::Offense::Reason::WrongType; if the
     *         value is not in the list, it will be added with the reason
     *         TestResult::Offense::Verification instead.
     */
    TestResult operator()(const ghoul::Dictionary& dictionary,
        const std::string& key) const override;

    std::string documentation() const override;

    /// The list of values against which the incoming value is tested
    std::vector<typename T::Type> values;
};

/**
 * This Verifier checks whether the incoming value is of the correct type, using the
 * Verifier passed as a template parameter \c T and then checks whether it is not part of
 * a list that is passed to the constructor. To the missing equality operator, \c T cannot
 * be a subclass of (or the same as) TableVerifier.
 */
template <typename T>
struct NotInListVerifier : public T {
    static_assert(!std::is_base_of<TableVerifier, T>::value, "T cannot be TableVerifier");

    /**
     * Constructs a NotInListVerifier that checks whether the incoming value is of the
     * correct type and whether the value is not part of the list passed as \p values.
     *
     * \param values The list of values against which the incoming value is tested
     */
    NotInListVerifier(std::vector<typename T::Type> values);

    /**
     * Tests whether the \p key exists in the \p dictionary, whether it has the correct
     * type by invoking the template parameter \c T, and then tests if the \p key's value
     * is not part of the list passed to the constructor.
     *
     * \param dictionary The ghoul::Dictionary that contains the \p key
     * \param key The key that is contained in the \p dictionary and whose value is tested
     * \return A TestResult containing the results of the specification testing. If the
     *         \p key%'s value has the wrong type, it will be added to the TestResult's
     *         offense list with the reason TestResult::Offense::Reason::WrongType; if the
     *         value is in the list, it will be added with the reason
     *         TestResult::Offense::Verification instead.
     */
    TestResult operator()(const ghoul::Dictionary& dictionary,
        const std::string& key) const override;

    std::string documentation() const override;

    std::vector<typename T::Type> values;
};

//----------------------------------------------------------------------------------------
// Range verifiers
//----------------------------------------------------------------------------------------

/**
 * This Verifier checks whether the incoming value is of the correct type, using the
 * Verifier passed as a template parameter \c T and then checks whether it is greater or
 * equal to a lower limit and less or equal to a higher limit. To the missing comparison
 * operators, \c T cannot be a subclass of (or the same as) BoolVerifier, StringVerifier,
 * TableVerifier, or VectorVerifier. Both the lower and the higher limit are inclusive).
 */
template <typename T>
struct InRangeVerifier : public T {
    static_assert(
        !std::is_base_of<BoolVerifier, T>::value,
        "T cannot be BoolVerifier"
    );
    static_assert(
        !std::is_base_of<StringVerifier, T>::value,
        "T cannot be StringVerifier"
    );
    static_assert(
        !std::is_base_of<TableVerifier, T>::value,
        "T cannot be TableVerifier"
    );
    static_assert(
        !std::is_base_of<VectorVerifier, T>::value,
        "T cannot be VectorVerifier"
    );

    /**
     * Constructs a InRangeVerifier that checks whether the incoming value is of the
     * correct type and whether the value is greater or equal to \p lower and less or
     * equal to \upper.
     *
     * \param lower The (inclusive) lower limit of the range
     * \param upper The (inclusive) upper limit of the range
     *
     * \pre \p lower must be smaller or equal to \p upper
     */
    InRangeVerifier(typename T::Type lower, typename T::Type upper);

    /**
     * Tests whether the \p key exists in the \p dictionary, whether it has the correct
     * type by invoking the template parameter \c T, and then tests if the \p key's value
     * is between the lower and upper limits (both inclusive) that were passed to the
     * constructor.
     *
     * \param dictionary The ghoul::Dictionary that contains the \p key
     * \param key The key that is contained in the \p dictionary and whose value is tested
     * \return A TestResult containing the results of the specification testing. If the
     *         \p key%'s value has the wrong type, it will be added to the TestResult's
     *         offense list with the reason TestResult::Offense::Reason::WrongType; if the
     *         value is outside the range defined by the lower and upper limits passed to
     *         the constructor, it will be added with the reason
     *         TestResult::Offense::Verification instead.
     */
    TestResult operator()(const ghoul::Dictionary& dictionary,
        const std::string& key) const override;

    std::string documentation() const override;

    typename T::Type lower;
    typename T::Type upper;
};

/**
 * This Verifier checks whether the incoming value is of the correct type, using the
 * Verifier passed as a template parameter \c T and then checks whether it is outside the
 * (exclusive) range defined by a lower and upper limit. To the missing comparison
 * operators, \c T cannot be a subclass of (or the same as) BoolVerifier, StringVerifier,
 * TableVerifier, or VectorVerifier. Both the lower and the higher limit are exclusive).
 */
template <typename T>
struct NotInRangeVerifier : public T {
    static_assert(
        !std::is_base_of<BoolVerifier, T>::value,
        "T cannot be BoolVerifier"
    );
    static_assert(
        !std::is_base_of<StringVerifier, T>::value,
        "T cannot be StringVerifier"
    );
    static_assert(
        !std::is_base_of<TableVerifier, T>::value,
        "T cannot be TableVerifier"
    );
    static_assert(
        !std::is_base_of<VectorVerifier, T>::value,
        "T cannot be VectorVerifier"
    );

    /**
     * Constructs a InRangeVerifier that checks whether the incoming value is of the
     * correct type and whether the value is less then \p lower and greater than \p upper.
     *
     * \param lower The (exclusive) lower limit of the range
     * \param upper The (exclusive) upper limit of the range
     *
     * \pre \p lower must be smaller or equal to \p upper
     */
    NotInRangeVerifier(typename T::Type lower, typename T::Type upper);

    /**
     * Tests whether the \p key exists in the \p dictionary, whether it has the correct
     * type by invoking the template parameter \c T, and then tests if the \p key's value
     * is outside the lower and upper limits (both exclusive) that were passed to the
     * constructor.
     *
     * \param dictionary The ghoul::Dictionary that contains the \p key
     * \param key The key that is contained in the \p dictionary and whose value is tested
     * \return A TestResult containing the results of the specification testing. If the
     *         \p key%'s value has the wrong type, it will be added to the TestResult's
     *         offense list with the reason TestResult::Offense::Reason::WrongType; if the
     *         value is greater or equal to the lower limit and less or equal to the upper
     *         limit, it will be added with the reason TestResult::Offense::Verification
     *         instead.
     */
    TestResult operator()(const ghoul::Dictionary& dictionary,
        const std::string& key) const override;

    std::string documentation() const override;

    typename T::Type lower;
    typename T::Type upper;
};


//----------------------------------------------------------------------------------------
// Misc verifiers
//----------------------------------------------------------------------------------------

/**
 * This Verifier only checks for the correct type of the incoming value. If the
 * documentation is requested, it will return an additional string that is the annotation.
 * This can be used to specify further conditions that are hard (or impossible) to codify,
 * but the user should be notified about. This, for example, can be that used to notify
 * the user that the parameter should be a file of a specific type.
 */
template <typename T>
struct AnnotationVerifier : public T {
    /**
     * Constructs an AnnotationVerifier that contains the passed \p annotation which is
     * passed to the user when a documentation is requested.
     *
     * \param annotation The annotation that is stored and returned to the user when it
     *        is requested.
     *
     * \pre annotation must not be empty
     */
    AnnotationVerifier(std::string annotation);

    std::string documentation() const override;

    /// The annotation that is returned to the user in the documentation
    std::string annotation;
};

/**
 * This Verifier is a marker that performs the same testing as the \c T parameter, but
 * also adds a warning to the test result informing the user of the deprecation.
 * Furthermore, the documentation will contain the word <code>(deprecated)</code> in
 * addition to the documentation returned by \c T
 * \tparam T The Verifier that is to be marked deprecated
 */
template <typename T>
struct DeprecatedVerifier : public T {
    /**
     * Tests the \p dictionary%s \p key using the Verifier \c T and adds a warning to the
     * TestResult informing the caller of the deprecation.
     *
     * \param dictionary The ghoul::Dictionary whose \p key should be tested
     * \param key The key inside the \p dictionary that is to be tested
     * \return A TestResult that contains the results of the testing
     */
    TestResult operator()(const ghoul::Dictionary& dictionary,
                          const std::string& key) const override;

    /**
     * Returns the documentation as reported by \c T and adds the word
     * <code>(deprecated)</code> to it.
     * \return The deprecated version of \c T%'s documentation
     */
    std::string documentation() const override;
};

/**
 * This Verifier can reference and apply other Documentation%s that have been registered
 * with a DocumentationEngine. The dependency is only resolved when the operator() is
 * called, at which the referencing Documentation must have been registered, or the
 * TestResult will contain an offense of TestResult::Offense::Reason::UnknownIdentifier.
 * If the referenced Documentation exists, the stored Table will be checked against that
 * Documentation.
 */
struct ReferencingVerifier : public TableVerifier {
    /**
     * Creates a ReferencingVerifier that references a documentation with the provided
     * identifier \p id. The ReferencingVerifier will use the static DocumentationEngine
     * to retrieve Documentation%s and find the \p identifier among them.
     *
     * \param identifier The identifier of the Documentation that this Verifier references
     */
    ReferencingVerifier(std::string id);

    /**
     * Checks whether the \p key in the \p dictionary exists and is of type Table (similar
     * to the TableVerifier). If it exists and is a Table, the Documentation referenced by
     * the identifier provided in the constructor is used to validate the Table. If the
     * identifier does not name a registered Documentation, the TestResult::offenses
     * will contain the \p key and TestResult::Offense::Reason::UnknownIdentifier will be
     * signaled. If the identifier exists and the \p key%'s value does not comply with the
     * Documentation, the offending keys will be returned in the TestResult with their
     * fully qualified names.
     *
     * \param dictionary The ghoul::Dictionary whose \p key should be tested
     * \param key The key contained in the \p dictionary that should be tested
     * \return A TestResult struct that contains the results of the testing
     */
    TestResult operator()(const ghoul::Dictionary& dictionary,
                          const std::string& key) const override;

    std::string documentation() const override;

    /// The identifier that references another Documentation registered with the
    /// DocumentationEngine
    std::string identifier;
};

//----------------------------------------------------------------------------------------
// Misc verifiers
//----------------------------------------------------------------------------------------

/**
 * This Verifier takes two Verifiers and performs a boolean \c and operation on their
 * results. In essence, a value only passes this Verifier if it passes both Verifier%s
 * that are passed in the constructor. Opposed to the <code>C++</code> <code>&&</code>
 * operator, the AndVerifier does not perform any short-circut evaluation.
 */
struct AndVerifier : public Verifier {
    /**
     * Constructs an AndVerifier with Verifiers that must be cleared by incoming values in
     * order to pass this Verifier.
     *
     * \param values The list of Verifiers that are to be tested
     *
     * \pre values must contain at least two values
     */
    AndVerifier(const std::vector<Verifier*> values);

    /**
     * Checks whether the \p dictionary contains the \p key and whether this key passes
     * all Verifier%s that were passed in the constructor. If the value fails at least
     * one Verifiers, it is only added once to the TestResult::offenses list with a reason
     * of TestResult::Offense::Reason::Verification.
     *
     * \param dictionary The ghoul::Dictionary that is to be tested
     * \param key The key contained in \p dictionary that is to be tested
     * \return A TestResult object that contains the test results. If the value fails
     *         any passed  Verifiers, TestResult::success is \c false and the
     *         TestResult::offenses list contains \p with a reason of
     *         TestResult::Offense::Reason::Verification. If \p key%'s value passes both
     *         Verifier%s, the result's TestResult::success is \c true and the
     *         TestResult::offenses is empty.
     */
    TestResult operator()(const ghoul::Dictionary& dictionary,
        const std::string& key) const override;

    std::string type() const override;
    std::string documentation() const override;

    std::vector<std::shared_ptr<Verifier>> values;
};

/**
 * This Verifier takes two Verifiers and performs a boolean \c or operation on their
 * results. In essence, a value only passes this Verifier if it passes either of the two
 * Verifier%s that are passed in the constructor. Opposed to the <code>C++</code>
 * <code>||</code> operator, the OrVerifier does not perform any short-circut evaluation.
 */
struct OrVerifier : public Verifier {
    /**
     * Constructs an OrVerifier with Verifiers that must be cleared by incoming values in
     * order to pass this Verifier.
     *
     * \param values The list of Verifiers that are to be tested
     *
     * \pre values must contain at least two values
     */
    OrVerifier(const std::vector<Verifier*> values);

    /**
     * Checks whether the \p dictionary contains the \p key and whether this key passes
     * any of the Verifier%s that were passed in the constructor. If the value fails all
     * Verifiers, it is added to the TestResult::offenses list with a reason of
     * TestResult::Offense::Reason::Verification.
     *
     * \param dictionary The ghoul::Dictionary that is to be tested
     * \param key The key contained in \p dictionary that is to be tested
     * \return A TestResult object that contains the test results. If the value fails
     *         all Verifiers, TestResult::success is \c false and the
     *         TestResult::offenses list contains \p with a reason of
     *         TestResult::Offense::Reason::Verification. If \p key%'s value passes either
     *         of the two Verifier%s, the result's TestResult::success is \c true and the
     *         TestResult::offenses is empty.
     */
    TestResult operator()(const ghoul::Dictionary& dictionary,
        const std::string& key) const override;

    std::string type() const override;
    std::string documentation() const override;

    std::vector<std::shared_ptr<Verifier>> values;
};

/// A short-hand definition for a Verifier checking for <code>glm::bvec2</code>
using BoolVector2Verifier = Vector2Verifier<bool>;
/// A short-hand definition for a Verifier checking for <code>glm::ivec2</code>
using IntVector2Verifier = Vector2Verifier<int>;
/// A short-hand definition for a Verifier checking for <code>glm::dvec2</code>
using DoubleVector2Verifier = Vector2Verifier<double>;
/// A short-hand definition for a Verifier checking for <code>glm::bvec3</code>
using BoolVector3Verifier = Vector3Verifier<bool>;
/// A short-hand definition for a Verifier checking for <code>glm::ivec3</code>
using IntVector3Verifier = Vector3Verifier<int>;
/// A short-hand definition for a Verifier checking for <code>glm::dvec3</code>
using DoubleVector3Verifier = Vector3Verifier<double>;
/// A short-hand definition for a Verifier checking for <code>glm::bvec4</code>
using BoolVector4Verifier = Vector4Verifier<bool>;
/// A short-hand definition for a Verifier checking for <code>glm::ivec4</code>
using IntVector4Verifier = Vector4Verifier<int>;
/// A short-hand definition for a Verifier checking for <code>glm::dvec4</code>
using DoubleVector4Verifier = Vector4Verifier<double>;

/// A short-hand definition for a Verifier checking for <code>glm::dmat2x2</code>
using DoubleMatrix2x2Verifier = Matrix2x2Verifier<double>;
using DoubleMatrix2Verifier = DoubleMatrix2x2Verifier;
/// A short-hand definition for a Verifier checking for <code>glm::dmat2x3</code>
using DoubleMatrix2x3Verifier = Matrix2x3Verifier<double>;
/// A short-hand definition for a Verifier checking for <code>glm::dmat2x4</code>
using DoubleMatrix2x4Verifier = Matrix2x4Verifier<double>;
/// A short-hand definition for a Verifier checking for <code>glm::dmat3x2</code>
using DoubleMatrix3x2Verifier = Matrix3x2Verifier<double>;
/// A short-hand definition for a Verifier checking for <code>glm::dmat3x3</code>
using DoubleMatrix3x3Verifier = Matrix3x3Verifier<double>;
using DoubleMatrix3Verifier = DoubleMatrix3x3Verifier;
/// A short-hand definition for a Verifier checking for <code>glm::dmat3x4</code>
using DoubleMatrix3x4Verifier = Matrix3x4Verifier<double>;
/// A short-hand definition for a Verifier checking for <code>glm::dmat4x2</code>
using DoubleMatrix4x2Verifier = Matrix4x2Verifier<double>;
/// A short-hand definition for a Verifier checking for <code>glm::dmat4x3</code>
using DoubleMatrix4x3Verifier = Matrix4x3Verifier<double>;
/// A short-hand definition for a Verifier checking for <code>glm::dmat4x4</code>
using DoubleMatrix4x4Verifier = Matrix4x4Verifier<double>;
using DoubleMatrix4Verifier = DoubleMatrix4x4Verifier;

/// A short-hand definition for a LessVerifier with a type check for \c int
using IntLessVerifier = LessVerifier<IntVerifier>;
/// A short-hand definition for a LessVerifier with a type check for \c double
using DoubleLessVerifier = LessVerifier<DoubleVerifier>;
/// A short-hand definition for a LessEqualVerifier with a type check for \c int
using IntLessEqualVerifier = LessEqualVerifier<IntVerifier>;
/// A short-hand definition for a LessEqualVerifier with a type check for \c double
using DoubleLessEqualVerifier = LessEqualVerifier<DoubleVerifier>;
/// A short-hand definition for a GreaterVerifier with a type check for \c int
using IntGreaterVerifier = GreaterVerifier<IntVerifier>;
/// A short-hand definition for a GreaterVerifier with a type check for \c double
using DoubleGreaterVerifier = GreaterVerifier<DoubleVerifier>;
/// A short-hand definition for a GreaterEqualVerifier with a type check for \c int
using IntGreaterEqualVerifier = GreaterEqualVerifier<IntVerifier>;
/// A short-hand definition for a GreaterEqualVerifier with a type check for \c double
using DoubleGreaterEqualVerifier = GreaterEqualVerifier<DoubleVerifier>;
/// A short-hand definition for a EqualVerifier with a type check for \c bool
using BoolEqualVerifier = EqualVerifier<BoolVerifier>;
/// A short-hand definition for a EqualVerifier with a type check for \c int
using IntEqualVerifier = EqualVerifier<IntVerifier>;
/// A short-hand definition for a EqualVerifier with a type check for \c double
using DoubleEqualVerifier = EqualVerifier<DoubleVerifier>;
/// A short-hand definition for a EqualVerifier with a type check for \c string
using StringEqualVerifier = EqualVerifier<StringVerifier>;
/// A short-hand definition for a UnequalVerifier with a type check for \c bool
using BoolUnequalVerifier = UnequalVerifier<BoolVerifier>;
/// A short-hand definition for a UnequalVerifier with a type check for \c int
using IntUnequalVerifier = UnequalVerifier<IntVerifier>;
/// A short-hand definition for a UnequalVerifier with a type check for \c double
using DoubleUnequalVerifier = UnequalVerifier<DoubleVerifier>;
/// A short-hand definition for a UnequalVerifier with a type check for \c string
using StringUnequalVerifier = UnequalVerifier<StringVerifier>;

/// A short-hand definition for a InListVerifier with a type check for \c bool
using BoolInListVerifier = InListVerifier<BoolVerifier>;
/// A short-hand definition for a InListVerifier with a type check for \c int
using IntInListVerifier = InListVerifier<IntVerifier>;
/// A short-hand definition for a InListVerifier with a type check for \c double
using DoubleInListVerifier = InListVerifier<DoubleVerifier>;
/// A short-hand definition for a InListVerifier with a type check for \c string
using StringInListVerifier = InListVerifier<StringVerifier>;
/// A short-hand definition for a NotInListVerifier with a type check for \c bool
using BoolNotInListVerifier = NotInListVerifier<BoolVerifier>;
/// A short-hand definition for a NotInListVerifier with a type check for \c int
using IntNotInListVerifier = NotInListVerifier<IntVerifier>;
/// A short-hand definition for a NotInListVerifier with a type check for \c double
using DoubleNotInListVerifier = NotInListVerifier<DoubleVerifier>;
/// A short-hand definition for a NotInListVerifier with a type check for \c string
using StringNotInListVerifier = NotInListVerifier<StringVerifier>;

/// A short-hand definition for a InRangeVerifier with a type check for \c int
using IntInRangeVerifier = InRangeVerifier<IntVerifier>;
/// A short-hand definition for a InRangeVerifier with a type check for \c double
using DoubleInRangeVerifier = InRangeVerifier<DoubleVerifier>;
/// A short-hand definition for a NotInRangeVerifier with a type check for \c int
using IntNotInRangeVerifier = NotInRangeVerifier<IntVerifier>;
/// A short-hand definition for a NotInRangeVerifier with a type check for \c double
using DoubleNotInRangeVerifier = NotInRangeVerifier<DoubleVerifier>;

/// A short-hand definition for a AnnotationVerifier with a type check for \c bool
using BoolAnnotationVerifier = AnnotationVerifier<BoolVerifier>;
/// A short-hand definition for a AnnotationVerifier with a type check for \c int
using IntAnnotationVerifier = AnnotationVerifier<IntVerifier>;
/// A short-hand definition for a AnnotationVerifier with a type check for \c double
using DoubleAnnotationVerifier = AnnotationVerifier<DoubleVerifier>;
/// A short-hand definition for a AnnotationVerifier with a type check for \c string
using StringAnnotationVerifier = AnnotationVerifier<StringVerifier>;
/// A short-hand definition for a AnnotationVerifier with a type check for
/// <code>ghoul::Dictionary</code>
using TableAnnotationVerifier = AnnotationVerifier<TableVerifier>;

/// A short-hand definition for a DeprecatedVerifier with a type check for \c bool
using BoolDeprecatedVerifier = DeprecatedVerifier<BoolVerifier>;
/// A short-hand definition for a DeprecatedVerifier with a type check for \c int
using IntDeprecatedVerifier = DeprecatedVerifier<IntVerifier>;
/// A short-hand definition for a DeprecatedVerifier with a type check for \c double
using DoubleDeprecatedVerifier = DeprecatedVerifier<DoubleVerifier>;
/// A short-hand definition for a DeprecatedVerifier with a type check for \c string
using StringDeprecatedVerifier = DeprecatedVerifier<StringVerifier>;
/// A short-hand definition for a DeprecatedVerifier with a type check for
/// <code>ghoul::Dictionary</code>
using TableDeprecatedVerifier = DeprecatedVerifier<TableVerifier>;

// Definitions of external templates that are instantiated in the cpp file
// This cuts down the compilation times as almost all of the possible template types do
// not need to be instantiated multiple times
extern template struct Vector2Verifier<bool>;
extern template struct Vector2Verifier<int>;
extern template struct Vector2Verifier<double>;
extern template struct Vector3Verifier<bool>;
extern template struct Vector3Verifier<int>;
extern template struct Vector3Verifier<double>;
extern template struct Vector4Verifier<bool>;
extern template struct Vector4Verifier<int>;
extern template struct Vector4Verifier<double>;

extern template struct Matrix2x2Verifier<double>;
extern template struct Matrix2x3Verifier<double>;
extern template struct Matrix2x4Verifier<double>;
extern template struct Matrix3x2Verifier<double>;
extern template struct Matrix3x3Verifier<double>;
extern template struct Matrix3x4Verifier<double>;
extern template struct Matrix4x2Verifier<double>;
extern template struct Matrix4x3Verifier<double>;
extern template struct Matrix4x4Verifier<double>;

extern template struct LessVerifier<IntVerifier>;
extern template struct LessVerifier<DoubleVerifier>;
extern template struct LessEqualVerifier<IntVerifier>;
extern template struct LessEqualVerifier<DoubleVerifier>;
extern template struct GreaterVerifier<IntVerifier>;
extern template struct GreaterVerifier<DoubleVerifier>;
extern template struct GreaterEqualVerifier<IntVerifier>;
extern template struct GreaterEqualVerifier<DoubleVerifier>;
extern template struct EqualVerifier<BoolVerifier>;
extern template struct EqualVerifier<IntVerifier>;
extern template struct EqualVerifier<DoubleVerifier>;
extern template struct EqualVerifier<StringVerifier>;
extern template struct UnequalVerifier<BoolVerifier>;
extern template struct UnequalVerifier<IntVerifier>;
extern template struct UnequalVerifier<DoubleVerifier>;
extern template struct UnequalVerifier<StringVerifier>;

extern template struct InListVerifier<BoolVerifier>;
extern template struct InListVerifier<IntVerifier>;
extern template struct InListVerifier<DoubleVerifier>;
extern template struct InListVerifier<StringVerifier>;
extern template struct NotInListVerifier<BoolVerifier>;
extern template struct NotInListVerifier<IntVerifier>;
extern template struct NotInListVerifier<DoubleVerifier>;
extern template struct NotInListVerifier<StringVerifier>;

extern template struct InRangeVerifier<IntVerifier>;
extern template struct InRangeVerifier<DoubleVerifier>;
extern template struct NotInRangeVerifier<IntVerifier>;
extern template struct NotInRangeVerifier<DoubleVerifier>;

extern template struct AnnotationVerifier<BoolVerifier>;
extern template struct AnnotationVerifier<IntVerifier>;
extern template struct AnnotationVerifier<DoubleVerifier>;
extern template struct AnnotationVerifier<StringVerifier>;
extern template struct AnnotationVerifier<TableVerifier>;
extern template struct AnnotationVerifier<BoolVector2Verifier>;
extern template struct AnnotationVerifier<IntVector2Verifier>;
extern template struct AnnotationVerifier<DoubleVector2Verifier>;
extern template struct AnnotationVerifier<BoolVector3Verifier>;
extern template struct AnnotationVerifier<IntVector3Verifier>;
extern template struct AnnotationVerifier<DoubleVector3Verifier>;
extern template struct AnnotationVerifier<BoolVector4Verifier>;
extern template struct AnnotationVerifier<IntVector4Verifier>;
extern template struct AnnotationVerifier<DoubleVector4Verifier>;

extern template struct DeprecatedVerifier<BoolVerifier>;
extern template struct DeprecatedVerifier<IntVerifier>;
extern template struct DeprecatedVerifier<DoubleVerifier>;
extern template struct DeprecatedVerifier<StringVerifier>;
extern template struct DeprecatedVerifier<TableVerifier>;
extern template struct DeprecatedVerifier<BoolVector2Verifier>;
extern template struct DeprecatedVerifier<IntVector2Verifier>;
extern template struct DeprecatedVerifier<DoubleVector2Verifier>;
extern template struct DeprecatedVerifier<BoolVector3Verifier>;
extern template struct DeprecatedVerifier<IntVector3Verifier>;
extern template struct DeprecatedVerifier<DoubleVector3Verifier>;
extern template struct DeprecatedVerifier<BoolVector4Verifier>;
extern template struct DeprecatedVerifier<IntVector4Verifier>;
extern template struct DeprecatedVerifier<DoubleVector4Verifier>;

} // namespace openspace::documentation

#include "verifier.inl"

#endif // __OPENSPACE_CORE___VERIFIER___H__
