/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#ifndef __GHOUL___TEMPLATEFACTORY___H__
#define __GHOUL___TEMPLATEFACTORY___H__

#include <ghoul/misc/exception.h>
#include <ghoul/misc/memorypool.h>
#include <functional>
#include <map>
#include <memory_resource>
#include <string>
#include <string_view>
#include <typeinfo>
#include <vector>

namespace ghoul {

class Dictionary;

/**
 * Main exception that is thrown by the TemplateFactory in the case of errors.
 */
struct TemplateFactoryError : public RuntimeError {
    explicit TemplateFactoryError(std::string msg);
};

/**
 * Exception that is thrown if a requested class has not been registered before.
 */
struct TemplateClassNotFoundError final : public TemplateFactoryError {
    explicit TemplateClassNotFoundError(std::string name);
    const std::string className;
};

/**
 * Exception that is thrown if a registered class is called with a wrong constructor.
 */
struct TemplateConstructionError final : public TemplateFactoryError {
    explicit TemplateConstructionError(std::string msg);
};

class TemplateFactoryBase {
public:
    virtual ~TemplateFactoryBase() = default;

    virtual const std::type_info& baseClassType() const = 0;
    virtual bool hasClass(const std::string& className) const = 0;
    virtual std::vector<std::string> registeredClasses() const = 0;
};

/**
 * This class implements a generic Factory pattern that can be used for any class as long
 * as a default constructor and/or constructor taking a Dictionary is available for the
 * subclass. The usage of this TemplateFactory is as follows: The `BaseClass` template
 * parameter is the base class of all classes that can be created using this factory. Only
 * subclasses of `BaseClass` can be added to the factory, or a compile-time error will be
 * generated. Subclasses of the `BaseClass` (or the `BaseClass` itself) can be registered
 * using the #registerClass method, which takes the subclass as a template parameter. All
 * registered classes can then be created using the #create method, providing the same
 * `className` with which the class was registered. If the subclass that was registered
 * provides both an empty default constructor as well as a constructor taking a single
 * `const ghoul::Dictionary&` parameter, the second #create method can be used which takes
 * the `className` as well as the Dictionary with which to instantiate the class. If this
 * method is used and the class does not provide a Dictionary constructor, an exception is
 * thrown. Likewise, if the registered class does not provide a default constructor and is
 * called with the first #create method, a similar exception is thrown. #hasClass tests if
 * a specific `className` was registered previously. For example:
 * ```
 * class A {};
 * class B : public A {};
 * class C {};
 * class D : public A {
 * public:
 *     D(const ghoul::Dictionary&);
 * };
 *
 * TemplateFactory<A> factory;
 * factory.registerClass<B>("B");
 * factory.registerClass<C>("C"); // compile error as C is not a subclass of A
 * factory.registerClass<D>("D");
 * A* i = factory.create("B"); // creates an instance of B with the default constructor
 * i = factory.create("D"); // creates an instance of D with the default constructor
 * i = factory.create("D", { }); // creates instance of D with the Dictionary constructor
 * i = factory.create("B", { }); // throws exception as B does not have a Dictionary ctor
 * ```
 *
 * \tparam BaseClass The base class of all classes that can be registered and created
 *         using this factory
 */
template <typename BaseClass>
class TemplateFactory : public TemplateFactoryBase {
public:
    /**
     * This is a function pointer that is called when a new subclass is to be created and
     * must return the new-allocated class. The function pointer is stored in the
     * TemplateFactory.
     *
     * \param useDictionary `true` if the class was called with a provided Dictionary and
     *        the Dictionary should be used
     * \param dict The Dictionary that should be used to initialize the subclass. If
     *        \p useDictionary is `false`, this is the empty Dictionary
     * \return The initialized subclass of type `BaseClass`
     *
     * \throw TemplateConstructionError If the class was initialized using the wrong
     *        constructor, for example a class that does not have a Dictionary
     *        constructor, but a Dictionary was used
     */
    using FactoryFunction = std::function<
        BaseClass*(bool useDictionary, const Dictionary& dict, pmr::memory_resource* pool)
    >;

    /**
     * Creates an instance of the class which was registered under the provided
     * \p className. This creation uses the parameterless default constructor of the
     * class. If \p className does not name a registered class, an exception is thrown.
     * Classes can be registered with the #registerClass method.
     *
     * \param className The class name of the instance that should be created
     * \param pool The optional memory pool that is used to allocate the new object. If
     *        this value is `std::nullopt`, the operating systems memory allocated is used
     * \return A fully initialized instance of the registered class
     *
     * \throw TemplateClassNotFoundError If the \p className did not name a previously
     *        registered class
     * \throw TemplateConstructionError If the class registered under \p className does
     *        not have a default constructor
     * \pre \p className must not be empty
     */
    BaseClass* create(std::string_view className,
        pmr::memory_resource* pool = nullptr) const;

    /**
     * Creates an instance of the class which was registered under the provided
     * \p className. This creation uses the constructor of the class and passes the
     * \p dictionary to the constructor. For this method to work, the class which was
     * registered with \p className has to have a constructor with a single Dictionary as
     * parameter. If \p className does not name a registered class, an exception is
     * thrown. Classes can be registered with the #registerClass method.
     *
     * \param className The class name of the instance that should be created
     * \param dictionary The dictionary that will be passed to the constructor of the
     *        class
     * \param pool The optional memory pool that is used to allocate the new object. If
     *        this value is `std::nullopt`, the operating systems memory allocated is used
     * \return A fully initialized instance of the registered class
     *
     * \throw TemplateClassNotFoundError If the \p className did not name a previously
     *        registered class
     * \throw TemplateConstructionError If the class registered under \p className does
     *        not have a constructor using a Dictionary object
     * \pre \p className must not be empty
     */
    BaseClass* create(std::string_view className, const Dictionary& dictionary,
        pmr::memory_resource* pool = nullptr) const;

    /**
     * Registers a `Class` with the provided \p className so that it can later be
     * #create%d using the same \p className. `Class` has to be a subclass of `BaseClass`
     * or a compile-error will occur. If a class already has been registered under the
     * \p className, an exception will be thrown.
     *
     * \tparam Class The class that should be registered under the provided \p className
     * \param className The class name under which the `Class` is registered
     *
     * \throw TemplateFactoryError If the \p className has been registered before
     * \pre `Class` must be derived from `BaseClass`
     * \pre `Class` must have a default constructor or a constructor that takes a
     *      Dictionary
     * \pre \p className must not be empty
     */
    template <typename Class>
    void registerClass(std::string className);

    /**
     * Registers a class with the provided \p className and the user-defined factory
     * function. The \p factoryFunction must return a valid subclass of `BaseClass` when
     * is it called in the #create methods of TemplateFactory. The `std::function` object
     * is stored inside.
     *
     * \param className The class name, which will be registered with the provided factory
     *        function
     * \param factoryFunction The `std::function` that will be called if the
     *        TemplateFactory is asked to create a class of type \p className. The first
     *        argument of the function pointer is `true` if the #create method was called
     *        with a Dictionary as an additional parameter, implying that the subclass
     *        should be constructed using the Dictionary provided as the second argument.
     *        The factory function is free to ignore this request
     *
     * \throw TemplateFactoryError If the \p className has been registered before
     * \pre \p className must not be empty
     * \pre \p factoryFunction must not be `nullptr`
     */
    void registerClass(std::string className,
        std::function<
            BaseClass*(bool, const Dictionary&, pmr::memory_resource* pool)
        > factoryFunction);

    /**
     * Checks if any class has been registered under the provided \p className As any
     * invalid class will create a compile-time error, if a class has been registered, it
     * is guaranteed that the \p className can be used to generate a valid instance.
     *
     * \param className The class name that is to be tested
     * \return `true` if the \p className was used in #registerClass before; `false`
     *         otherwise
     *
     * \pre \p className must not be empty
     */
    bool hasClass(const std::string& className) const override;

    /**
     * Returns the list of all registered classes. All values in this vector can be used
     * to instantiate a new class sing the #create method.
     *
     * \return The list of all registered classes
     */
    std::vector<std::string> registeredClasses() const override;

    /**
     * Returns the `type_info` of the baseclass for this factory, i.e., the base class
     * which is the super class for all actual instances that can be created using the
     * #create method.
     *
     * \return The `type_info` of the baseclass for this factory
     */
    const std::type_info& baseClassType() const override;

private:
    /// The map storing all the associations from `className` to classes
    std::map<std::string, FactoryFunction, std::less<>> _map;
};

} // namespace ghoul

#include "ghoul/misc/templatefactory.inl"

#endif // __GHOUL___TEMPLATEFACTORY___H__
