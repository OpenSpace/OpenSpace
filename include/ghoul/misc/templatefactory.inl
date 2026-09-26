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

#include <ghoul/format.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/dictionary.h>
#include <type_traits>
#include <utility>

namespace {
    // The working principle is as follows: There are two methods which can create
    // subclasses, 'create' and 'createWithDictionary'. The first one will create the
    // subclass using the default constructor while the second can use the default
    // constructor or a constructor using a ghoul::Dictionary as an input. Since both
    // methods adhere to the same function prototype 'BaseClass* (*FactoryFuncPtr)(bool,
    // const Dictionary&)', they can be stored in the same TemplateFactory's '_map'
    // without the TemplateFactory knowing which of the two functions it is. If C++ would
    // support partial template specialization, two separate methods wouldn't be necessary
    // and could instead be split up by a single template argument. Instead, a helper
    // class 'CreateHelper' has to be created as partial template specialization is
    // allowed for classes. 'CreateHelper' has a single method that returns either a
    // 'create' or a 'createWithDictionary' function pointer, depending on the third
    // parameter Constructor; this parameter is determined at compile-time in the
    // registerClass by using the constant values DEFAULT_CONSTRUCTOR with
    // std::has_default_constructor and DICTIONARY_CONSTRUCTOR with std::is_convertible

    constexpr int DefaultConstructor = 1;
    constexpr int DictionaryConstructor = 2;

    /**
     * Create Class using only the default constructor.
     */
    template <typename BaseClass, typename Class>
    BaseClass* createDefault(bool useDictionary, const ghoul::Dictionary& dict,
                             pmr::memory_resource* pool)
    {
        // We don't have a dictionary constructor, but the user tried to create it with a
        // Dictionary
        if (useDictionary || dict.size() != 0) {
            std::string className = typeid(Class).name();
            throw ghoul::TemplateConstructionError(std::format(
                "Class '{}' does not provide a constructor receiving a Dictionary",
                className
            ));
        }

        if (pool) {
            void* ptr = pool->allocate(sizeof(Class));
            return new (ptr) Class;
        }
        else {
            return new Class;
        }
    }

    /**
     * Create Class using the default constructor or the Dictionary.
     */
    template <typename BaseClass, typename Class>
    BaseClass* createDefaultAndDictionary(bool useDictionary,
                                          const ghoul::Dictionary& dict,
                                          pmr::memory_resource* pool)
    {
        if (useDictionary) {
            if (pool) {
                void* ptr = pool->allocate(sizeof(Class));
                return new (ptr) Class(dict);
            }
            else {
                return new Class(dict);
            }
        }
        else {
            if (pool) {
                void* ptr = pool->allocate(sizeof(Class));
                return new (ptr) Class;
            }
            else {
                return new Class;
            }
        }
    }

    /**
     * Create Class using only the Dictionary constructor.
     */
    template <typename BaseClass, typename Class>
    BaseClass* createDictionary(bool useDictionary, const ghoul::Dictionary& dict,
                                pmr::memory_resource* pool)
    {
        if (!useDictionary) {
            std::string className = typeid(Class).name();
            throw ghoul::TemplateConstructionError(std::format(
                "Class '{}' does only provide a Dictionary constructor but was called "
                "using the default constructor", className
            ));
        }
        if (pool) {
            void* ptr = pool->allocate(sizeof(Class));
            return new (ptr) Class(dict);
        }
        else {
            return new Class(dict);
        }
    }

    template <typename BaseClass, typename Class, int Constructor>
    struct CreateHelper {
        using FactoryFuncPtr = BaseClass* (*)(
            bool useDictionary, const ghoul::Dictionary& dict, pmr::memory_resource* pool
        );
        FactoryFuncPtr createFunction();
    };

    template <typename BaseClass, typename Class>
    struct CreateHelper<BaseClass, Class, DefaultConstructor | DictionaryConstructor> {
        using FactoryFuncPtr = BaseClass* (*)(
            bool useDictionary, const ghoul::Dictionary& dict, pmr::memory_resource* pool
        );
        FactoryFuncPtr createFunction() {
            return &createDefaultAndDictionary<BaseClass, Class>;
        }
    };

    template <typename BaseClass, typename Class>
    struct CreateHelper<BaseClass, Class, DefaultConstructor> {
        using FactoryFuncPtr = BaseClass* (*)(
            bool useDictionary, const ghoul::Dictionary& dict, pmr::memory_resource* pool
        );
        FactoryFuncPtr createFunction() {
            return &createDefault<BaseClass, Class>;
        }
    };

    template <typename BaseClass, typename Class>
    struct CreateHelper<BaseClass, Class, DictionaryConstructor> {
        using FactoryFuncPtr = BaseClass* (*)(
            bool useDictionary, const ghoul::Dictionary& dict, pmr::memory_resource* pool
        );
        FactoryFuncPtr createFunction() {
            return &createDictionary<BaseClass, Class>;
        }
    };
} // namespace

namespace ghoul {

template <typename BaseClass>
BaseClass* TemplateFactory<BaseClass>::create(std::string_view className,
                                              pmr::memory_resource* pool) const
{
    ghoul_assert(!className.empty(), "Classname must not be empty");

    const auto it = _map.find(className);
    if (it == _map.cend()) {
        throw TemplateClassNotFoundError(std::string(className));
    }

    // If 'className' is a valid name, we can use the stored function pointer to create
    // the class using the 'createType' method
    BaseClass* res = it->second(false, {}, pool);
    return res;
}

template <typename BaseClass>
BaseClass* TemplateFactory<BaseClass>::create(std::string_view className,
                                              const Dictionary& dictionary,
                                              pmr::memory_resource* pool) const
{
    ghoul_assert(!className.empty(), "Classname must not be empty");

    const auto it = _map.find(className);
    if (it == _map.end()) {
        throw TemplateClassNotFoundError(std::string(className));
    }

    // If 'className' is a valid name, we can use the stored function pointer to create
    // the class using the 'createType' method
    BaseClass* res = it->second(true, dictionary, pool);
    return res;
}

template <typename BaseClass>
template <typename Class>
void TemplateFactory<BaseClass>::registerClass(std::string className) {
    static_assert(!std::is_abstract<Class>::value, "Class must not be an abstract class");
    static_assert(
        std::is_base_of<BaseClass, Class>::value,
        "BaseClass must be the base class of Class"
    );
    static_assert(
        std::is_default_constructible<Class>::value |
        std::is_constructible<Class, const Dictionary&>::value,
        "Class needs a public default or Dictionary constructor"
    );

    ghoul_assert(!className.empty(), "Classname must not be empty");

    // Use the correct CreateHelper struct to create a function pointer that we can store
    // for later usage. std::is_constructible<>::value returns a boolean that checks at
    // run-time if there is a proper constructor for it)
    FactoryFunction&& function = CreateHelper<
        BaseClass,
        Class,
        (std::is_default_constructible<Class>::value * DefaultConstructor) |
        (std::is_constructible<Class, const Dictionary&>::value * DictionaryConstructor)
    >().createFunction();

    registerClass(std::move(className), FactoryFunction(function));
}

template <typename BaseClass>
void TemplateFactory<BaseClass>::registerClass(std::string className,
                                               FactoryFunction factoryFunction)
{
    ghoul_assert(!className.empty(), "Classname must not be empty");
    ghoul_assert(factoryFunction, "Factory function must not be nullptr");

    if (_map.find(className) != _map.end()) {
        throw TemplateFactoryError(std::format(
            "Class '{}' was registered before", className
        ));
    }

    _map.emplace(std::move(className), std::move(factoryFunction));
}

template <typename BaseClass>
bool TemplateFactory<BaseClass>::hasClass(const std::string& className) const {
    ghoul_assert(!className.empty(), "Classname must not be empty");
    return _map.find(className) != _map.end();
}

template <typename BaseClass>
std::vector<std::string> TemplateFactory<BaseClass>::registeredClasses() const {
    std::vector<std::string> result;
    result.reserve(_map.size());
    for (const std::pair<const std::string, FactoryFunction>& it : _map) {
        result.push_back(it.first);
    }
    return result;
}

template <typename BaseClass>
const std::type_info& TemplateFactory<BaseClass>::baseClassType() const {
    return typeid(BaseClass);
}

} // namespace ghoul
