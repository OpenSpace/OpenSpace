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

#include <catch2/catch_test_macros.hpp>

#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/managedmemoryuniqueptr.h>
#include <ghoul/misc/memorypool.h>
#include <ghoul/misc/templatefactory.h>

 /*
  * Test checklist:
  * +++ Correctness for direct subclass
  * +++ Non-interference
  * +++ Deep inheritance
  * +++ Default constructor
  * +++ Default constructor does not exist
  * +++ Dictionary constructor
  * +++ Dictionary constructor does not exist
  * +++ Default + Dictionary ctor
  * +++ Class does not exist
  * +++ Correctness for 'hasClass'
  * +++ Custom factory function pointer
  * +++ Custom factory std::function
  */

namespace {
    struct BaseClass {
    public:
        // virtual method necessary for RTTI
        virtual ~BaseClass() = default;

        int value1 = -1;
        int value2 = -2;
    };

    struct SubClassDefault : public BaseClass {
    public:
        SubClassDefault() : BaseClass() {
            value1 = 1;
            value2 = 2;
        }
    };

    struct SubClassDefault2 : public BaseClass {
    public:
        SubClassDefault2() : BaseClass() {
            value1 = 21;
            value2 = 22;
        }
    };

    struct SubClassDictionary : public BaseClass {
    public:
        explicit SubClassDictionary(const ghoul::Dictionary& dict) : BaseClass() {
            if (dict.hasValue<int>("value1")) {
                value1 = dict.value<int>("value1");
            }
            if (dict.hasValue<int>("value2")) {
                value2 = dict.value<int>("value2");
            }
        }
    };

    struct SubClassDefaultDictionary : public BaseClass {
    public:
        SubClassDefaultDictionary() : BaseClass() {
            value1 = 31;
            value2 = 32;
        }
        explicit SubClassDefaultDictionary(const ghoul::Dictionary& dict) : BaseClass() {
            if (dict.hasValue<int>("value1")) {
                value1 = dict.value<int>("value1");
            }
            if (dict.hasValue<int>("value2")) {
                value2 = dict.value<int>("value2");
            }
        }
    };

    struct SubClassMultipleLayers : public SubClassDefault {
        SubClassMultipleLayers() {}
    };

    struct FunctionPointerClass : public BaseClass {};
    struct StdFunctionClass : public BaseClass {};
} // namespace

TEST_CASE("TemplateFactory: Correctness Direct Subclass", "[templatefactory]") {
    ghoul::TemplateFactory<BaseClass> factory;

    factory.registerClass<SubClassDefault>("SubClassDefault");

    ghoul::mm_unique_ptr<BaseClass> obj = ghoul::mm_unique_ptr<BaseClass>(
        factory.create("SubClassDefault")
    );
    REQUIRE(obj != nullptr);

    SubClassDefault* derived = dynamic_cast<SubClassDefault*>(obj.get());
    CHECK(derived != nullptr);
}

TEST_CASE("TemplateFactory: Correctness Deep SubClass", "[templatefactory]") {
    ghoul::TemplateFactory<BaseClass> factory;

    factory.registerClass<SubClassMultipleLayers>("SubClassMultipleLayers");

    ghoul::mm_unique_ptr<BaseClass> obj = ghoul::mm_unique_ptr<BaseClass>(
        factory.create("SubClassMultipleLayers")
    );
    REQUIRE(obj != nullptr);

    SubClassMultipleLayers* derived = dynamic_cast<SubClassMultipleLayers*>(obj.get());
    CHECK(derived != nullptr);
}

TEST_CASE("TemplateFactory: Non Interference", "[templatefactory]") {
    ghoul::TemplateFactory<BaseClass> factory;

    factory.registerClass<SubClassDefault>("SubClassDefault");
    factory.registerClass<SubClassDefault2>("SubClassDefault2");

    ghoul::mm_unique_ptr<BaseClass> obj = ghoul::mm_unique_ptr<BaseClass>(
        factory.create("SubClassDefault")
    );
    REQUIRE(obj != nullptr);

    ghoul::mm_unique_ptr<BaseClass> obj2 = ghoul::mm_unique_ptr<BaseClass>(
        factory.create("SubClassDefault2")
    );
    REQUIRE(obj2 != nullptr);
    CHECK(obj != obj2);

    SubClassDefault* derived = dynamic_cast<SubClassDefault*>(obj.get());
    CHECK(derived != nullptr);

    SubClassDefault2* derived2 = dynamic_cast<SubClassDefault2*>(obj2.get());
    CHECK(derived2 != nullptr);
}

TEST_CASE("TemplateFactory: Default Constructor", "[templatefactory]") {
    ghoul::TemplateFactory<BaseClass> factory;

    factory.registerClass<SubClassDefault>("SubClassDefault");

    ghoul::mm_unique_ptr<BaseClass> obj = ghoul::mm_unique_ptr<BaseClass>(
        factory.create("SubClassDefault")
    );
    REQUIRE(obj != nullptr);
    CHECK(obj->value1 == 1);
    CHECK(obj->value2 == 2);
}

TEST_CASE("TemplateFactory: No Default Constructor Exists", "[templatefactory]") {
    ghoul::TemplateFactory<BaseClass> factory;

    factory.registerClass<SubClassDictionary>("SubClassDictionary");

    CHECK_THROWS_AS(
        factory.create("SubClassDictionary"),
        ghoul::TemplateConstructionError
    );
}

TEST_CASE("TemplateFactory: Dictionary Constructor", "[templatefactory]") {
    ghoul::TemplateFactory<BaseClass> factory;

    factory.registerClass<SubClassDictionary>("SubClassDictionary");

    ghoul::Dictionary dict;
    dict.setValue("value1", 100);
    dict.setValue("value2", 200);

    ghoul::mm_unique_ptr<BaseClass> obj = ghoul::mm_unique_ptr<BaseClass>(
        factory.create("SubClassDictionary", dict)
    );
    REQUIRE(obj != nullptr);
    CHECK(obj->value1 == 100);
    CHECK(obj->value2 == 200);
}

TEST_CASE("TemplateFactory: No Dictionary Constructor Exists", "[templatefactory]") {
    ghoul::TemplateFactory<BaseClass> factory;

    factory.registerClass<SubClassDefault>("SubClassDefault");

    ghoul::Dictionary dict;
    dict.setValue("value1", 100);
    dict.setValue("value2", 200);

    CHECK_THROWS_AS(
        factory.create("SubClassDefault", dict),
        ghoul::TemplateConstructionError
    );
}

TEST_CASE("TemplateFactory: Class Does Not Exist", "[templatefactory]") {
    ghoul::TemplateFactory<BaseClass> factory;

    factory.registerClass<SubClassDefault>("SubClassDefault");

    ghoul::mm_unique_ptr<BaseClass> obj = ghoul::mm_unique_ptr<BaseClass>(
        factory.create("SubClassDefault")
    );
    CHECK(obj != nullptr);

    CHECK_THROWS_AS(
        factory.create("DoesNotExist"),
        ghoul::TemplateClassNotFoundError
    );
}

TEST_CASE("TemplateFactory: Default Dictionary Constructor", "[templatefactory]") {
    ghoul::TemplateFactory<BaseClass> factory;

    // SubClassDefaultDictionary 31 32
    factory.registerClass<SubClassDefaultDictionary>("class");

    ghoul::mm_unique_ptr<BaseClass> obj = ghoul::mm_unique_ptr<BaseClass>(
        factory.create("class")
    );
    REQUIRE(obj != nullptr);
    CHECK(obj->value1 == 31);
    CHECK(obj->value2 == 32);

    ghoul::Dictionary dict;
    dict.setValue("value1", 41);
    dict.setValue("value2", 42);

    ghoul::mm_unique_ptr<BaseClass> obj2 = ghoul::mm_unique_ptr<BaseClass>(
        factory.create("class", dict)
    );
    REQUIRE(obj2 != nullptr);
    CHECK(obj2->value1 == 41);
    CHECK(obj2->value2 == 42);
}

TEST_CASE("TemplateFactory: Correctness For HasClass", "[templatefactory]") {
    ghoul::TemplateFactory<BaseClass> factory;

    factory.registerClass<SubClassDictionary>("SubClassDictionary");

    CHECK(factory.hasClass("SubClassDictionary"));
    CHECK_FALSE(factory.hasClass("DoesNotExist"));
}

TEST_CASE("TemplateFactory: Std Function Construction", "[templatefactory]") {
    ghoul::TemplateFactory<BaseClass> factory;

    ghoul::TemplateFactory<BaseClass>::FactoryFunction function =
        [](bool use, const ghoul::Dictionary&, pmr::memory_resource*) -> BaseClass* {
            return use ? new StdFunctionClass : nullptr;
    };
    factory.registerClass("ptr", function);

    ghoul::mm_unique_ptr<BaseClass> obj = ghoul::mm_unique_ptr<BaseClass>(
        factory.create("ptr")
    );
    CHECK(obj == nullptr);

    ghoul::mm_unique_ptr<BaseClass> obj2 = ghoul::mm_unique_ptr<BaseClass>(
        factory.create("ptr", ghoul::Dictionary())
    );
    CHECK(obj2 != nullptr);
}

TEST_CASE("TemplateFactory: MemoryPool construction", "[templatefactory]") {
    ghoul::MemoryPool<64, true> pool;

    ghoul::TemplateFactory<BaseClass> factory;
    factory.registerClass<SubClassDefault>("sc");

    factory.create("sc", &pool);
    REQUIRE(pool.nBuckets() == 1);
    CHECK(pool.occupancies()[0] == 16);

    factory.create("sc", &pool);
    REQUIRE(pool.nBuckets() == 1);
    CHECK(pool.occupancies()[0] == 32);
}
