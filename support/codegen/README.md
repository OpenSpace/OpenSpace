# Overview
The `codegen` application automatically generated often-used and repetetive C++ code which is saved to an external file that can be included in the main application.  This is handled through the use of attribute markers in specific locations.  At the moment, there are three different locations that can accept attributes:
1. `struct`s can be marked with the `[[codegen::Dictionary(Name)]]` attribute, where *Name* is a unique name (mostly the class name of the Renderable for which the code is generated).  Everything touched by the code generation has to be delared inside a struct (referred to as a *root struct*).  Member variables, structs, documentations can be modified by adding attributes of the style `[[codegen::TYPE(PARAMETER)]]` where *TYPE* is a keyword for the attribute and *PARAMETER* are optional parameters that might be necessary. Multiple attributes can be added by separating them with a comma (`[[codegen::key(ABC), codegen::inrange(0.0, 1.0)]]`). Structs can also be commented either with `//` comment lines or `/*` block comments before the definition of the struct begins
2. `enum class` can be marked with the `[[codegen::stringify]]` or `[[codegen::map]]` attribute to either create conversion functions that convert enum values to and from `string` values (for the first attribute) or that can map an enum to a second enum where all of the values have the same name. Alternatively `[[codegen::arrayify]]` will cause a function to be created that returns a vector containing all of the elements of the enum
3. A C-style function can be marked with `[[codegen::luawrap]]` which will generate code that extracts the necessary functions from a Lua state to call the function and then push the return value of the function back to the Lua stack.  The generated code also contains information about the name of the function, descriptions of the arguments and return values, and also the documentation for the marked function

Execution:
`codegen.exe C:/Development/OpenSpace/modules` to run it an all files recursively in the modules folder or `codegen.exe C:/Development/OpenSpace/modules/base/basemodule.cpp` to run in on this specific file.  Every file that does not contain a marked struct will be ignored.  For every other file `renderabletest.cpp`, a `renderabletest_codegen.cpp` will be generated that will have to be included directly *after* the struct definition.

Additionally, passing the `--verbose` parameter will cause CodeGen to emit extra information, including which files are currently being processed.

## Building
codegen configures through the [vcpkg](https://vcpkg.io) toolchain, which expects the `VCPKG_ROOT` environment variable to point at a vcpkg checkout.  It has no third-party runtime dependencies and no submodules; the shared `common-compile-settings` CMake helper is consumed as a vcpkg overlay port from `support/vcpkg/ports`.

```
git clone https://github.com/OpenSpace/codegen
cd codegen
cmake --preset windows-msvc
cmake --build --preset windows-msvc
```

The configure presets are `windows-msvc`, `windows-ninja-debug`, `windows-ninja-release`, `linux-ninja-debug`, `linux-ninja-release`, `linux-makefiles-debug`, and `linux-makefiles-release`.  Each has a build preset of the same name; the multi-config `windows-msvc` preset additionally provides `windows-msvc-debug` and `windows-msvc-relwithdebinfo`.

The unit tests under `tests/` link `openspace-core` and are only built from within the OpenSpace tree, which adds `support/coding/codegen/tests` directly; this standalone build never builds them.

### Consuming codegen
A superproject can pull codegen in as a vcpkg overlay port (`support/vcpkg/ports`) instead of `add_subdirectory`:

```cmake
find_package(codegen CONFIG REQUIRED)
target_link_libraries(main PRIVATE codegen::codegen-lib)   # and run codegen::codegen-tool as a build step
```

## Generated functions
Running the codegen will create a number of functions in the generated `_codegen.cpp` file that can be used by including the file in the main `.cpp` file.

If a struct  was marked with `codegen::Dictionary` the following functions will exist (this example assumes that the name of the marked struct was `P`):
 - `P bake(const ghoul::Dictionary&)`:  Will extract the parameters used to create `P` out of the passed Dictionary and will also verify that all parameters that are non-optional do exist and that all parameters have the correct type
 - `openspace::Documentation doc(std::string, openspace::Documentation)`:  Returns the documentation object that describes the parameters that a `Dictionary` need to fulfill to be successfully passed into the `bake` function.  The first parameter is the identifier of the documentation which needs to be unique. The optional second argument is a parent Documentation whose entires will be copied

If any enum in the file was marked with the `codegen::map(abc)` attribute the function `codegen::map<myspace::ABC>` is available that returns the corresponding type to the passed in value.

If any enum is marked with with `codegen::stringify()` attribute, the `toString` and `fromString` methods are created with allow the enum values to be converted to and from std representations repectively.

If a function `abc` was marked with the `codegen::luawrap` attribute, the generated file will contain a namespace `codegen::lua` with a struct `Abc` in it that contains all of the information about the marked function.


## Root struct
The root struct needs to be marked with the `[[codegen::Dictionary(Name)]]` attribute, where *Name* is a unique name.  Every struct not marked as such will be ignored by the codegen program.  The root struct (and the following include) **must** be declared in an anonymous namespace or else the resulting code might produce violations of the ODR rule.

### Attributes
 - `[[codegen::namespace(NS)]]`: Necessary if the type specified in the `Dictionary` attribute is living in namespace other than `openspace`

## Documentation
All types and variable definitions can have comments defined directly before the struct, or variable.

### Attributes
 - `[[codegen::verbatim(ID.description)]]` will be replaced with `ID.description` to be able to reuse descriptions from a PropertyInfo `ID` or any other parameter.  The only difference to writing `// ID.description` is that the latter will add " around the parameter which `codegen::verbatim` omits

## Supported types for member variables
 - C++ types: `bool`, `int`, `float`, `double`, `std::vector`, `std::optional`, `std::variant`, `std::string`, `std::filesystem::path`
 - `glm::ivec2`, `glm::ivec3`, `glm::ivec4`, `glm::dvec2`, `glm::dvec3`, `glm::dvec4`, `glm::vec2`, `glm::vec3`, `glm::vec4`, `glm::mat2x2`, `glm::mat2x3`, `glm::mat2x4`, `glm::mat3x2`, `glm::mat3x3`, `glm::mat3x4`, `glm::mat4x2`, `glm::mat4x3`, `glm::mat4x4`, `glm::dmat2x2`, `glm::dmat2x3`, `glm::dmat2x4`, `glm::dmat3x2`, `glm::dmat3x3`, `glm::dmat3x4`, `glm::dmat4x2`, `glm::dmat4x3`, `glm::dmat4x4`, `ghoul::Dictionary`
 - `struct`s (must be defined inside the root struct)
 - `enum class` (must be defind inside the root struct)

The variable's name will be used to get a value out of the dictionary in the baking process.  The only transformation of the name is that the first letter is capitalized (`referenceName` will be looked up as `ReferenceName`).

### Member Attributes
 - `[[codegen::key("Name")]]`:  Use the `Name` instead of the (first letter capitalized) variable name to look up a value in the Dictionary during the baking. This value has to be encapsulated in "" if it is a string literal.  If you want to use the return value of a function, the "" have to be omitted, like `[[codegen::key(unitList())]]`
 - `[[codegen::inrange(min, max)]]`:  Makes sure that the `int`, `float`, or `double` value is between the `min` and `max` values.  Both `min` and `max` are considered to be inclusive intervals.  Example: `float foo [[codegen::inrange(0.0, 1.0)]];`
 - `[[codegen::notinrange(min, max)]]`:  Makes sure that the `int`, `float`, or `double` value is outside the `min` and `max` values.  Both `min` and `max` are considered to be inclusive intervals.  Example: `float foo [[codegen::notinrange(0.0, 1.0)]];`
 - `[[codegen::less(val)]]`:  Makes sure that the `int`, `float`, or `double` value is less than the `val` value.  Example: `float foo [[codegen::less(0.0)]];`
 - `[[codegen::lessequal(val)]]`:  Makes sure that the `int`, `float`, or `double` value is less than or equal to the `val` value.  Example: `float foo [[codegen::lessequal(0.0)]];`
 - `[[codegen::greater(val)]]`, `[[codegen::greaterequal(val)]]`, `[[codegen:unequal(val)]]` likewise
 - `[[codegen::inlist("v1", "v2", "v3")]]`:  Checks whether a `std::string` variable is one of a finite list of values.  Example: `std::string foo [[codegen::inlist("v1", "v2", "v3")]];`
 - `[[codegen::notinlist("v1", "v2", "v3")]]`:  Checks whether a `std::string` variable is not one of a finite list of values.  Example: `std::string foo [[codegen::notinlist("v1", "v2", "v3")]];`
 - `[[codegen::annotation(text)]]`:  Adds an annotation decorator to the member.  Currently only supported for `std::string` and it cannot be used together with other attributes.  Example: `std::string foo [[codegen::annotation(Must be a valid bar)]]`
 - `[[codegen::reference("foo")]]`:  Marks a `ghoul::Dictionary` as a referencing verifier that will look up a different Documentation elsewhere in the code.
 - `[[codegen::color()]]`:  Marks a glm::vec3, glm::vec4, glm::dvec3, or glm::dvec4 as containing a color, meaning that a `ColorVerifier` is generated that checks whether all components are in the range `[0,1]`. The parameter must be either empty, `true`, or `false`
 - `[[codegen::directory()]]`: Marks a `std::filesystem::path` to be allowed to be a directory. The parameter must be either empty, `true`, or `false`
 - `[[codegen::notempty()]]`: Checks that a `std::string` that this attribute is attached to is not empty
 - `[[codegen::identifier()]]`: Checks that a `std::string` that this attribute is attached to fulfills the requirements of an identifier, meaning that is doesn't contain any whitespaces or dots
 - `[[codegen::datetime()]]`: Checks that a `std::string` is a valid ISO 8601 date string of the form `YYYY-MM-DD HH:MM:SS.uuu`
 - `[[codegen::private()]]`: This is value is specified, the value will be checked in the `bake` function, but not reported on in the `docs` function

## Enum class
`enum class` value are looked up through string matching against the enum value when baking.  For example:
```cpp
enum class E {
    V1,
    V2,
    V3
};
```
When baking, a Dictionary containing a string "V1" will result in the `E::V1` enum to be selected.

It is also possible to automatically generate the code to map an enum to an external enum automatically by adding the `[[codegen::map(OTHER_NAME)]]` attribute to the enum class.  For example if you have:
```cpp
namespace myspace {
enum class External {
  ValueA,
  ValueB,
  ValueC
};
}
```
somewhere in the code an you define a codegen:ed struct with:
```cpp
struct [[codegen::Dictionary(Name)]] {
  enum class [[codegen::map(myspace::External)]] Internal {
    ValueA,
    ValueB,
    ValueC
  };
}
```
then there is a function `codegen::map<myspace::External>` available that takes an `Internal` enum and returns the `External` with the same name.  NB: This matching is done purely on a name-to-name basis, so the enum values of `External` and `Internal` have to be **exactly** the same.

When using enums, often it is useful to inspect the individual elements of an enum, for which C++ does not have native support. In order to be able to do this, you can mark the enum using `[[codegen::arrayify()]]` in order to cause a function `codegen::arrayify` to be created.
For example:
```cpp
enum class [[codegen::arrayify()]] E {
    V1,
    V2,
    V3
};
```

will cause a function `codegen::arrayfiy<E>()` to be created that returns a `std::vector<E>` that contains each enum element in order in which it is specified.

If you want to make an `enum class` known to codegen for any other reason (most likely to use it as a parameter in a Lua function, the annotation `[[codegen::enum]]` will cause codegen to inspect the enum, without doing anything with it directly, thus making it possible to use it later in the file.

### Enum Attributes
 - `[[codegen::key(Name)]]`: Use the "Name" instead of the enum values name

## Root enums
`enum class` definitions can also happen on the root level.  Codegen will create code for these if they are marked with either the `codegen::stringify` or `codegen::map` attributes.  For the usage of the `codegen::map` attribute, see the relevant section above.  Any root enum that is marked with the `codegen::stringify` attribute will cause two functions to appear in the `_codegen.cpp` file: `codegen::toString` and `codegen::fromString`.  These functions can be used to convert between string representations of the enum values and the enum values.  Please note that the `codegen::key` value is used if an enum value uses one.  For example:
```cpp
enum class [[codegen::stringify()]] TestEnum {
  Value1,
  Value2 [[codegen::key("Different key")]]
};

assert(codegen::fromString<TestEnum>("Value1") == TestEnum::Value1);
assert(codegen::toString(TestEnum::Value1) == "Value1");

assert(codegen::fromString<TestEnum>("Different key") == TestEnum::Value2);
assert(codegen::toString(TestEnum::Value2) == "Different key");
```

## Lua Wrapping functions
The `codegen::luawrap` attribute can take an optional argument that overwrites the default behavior of using the function name as the exported Lua name.

```cpp
/**
 * Some documentation at the top
 */
[[codegen::luawrap("foo")]] std::string bar(int a, float f = 1.f) {
  return "abc";
}
```
Will be made available as the `foo` function in the Lua context and will generated the following:
```cpp
static const openspace::LuaLibrary::Function Bar = {
    "foo",
    [](lua_State* L) -> int {
        ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "foo");
        auto [a, f] = ghoul::lua::values<int, std::optional<float>>(L);
        try {
            std::string res = bar(
              a,
              f.has_value() ? std::move(*f) : 1.f
            );
            ghoul::lua::push(L, std::move(res));
            return 1;
        }
        catch (const ghoul::lua::LuaError& e) {
            return ghoul::lua::luaError(L, e.message);
        }
    },
    {
        { "a", "Integer" },
        { "f", "Number?", "1.f" }
    },
    "String",
    "Some documentation at the top"
};
```


# Verifier Mappings
This is a complete list of variable types and attribute combinations.  We are **not** listing the `[[codegen::key(...)]]` attribute here, as this is allowed with *every* variable.

 - `int` + `[[codegen::inrange]]` -> `InRangeVerifier<IntVerifier>`
 - `int` + `[[codegen::notinrange]]` -> `NotInRangeVerifier<IntVerifier>`
 - `int` + `[[codegen::less]]` -> `LessVerifier<IntVerifier>`
 - `int` + `[[codegen::lessequal]]` -> `LessEqualVerifier<IntVerifier>`
 - `int` + `[[codegen::greater]]` -> `GreaterVerifier<IntVerifier>`
 - `int` + `[[codegen::greaterequal]]` -> `GreaterEqualVerifier<IntVerifier>`
 - `int` + `[[codegen::unequal]]` -> `UnequalVerifier<IntVerifier>`
 - `double` + `[[codegen::inrange]]` -> `InRangeVerifier<DoubleVerifier>`
 - `double` + `[[codegen::notinrange]]` -> `NotInRangeVerifier<DoubleVerifier>`
 - `double` + `[[codegen::less]]` -> `LessVerifier<DoubleVerifier>`
 - `double` + `[[codegen::lessequal]]` -> `LessEqualVerifier<DoubleVerifier>`
 - `double` + `[[codegen::greater]]` -> `GreaterVerifier<DoubleVerifier>`
 - `double` + `[[codegen::greaterequal]]` -> `GreaterEqualVerifier<DoubleVerifier>`
 - `double` + `[[codegen::unequal]]` -> `UnequalVerifier<DoubleVerifier>`
 - `float` + `[[codegen::inrange]]` -> `InRangeVerifier<DoubleVerifier>`
 - `float` + `[[codegen::notinrange]]` -> `NotInRangeVerifier<DoubleVerifier>`
 - `float` + `[[codegen::less]]` -> `LessVerifier<DoubleVerifier>`
 - `float` + `[[codegen::lessequal]]` -> `LessEqualVerifier<DoubleVerifier>`
 - `float` + `[[codegen::greater]]` -> `GreaterVerifier<DoubleVerifier>`
 - `float` + `[[codegen::greaterequal]]` -> `GreaterEqualVerifier<DoubleVerifier>`
 - `float` + `[[codegen::unequal]]` -> `UnequalVerifier<DoubleVerifier>`
 - `std::string` + `[[codegen::inlist]]` -> `InListVerifier<StringVerifier>`
 - `std::string` + `[[codegen::unequal]]` -> `UnequalVerifier<StringVerifier>`
 - `std::string` + `[[codegen::annotation]]` -> `AnnotationVerifier<StringVerifier>`
 - `std::string` + `[[codegen::notempty]]` -> `StringVerifier(true)`
 - `glm::vec2` + `[[codegen::inrange]]` -> `InRangeVerifier<DoubleVec2Verifier>`
 - `glm::vec3` + `[[codegen::inrange]]` -> `InRangeVerifier<DoubleVec3Verifier>`
 - `glm::vec4` + `[[codegen::inrange]]` -> `InRangeVerifier<DoubleVec4Verifier>`
 - `glm::ivec2` + `[[codegen::inrange]]` -> `InRangeVerifier<IntVec2Verifier>`
 - `glm::ivec3` + `[[codegen::inrange]]` -> `InRangeVerifier<IntVec3Verifier>`
 - `glm::ivec4` + `[[codegen::inrange]]` -> `InRangeVerifier<IntVec4Verifier>`
 - `glm::vec2` + `[[codegen::notinrange]]` -> `NotInRangeVerifier<DoubleVec2Verifier>`
 - `glm::vec3` + `[[codegen::notinrange]]` -> `NotInRangeVerifier<DoubleVec3Verifier>`
 - `glm::vec4` + `[[codegen::notinrange]]` -> `NotInRangeVerifier<DoubleVec4Verifier>`
 - `glm::ivec2` + `[[codegen::notinrange]]` -> `NotInRangeVerifier<IntVec2Verifier>`
 - `glm::ivec3` + `[[codegen::notinrange]]` -> `NotInRangeVerifier<IntVec3Verifier>`
 - `glm::ivec4` + `[[codegen::notinrange]]` -> `NotInRangeVerifier<IntVec4Verifier>`
 - `glm::vec2` + `[[codegen::less]]` -> `LessVerifier<DoubleVec2Verifier>`
 - `glm::vec3` + `[[codegen::less]]` -> `LessVerifier<DoubleVec3Verifier>`
 - `glm::vec4` + `[[codegen::less]]` -> `LessVerifier<DoubleVec4Verifier>`
 - `glm::ivec2` + `[[codegen::less]]` -> `LessVerifier<IntVec2Verifier>`
 - `glm::ivec3` + `[[codegen::less]]` -> `LessVerifier<IntVec3Verifier>`
 - `glm::ivec4` + `[[codegen::less]]` -> `LessVerifier<IntVec4Verifier>`
 - `glm::vec2` + `[[codegen::lessequal]]` -> `LessEqualVerifier<DoubleVec2Verifier>`
 - `glm::vec3` + `[[codegen::lessequal]]` -> `LessEqualVerifier<DoubleVec3Verifier>`
 - `glm::vec4` + `[[codegen::lessequal]]` -> `LessEqualVerifier<DoubleVec4Verifier>`
 - `glm::ivec2` + `[[codegen::lessequal]]` -> `LessEqualVerifier<IntVec2Verifier>`
 - `glm::ivec3` + `[[codegen::lessequal]]` -> `LessEqualVerifier<IntVec3Verifier>`
 - `glm::ivec4` + `[[codegen::lessequal]]` -> `LessEqualVerifier<IntVec4Verifier>`
 - `glm::vec2` + `[[codegen::greater]]` -> `GreaterVerifier<DoubleVec2Verifier>`
 - `glm::vec3` + `[[codegen::greater]]` -> `GreaterVerifier<DoubleVec3Verifier>`
 - `glm::vec4` + `[[codegen::greater]]` -> `GreaterVerifier<DoubleVec4Verifier>`
 - `glm::ivec2` + `[[codegen::greater]]` -> `GreaterVerifier<IntVec2Verifier>`
 - `glm::ivec3` + `[[codegen::greater]]` -> `GreaterVerifier<IntVec3Verifier>`
 - `glm::ivec4` + `[[codegen::greater]]` -> `GreaterVerifier<IntVec4Verifier>`
 - `glm::vec2` + `[[codegen::greaterequal]]` -> `GreaterEqualVerifier<DoubleVec2Verifier>`
 - `glm::vec3` + `[[codegen::greaterequal]]` -> `GreaterEqualVerifier<DoubleVec3Verifier>`
 - `glm::vec4` + `[[codegen::greaterequal]]` -> `GreaterEqualVerifier<DoubleVec4Verifier>`
 - `glm::ivec2` + `[[codegen::greaterequal]]` -> `GreaterEqualVerifier<IntVec2Verifier>`
 - `glm::ivec3` + `[[codegen::greaterequal]]` -> `GreaterEqualVerifier<IntVec3Verifier>`
 - `glm::ivec4` + `[[codegen::greaterequal]]` -> `GreaterEqualVerifier<IntVec4Verifier>`
 - `glm::vec2` + `[[codegen::unequal]]` -> `UnequalVerifier<DoubleVec2Verifier>`
 - `glm::vec3` + `[[codegen::unequal]]` -> `UnequalVerifier<DoubleVec3Verifier>`
 - `glm::vec4` + `[[codegen::unequal]]` -> `UnequalVerifier<DoubleVec4Verifier>`
 - `glm::ivec2` + `[[codegen::unequal]]` -> `UnequalVerifier<IntVec2Verifier>`
 - `glm::ivec3` + `[[codegen::unequal]]` -> `UnequalVerifier<IntVec3Verifier>`
 - `glm::ivec4` + `[[codegen::unequal]]` -> `UnequalVerifier<IntVec4Verifier>`
 - `glm::vec3` + `[[codegen::color]]` -> `Color3Verifier`
 - `glm::vec4` + `[[codegen::color]]` -> `Color4Verifier`
 - `glm::dvec3` + `[[codegen::color]]` -> `Color3Verifier`
 - `glm::dvec4` + `[[codegen::color]]` -> `Color4Verifier`
 - `std::filesystem::path` + `[[codegen::directory()]]` -> `DirectoryVerifier`
 - `ghoul::Dictionary` + `[[codegen::reference()]]` -> `ReferencingVerifier`
