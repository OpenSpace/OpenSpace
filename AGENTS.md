# AGENTS.md
Guidance for AI coding agents working in the OpenSpace repository. Read this before changing code. It describes the conventions the codebase *already* follows; when in doubt, open a neighboring file and match it exactly.

Also read [.github/AI_CODE_OF_CONDUCT.md](.github/AI_CODE_OF_CONDUCT.md). AI is a tool, never an author. The human submitting the change has to understand and be able to explain every line. Do not add dead code, unused dependencies, or speculative abstractions.

## Project overview
OpenSpace is an open-source C++23/OpenGL astrovisualization engine. Its uses include planetariums, research and public presentations. It supports Windows and Linux; macOS is not supported (no double precision in Metal).

| Path | Contents |
|---|---|
| `include/openspace/`, `src/` | Core engine (`openspace-core`). The header in `include/openspace/x/y.h` pairs with `src/x/y.cpp` |
| `modules/<name>/` | Optional feature modules (rendering types, data formats, integrations) |
| `apps/` | Executables: `OpenSpace` (main app and launcher), `TaskRunner`, `Sync`, `DocsWriter`, ... |
| `support/codegen/` | In-tree code generator for `[[codegen::...]]` attributes (see its `README.md`) |
| `support/cmake/` | CMake helpers (`create_new_module`, `set_openspace_compile_settings`, ...) |
| `support/coding/check_style_guide.py` | Style checker for C++ files |
| `shaders/` | Core GLSL shaders and includes (`fragment.glsl`, `powerscaling/...`) |
| `data/assets/` | Lua `.asset` files that describe scene content |
| `data/profiles/` | JSON `.profile` files that pick which assets to load |
| `tests/` | Catch2 unit tests (single `OpenSpaceTest` executable) |
| `visualtests/` | Image-regression tests (`.ostest`), run through `support/runner/` |
| `ext/` | Only `renderdoc`. All other third-party dependencies come from vcpkg (`vcpkg.json`) |

## Build and test
- **Requirements:** CMake >= 4.0, a C++23 compiler (MSVC, GCC or Clang), and the `VCPKG_ROOT` environment variable set. Builds go into `build/<preset>/`; binaries go into `bin/`.
- **Configure presets:** `windows-msvc`, `windows-msvc-arm64`, `windows-ninja-{debug,release}`, `linux-ninja-{debug,release}` and `linux-makefiles-{debug,release}`.
- **Build presets:** match the configure presets. `windows-msvc` also has the variants `-debug`, `-relwithdebinfo` and `-release`.

```sh
cmake --preset windows-msvc                       # or linux-ninja-release, ...
cmake --build --preset windows-msvc --parallel
ctest --preset windows-msvc                       # runs OpenSpaceTest
bin/<Config>/OpenSpaceTest "[lrucache]"           # run a single Catch2 tag directly
python support/coding/check_style_guide.py        # style check; add `strict` for 90-col etc.
```

- **Modules:** each module can be toggled with `-DOPENSPACE_MODULE_<NAME>=OFF`, and all are on by default. Tests are controlled by `OPENSPACE_HAVE_TESTS` (ON).
- **vcpkg features:** vcpkg reads manifest features only when the build directory is first created. After changing those options, delete `build/<preset>/vcpkg_installed`.
- **Codegen:** runs automatically on every build (the `run_codegen` target). It produces `<file>_codegen.cpp` next to each source that uses `[[codegen::...]]`. Never commit or hand-edit these files.
- **CI** (`Jenkinsfile`) builds on Linux with GCC and Clang (`linux-makefiles-release`) and on Windows with MSVC (`windows-msvc`). It runs `ctest` and cppcheck and records compiler warnings. It does **not** run the style checker, so run it yourself.
- **Warnings:** the warning sets are strict (`/W4` plus extras, `-Wall -Wextra -Wpedantic -Wshadow -Wconversion ...`). New code must compile warning-free on all three compilers.


## Mechanical rules (enforced by `check_style_guide.py`)
- **Copyright header:** every C++, GLSL and CMake file starts with the boxed MIT header, `Copyright (c) 2014-2026`, using the current year. Copy it verbatim from a neighboring file. C++ and GLSL use the `/****...*/` box; CMake uses the `####...#` box. The box is exactly 90 columns wide.
- **Include guards:** no `#pragma once`. Line 24 is blank, the guard starts at line 25, and the file ends with a matching comment:
  - Core: `__OPENSPACE_CORE___<FILENAME>___H__`
  - Module: `__OPENSPACE_MODULE_<MODULE>___<FILENAME>___H__`

  ```cpp
  #ifndef __OPENSPACE_MODULE_BASE___RENDERABLESPHERE___H__
  #define __OPENSPACE_MODULE_BASE___RENDERABLESPHERE___H__
  ...
  #endif // __OPENSPACE_MODULE_BASE___RENDERABLESPHERE___H__
  ```
- **Line length:** at most **90 columns**.
- **Whitespace:** 4-space indentation, no tabs, no trailing whitespace, no lines that contain only whitespace, a final newline, UTF-8 without BOM. `.asset` and `.glsl` files use **2 spaces** (see `.editorconfig`).
- **GLM:** `#include <openspace/glm.h>`, never `<glm/glm.hpp>`. Extra `glm/gtx/...` headers may be included directly.
- **Assertions:** avoid bare `assert(`. Use `assert_msg(cond, "message")` (`openspace/misc/assert.h`), `static_assert`, or `precondition`/`postcondition` (`openspace/misc/invariants.h`). Conditions must not have side effects.
- **`using namespace`:** never in headers or `.inl` files that leak it. In `.cpp` files, put `using namespace openspace;` inside the top anonymous namespace.
- **`std::getline`:** use `openspace::getline` from `openspace/misc/stringhelper.h`.
- **Core dependencies:** the core (`include/`, `src/`) must never depend on a module. Soft dependencies between modules use `#ifdef OPENSPACE_MODULE_<NAME>_ENABLED`.


## Formatting
There is no `.clang-format`. Follow these rules by hand.

- **Braces:** attached (K&R) for functions, classes, namespaces and control flow. Always use braces, even for one-line bodies.
- **`else`:** goes on its own line after the closing brace. Do not write `} else {`.
- **Wrapping at 90 columns:**
  - **Definitions:** align wrapped parameters under the first one and put `{` on its own line.
  - **Constructors:** the initializer list uses a leading `:` and leading `,`, indented 4, with `{` on its own line.
  - **Declarations in headers:** continue at +4 rather than aligning.
  - **Wrapped conditions:** the operator stays at the end of the line and `{` moves to its own line.
  - **Long calls:** one argument per line at +4, with `)` on its own line.

  ```cpp
  RenderableSphere::RenderableSphere(const Dictionary& dictionary,
                                     RenderableSettings settings)
      : Renderable(dictionary, std::move(settings))
      , _size(SizeInfo)
      , _segments(SegmentsInfo)
  {
  ```
  ```cpp
  if (status == Astrocast::Status::ClientWithHost ||
      status == Astrocast::Status::ClientWithoutHost)
  {
  ```
- **Pointers and `const`:** attach `*` and `&` to the type (`const T& x`, `T* p`), and put `const` first (west const).
- **Namespaces:**
  - Content of `namespace openspace {` is **not** indented.
  - The anonymous namespace at the top of a `.cpp` file **is** indented 4.
  - Every namespace closes with a comment: `} // namespace openspace`, `} // namespace`.
  - Nested namespaces use the compact form `namespace openspace::io {`.
- **`switch`:** indent `case` inside `switch`. Unhandled enum values end in `default: throw MissingCaseException();`.
- **Spacing:** one blank line between function definitions.
- **Attributes:** place them after the condition: `if (_shader->isDirty()) [[unlikely]] {`.


## Naming
| Kind | Style | Example |
|---|---|---|
| Classes, structs, enums, enum values | PascalCase | `RenderableSphere`, `RenderBin::Opaque` |
| Functions, methods, locals, parameters | camelCase | `calcModelTransform`, `adjustedOpacity` |
| Private/protected members | `_camelCase` | `_size`, `_uniformCache`, `_sphereIsDirty` |
| Public struct fields | camelCase, no underscore | `automaticallyUpdateRenderBin` |
| Constants, `static` members | PascalCase | `constexpr int DefaultBlending = 0;`, `ProgramObjectManager` |
| Property info constants | `<Thing>Info` | `SizeInfo`, `SegmentsInfo` |
| Logger category | `_loggerCat` | `constexpr std::string_view _loggerCat = "RenderableSphere";` |
| Files | all lowercase, no separators | `renderableconstellationbounds.cpp` |
| Template parameters | `T`, or PascalCase | `KeyType` |

More rules:
- **Enums:** prefer `enum class`.
- **Boolean parameters:** use strong types, `BooleanType(IsReadOnly);` used as `IsReadOnly::Yes`, instead of raw `bool` parameters.
- **Booleans:** read as predicates (`isX`, `hasX`, `shouldX`, `useX`, `xIsDirty`).
- **File suffixes:** reserved for `_lua.inl` (Lua API) and the generated `_codegen.cpp`.


## Includes and headers
- **Order:**
  1. The file's own header (in a `.cpp`) or base class header (in a `.h`).
  2. A blank line.
  3. One block containing `<modules/...>`, then `<openspace/...>`, then third-party, then the standard library, each roughly alphabetical.
- **Brackets:** always use angle brackets with the full path, e.g. `#include <modules/base/basemodule.h>`. The only quoted includes are the generated `"foo_codegen.cpp"` and `.inl` files.
- **Headers:** keep them light. Forward-declare inside the namespace, e.g. `namespace opengl { class ProgramObject; }`.
- **Header documentation:** use Doxygen `/** ... */` with **backslash** commands (`\param`, `\return`, `\pre`, `\p name`). Wrap a `\param` continuation so it aligns after the parameter name. Use `///` for one-line member docs. Put code identifiers in backticks.
- **Comments:** implementation comments use `//` and are written as full sentences. TODOs are written `// @TODO` or `// @TODO(name)`.


## C++ idioms
- **Standard:** C++23, used conservatively. Prefer explicit, `const`-qualified local types
  (`const glm::dmat4 modelView = ...;`). Use `auto` for iterators, lambdas, structured
  bindings and obvious `make_unique` results, avoid in other places.
- **Formatting strings:** `std::format`. Do not use string concatenation chains.
- **Common types:** `std::string_view` for read-only strings, `std::filesystem::path` for paths, and `std::optional` for optional values.
- **Paths:** resolve with `absPath("${MODULE_BASE}/shaders/x.glsl")`, `${DATA}`, `${TEMPORARY}`
  and so on.
- **Errors:** use exceptions, not error codes:
  - `throw RuntimeError(msg, component)` in general
  - `FileNotFoundError` for missing files
  - `lua::LuaError` inside Lua functions
- **Logging:** `LDEBUG`, `LINFO`, `LWARNING` and `LERROR` with a file-local `_loggerCat`. Use the `LERRORC(category, msg)` variants for an explicit category.
- **Ownership:** use `std::unique_ptr`/`std::make_unique`. Use `std::shared_ptr` only when ownership is actually shared. Raw pointers are non-owning. Use `nullptr`, never `NULL`.
- **Casts:** `static_cast`/`reinterpret_cast`; no C-style casts.
- **Virtual functions:** write `override` on every override and never repeat `virtual`. Write `~X() override = default;`. Mark leaf types `final` where appropriate.
- **Compile-time code:** use `constexpr` liberally. Use designated initializers for aggregate configuration structs.
- **Scope cleanup:** `defer { ... };` (`openspace/misc/defer.h`).
- **Profiling:** add `ZoneScoped` (Tracy) in hot paths, as neighboring code does.
- **Global subsystems:** reach them through `global::` (`openspace/engine/globals.h`): `global::renderEngine`, `global::timeManager`, `global::navigationHandler`, `global::scriptEngine`, `global::eventEngine`, ...

### Property rules
- **`PropertyInfo`:** use the typed aliases (`FloatPropertyInfo`, `IntPropertyInfo`, `BoolPropertyInfo`, `Vec3PropertyInfo`, `StringPropertyInfo`, `OptionPropertyInfo`, ...) with designated initializers.
- **Field order:** `.identifier`, `.name`, `.description`, `.visibility`, `.isEnablable`, `.needsConfirmation`, `.isReadOnly`, `.defaults`.
- **Defaults:** put the default value and range in `.defaults` (`.value`, `.minMax`, `.stepSize`, `.exponent`), not in the constructor. Older code that passes positional `Property::PropertyInfo { ... }` is being migrated; do not copy it.
- **Identifiers:** PascalCase, no whitespace, no `.` (the dot is the URI separator).
- **Names:** sentence case ("Night factor").
- **Descriptions:** complete sentences ending in a period.
- **Visibility:** pick the level deliberately: `NoviceUser`, `User`, `AdvancedUser`, `Developer` or `Hidden`.
- **Wiring:**
  - Initialize each property in the member-init list from its `Info` and apply `p.x.value_or(_x)`.
  - Then call `setViewOption(...)` if needed, attach `onChange`, and finish with `addProperty(_x)`.
  - `onChange` callbacks set dirty flags; the GL work happens in `update`/`render`.
  - Group related properties into a nested `PropertyOwner` and add it with `addPropertySubOwner`.

### Codegen `Parameters` rules
- **Placement:** the root `Parameters` struct must be in the anonymous namespace. The `#include "<file>_codegen.cpp"` comes immediately after `} // namespace`, with no blank line.
- **Optional members:** use `std::optional<T>` for optional keys. Lua keys are the member names with the first letter capitalized (`size` becomes `Size`).
- **Validation:** validate in the attributes rather than by hand. Available attributes include `codegen::greater`, `greaterequal`, `inrange`, `inlist`, `reference("...")`, `color()`, `key("...")`, `map(Enum)`, ...
- **Documentation identifiers:** `<module>_<snake_case>` or `core_...`. Every documented type must be listed in its module's `documentations()`.


## Modules
A module lives in `modules/<name>/` and contains:
- `<name>module.h/.cpp`: a class `<Name>Module : public OpenSpaceModule` with `constexpr static ... Name = "<Name>"`.
- `CMakeLists.txt`
- optionally `include.cmake`, `<name>module_lua.inl`, and subfolders such as `rendering/`, `shaders/`, `scripts/`, `tasks/`, `rotation/`, `translation/`, `dashboard/` ...

Rules:
- **List every file explicitly** in the module's `CMakeLists.txt` under `HEADER_FILES`, `SOURCE_FILES` (including `_lua.inl`) and `SHADER_FILES`, each followed by `source_group(...)`. Then call `create_new_module("<Name>", <name>_module ...)`. Nothing is globbed. The same goes for `src/CMakeLists.txt` and `tests/CMakeLists.txt`.
- **`include.cmake`:** declares module dependencies (`set(OPENSPACE_DEPENDENCIES base ...)`), external `find_package` dependencies, and platform gating (`set(SUPPORTED OFF)`).
- **Register types in `internalInitialize`:**
  ```cpp
  TemplateFactory<Renderable>* fRenderable = FactoryManager::ref().factory<Renderable>();
  assert_msg(fRenderable, "Renderable factory was not created");
  fRenderable->registerClass<RenderableFoo>("RenderableFoo");
  ```
  The registered string equals the class name. Add the type's `Documentation()` to `documentations()`, keeping the list alphabetical.
- **Module paths:** each module gets a `${MODULE_<UPPERNAME>}` path token automatically.


## Lua scripting API
- **Location:** Lua functions are free functions in an anonymous namespace inside `<file>_lua.inl`. That file is `#include`d by the owning `.cpp` and ends with `#include "<file>_lua_codegen.cpp"`.
- **Naming:** functions are camelCase. Codegen exposes `foo` as `codegen::lua::Foo`. To use a different Lua name, write `[[codegen::luawrap("name")]]`.
- **Documentation:** the Doxygen `/** */` comment above the function becomes the Lua help text.
- **Arguments:** C++ default arguments become optional Lua arguments. `std::optional`, `std::variant`, `std::filesystem::path`, `Dictionary` and `glm` types are supported.
- **Errors:** report them with `throw lua::LuaError(std::format(...))`.

## Rendering and GLSL
- **Renderable lifecycle:** the constructor parses the `Dictionary`. `initializeGL` creates GPU resources and `deinitializeGL` releases them. `update(const UpdateData&)` handles per-frame state and dirty flags. `render(const RenderData&, RendererTasks&)` draws.
- **Shader programs:**
  - Build with `global::renderEngine->buildRenderProgram(name, vsPath, fsPath)` and release with `global::renderEngine->removeRenderProgram(...)`.
  - Share programs between instances through `<Module>Module::ProgramObjectManager.request` and `.release` (see `renderablecartesianaxes.cpp`).
  - Programs outside the render pipeline use `opengl::ProgramObject::Build`.
- **Uniforms:** use `UniformCache(a, b, c) _uniformCache;` together with `opengl::updateUniformLocations(*_program, _uniformCache)`. The cache names must match the GLSL uniform names exactly.
- **Shader hot reload:** in `update`, write `if (_program->isDirty()) [[unlikely]] { _program->rebuildFromFile(); updateUniformLocations(...); }`.
- **OpenGL API:** use the direct state access style (`glCreateBuffers`, `glNamedBufferStorage`, `glVertexArrayAttribFormat`, ...).
- **GL state:** restore blend, depth and line state through `global::renderEngine->openglStateCache()`.
- **Shader files:** named `<name>_vs.glsl`, `_fs.glsl`, `_gs.glsl` and stored in the module's `shaders/` folder. They carry the same copyright box and use 2-space indentation.
- **Vertex shaders:** start with `#version __CONTEXT__` and use `in_`/`out_` naming or interface blocks (`out Data { ... } out_data;`).
- **Fragment shaders:** `#include "fragment.glsl"` and implement `Fragment getFragment()`, setting `frag.color` and `frag.depth`.

## Assets and profiles (`data/`)
- **Format:** assets are Lua with 2-space indentation.
- **Structure:**
  1. `local x = asset.require("./transforms")` and/or `asset.resource({...})`.
  2. Local tables for nodes and actions.
  3. `asset.onInitialize` and `asset.onDeinitialize`. Deinitialize in **reverse** order.
  4. `asset.export(...)`.
  5. A trailing `asset.meta = { Name, Version, Description, Author, URL, License }`.
- **Naming:**
  - Scene graph node identifiers are PascalCase (`MarsTrail`).
  - Synchronization identifiers are snake_case (`mars_labels`).
  - Action identifiers are `os.PascalCase`.
  - GUI paths look like `"/Solar System/Planets/Mars"`.
- **Profiles:** `data/profiles/*.profile` files are JSON (`version: { major: 1, minor: 5 }`).
- **Lua tooling:** `.luarc.json` and `support/types/asset.lua` give LuaLS type hints. Keep `support/types/asset.lua` in sync when changing the asset Lua API in `src/scene/assetmanager.cpp`.

## Tests
- **Framework:** Catch2 v3. Add files as `tests/test_<topic>.cpp` (or in a subfolder) and
  list them in `tests/CMakeLists.txt`.
- **Naming:** `TEST_CASE("Component: Description", "[tag]")`. Prefer `CHECK` over `REQUIRE` unless continuing would crash.
- **Helpers:** put them in an anonymous namespace.
- **Asserts:** tests build with `OPENSPACE_THROW_ON_ASSERT`, so `assert_msg` failures can be tested as exceptions.
- **Coverage:** add or extend tests for non-GL logic you change (parsers, conversions, documentation/verifier, Lua bindings). Rendering changes can be covered by a `visualtests/` entry.

## Git and pull requests
- **Default branch:** `master`. Work on a branch such as `feature/<short-name>` or `issue/<number>`; `.github/CONTRIBUTING.md` mentions `pr/<feature>`.
- **Commit subjects:** imperative, capitalized, concise, with no prefix tags and no trailing period: "Fix GLSL compile issue (closes #4331)", "Add optional extended de441 kernels ...". PRs are squash-merged and GitHub appends ` (#NNNN)`.
- **PR descriptions:** reference the issue being solved with "closes #XXXX".
- **Generated files:** never commit them (`*_codegen.cpp`, `COMMIT.md`, `bin/`, `build/`, `sync/`, `cache/`, `logs/`).


## Checklist before finishing
1. The code compiles without new warnings (MSVC `/W4`, GCC and Clang `-Wall -Wextra -Wpedantic ...`).
2. `python support/coding/check_style_guide.py strict` reports nothing for files you touched.
3. New files:
   - have the current copyright header and correct include guard;
   - are lowercase;
   - are listed in the right `CMakeLists.txt`.
4. New types are registered with their factory and listed in `documentations()`. Their `Parameters` and `PropertyInfo`s have user-facing descriptions.
5. `OpenSpaceTest` passes, and tests were added where applicable.
6. The change matches the surrounding code in naming, formatting and comment density. It introduces no new dependency or abstraction that the task did not need.
