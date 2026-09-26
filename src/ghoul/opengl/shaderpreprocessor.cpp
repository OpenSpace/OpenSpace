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

#include <ghoul/opengl/shaderpreprocessor.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/glm.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/systemcapabilities/openglcapabilitiescomponent.h>
#include <scn/scan.h>
#include <fstream>
#include <iterator>
#include <limits>
#include <ranges>
#include <sstream>
#include <tuple>
#include <utility>

// #endfor
// #version __CONTEXT__
// #define __OS__
// #include
// #for <key>, <value> in <dictionary>

namespace {
    using namespace ghoul;
    using namespace ghoul::opengl;

    constexpr std::string_view Ws = " \n\r\t";

    struct ShaderPreprocessorError final : public RuntimeError {
        explicit ShaderPreprocessorError(std::string msg)
            : RuntimeError(std::move(msg), "ShaderPreprocessor")
        {}
    };

    bool hasKeyRecursive(const Dictionary& dictionary, std::string_view key) {
        const size_t dotPos = key.find('.');
        if (dotPos == std::string_view::npos) {
            return dictionary.hasKey(key);
        }

        const std::string_view before = key.substr(0, dotPos);
        if (!dictionary.hasKey(before)) {
            return false;
        }


        const Dictionary d = dictionary.value<Dictionary>(before);
        const std::string_view after = key.substr(dotPos + 1);
        return hasKeyRecursive(d, after);
    }

    template <typename T>
    bool hasValueRecursive(const Dictionary& dictionary, std::string_view key) {
        const size_t dotPos = key.find('.');
        if (dotPos == std::string_view::npos) {
            return dictionary.hasValue<T>(key);
        }

        const std::string_view before = key.substr(0, dotPos);
        if (!dictionary.hasValue<Dictionary>(before)) {
            return false;
        }

        const Dictionary d = dictionary.value<Dictionary>(before);
        const std::string_view after = key.substr(dotPos + 1);
        return hasValueRecursive<T>(d, after);
    }

    template <typename T>
    T valueRecursive(const Dictionary& dictionary, std::string_view key) {
        const size_t dotPos = key.find('.');
        if (dotPos == std::string_view::npos) {
            return dictionary.value<T>(key);
        }

        const std::string_view before = key.substr(0, dotPos);
        const std::string_view after = key.substr(dotPos + 1);
        const Dictionary d = dictionary.value<Dictionary>(before);
        return valueRecursive<T>(d, after);
    }

    class Env {
    public:
        explicit Env(Dictionary dictionary);

        void processFile(const std::filesystem::path& path);
        std::vector<std::filesystem::path> includedFiles() const;
        std::string output();

    private:
        struct InputFile {
            std::ifstream& stream;
            const std::filesystem::path file;
            unsigned int lineNumber = 1;
        };

        struct ForStatement {
            const std::filesystem::path file;
            const unsigned int lineNumber;
            const std::streampos streamPos;

            const std::string keyName;
            const std::string valueName;
            const std::string dictionary;

            static constexpr size_t EmptyLoop = std::numeric_limits<size_t>::max();
            size_t currentIteration;
        };

        bool parseFor(std::string_view line);
        bool parseEndFor(std::string_view line);
        bool parseInclude(std::string_view line);
        bool parseVersion(std::string_view line);
        bool parseOs(std::string_view line);

        std::string substitute(std::string_view in) const;
        std::string resolveAlias(std::string_view in) const;

        void addLineNumber();
        std::string fileInfo() const;

        std::stringstream _output;
        std::vector<InputFile> _inputFiles;
        std::vector<ForStatement> _forStatements;

        std::vector<std::filesystem::path> _includedFiles;
        Dictionary _dictionary;
    };

    Env::Env(Dictionary dictionary)
        : _dictionary(std::move(dictionary))
    {}

    void Env::processFile(const std::filesystem::path& path) {
        ZoneScoped;

        ghoul_assert(!path.empty(), "Path must not be empty");
        ghoul_assert(std::filesystem::is_regular_file(path), "Path must exist");

        auto it = std::find(_includedFiles.begin(), _includedFiles.end(), path);
        if (it == _includedFiles.end()) {
            _includedFiles.push_back(path);
        }

        std::ifstream stream = std::ifstream(path, std::ifstream::binary);
        if (!stream.good()) {
            throw ShaderPreprocessorError(std::format("Error loading file '{}'", path));
        }

        _inputFiles.emplace_back(stream, path);
        if (_inputFiles.size() > 1) {
            addLineNumber();
        }


        // Parse the individual lines
        while (true) {
            Env::InputFile& input = _inputFiles.back();
            std::string line;
            if (!ghoul::getline(input.stream, line)) {
                break;
            }
            input.lineNumber++;

            if (!line.empty() && line.back() == '\r') {
                line.pop_back();
            }

            // We need to check for the termination of the for loop already here or else
            // the next `continue` will always be called once it starts
            bool isLineProcessed = parseEndFor(line);

            // Check for an empty for loop. This can happen when the loop variables is
            // evaluated to be never true
            const bool isInsideEmptyForStatement =
                !_forStatements.empty() &&
                (_forStatements.back().currentIteration == ForStatement::EmptyLoop);
            if (isInsideEmptyForStatement) {
                continue;
            }

            // Replace all #{<name>} strings with data from <name> in dictionary
            while (true) {
                const size_t begin = line.rfind("#{");
                if (begin == std::string::npos) {
                    break;
                }
                const size_t end = line.find('}', begin + 2);
                if (end == std::string::npos) {
                    throw ShaderPreprocessorError(std::format(
                        "Could not parse line. {}", fileInfo()
                    ));
                }

                const std::string in = line.substr(begin + 2, end - (begin + 2));
                const std::string out = substitute(in);

                const std::string first = line.substr(0, begin);
                const std::string last = line.substr(end + 1);
                line = std::format("{}{}{}", first, out, last);
            }

            isLineProcessed |= parseVersion(line);
            isLineProcessed |= parseOs(line);
            isLineProcessed |= parseInclude(line);
            isLineProcessed |= parseFor(line);

            if (!isLineProcessed) {
                _output << std::format("{}\n", line);
            }
        }


        // We need to check that we don't have any dangling for statements that might not
        // have been closed. We only want to throw an error when the file that opened the
        // for loop has not closed it at the end. The extra checks are necessary as we
        // might include other shader files inside the for loop, which should not trip the
        // exception
        if (!_forStatements.empty()) {
            if (_inputFiles.size() == 1) {
                // If there are no other input files left, this was the last file and we
                // were responsible for the dangling for loop
                throw ShaderPreprocessorError(std::format(
                    "Unexpected end of file. Still processing #for loop from {}:{}. {}",
                    _inputFiles.back().file, _forStatements.back().lineNumber, fileInfo()
                ));
            }

            // Get the second-to-last input file. It has to be the one responsible for the
            // for loop
            const Env::InputFile& prev = _inputFiles[_inputFiles.size() - 2];
            if (_forStatements.back().file != prev.file) {
                const ForStatement& forStatement = _forStatements.back();
                const InputFile& input = _inputFiles.back();
                throw ShaderPreprocessorError(std::format(
                    "Unexpected end of file. Still processing #for loop from {}:{}. {}",
                    input.file, forStatement.lineNumber, fileInfo()
                ));
            }
        }

        _inputFiles.pop_back();

        if (!_inputFiles.empty()) {
            addLineNumber();
        }
    }

    std::vector<std::filesystem::path> Env::includedFiles() const {
        return _includedFiles;
    }

    std::string Env::output() {
        return _output.str();
    }

    bool Env::parseFor(std::string_view line) {
        constexpr std::string_view ForString = "#for";

        // Form 1: #for <key>, <value> in <dictionary>
        // Form 2: #for <key> in <a>..<b>

        trimWhitespace(line);

        if (!line.starts_with(ForString)) {
            return false;
        }

        std::string key;
        std::string value;
        std::string dictionary;

        auto form1 = scn::scan<std::string, std::string, std::string>(
            line,
            "#for {:[^,]}, {} in {}"
        );
        if (form1) {
            std::tie(key, value, dictionary) = form1->values();
        }

        auto form2 = scn::scan<std::string, int, int>(line, "#for {} in {}..{}");
        if (form2) {
            auto& [v, minimum, maximum] = form2->values();
            value = std::move(v);

            // Create all the elements in the dictionary
            Dictionary d;
            for (int i = 0; i <= maximum - minimum; i++) {
                d.setValue(std::to_string(i + 1), std::to_string(minimum + i));
            }

            dictionary = std::format("(Range {} to {})", minimum, maximum);
            _dictionary.setValue(dictionary, d);
        }

        if (!form1 && !form2) {
            throw ShaderPreprocessorError(std::format(
                "Error parsing #for  {}. {}", line, fileInfo()
            ));
        }

        // Fetch the dictionary to iterate over
        std::string dict = resolveAlias(dictionary);
        const Dictionary innerDict = _dictionary.value<Dictionary>(dict);

        size_t currentIteration = 0;
        std::vector<std::string_view> keys = innerDict.keys();
        if (!keys.empty()) {
            currentIteration = 0;
            _output << std::format(
                "//# For loop over {0}\n"
                "//# Key {1} in {0}\n",
                dict, keys[0]
            );
        }
        else {
            currentIteration = ForStatement::EmptyLoop;
            _output << "//# Empty for loop\n";
        }
        addLineNumber();

        Env::InputFile& input = _inputFiles.back();
        _forStatements.emplace_back(
            input.file,
            input.lineNumber,
            input.stream.tellg(),
            key,
            value,
            dict,
            currentIteration
        );

        return true;
    }

    bool Env::parseEndFor(std::string_view line) {
        constexpr std::string_view EndForString = "#endfor";

        trimWhitespace(line);

        if (!line.starts_with(EndForString)) {
            return false;
        }

        if (_forStatements.empty()) {
            throw ShaderPreprocessorError(std::format(
                "Unexpected #endfor. No corresponding #for was found. {}", fileInfo()
            ));
        }

        Env::ForStatement& forStmnt = _forStatements.back();

        // When we get here, we have already processed the first iteration of the for
        // loop. So we can pop the scope of that variable and move to the next key index
        forStmnt.currentIteration++;

        // Fetch the dictionary to iterate over
        const Dictionary innerDict = _dictionary.value<Dictionary>(forStmnt.dictionary);
        std::vector<std::string_view> keys = innerDict.keys();
        // This part of the code effectively behaves like the check in a for loop, except
        // that the for loop is extracting all of the lines of the file
        if (forStmnt.currentIteration < keys.size()) {
            std::string_view key = keys[forStmnt.currentIteration];
            _output << std::format("//# Key {} in {}\n", key, forStmnt.dictionary);
            addLineNumber();

            // Restore input to its state from when #for was found
            Env::InputFile& input = _inputFiles.back();
            input.stream.seekg(forStmnt.streamPos);
            input.lineNumber = forStmnt.lineNumber;
        }
        else {
            // This was the last iteration or there were zero iterations
            _output << std::format("//# Terminated loop over {}\n", forStmnt.dictionary);
            addLineNumber();
            _forStatements.pop_back();
        }
        return true;
    }

    bool Env::parseInclude(std::string_view line) {
        constexpr std::string_view IncludeString = "#include";

        trimWhitespace(line);

        if (!line.starts_with(IncludeString)) {
            return false;
        }

        const size_t p1 = line.find_first_not_of(Ws, IncludeString.size());
        if (p1 == std::string_view::npos) {
            throw ShaderPreprocessorError(std::format(
                "Expected file path after #include. {}", fileInfo()
            ));
        }

        if (line[p1] == '\"') {
            const size_t p2 = line.find_first_of('\"', p1 + 1);
            if (p2 == std::string_view::npos) {
                throw ShaderPreprocessorError(std::format("Expected \" {}", fileInfo()));
            }

            const size_t includeLength = p2 - p1 - 1;
            const std::string_view includeFilename = line.substr(p1 + 1, includeLength);
            std::filesystem::path includeFilepath =
                _inputFiles.back().file.parent_path() / includeFilename;

            // Resolve the include paths if this default includeFilename does not exist
            if (std::filesystem::is_regular_file(includeFilepath)) {
                processFile(includeFilepath);
                return true;
            }

            for (const std::filesystem::path& path : ShaderPreprocessor::includePaths()) {
                const std::filesystem::path inc = path / includeFilename;
                if (std::filesystem::is_regular_file(inc)) {
                    processFile(inc);
                    return true;
                }
            }

            // Our last chance is that the include file is an absolute path
            const bool found = std::filesystem::is_regular_file(includeFilename);
            if (found) {
                processFile(includeFilename);
                return true;
            }

            throw ShaderPreprocessorError(std::format(
                "Could not resolve file path for include file '{}'", includeFilename
            ));
        }
        else if (line.at(p1) == '<') {
            const size_t p2 = line.find_first_of('>', p1 + 1);
            if (p2 == std::string_view::npos) {
                throw ShaderPreprocessorError(std::format("Expected >. {}", fileInfo()));
            }

            const size_t includeLen = p2 - p1 - 1;
            const std::filesystem::path inc = absPath(line.substr(p1 + 1, includeLen));
            if (!std::filesystem::is_regular_file(inc)) {
                throw ShaderPreprocessorError(std::format(
                    "Could not resolve file path for include file '{}'", inc
                ));
            }

            processFile(inc);
            return true;
        }
        else {
            throw ShaderPreprocessorError(std::format(
                "Expected \" or <. {}", fileInfo()
            ));
        }
    }

    bool Env::parseVersion(std::string_view line) {
        constexpr std::string_view VersionString = "#version __CONTEXT__";

        trimWhitespace(line);

        if (!line.starts_with(VersionString)) {
            return false;
        }

        int major = 0;
        int minor = 0;
        int mask = 0;
        glGetIntegerv(GL_MAJOR_VERSION, &major);
        glGetIntegerv(GL_MINOR_VERSION, &minor);
        glGetIntegerv(GL_CONTEXT_PROFILE_MASK, &mask);

        const ContextProfileMask cpm = ContextProfileMask(mask);
        const bool isCore = cpm == ContextProfileMask::GL_CONTEXT_CORE_PROFILE_BIT;
        const bool isCompatibility =
            cpm == ContextProfileMask::GL_CONTEXT_COMPATIBILITY_PROFILE_BIT;

        ghoul_assert(
            isCore || isCompatibility,
            "OpenGL context is neither core nor compatibility"
        );

        std::string_view t = isCore ? "core" : (isCompatibility ? "compatibility" : "");
        _output << std::format("#version {}{}0 {}\n", major, minor, t);
        return true;
    }

    bool Env::parseOs(std::string_view line) {
        constexpr std::string_view OsString = "#define __OS__";

        trimWhitespace(line);

        if (!line.starts_with(OsString)) {
            return false;
        }

#ifdef WIN32
        constexpr std::string_view os = "WIN32";
#else  // ^^^^ WIN32 // !WIN32 vvvv
        constexpr std::string_view os = "linux";
#endif // WIN32
        _output << std::format(
            "#ifndef __OS__\n"
            "#define __OS__ {0}\n"
            "#define {0}\n"
            "#endif\n",
            os
        );
        addLineNumber();
        return true;
    }

    std::string Env::substitute(std::string_view in) const {
        std::string resolved = resolveAlias(in);

        if (resolved.starts_with('"') && resolved.ends_with('"')) {
            return resolved.substr(1, resolved.length() - 2);
        }
        else if (hasValueRecursive<bool>(_dictionary, resolved)) {
            return std::to_string(valueRecursive<bool>(_dictionary, resolved));
        }
        else if (hasValueRecursive<std::string>(_dictionary, resolved)) {
            return valueRecursive<std::string>(_dictionary, resolved);
        }
        else if (hasValueRecursive<int>(_dictionary, resolved)) {
            return std::to_string(valueRecursive<int>(_dictionary, resolved));
        }
        else if (hasValueRecursive<double>(_dictionary, resolved)) {
            return std::to_string(valueRecursive<double>(_dictionary, resolved));
        }
        else if (hasValueRecursive<glm::ivec2>(_dictionary, resolved)) {
            glm::ivec2 vec = valueRecursive<glm::ivec2>(_dictionary, resolved);
            return std::format("ivec2({},{})", vec.x, vec.y);
        }
        else if (hasValueRecursive<glm::ivec3>(_dictionary, resolved)) {
            glm::ivec3 vec = valueRecursive<glm::ivec3>(_dictionary, resolved);
            return std::format("ivec3({},{},{})", vec.x, vec.y, vec.z);
        }
        else if (hasValueRecursive<glm::ivec4>(_dictionary, resolved)) {
            glm::ivec4 vec = valueRecursive<glm::ivec4>(_dictionary, resolved);
            return std::format("ivec4({},{},{},{})", vec.x, vec.y, vec.z, vec.w);
        }
        else if (hasValueRecursive<glm::dvec2>(_dictionary, resolved)) {
            glm::dvec2 vec = valueRecursive<glm::dvec2>(_dictionary, resolved);
            return std::format("dvec2({},{})", vec.x, vec.y);
        }
        else if (hasValueRecursive<glm::dvec3>(_dictionary, resolved)) {
            glm::dvec3 vec = valueRecursive<glm::dvec3>(_dictionary, resolved);
            return std::format("dvec3({},{},{})", vec.x, vec.y, vec.z);
        }
        else if (hasValueRecursive<glm::dvec4>(_dictionary, resolved)) {
            glm::dvec4 vec = valueRecursive<glm::dvec4>(_dictionary, resolved);
            return std::format("dvec4({},{},{},{})", vec.x, vec.y, vec.z, vec.w);
        }

        throw ShaderPreprocessorError(std::format(
            "'{}' was resolved to '{}' which is a type that is not supported. {}",
            in, resolved, fileInfo()
        ));
    }

    std::string Env::resolveAlias(std::string_view in) const {
        std::string beforeDot;
        std::string afterDot;
        if (const size_t pos = in.find('.');  pos != std::string_view::npos) {
            beforeDot = in.substr(0, pos);
            afterDot = in.substr(pos);
        }
        else {
            beforeDot = in;
            afterDot = "";
        }

        // Resolve only part before dot
        for (const Env::ForStatement& fs : std::ranges::reverse_view(_forStatements)) {
            const Dictionary innerDict = _dictionary.value<Dictionary>(fs.dictionary);
            std::vector<std::string_view> keys = innerDict.keys();
            std::string_view key = keys[fs.currentIteration];
            if (beforeDot == fs.keyName) {
                beforeDot = std::format("\"{}\"", key);
            }
            if (beforeDot == fs.valueName) {
                beforeDot = std::format("{}.{}", fs.dictionary, key);
            }
        }

        std::string res = beforeDot + afterDot;
        if (!afterDot.empty() && !hasKeyRecursive(_dictionary, res)) {
            throw ShaderPreprocessorError(std::format(
                "Could not resolve variable '{}'. {}", in, fileInfo()
            ));
        }

        return res;
    }

    void Env::addLineNumber() {
        const std::filesystem::path filename = _inputFiles.back().file;
        auto it = std::find(_includedFiles.begin(), _includedFiles.end(), filename);
        ghoul_assert(it != _includedFiles.end(), "File not in included files");

        std::string_view includeSep;

        // Sofar, only Nvidia on Windows supports empty statements in the shader
        using Vendor = systemcapabilities::OpenGLCapabilitiesComponent::Vendor;
        if (OpenGLCap.gpuVendor() == Vendor::Nvidia) {
            includeSep = "; // preprocessor add semicolon to isolate error messages";
        }

        ptrdiff_t number = std::distance(_includedFiles.begin(), it);
        _output << std::format(
            "{}\n#line {} {} // {}\n",
            includeSep, _inputFiles.back().lineNumber, number, filename
        );
    }

    std::string Env::fileInfo() const {
        const Env::InputFile& input = _inputFiles.back();
        return std::format("{}: {}", input.file, input.lineNumber);
    }
} // namespace

namespace ghoul::opengl {

std::vector<std::filesystem::path> ShaderPreprocessor::_includePaths =
    std::vector<std::filesystem::path>();

ShaderPreprocessor::ShaderPreprocessor(std::filesystem::path shaderPath,
                                       Dictionary dictionary)
    : _shaderPath(std::move(shaderPath))
    , _dictionary(std::move(dictionary))
{}

void ShaderPreprocessor::setDictionary(Dictionary dictionary) {
    _dictionary = std::move(dictionary);
    if (_onChangeCallback) {
        _onChangeCallback();
    }
}

const Dictionary& ShaderPreprocessor::dictionary() const {
    return _dictionary;
}

const std::filesystem::path& ShaderPreprocessor::filename() const {
    return _shaderPath;
}

std::string ShaderPreprocessor::process() {
    Env env = Env(_dictionary);
    env.processFile(_shaderPath);
    std::string result = env.output();
    _includedFiles.clear();
    for (const std::filesystem::path& file : env.includedFiles()) {
        filesystem::File f = filesystem::File(file);
        f.setCallback([this]() { _onChangeCallback(); });
        _includedFiles.push_back(std::move(f));
    }
    return result;
}

void ShaderPreprocessor::setCallback(ShaderChangedCallback changeCallback) {
    _onChangeCallback = std::move(changeCallback);
    for (filesystem::File& file : _includedFiles) {
        file.setCallback([this]() { _onChangeCallback(); });
    }
}

std::string ShaderPreprocessor::includedFiles() const {
    std::string result;
    for (size_t i = 0; i < _includedFiles.size(); i++) {
        result = std::format("{}{}: {}\n", result, i, _includedFiles[i].path());
    }
    // Remove the trailing \n
    result.pop_back();
    return result;
}

void ShaderPreprocessor::addIncludePath(const std::filesystem::path& folderPath) {
    ghoul_assert(!folderPath.empty(), "Folder path must not be empty");
    ghoul_assert(
        std::filesystem::is_directory(folderPath),
        "Folder path must be an existing directory"
    );

    const auto it = std::find(_includePaths.begin(), _includePaths.end(), folderPath);
    if (it == _includePaths.end()) {
        _includePaths.push_back(folderPath);
    }
}

const std::vector<std::filesystem::path> ShaderPreprocessor::includePaths() {
    return _includePaths;
}

} // namespace ghoul::opengl
