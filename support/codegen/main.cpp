/*****************************************************************************************
 *                                                                                       *
 * OpenSpace Codegen                                                                     *
 *                                                                                       *
 * Copyright (c) 2021-2026                                                               *
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

#include "codegen.h"
#include "settings.h"
#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <iostream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

// TODO
//   - Check for extra keys in the bake function to protect against misspellings
//   - Name used for ReferencingVerifier has to be generated in a better way (including
//     some more information to disambiguate)
//   - Add support for glm::uvecX

namespace {
    long long totalTime = 0;
    int ChangedFiles = 0;
    int AllFiles = 0;
    bool isVerbose = false;
} // namespace

template <>
struct std::formatter<std::filesystem::path> {
    constexpr auto parse(std::format_parse_context& ctx) {
        return ctx.begin();
    }

    auto format(const std::filesystem::path& path, std::format_context& ctx) const {
        return std::format_to(ctx.out(), "{}", path.string());
    }
};

namespace fs = std::filesystem;

int main(int argc, char** argv) {
    if (argc < 2) {
        std::cerr <<
            "Wrong number of parameters. Expected at least 2.\nUsage: codegen <folder>\n";
        exit(EXIT_FAILURE);
    }

    std::cout << "codegen";
    std::vector<std::string_view> srcs;
    for (int i = 1; i < argc; i++) {
        std::string_view src = argv[i];
        if (src == "--verbose") {
            isVerbose = true;
            continue;
        }

        std::cout << " " << src;

        if (!fs::is_directory(src) && !fs::is_regular_file(src)) {
            std::cerr << std::format(
                "All non-dashed parameters have to be files or folders. '{}' is not\n",
                src
            );
            exit(EXIT_FAILURE);
        }

        srcs.push_back(src);
    }
    std::cout << '\n';

    auto beg = std::chrono::high_resolution_clock::now();

    std::vector<fs::path> entries;
    const std::string extFolder = std::format(
        "{0}ext{0}", static_cast<char>(std::filesystem::path::preferred_separator)
    );
    for (const std::string_view src : srcs) {
        if (fs::is_regular_file(src)) {
            entries.emplace_back(src);
            continue;
        }

        // It's a folder then
        for (const fs::directory_entry& p : fs::recursive_directory_iterator(src)) {
            const std::filesystem::path& path = p.path();

            if ((path.extension() == ".cpp" || path.extension() == ".inl") &&
                !path.string().contains("_codegen.cpp") &&
                !path.string().contains(extFolder))
            {
                entries.push_back(p);
            }
            else {
                if (isVerbose) {
                    std::cout << std::format(
                        "Rejecting {}. Extension: {}; Codegen-ness: {}; Ext-ness: {}\n",
                        path,
                        path.extension(),
                        !path.string().contains("_codegen.cpp"),
                        !path.string().contains(extFolder)
                    );
                }
            }
        }
    }

    std::for_each(
        entries.cbegin(), entries.cend(),
        [](const fs::path& p) {
            try {
                if (isVerbose) {
                    std::cout << "Processing: " << p << '\n';
                }

                auto begin = std::chrono::high_resolution_clock::now();
                const Result res = handleFile(p);
                auto end = std::chrono::high_resolution_clock::now();
                if (res == Result::Processed) {
                    ChangedFiles++;
                    totalTime += (end - begin).count();
                }
                if (res != Result::Skipped) {
                    AllFiles++;
                }
            }
            catch (const std::runtime_error& e) {
                std::cerr << std::format("\n\n{}: error: {}\n\n\n", p.string(), e.what());
                exit(EXIT_FAILURE);
            }
        }
    );

    auto end = std::chrono::high_resolution_clock::now();
    const double ms = static_cast<double>((end - beg).count()) / 1000000.0;

    if (PrintTiming) {
        std::cout << std::format(
            "{}/{} files changed in {} ms.  Pure time in codegen: {} ms\n",
            ChangedFiles, AllFiles, ms, static_cast<double>(totalTime) / 1000000.0
        );
    }
    else {
        std::cout << std::format("{}/{} files changed\n", ChangedFiles, AllFiles);
    }
}
