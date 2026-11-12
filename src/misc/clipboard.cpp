/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/misc/clipboard.h>

#include <openspace/misc/defer.h>
#include <openspace/misc/exception.h>
#include <openspace/format.h>
#include <cstring>
#include <memory>

#ifdef WIN32
#include <Windows.h>
#endif // WIN32

namespace {
#ifndef WIN32
    // This is called with a specific input, but has the potential to be very dangerous
    bool exec(const std::string& cmd, std::string& value) {
        FILE* pipe = popen(cmd.c_str(), "r");
        if (!pipe) {
            return false;
        }

        constexpr int BufferSize = 1024;
        std::array<char, BufferSize> buffer = {};
        value.clear();
        while (!feof(pipe)) {
            if (fgets(buffer.data(), BufferSize, pipe) != nullptr) {
                value += buffer.data();
            }
        }
        // Opening the pipe succeeds even if the command itself fails, so the exit status
        // of the command is what determines whether the returned value is usable
        return pclose(pipe) == 0;
    }
#endif // WIN32
} // namespace

namespace openspace {

std::string clipboardText([[maybe_unused]] SelectionArea selectionArea) {
#ifdef WIN32
    // Try opening the clipboard
    if (!OpenClipboard(nullptr)) {
        return "";
    }
    defer { CloseClipboard(); };

    // Get handle of clipboard object for the UTF-16 encoded text
    HANDLE hData = GetClipboardData(CF_UNICODETEXT);
    if (!hData) {
        return "";
    }

    // Lock the handle to get the actual text pointer
    const wchar_t* wideText = static_cast<const wchar_t*>(GlobalLock(hData));
    if (!wideText) {
        return "";
    }
    defer { GlobalUnlock(hData); };

    // The returned size includes the null terminator
    const int size = WideCharToMultiByte(
        CP_UTF8,
        0,
        wideText,
        -1,
        nullptr,
        0,
        nullptr,
        nullptr
    );
    if (size <= 1) {
        return "";
    }

    std::string text(size - 1, '\0');
    WideCharToMultiByte(CP_UTF8, 0, wideText, -1, text.data(), size, nullptr, nullptr);

    text.erase(std::remove(text.begin(), text.end(), '\r'), text.end());
    return text;
#else // ^^^^ WIN32 // !WIN32 vvvv
    std::string_view s = [](SelectionArea selection) {
        switch (selection) {
            case SelectionArea::Clipboard: return "clipboard";
            case SelectionArea::Primary:   return "primary";
            case SelectionArea::Secondary: return "secondary";
            default:                       throw MissingCaseException();
        }
    }(selectionArea);

    std::string text;
    // Try UTF8_STRING first
    std::string cmd = std::format("xclip -o -selection {} -target UTF8_STRING", s);
    if (exec(cmd.c_str(), text)) {
        if (!text.empty() && text.back() == '\n') {
            text.pop_back();
        }
        return text;
    }

    // Fallback: try text/plain;charset=utf-8
    cmd = std::format("xclip -o -selection {} -target text/plain;charset=utf-8", s);
    if (exec(cmd.c_str(), text)) {
        if (!text.empty() && text.back() == '\n') {
            text.pop_back();
        }
        return text;
    }

    // Final fallback: default text/plain
    cmd = std::format("xclip -o -selection {} -target text/plain", s);
    if (exec(cmd.c_str(), text)) {
        if (!text.empty() && text.back() == '\n') {
            text.pop_back();
        }
        return text;
    }

    // If all else fails
    return "";
#endif // WIN32
}

void setClipboardText(std::string_view text, [[maybe_unused]] SelectionArea selectionArea)
{
#ifdef WIN32
    // The clipboard stores the text in UTF-16
    const int size = MultiByteToWideChar(
        CP_UTF8,
        0,
        text.data(),
        static_cast<int>(text.size()),
        nullptr,
        0
    );

    HANDLE hData = GlobalAlloc(GMEM_MOVEABLE, (size + 1) * sizeof(wchar_t));
    if (!hData) {
        throw RuntimeError("Error allocating memory", "Clipboard");
    }

    wchar_t* ptrData = static_cast<wchar_t*>(GlobalLock(hData));
    if (!ptrData) {
        GlobalFree(hData);
        throw RuntimeError("Error acquiring lock", "Clipboard");
    }
    MultiByteToWideChar(
        CP_UTF8,
        0,
        text.data(),
        static_cast<int>(text.size()),
        ptrData,
        size
    );
    ptrData[size] = L'\0';

    GlobalUnlock(hData);

    if (!OpenClipboard(nullptr)) {
        GlobalFree(hData);
        throw RuntimeError("Error opening clipboard", "Clipboard");
    }
    defer { CloseClipboard(); };

    if (!EmptyClipboard()) {
        GlobalFree(hData);
        throw RuntimeError("Error cleaning clipboard", "Clipboard");
    }

    // The clipboard takes over the ownership of the memory if setting the data succeeds
    if (!SetClipboardData(CF_UNICODETEXT, hData)) {
        GlobalFree(hData);
        throw RuntimeError("Error setting text to clipboard", "Clipboard");
    }
#else // ^^^^ WIN32 // !WIN32 vvvv
    std::string_view s = [](SelectionArea selection) {
        switch (selection) {
            case SelectionArea::Clipboard: return "clipboard";
            case SelectionArea::Primary:   return "primary";
            case SelectionArea::Secondary: return "secondary";
            default:                       throw MissingCaseException();
        }
    }(selectionArea);

    // The text is written to the standard input of xclip instead of being made part of
    // the command, as it would be interpreted by the shell otherwise
    const std::string cmd = std::format("xclip -i -selection {}", s);
    FILE* pipe = popen(cmd.c_str(), "w");
    if (!pipe) {
        throw RuntimeError("Error setting text to clipboard", "Clipboard");
    }

    const size_t nWritten = fwrite(text.data(), sizeof(char), text.size(), pipe);
    const int status = pclose(pipe);
    if (nWritten != text.size() || status != 0) {
        throw RuntimeError("Error setting text to clipboard", "Clipboard");
    }
#endif // WIN32
}

} // namespace openspace
