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

#include <ghoul/misc/clipboard.h>

#include <ghoul/misc/exception.h>
#include <ghoul/format.h>
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
        pclose(pipe);
        return true;
    }
#endif // WIN32
} // namespace

namespace ghoul {

std::string clipboardText([[maybe_unused]] SelectionArea selectionArea) {
#ifdef WIN32
    // Try opening the clipboard
    if (!OpenClipboard(nullptr)) {
        return "";
    }

    // Get handle of clipboard object for ANSI text
    HANDLE hData = GetClipboardData(CF_TEXT);
    if (!hData) {
        return "";
    }

    // Lock the handle to get the actual text pointer
    char* pszText = static_cast<char*>(GlobalLock(hData));
    if (!pszText) {
        return "";
    }

    // Save text in a string class instance
    std::string text(pszText);

    // Release the lock
    GlobalUnlock(hData);

    // Release the clipboard
    CloseClipboard();

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
    HANDLE hData = GlobalAlloc(GMEM_MOVEABLE | GMEM_DDESHARE, text.length() + 1);
    if (!hData) {
        throw RuntimeError("Error allocating memory", "Clipboard");
    }

    char* ptrData = static_cast<char*>(GlobalLock(hData));
    if (!ptrData) {
        GlobalFree(hData);
        throw RuntimeError("Error acquiring lock", "Clipboard");
    }
    std::memset(ptrData, 0, text.length() + 1);
    std::memcpy(ptrData, text.data(), text.length());

    GlobalUnlock(hData);

    if (!OpenClipboard(nullptr)) {
        throw RuntimeError("Error opening clipboard", "Clipboard");
    }

    if (!EmptyClipboard()) {
        throw RuntimeError("Error cleaning clipboard", "Clipboard");
    }

    SetClipboardData(CF_TEXT, hData);
    CloseClipboard();
#else // ^^^^ WIN32 // !WIN32 vvvv
    std::string_view s = [](SelectionArea selection) {
        switch (selection) {
            case SelectionArea::Clipboard: return "clipboard";
            case SelectionArea::Primary:   return "primary";
            case SelectionArea::Secondary: return "secondary";
            default:                       throw MissingCaseException();
        }
    }(selectionArea);

    std::string cmd = std::format("echo \"{}\" | xclip -i -selection {} -f", text, s);
    std::string buf;
    const bool success = exec(cmd.c_str(), buf);
    if (!success) {
        throw RuntimeError("Error setting text to clipboard", "Clipboard");
    }
#endif // WIN32
}

} // namespace ghoul
