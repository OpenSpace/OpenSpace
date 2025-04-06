/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include "notificationwindow.h"

#include <openspace/util/httprequest.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/stringhelper.h>
#include <string_view>
#include <vector>

using namespace openspace;

namespace {
    constexpr std::string_view URL = "https://raw.githubusercontent.com/OpenSpace/Notifications/refs/heads/master/test.txt";

    struct Entry {
        std::string date;
        std::string text;
    };

    // Parses a single notification entry out of the list of lines
    Entry parseEntry(std::vector<std::string>::const_iterator& curr) {
        ghoul_assert(!curr->empty(), "First line must not be empty");

        std::string date = *curr;
        std::string text;
        do {
            curr++;

            text += *curr;
        } while (!curr->empty());

        return { std::move(date), std::move(text) };
    }

    std::vector<Entry> parseEntries(const std::string& data) {
        std::vector<Entry> entries;

        std::vector<std::string> lines = ghoul::tokenizeString(data, '\n');
        std::vector<std::string>::const_iterator curr = lines.cbegin();
        while (curr != lines.end()) {
            Entry e = parseEntry(curr);
            entries.push_back(std::move(e));
        }

        return entries;
    }
} // namespace

NotificationWindow::NotificationWindow(QWidget* parent)
    : QTextEdit(parent)
{
    setAcceptRichText(true);
    setReadOnly(true);

    HttpMemoryDownload req = HttpMemoryDownload(std::string(URL));
    req.start(std::chrono::seconds(1));

    std::thread t = std::thread([this, &req]() {
        if (!req.hasSucceeded()) {
            // The download has failed for some reason
            return;
        }
        const std::vector<char> data = req.downloadedData();
        std::string notificationText = std::string(data.begin(), data.end());

        std::vector<Entry> entries = parseEntries(notificationText);

        std::string text = std::accumulate(
            entries.begin(),
            entries.end(),
            std::string(),
            [](std::string t, const Entry& e) {
                return std::format("{}\n# {}\n{}", std::move(t), e.date, e.text);
            }
        );
        setText(QString::fromStdString(text));
    });
    t.detach();
}
