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

#include <openspace/openspace.h>
#include <openspace/engine/settings.h>
#include <openspace/util/httprequest.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/stringhelper.h>
#include <QGuiApplication>
#include <QStyleHints>
#include <QTimer>
#include <scn/scan.h>
#include <date/date.h>
#include <string_view>
#include <vector>

using namespace openspace;

namespace {
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
        curr++;

        return { std::move(date), std::move(text) };
    }

    std::vector<Entry> parseEntries(const std::string& data) {
        std::vector<Entry> entries;

        std::vector<std::string> lines = ghoul::tokenizeString(data, '\n');
        if (lines.empty() || lines[0].empty()) {
            // The notification file is empty and we don't want to show anything
            return entries;
        }

        std::vector<std::string>::const_iterator curr = lines.cbegin();
        while (curr != lines.end()) {
            Entry e = parseEntry(curr);
            entries.push_back(std::move(e));
        }

        return entries;
    }

    std::string formatEntry(const Entry& e, date::year_month_day lastStartedDate) {
        auto r = scn::scan<int, int, int>(e.date, "{}-{}-{}");
        ghoul_assert(r, "Invalid date");
        auto& [year, month, day] = r->values();
        const date::year_month_day ymd = date::year_month_day(
            date::year(year),
            date::month(month),
            date::day(day)
        );

        QColor text = QGuiApplication::palette().text().color();
        text = text.darker();

        if (date::sys_days(ymd) < date::sys_days(lastStartedDate)) {
            const QColor textColor = QColor(120, 120, 120);
            return std::format(
                "<tr>"
                    "<td width='15%'>"
                        "<font color='#{2:x}{3:x}{4:x}'>{0}</font>"
                    "</td>"
                    "<td width='85%' align='left'>"
                        "<font color='#{2:x}{3:x}{4:x}'>{1}</font>"
                    "</td>"
                "</tr>",
                e.date, e.text, textColor.red(), textColor.green(), textColor.blue()
            );
        }
        else {
            return std::format(
                "<tr>"
                    "<td width='15%'>"
                        "{0}"
                    "</td>"
                    "<td width='85%' align='left'>"
                        "{1}"
                    "</td>"
                "</tr>",
                e.date, e.text
            );
        }
    }
} // namespace

NotificationWindow::NotificationWindow(QWidget* parent)
    : QTextEdit(parent)
{
    setAcceptRichText(true);
    setReadOnly(true);
    setFocusPolicy(Qt::NoFocus);
    setObjectName("notifications");

    std::string URL = std::format(
        "https://raw.githubusercontent.com/OpenSpace/Notifications/refs/heads/master/"
        "{}.txt",
        OPENSPACE_IS_RELEASE_BUILD ? OPENSPACE_VERSION_NUMBER : "master"
    );

    _request = std::make_unique<HttpMemoryDownload>(std::string(URL));
    _request->start(std::chrono::seconds(1));

    // The download has a timeout of 1s, so after 1250ms we'll definitely have answer.
    constexpr int TimeOut = 1250;
    QTimer::singleShot(TimeOut, [this](){
        while (!_request->hasSucceeded() && !_request->hasFailed()) {
            std::this_thread::sleep_for(std::chrono::milliseconds(250));
        }
        if (_request->hasFailed()) {
            LWARNINGC("Notification", "Failed to retrieve notification file");
            // The download has failed for some reason
            return;
        }

        // 1. Get the downloaded data
        const std::vector<char>& data = _request->downloadedData();
        std::string notificationText = std::string(data.begin(), data.end());

        // 2. Parse the retrieved data into entries
        std::vector<Entry> entries = parseEntries(notificationText);

        // 3. Filter the entries to not show anything that is older than 6 months
        const date::year_month_day now = date::year_month_day(
            floor<date::days>(std::chrono::system_clock::now())
        );
        std::erase_if(
            entries,
            [now](const Entry& e) {
                auto r = scn::scan<int, int, int>(e.date, "{}-{}-{}");
                if (!r) {
                    return false;
                }

                auto& [year, month, day] = r->values();

                const date::year_month_day ymd = date::year_month_day(
                    date::year(year),
                    date::month(month),
                    date::day(day)
                );

                const std::chrono::days diff = date::sys_days(now) - date::sys_days(ymd);
                const bool older = diff.count() > (365 / 2);
                return older;
            }
        );

        // 4. Format the entries into a table format
        Settings settings = loadSettings();
        // Picking a date as the default date that is far enough in the past
        date::year_month_day lastStart = date::year_month_day(
            date::year(2000),
            date::month(1),
            date::day(1)
        );
        if (settings.lastStartedDate.has_value()) {
            auto r = scn::scan<int, int, int>(*settings.lastStartedDate, "{}-{}-{}");
            if (r) {
                auto& [year, month, day] = r->values();

                lastStart = date::year_month_day(
                    date::year(year),
                    date::month(month),
                    date::day(day)
                );
            }
        }

        std::string text = std::accumulate(
            entries.begin(),
            entries.end(),
            std::string(),
            [&lastStart](std::string t, const Entry& e) {
                return std::format(
                    "{}{}",
                    std::move(t), formatEntry(e, lastStart)
                );
            }
        );

        // Add the HTML-like table attributes
        text = std::format("<table border='0'>{}</table>", std::move(text));

        // 5. Set the text
        QString t = QString::fromStdString(text);
        setText(t);
    });
}
