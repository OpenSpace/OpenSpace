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

#include "profile/uipanelsdialog.h"

#include "profile/line.h"
#include <openspace/json.h>
#include <ghoul/filesystem/filesystem.h>
#include <QCheckBox>
#include <QDialogButtonBox>
#include <QLabel>
#include <QVBoxLayout>
#include <fstream>
#include <string_view>

namespace {
    constexpr std::string_view DefaultPanelPath = "${DATA}/web/default_ui_panels.json";

    struct Panel {
        std::string id;
        std::string name;
        bool isVisible;
    };

    void from_json(const nlohmann::json& j, Panel& layout) {
        j["id"].get_to(layout.id);
        j["name"].get_to(layout.name);
        j["visible"].get_to(layout.isVisible);
    }

    std::vector<Panel> loadPanels() {
        std::ifstream panelFile = std::ifstream(absPath(DefaultPanelPath));
        const std::string panelContent = std::string(
            std::istreambuf_iterator<char>(panelFile),
            std::istreambuf_iterator<char>()
        );
        const nlohmann::json panel = nlohmann::json::parse(panelContent);
        std::map<std::string, Panel> panels = panel.get<std::map<std::string, Panel>>();

        std::vector<Panel> result;
        for (const auto& [key, value] : panels) {
            result.push_back(value);
        }

        std::sort(
            result.begin(),
            result.end(),
            [](const Panel& lhs, const Panel& rhs) { return lhs.name < rhs.name; }
        );

        return result;
    }
} // namespace

UiPanelsDialog::UiPanelsDialog(QWidget* parent, std::map<std::string, bool>* uiPanels)
    : QDialog(parent)
    , _uiPanels(uiPanels)
{
    setWindowTitle("User Interface Panels");

    std::vector<Panel> panels = loadPanels();

    QBoxLayout* layout = new QVBoxLayout(this);

    QLabel* info = new QLabel(
        "Select the user interface panels that should be visible by default in the "
        "current profile."
    );
    info->setWordWrap(true);
    layout->addWidget(info);

    for (const Panel& panel : panels) {
        QCheckBox* box = new QCheckBox(QString::fromStdString(panel.name));

        // If the profile already has a desired value for the checkbox, use it. Otherwise
        // use the default values
        auto it = _uiPanels->find(panel.id);
        if (it != _uiPanels->end()) {
            box->setChecked(it->second);
        }
        else {
            box->setChecked(panel.isVisible);
        }

        layout->addWidget(box);
        _checkboxToId[box] = panel.id;
    }

    layout->addWidget(new Line);

    {
        QDialogButtonBox* buttons = new QDialogButtonBox;
        buttons->setStandardButtons(QDialogButtonBox::Save | QDialogButtonBox::Cancel);
        connect(
            buttons, &QDialogButtonBox::accepted,
            this, &UiPanelsDialog::parseSelections
        );
        connect(buttons, &QDialogButtonBox::rejected, this, &UiPanelsDialog::reject);
        layout->addWidget(buttons);
    }
}

void UiPanelsDialog::parseSelections() {
    _uiPanels->clear();
    for (const auto& [key, value] : _checkboxToId) {
        _uiPanels->emplace(value, key->isChecked());
    }
    accept();
}
