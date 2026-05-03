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

#include "scenegraphnodeeditor.h"

#include "form/collapsiblesection.h"
#include "form/schemaformwidget.h"
#include "utils.h"
#include <ghoul/misc/assert.h>
#include <QLabel>
#include <QLineEdit>
#include <QVBoxLayout>
#include <map>
#include <string>

namespace {
    constexpr const char* AdditionalSectionKey = "__additional__";

    // All editor instances copy/paste into this shared clipboard. Each section key is
    // stored independently, so copying Renderable from one item and TimeFrame from
    // another keeps both available
    std::map<std::string, PropertyValue> sectionClipboard;

    // SGN members handled by explicit top-level sections (excluded from "Additional")
    constexpr std::array<std::string_view, 5> TopLevelMembers = {
        "GUI", "Renderable", "Transform", "Tag", "TimeFrame"
    };

    /**
     * Converts a display name to PascalCase suitable for use as an identifier. Splits on
     * spaces, hyphens and underscores; removes non-alphanumeric characters.
     *
     * \param name The display name to convert
     * \return PascalCase string (e.g. "My Planet" -> "MyPlanet")
     */
    QString toPascalCase(const QString& name) {
        QString result;
        bool capitalizeNext = true;
        for (const QChar character : name) {
            if (character == ' ' || character == '-' || character == '_') {
                capitalizeNext = true;
            }
            else if (character.isLetterOrNumber()) {
                result += capitalizeNext ? character.toUpper() : character;
                capitalizeNext = false;
            }
        }
        return result;
    }

    /**
     * Collects members from a SchemaType by name. If \p parentName is empty, searches
     * top-level members. Otherwise, finds the parent member first, then searches its
     * children.
     *
     * \param schemaType The schema type to search within
     * \param parentName Parent member name, or empty for top-level lookup
     * \param names Member names to collect, in desired order
     * \return Vector of matching members in the order of \p names, empty if parent not
     *         found
     */
    std::vector<SchemaMember> collectMembers(const SchemaType& schemaType,
                                             const std::string& parentName,
                                             const std::vector<std::string>& names)
    {
        const std::vector<SchemaMember>* source = &schemaType.members;
        if (!parentName.empty()) {
            source = nullptr;
            for (const SchemaMember& member : schemaType.members) {
                if (member.name == parentName) {
                    source = &member.members;
                    break;
                }
            }
            // No top level member found; return empty
            if (!source) {
                return std::vector<SchemaMember>();
            }
        }

        std::vector<SchemaMember> result;
        for (const std::string& name : names) {
            for (const SchemaMember& member : *source) {
                if (member.name == name) {
                    result.push_back(member);
                    break;
                }
            }
        }
        return result;
    }
} // namespace

SceneGraphNodeEditor::SceneGraphNodeEditor(JAsset& asset,
                                           const IdentifierRegistry* registry,
                                           size_t index, QWidget* parent)
    : QWidget(parent)
    , _asset(asset)
    , _registry(registry)
    , _index(index)
    , _localProperties(std::make_shared<PropertyMap>(asset.contents[index].properties))
{
    const SchemaType* sgnTypePtr = AssetSchema::instance().findType("core_scene_node");
    if (!sgnTypePtr) {
        return;
    }
    const SchemaType& sgnType = *sgnTypePtr;

    QBoxLayout* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(12);

    QLabel* titleLabel = new QLabel("Scene Graph Node", this);
    titleLabel->setObjectName("node-title");
    layout->addWidget(titleLabel);

    SchemaFormWidget* renderable = nullptr;
    {
        std::vector<SchemaMember> members = collectMembers(sgnType, "", { "Renderable" });
        renderable = createForm(members, _localProperties, this, true, true);
    }

    SchemaFormWidget* transform = nullptr;
    {
        std::vector<SchemaMember> members = collectMembers(sgnType, "", { "Transform" });
        transform = createForm(members, _localProperties, this, true, true);
    }

    SchemaFormWidget* gui = nullptr;
    {
        std::vector<SchemaMember> members = collectMembers(sgnType, "", { "GUI" });
        gui = createForm(members, _localProperties, this, false, true);
    }

    SchemaFormWidget* timeFrame = nullptr;
    {
        std::vector<SchemaMember> members = collectMembers(sgnType, "", { "TimeFrame" });
        timeFrame = createForm(members, _localProperties, this, false, true);
    }

    // Tag has schema type "String, or Table" (union) so buildMemberSection renders it as
    // a flat field. Wrap it in a CollapsibleSection manually
    SchemaFormWidget* tagForm = nullptr;
    {
        std::vector<SchemaMember> members = collectMembers(sgnType, "", { "Tag" });
        tagForm = createForm(members, _localProperties, this, false, true);
    }
    CollapsibleSection* tags = nullptr;
    if (tagForm) {
        tags = new CollapsibleSection(this, "Tag", false, false, std::nullopt, "Tag");
        connect(
            tags, &CollapsibleSection::documentationRequested,
            this, &SceneGraphNodeEditor::documentationRequested
        );
        connect(
            tags, &CollapsibleSection::copyRequested,
            this, &SceneGraphNodeEditor::onSectionCopy
        );
        connect(
            tags, &CollapsibleSection::pasteRequested,
            this, &SceneGraphNodeEditor::onSectionPaste
        );
        tags->setContentWidget(tagForm);
    }

    QWidget* additional = buildAdditionalSection(sgnType);

    SchemaFormWidget* guiInnerForm = gui->findChild<SchemaFormWidget*>();
    QWidget* quickAccess = buildQuickAccessFields(sgnType, guiInnerForm, additional);

    // Automatically generate identifier
    SchemaFormWidget* additionalForm = additional->findChild<SchemaFormWidget*>();
    QLineEdit* identifierEdit = qobject_cast<QLineEdit*>(
        additionalForm->widgetForMember("Identifier")
    );
    QLineEdit* nameEdit = qobject_cast<QLineEdit*>(guiInnerForm->widgetForMember("Name"));

    // We only wire it to the Name field because it updates when quick access is updated
    connect(
        nameEdit,
        &QLineEdit::textChanged,
        this,
        [this, identifierEdit](const QString& text) {
            // If someone edited the identifier manually, don't override
            if (identifierEdit->isModified()) {
                return;
            }
            const QString identifierText = toPascalCase(text);
            // Make sure we never empty the identifier
            if (!identifierText.isEmpty()) {
                identifierEdit->setText(identifierText);
                (*_localProperties)["Identifier"] =
                    PropertyValue{ identifierText.toStdString() };
                _asset.contents[_index].properties = *_localProperties;
                emit contentModified();
            }
        }
    );

    // After a rebuild (e.g. paste), signals are blocked during populate so the
    // auto-generation above doesn't fire. Seed the identifier if it is still empty
    const QString currentName = nameEdit->text();
    if (!currentName.isEmpty() && identifierEdit->text().isEmpty()) {
        const QString identifierText = toPascalCase(currentName);
        if (!identifierText.isEmpty()) {
            identifierEdit->setText(identifierText);
            (*_localProperties)["Identifier"] =
                PropertyValue{ identifierText.toStdString() };
            _asset.contents[_index].properties = *_localProperties;
        }
    }

    // Layout
    layout->addWidget(quickAccess);

    layout->addWidget(renderable);
    layout->addWidget(transform);
    layout->addWidget(gui);
    layout->addWidget(tags);
    layout->addWidget(timeFrame);

    layout->addWidget(additional);
    updatePasteButtons();
}

SchemaFormWidget* SceneGraphNodeEditor::createForm(
                                                 const std::vector<SchemaMember>& members,
                                                    std::weak_ptr<PropertyMap> properties,
                                                                          QWidget* parent,
                                                                            bool expanded,
                                                                         bool collapsible)
{
    if (members.empty()) {
        return nullptr;
    }

    SchemaFormWidget* form = new SchemaFormWidget(
        members,
        properties,
        parent,
        expanded,
        collapsible,
        _registry
    );
    connect(
        form,
        &SchemaFormWidget::fieldChanged,
        this,
        [this]() {
            _asset.contents[_index].properties = *_localProperties;
            _asset.contents[_index].isDirty = true;
            emit contentModified();
        }
    );

    connect(
        form, &SchemaFormWidget::documentationRequested,
        this, &SceneGraphNodeEditor::documentationRequested
    );
    connect(
        form, &SchemaFormWidget::sectionCopyRequested,
        this, &SceneGraphNodeEditor::onSectionCopy
    );
    connect(
        form, &SchemaFormWidget::sectionPasteRequested,
        this, &SceneGraphNodeEditor::onSectionPaste
    );
    connect(
        form, &SchemaFormWidget::addDependency,
        this, &SceneGraphNodeEditor::addDependency
    );

    form->populateFromProperties();

    return form;
}

void SceneGraphNodeEditor::onSectionCopy(const QString& key) {
    const std::string keyString = key.toStdString();

    if (keyString == AdditionalSectionKey) {
        // Collect all non-top-level properties into a map
        PropertyMap extras;
        for (const auto& [name, value] : *_localProperties) {
            auto it = std::find(TopLevelMembers.begin(), TopLevelMembers.end(), name);
            if (it == TopLevelMembers.end()) {
                extras[name] = value;
            }
        }
        sectionClipboard[keyString] = PropertyValue{ std::move(extras) };
    }
    else if (_localProperties->count(keyString) > 0) {
        sectionClipboard[keyString] = _localProperties->at(keyString);
    }
    updatePasteButtons();
}

void SceneGraphNodeEditor::onSectionPaste(const QString& key) {
    const std::string keyString = key.toStdString();

    // Nothing was copied; opt out
    if (sectionClipboard.count(keyString) == 0) {
        return;
    }
    if (keyString == AdditionalSectionKey && sectionClipboard.at(keyString).isMap()) {
        // Spread the stored map back into root properties
        for (const auto& [name, value] : sectionClipboard.at(keyString).toMap()) {
            (*_localProperties)[name] = value;
        }
    }
    else {
        (*_localProperties)[keyString] = sectionClipboard.at(keyString);
    }
    _asset.contents[_index].properties = *_localProperties;
    _asset.contents[_index].isDirty = true;
    emit contentModified();
    emit rebuildRequested();
}

void SceneGraphNodeEditor::updatePasteButtons() {
    const QList<CollapsibleSection*> sections = findChildren<CollapsibleSection*>();

    for (CollapsibleSection* section : sections) {
        const QString key = section->sectionKey();
        if (key.isEmpty()) {
            continue;
        }
        section->setPasteAvailable(sectionClipboard.count(key.toStdString()) > 0);
    }
}


QWidget* SceneGraphNodeEditor::buildQuickAccessFields(const SchemaType& sgnType,
                                                      SchemaFormWidget* guiForm,
                                                      QWidget* additionalSection)
{
    SchemaFormWidget* additionalForm = additionalSection->findChild<SchemaFormWidget*>();

    QWidget* quickAccessWidget = new QWidget(this);
    QBoxLayout* quickAccessLayout = new QVBoxLayout(quickAccessWidget);
    quickAccessLayout->setContentsMargins(0, 4, 0, 16);
    quickAccessLayout->setSpacing(4);

    // Add "Name" "Description" and "Path" from GUI to quick access
    std::vector<SchemaMember> quickAccessGuiMembers =
        collectMembers(sgnType, "GUI", { "Name", "Description", "Path" });

    // Ensure the nested GUI map exists so we can reference it below. This is not
    // necessary at the root level but it is for nested members
    if (!_localProperties->contains("GUI") || _localProperties->at("GUI").isNull()) {
        (*_localProperties)["GUI"] = PropertyValue{ PropertyMap{} };
    }
    PropertyMap& guiProperties = (*_localProperties)["GUI"].toMap();
    // Using the aliasing constructor for shared_ptr: points to the GUI sub-map while
    // sharing ownership with the root, so the weak_ptr stays valid. This is necessary
    // because createForm needs the pointer to the root of the submap
    auto guiAlias = std::shared_ptr<PropertyMap>(_localProperties, &guiProperties);

    SchemaFormWidget* quickAccessGuiForm = createForm(
        quickAccessGuiMembers,
        guiAlias,
        quickAccessWidget,
        false,
        false
    );
    quickAccessLayout->addWidget(quickAccessGuiForm);

    // Add "Parent" field to quick access
    std::vector<SchemaMember> parentMembers = collectMembers(sgnType, "", { "Parent" });
    SchemaFormWidget* quickAccessAdditionalForm = createForm(
        parentMembers,
        _localProperties,
        quickAccessWidget,
        false,
        false
    );
    quickAccessLayout->addWidget(quickAccessAdditionalForm);

    // Wire bidirectional sync between quick-access and full sections
    if (quickAccessGuiForm && guiForm) {
        quickAccessGuiForm->syncFieldWith("Name", guiForm);
        quickAccessGuiForm->syncFieldWith("Description", guiForm);
        quickAccessGuiForm->syncFieldWith("Path", guiForm);
    }
    if (quickAccessAdditionalForm && additionalForm) {
        quickAccessAdditionalForm->syncFieldWith("Parent", additionalForm);
    }

    return quickAccessWidget;
}

QWidget* SceneGraphNodeEditor::buildAdditionalSection(const SchemaType& sgnType) {
    CollapsibleSection* section = new CollapsibleSection(
        this,
        "Additional Settings",
        false,
        false,
        std::nullopt,
        AdditionalSectionKey
    );

    connect(
        section, &CollapsibleSection::documentationRequested,
        this, &SceneGraphNodeEditor::documentationRequested
    );
    connect(
        section, &CollapsibleSection::copyRequested,
        this, &SceneGraphNodeEditor::onSectionCopy
    );
    connect(
        section, &CollapsibleSection::pasteRequested,
        this, &SceneGraphNodeEditor::onSectionPaste
    );

    QWidget* content = new QWidget;
    QBoxLayout* contentLayout = new QVBoxLayout(content);
    contentLayout->setContentsMargins(0, 4, 0, 8);
    contentLayout->setSpacing(0);

    // Get all members that are direct children on SceneGraphNode
    std::vector<SchemaMember> additionalMembers;
    for (const SchemaMember& member : sgnType.members) {
        auto it = std::find(TopLevelMembers.begin(), TopLevelMembers.end(), member.name);
        if (it == TopLevelMembers.end()) {
            additionalMembers.push_back(member);
        }
    }

    // Add to Collapsible Section
    SchemaFormWidget* form = createForm(
        additionalMembers,
        _localProperties,
        content,
        false,
        true
    );
    contentLayout->addWidget(form);
    section->setContentWidget(content);
    return section;
}
