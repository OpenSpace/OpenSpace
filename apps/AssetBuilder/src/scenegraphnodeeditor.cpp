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
    const std::vector<std::string> TopLevelSgnMembers = {
        "GUI", "Renderable", "Transform", "Tag", "TimeFrame"
    };

    bool isTopLevelMember(const std::string& name) {
        for (const std::string& topLevelName : TopLevelSgnMembers) {
            if (name == topLevelName) {
                return true;
            }
        }
        return false;
    }
} // namespace

SceneGraphNodeEditor::SceneGraphNodeEditor(JAsset* asset, IdentifierRegistry* registry,
                                           size_t index, QWidget* parent)
    : QWidget(parent)
    , _asset(asset)
    , _registry(registry)
    , _index(index)
    , _localProperties(asset->contents[index].properties)
{
    buildUi();
}

SchemaFormWidget* SceneGraphNodeEditor::createForm(
                                                 const std::vector<SchemaMember>& members,
                                                                  PropertyMap& properties,
                                                                          QWidget* parent,
                                                                            bool expanded,
                                                                         bool collapsible)
{
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
            _asset->contents[_index].properties = _localProperties;
            _asset->contents[_index].isDirty = true;
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
        for (const auto& [name, value] : _localProperties) {
            if (!isTopLevelMember(name)) {
                extras[name] = value;
            }
        }
        sectionClipboard[keyString] = PropertyValue{ std::move(extras) };
    }
    else if (_localProperties.count(keyString) > 0) {
        sectionClipboard[keyString] = _localProperties.at(keyString);
    }
    updatePasteButtons();
}

void SceneGraphNodeEditor::onSectionPaste(const QString& key) {
    const std::string keyString = key.toStdString();

    // Nothing was copied; opt out
    if (sectionClipboard.count(keyString) == 0) {
        return;
    }
    if (keyString == AdditionalSectionKey &&
        sectionClipboard.at(keyString).isMap())
    {
        // Spread the stored map back into root properties
        for (const auto& [name, value] : sectionClipboard.at(keyString).toMap()) {
            _localProperties[name] = value;
        }
    }
    else {
        _localProperties[keyString] = sectionClipboard.at(keyString);
    }
    _asset->contents[_index].properties = _localProperties;
    _asset->contents[_index].isDirty = true;
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

void SceneGraphNodeEditor::wireIdentifierAutoGeneration(
    SchemaFormWidget* additionalForm,
    SchemaFormWidget* guiForm)
{
    if (!additionalForm || !guiForm) {
        qWarning("wireIdentifierAutoGeneration: form is null");
        return;
    }

    QLineEdit* identifierEdit = qobject_cast<QLineEdit*>(
        additionalForm->widgetForMember("Identifier")
    );
    QLineEdit* nameEdit = qobject_cast<QLineEdit*>(
        guiForm->widgetForMember("Name")
    );
    if (!identifierEdit || !nameEdit) {
        qWarning("wireIdentifierAutoGeneration: widget lookup failed");
        return;
    }

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
                _localProperties["Identifier"] =
                    PropertyValue{ identifierText.toStdString() };
                _asset->contents[_index].properties = _localProperties;
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
            _localProperties["Identifier"] =
                PropertyValue{ identifierText.toStdString() };
            _asset->contents[_index].properties = _localProperties;
        }
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
    SchemaFormWidget* quickAccessGuiForm = nullptr;
    std::vector<SchemaMember> quickAccessGuiMembers =
        collectMembers(sgnType, "GUI", { "Name", "Description", "Path" });

    if (!quickAccessGuiMembers.empty()) {
        // Ensure the nested GUI map exists so we can reference it below.
        // This is not necessary at the root level but it is for nested members
        if (_localProperties.count("GUI") == 0 ||
            _localProperties.at("GUI").isNull())
        {
            _localProperties["GUI"] = PropertyValue{ PropertyMap{} };
        }
        PropertyMap& guiProperties = _localProperties["GUI"].toMap();
        quickAccessGuiForm = createForm(
            quickAccessGuiMembers,
            guiProperties,
            quickAccessWidget,
            false, false
        );
        quickAccessLayout->addWidget(quickAccessGuiForm);
    }

    // Add "Parent" field to quick access
    SchemaFormWidget* quickAccessAdditionalForm = nullptr;
    std::vector<SchemaMember> parentMembers = collectMembers(sgnType, "", { "Parent" });

    if (!parentMembers.empty()) {
        quickAccessAdditionalForm = createForm(
            parentMembers,
            _localProperties,
            quickAccessWidget,
            false,
            false
        );
        quickAccessLayout->addWidget(quickAccessAdditionalForm);
    }

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

SchemaFormWidget* SceneGraphNodeEditor::buildMemberSection(const SchemaType& sgnType,
                                                           const std::string& memberName,
                                                           bool expanded)
{
    std::vector<SchemaMember> members = collectMembers(sgnType, "", { memberName });
    if (members.empty()) {
        return nullptr;
    }
    return createForm(members, _localProperties, this, expanded, true);
}

QWidget* SceneGraphNodeEditor::buildAdditionalSection(const SchemaType& sgnType) {
    CollapsibleSection* section = new CollapsibleSection("Additional Settings", this);
    section->setExpanded(false);
    section->setSectionKey(AdditionalSectionKey);

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

    QWidget* content = new QWidget();
    QBoxLayout* contentLayout = new QVBoxLayout(content);
    contentLayout->setContentsMargins(0, 4, 0, 8);
    contentLayout->setSpacing(0);

    // Get all members that are direct children on SceneGraphNode
    std::vector<SchemaMember> additionalMembers;
    for (const SchemaMember& member : sgnType.members) {
        if (!isTopLevelMember(member.name)) {
            additionalMembers.push_back(member);
        }
    }

    // Add to Collapsible Section
    if (!additionalMembers.empty()) {
        SchemaFormWidget* form = createForm(
            additionalMembers,
            _localProperties,
            content,
            false,
            true
        );
        contentLayout->addWidget(form);
    }
    section->setContentWidget(content);
    return section;
}

void SceneGraphNodeEditor::buildUi() {
    const SchemaType* sgnTypePtr = AssetSchema::instance().findType("core_scene_node");
    if (!sgnTypePtr) {
        return;
    }
    const SchemaType& sgnType = *sgnTypePtr;

    QBoxLayout* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(12);

    // Build
    QLabel* titleLabel = new QLabel("Scene Graph Node", this);
    titleLabel->setObjectName("node-title");

    SchemaFormWidget* renderable = buildMemberSection(sgnType, "Renderable", true);
    SchemaFormWidget* transform = buildMemberSection(sgnType, "Transform", true);
    SchemaFormWidget* gui = buildMemberSection(sgnType, "GUI", false);
    SchemaFormWidget* timeFrame = buildMemberSection(sgnType, "TimeFrame", false);

    // Tag has schema type "String, or Table" (union) so buildMemberSection renders it as
    // a flat field. Wrap it in a CollapsibleSection manually
    SchemaFormWidget* tagForm = buildMemberSection(sgnType, "Tag", false);
    CollapsibleSection* tags = nullptr;
    if (tagForm) {
        tags = new CollapsibleSection("Tag", this);
        tags->setExpanded(false);
        tags->setSectionKey("Tag");
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

    // buildMemberSection wraps Table members in a CollapsibleSection, so the actual form
    // with Name/Description/Path is nested inside. Unwrap it for syncing and identifier
    // auto-generation
    SchemaFormWidget* guiInnerForm = gui ? gui->findChild<SchemaFormWidget*>() : nullptr;

    QWidget* quickAccess = buildQuickAccessFields(sgnType, guiInnerForm, additional);

    // Automatically generate identifier
    SchemaFormWidget* additionalForm = additional->findChild<SchemaFormWidget*>();
    wireIdentifierAutoGeneration(additionalForm, guiInnerForm);

    // Layout
    layout->addWidget(titleLabel);
    layout->addWidget(quickAccess);

    layout->addWidget(renderable);
    layout->addWidget(transform);
    layout->addWidget(gui);
    layout->addWidget(tags);
    layout->addWidget(timeFrame);

    layout->addWidget(additional);
    updatePasteButtons();
}
