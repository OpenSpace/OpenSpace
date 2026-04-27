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

#include "form/schemaformwidget.h"

#include "form/colorwidget.h"
#include "form/matrixwidget.h"
#include "identifierregistry.h"
#include "utils.h"
#include <QCheckBox>
#include <QComboBox>
#include <QFileDialog>
#include <QHBoxLayout>
#include <QLineEdit>
#include <QPushButton>
#include <QRegularExpression>
#include <QVBoxLayout>
#include <limits>
#include <string>

namespace {
    constexpr MatrixTypeEntry MatrixTypeLookup[] = {
        { "Vector2<double>",    2,  2, false, false },
        { "Vector2<int>",       2,  2, true,  false },
        { "Vector3<double>",    3,  3, false, false },
        { "Vector3<int>",       3,  3, true,  false },
        { "Vector4<double>",    4,  4, false, false },
        { "Vector4<int>",       4,  4, true,  false },
        { "Color3",             3,  3, false, true  },
        { "Color4",             4,  4, false, true  },
        { "Matrix3x3<double>",  9,  3, false, false },
        { "Matrix4x4<double>", 16,  4, false, false }
    };

    struct NumericRange {
        double min = -std::numeric_limits<double>::max();
        double max = std::numeric_limits<double>::max();
        bool hasMin = false;
        bool hasMax = false;
    };

    // Extracts numeric bounds from a schema description string. Supports "In range",
    // "Greater or equal to", "Greater than", "Less or equal to", and "Less than" patterns
    NumericRange parseRange(const std::string& description) {
        NumericRange range;
        const QString descriptionText = QString::fromStdString(description);

        // "In range: ( min, max )"
        static const QRegularExpression reRange(
            R"(In range:\s*\(\s*([^,]+),\s*([^)]+)\))"
        );
        QRegularExpressionMatch match = reRange.match(descriptionText);
        if (match.hasMatch()) {
            bool okMin = false;
            bool okMax = false;
            const double lower = match.captured(1).trimmed().toDouble(&okMin);
            const double upper = match.captured(2).trimmed().toDouble(&okMax);
            if (okMin) {
                range.min = lower;
                range.hasMin = true;
            }
            if (okMax) {
                range.max = upper;
                range.hasMax = true;
            }
            return range;
        }

        // "Greater or equal to: N"
        static const QRegularExpression reGe(R"(Greater or equal to:\s*([-\d.eE+]+))");
        match = reGe.match(descriptionText);
        if (match.hasMatch()) {
            bool ok = false;
            const double value = match.captured(1).toDouble(&ok);
            if (ok) {
                range.min = value;
                range.hasMin = true;
            }
        }

        // "Greater than: N"
        static const QRegularExpression reGt(R"(Greater than:\s*([-\d.eE+]+))");
        match = reGt.match(descriptionText);
        if (match.hasMatch()) {
            bool ok = false;
            const double value = match.captured(1).toDouble(&ok);
            if (ok) {
                range.min = value;
                range.hasMin = true;
            }
        }

        // "Less or equal to: N"
        static const QRegularExpression reLe(R"(Less or equal to:\s*([-\d.eE+]+))");
        match = reLe.match(descriptionText);
        if (match.hasMatch()) {
            bool ok = false;
            const double value = match.captured(1).toDouble(&ok);
            if (ok) {
                range.max = value;
                range.hasMax = true;
            }
        }

        // "Less than: N"
        static const QRegularExpression reLt(R"(Less than:\s*([-\d.eE+]+))");
        match = reLt.match(descriptionText);
        if (match.hasMatch()) {
            bool ok = false;
            const double value = match.captured(1).toDouble(&ok);
            if (ok) {
                range.max = value;
                range.hasMax = true;
            }
        }

        return range;
    }

    // Builds a placeholder string describing the valid range for a numeric field. Returns
    // e.g. "1 to 10", ">= 0", "<= 100", or a default "0"/"0.0"
    QString rangePlaceholder(const NumericRange& range, bool isInteger) {
        auto format = [isInteger](double value) {
            if (isInteger) {
                return QString::number(static_cast<int>(value));
            }
            QString formatted = QString::number(value, 'g', 10);
            if (!formatted.contains('.')) {
                formatted += ".0";
            }
            return formatted;
        };

        if (range.hasMin && range.hasMax) {
            return format(range.min) + " to " + format(range.max);
        }
        if (range.hasMin) {
            return ">= " + format(range.min);
        }
        if (range.hasMax) {
            return "<= " + format(range.max);
        }
        return isInteger ? "0" : "0.0";
    }

    // Extracts the allowed values from an "In list { ... }" description pattern. Returns
    // the values as a trimmed string list, or empty if no match
    QStringList parseInList(const std::string& description) {
        const QString descriptionText = QString::fromStdString(description);
        static const QRegularExpression re(R"(In list \{\s*(.+?)\s*\})");
        QRegularExpressionMatch match = re.match(descriptionText);
        if (!match.hasMatch()) {
            return QStringList();
        }
        QStringList items = match.captured(1).split(",");
        for (QString& item : items) {
            item = item.trimmed();
        }
        items.removeAll("");
        return items;
    }

    // Splits a union type string like "Integer, or Double" into individual types. Handles
    // both ", or " and " or " separators
    QStringList splitUnionTypes(const QString& type) {
        QString cleaned = type;
        cleaned.replace(", or ", ", ");
        cleaned.replace(" or ", ", ");
        QStringList parts = cleaned.split(", ", Qt::SkipEmptyParts);
        for (QString& part : parts) {
            part = part.trimmed();
        }
        return parts;
    }
} // namespace

QWidget* SchemaFormWidget::createFlatWidget(const SchemaMember& member) {
    const QString typeString = QString::fromStdString(member.type);

    // Union types (contain ", " or " or ")
    if (typeString.contains(", ") || typeString.contains(" or ")) {
        return createUnionWidget(member);
    }

    if (member.type == "Boolean") {
        return createBooleanWidget(member.name);
    }
    if (member.type == "Integer") {
        return createIntegerWidget(member.name, member.description);
    }
    if (member.type == "Double") {
        return createDoubleWidget(member.name, member.description);
    }
    for (const MatrixTypeEntry& entry : MatrixTypeLookup) {
        if (member.type == entry.typeName) {
            return createMatrixWidget(member.name, entry);
        }
    }
    if (member.type == "File") {
        return createFileWidget(member.name, member.description, false);
    }
    if (member.type == "Directory") {
        return createFileWidget(member.name, member.description, true);
    }
    if (member.type == "Date and time") {
        return createDateTimeWidget(member.name);
    }
    // Identifier fields that reference other nodes (not the node's own Identifier
    // definition) get a registry combo + browse button
    if (member.type == "Identifier" && member.name != "Identifier" && _registry) {
        return createIdentifierComboWidget(member);
    }
    const QStringList options = parseInList(member.description);
    if (!options.isEmpty()) {
        return createInListWidget(member.name, options);
    }

    // Fallback to string
    return createStringWidget(member);
}

QWidget* SchemaFormWidget::createBooleanWidget(const std::string& name) {
    QCheckBox* checkBox = new QCheckBox(this);

    connect(
        checkBox,
        &QCheckBox::toggled,
        this,
        [this, name](bool checked) {
            _properties[name] = PropertyValue{ checked };
            emit fieldChanged();
        }
    );
    return checkBox;
}

QWidget* SchemaFormWidget::createIntegerWidget(const std::string& name,
                                               const std::string& description)
{
    QLineEdit* lineEdit = new QLineEdit(this);
    const NumericRange range = parseRange(description);
    // Cannot cast the NumericRange double defaults to int (overflow / UB), so fall back
    // to int limits when no bound was parsed
    const int lowerBound =
        range.hasMin ? static_cast<int>(range.min) : std::numeric_limits<int>::min();
    const int upperBound =
        range.hasMax ? static_cast<int>(range.max) : std::numeric_limits<int>::max();
    lineEdit->setValidator(new QIntValidator(lowerBound, upperBound, lineEdit));
    lineEdit->setPlaceholderText(rangePlaceholder(range, true));

    connect(
        lineEdit,
        &QLineEdit::textEdited,
        this,
        [this, name](const QString& text) {
            bool ok = false;
            const int value = text.toInt(&ok);
            _properties[name] = PropertyValue{
                static_cast<double>(ok ? value : 0)
            };
            emit fieldChanged();
        }
    );
    return lineEdit;
}

QWidget* SchemaFormWidget::createDoubleWidget(const std::string& name,
                                              const std::string& description)
{
    QLineEdit* lineEdit = new QLineEdit(this);
    const NumericRange range = parseRange(description);
    // QDoubleValidator misbehaves with extreme double values, so use +/- 1e15 as a
    // practical fallback when no bound was parsed
    const double lowerBound = range.hasMin ? range.min : -1e15;
    const double upperBound = range.hasMax ? range.max : 1e15;
    lineEdit->setValidator(new QDoubleValidator(lowerBound, upperBound, -1, lineEdit));
    lineEdit->setPlaceholderText(rangePlaceholder(range, false));
    connect(
        lineEdit,
        &QLineEdit::textEdited,
        this,
        [this, name](const QString& text) {
            bool ok = false;
            const double value = text.toDouble(&ok);
            _properties[name] = PropertyValue{ ok ? value : 0.0 };
            emit fieldChanged();
        }
    );
    return lineEdit;
}

QWidget* SchemaFormWidget::createMatrixWidget(const std::string& name,
                                              const MatrixTypeEntry& entry)
{
    MatrixWidget* matrixWidget = entry.isColor ?
        static_cast<MatrixWidget*>(new ColorWidget(entry.nComponents, this)) :
        new MatrixWidget(entry.nComponents, entry.nColumns, entry.isInteger, this);
    connect(
        matrixWidget,
        &MatrixWidget::valueChanged,
        this,
        [this, name, matrixWidget]() {
            _properties[name] = PropertyValue{ matrixWidget->values() };
            emit fieldChanged();
        }
    );
    return matrixWidget;
}

QWidget* SchemaFormWidget::createFileWidget(const std::string& name,
                                            const std::string& description,
                                            bool isDirectory)
{
    QWidget* container = new QWidget(this);
    container->setObjectName(FileContainerName);
    QBoxLayout* horizontalLayout = new QHBoxLayout(container);
    horizontalLayout->setContentsMargins(0, 0, 0, 0);
    horizontalLayout->setSpacing(4);

    QLineEdit* lineEdit = new QLineEdit(container);
    if (!description.empty()) {
        lineEdit->setPlaceholderText(QString::fromStdString(description));
    }
    else {
        lineEdit->setPlaceholderText(
            isDirectory ? "Path to directory..." : "Path to file..."
        );
    }
    connect(
        lineEdit,
        &QLineEdit::textEdited,
        this,
        [this, name](const QString& text) {
            _properties[name] = PropertyValue{ text.toStdString() };
            emit fieldChanged();
        }
    );

    QPushButton* browseButton = new QPushButton("Browse...", container);
    browseButton->setObjectName("file-browse-button");
    connect(
        browseButton,
        &QPushButton::clicked,
        this,
        [this, lineEdit, name, isDirectory]() {
            const QString path = isDirectory ?
                QFileDialog::getExistingDirectory(this, "Select Directory") :
                QFileDialog::getOpenFileName(this, "Select File");
            if (!path.isEmpty()) {
                lineEdit->setText(path);
                _properties[name] = PropertyValue{ path.toStdString() };
                emit fieldChanged();
            }
        }
    );

    horizontalLayout->addWidget(lineEdit, 1);
    horizontalLayout->addWidget(browseButton);
    return container;
}

QWidget* SchemaFormWidget::createDateTimeWidget(const std::string& name) {
    QLineEdit* lineEdit = new QLineEdit(this);
    lineEdit->setObjectName(DateEditName);
    // Human-readable hint; the actual Qt format string is DateDisplayFormat
    lineEdit->setPlaceholderText("YYYY-MM-DD hh:mm:ss");
    lineEdit->setValidator(new QRegularExpressionValidator(
        QRegularExpression(R"(^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}$)"),
        lineEdit
    ));
    connect(
        lineEdit,
        &QLineEdit::textEdited,
        this,
        [this, name](const QString& text) {
            const QDateTime dateTime = QDateTime::fromString(text, DateDisplayFormat);
            if (dateTime.isValid()) {
                _properties[name] = PropertyValue{
                    dateTime.toUTC().toString(Qt::ISODate).toStdString()
                };
            }
            emit fieldChanged();
        }
    );
    return lineEdit;
}

QWidget* SchemaFormWidget::createIdentifierComboWidget(const SchemaMember& member) {
    QWidget* container = new QWidget(this);
    QBoxLayout* horizontalLayout = new QHBoxLayout(container);
    horizontalLayout->setContentsMargins(0, 0, 0, 0);
    horizontalLayout->setSpacing(4);

    // Returns identifiers excluding this node's own
    auto filteredIdentifiers = [this]() {
        QStringList identifiers = _registry->knownIdentifiers();
        const auto it = _properties.find("Identifier");
        if (it != _properties.end() && it->second.isString()) {
            identifiers.removeAll(QString::fromStdString(it->second.toString()));
        }
        return identifiers;
    };

    QComboBox* combo = new QComboBox(container);
    combo->setEditable(true);
    combo->addItems(filteredIdentifiers());
    if (!member.description.empty()) {
        combo->lineEdit()->setPlaceholderText(QString::fromStdString(member.description));
    }

    connect(
        _registry,
        &IdentifierRegistry::registryChanged,
        combo,
        [combo, filteredIdentifiers]() {
        const QString current = combo->currentText();
            combo->blockSignals(true);
            combo->clear();
            combo->addItems(filteredIdentifiers());
            combo->setCurrentText(current);
            combo->blockSignals(false);
        }
    );
    connect(
        combo,
        &QComboBox::currentTextChanged,
        this,
        [this, name = member.name](const QString& text) {
            _properties[name] = PropertyValue{ text.toStdString() };
            emit fieldChanged();
        }
    );

    QPushButton* browseButton = new QPushButton("Browse .jasset", container);
    browseButton->setObjectName("identifier-browse-button");
    connect(
        browseButton, &QPushButton::clicked,
        this, &SchemaFormWidget::browseJassetRequested
    );

    horizontalLayout->addWidget(combo, 1);
    horizontalLayout->addWidget(browseButton);
    return container;
}

QWidget* SchemaFormWidget::createInListWidget(const std::string& name,
                                              const QStringList& listOptions)
{
    QComboBox* combo = new QComboBox(this);
    combo->addItem("Select...");
    combo->addItems(listOptions);
    combo->setCurrentIndex(0);
    connect(
        combo,
        &QComboBox::currentIndexChanged,
        this,
        [this, name, combo](int index) {
            // Index 0 is the "Select..." placeholder - erase the property so the
            // placeholder choice is not serialized into the asset
            if (index <= 0) {
                _properties.erase(name);
            }
            else {
                _properties[name] = PropertyValue{ combo->currentText().toStdString() };
            }
            emit fieldChanged();
        }
    );
    return combo;
}

QWidget* SchemaFormWidget::createUnionWidget(const SchemaMember& member) {
    const QString typeString = QString::fromStdString(member.type);
    QStringList types = splitUnionTypes(typeString);

    // Combo selector + show/hide pages. Hidden widgets take zero layout
    // space, so the container fits the active page.
    QWidget* container = new QWidget(this);
    container->setObjectName(UnionContainerName);
    QBoxLayout* verticalLayout = new QVBoxLayout(container);
    verticalLayout->setContentsMargins(0, 0, 0, 0);
    verticalLayout->setSpacing(4);

    QComboBox* typeCombo = new QComboBox(container);
    typeCombo->setObjectName(UnionTypeComboName);
    verticalLayout->addWidget(typeCombo);

    QList<QWidget*> typeWidgets;
    for (int j = 0; j < types.size(); j++) {
        typeCombo->addItem(types[j]);
        // Empty description — the union description applies to the union
        // as a whole, not the individual type
        SchemaMember typeMember = { member.name, types[j].toStdString(), {} };
        QWidget* widget = (types[j] == "Table") ?
            createTableSection(typeMember) :
            createFlatWidget(typeMember);

        // Only the first type is visible; the combo switches between them
        widget->setVisible(j == 0);
        verticalLayout->addWidget(widget);
        typeWidgets.append(widget);
    }

    connect(
        typeCombo,
        &QComboBox::currentIndexChanged,
        this, [typeWidgets](int index) {
            for (int i = 0; i < typeWidgets.size(); ++i) {
                typeWidgets[i]->setVisible(i == index);
            }
        }
    );

    return container;
}

QWidget* SchemaFormWidget::createStringWidget(const SchemaMember& member) {
    QLineEdit* lineEdit = new QLineEdit(this);

    if (!member.description.empty()) {
        lineEdit->setPlaceholderText(QString::fromStdString(member.description));
    }

    connect(
        lineEdit,
        &QLineEdit::textEdited,
        this,
        [this, name = member.name](const QString& text) {
            _properties[name] = PropertyValue{ text.toStdString() };
            emit fieldChanged();
        }
    );

    return lineEdit;
}
