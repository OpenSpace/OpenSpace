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

#ifndef __OPENSPACE_ASSET_BUILDER___METADATAWIDGET___H__
#define __OPENSPACE_ASSET_BUILDER___METADATAWIDGET___H__

#include <jasset.h>

#include <QWidget>

class QComboBox;
class QLineEdit;
class QPlainTextEdit;

/**
 * Widget showing the asset metadata form (name, version, author, description,
 * license).
 */
class MetadataWidget final : public QWidget {
Q_OBJECT
public:
    explicit MetadataWidget(QWidget* parent = nullptr);

    /**
     * Sets the asset pointer used for editing.
     *
     * \param asset Non-owning pointer to the JAsset
     */
    void setAsset(JAsset* asset);

    /** Re-reads the asset metadata into the form fields. */
    void refresh();

signals:
    /** Emitted whenever this widget mutates the asset. */
    void assetModified();

private:
    void buildUi();

    JAsset* _asset = nullptr;

    QLineEdit* _nameEdit        = nullptr;
    QLineEdit* _versionEdit     = nullptr;
    QLineEdit* _authorEdit      = nullptr;
    QPlainTextEdit* _descriptionEdit = nullptr;
    QComboBox* _licenseCombo    = nullptr;
};

#endif // __OPENSPACE_ASSET_BUILDER___METADATAWIDGET___H__
