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

#ifndef __OPENSPACE_ASSETBUILDER___FORM_COLORWIDGET___H__
#define __OPENSPACE_ASSETBUILDER___FORM_COLORWIDGET___H__

#include "form/matrixwidget.h"

class QPushButton;

/**
 * MatrixWidget subclass for Color3/Color4 fields. Adds a 24x24 color swatch button that
 * shows the current color and opens a QColorDialog on click.
 */
class ColorWidget final : public MatrixWidget {
Q_OBJECT
public:
    /**
     * \param nComponents 3 for RGB, 4 for RGBA
     * \param parent Parent widget
     */
    explicit ColorWidget(int nComponents, QWidget* parent = nullptr);

    void setValues(const PropertyList& vals) override;

private:
    /**
     * Updates the swatch button background to reflect the current field values.
     */
    void updateSwatch();

    /// Color preview button that opens a QColorDialog on click
    QPushButton* _swatchButton = nullptr;
};

#endif // __OPENSPACE_ASSETBUILDER___FORM_COLORWIDGET___H__
