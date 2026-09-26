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

#ifndef __GHOUL___CLIPBOARD___H__
#define __GHOUL___CLIPBOARD___H__

#include <string>
#include <string_view>

namespace ghoul {

/**
 * Determines the selection area to which a clipboard action is applied to. Note that this
 * value only has an effect when running on a Linux system. On Windows, there is only a
 * single selection area exposed to applications.
 */
enum class SelectionArea {
    Clipboard,
    Primary,
    Secondary
};

/**
 * This function retrieves the contents of the system-wide clipboard and returns the
 * textual representation. If the clipboard does not contain anything, or if there was an
 * error accessing the clipboard, an empty string is returned.
 *
 * \param selectionArea The selection area from which to return the contents of. This
 *        parameter is only used on Linux and is ignored on other operating systems
 * \return The contents of the system-wide clipboard
 */
std::string clipboardText(SelectionArea selectionArea = SelectionArea::Clipboard);

/**
 * Writes a passed text into the system-wide clipboard, overwriting its previous content.
 *
 * \param text The text that is to be written into the clipboard
 * \param selectionArea The selection area into which the text should be copied. This
 *        parameter is only used on Linux and is ignored on other operating systems
 *
 * \throw RuntimeError If there was an error writing the text to the clipboard
 */
void setClipboardText(std::string_view text,
    SelectionArea selectionArea = SelectionArea::Clipboard);

} // namespace ghoul

#endif // __GHOUL___CLIPBOARD___H__
