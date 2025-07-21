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

#ifndef __OPENSPACE_MODULE_SPACE___GPTRANSLATION___H__
#define __OPENSPACE_MODULE_SPACE___GPTRANSLATION___H__

#include <modules/space/translation/keplertranslation.h>

namespace openspace {

/**
 * A specialization of the KeplerTranslation that utilizes general pertubation file
 * formats to extracts the Keplerian elements.
 */
class GPTranslation : public KeplerTranslation {
public:
    /**
     * Constructor for the GPTranslation class. The \p dictionary must contain a key for
     * the file that contains the general pertubation information as well as the file
     * format that is to be used.
     *
     * \param dictionary The ghoul::Dictionary that contains the information for this
     *        TLETranslation
     */
    explicit GPTranslation(const ghoul::Dictionary& dictionary);

    /**
     * Method returning the openspace::Documentation that describes the ghoul::Dictionary
     * that can be passed to the constructor.
     *
     * \return The openspace::Documentation that describes the ghoul::Dicitonary that can
     *         be passed to the constructor
     */
    static documentation::Documentation Documentation();
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___GPTRANSLATION___H__
