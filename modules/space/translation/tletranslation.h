/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_SPACE___TLETRANSLATION___H__
#define __OPENSPACE_MODULE_SPACE___TLETRANSLATION___H__

#include <modules/space/translation/keplertranslation.h>

namespace openspace {

/**
 * A specialization of the KeplerTranslation that extracts the Keplerian elements from a
 * two-line element as described by the US Space Command
 * https://celestrak.com/columns/v04n03
 * The ghoul::Dictionary passed to the constructor must contain the pointer to a file that
 * will be read.
 */
class TLETranslation : public KeplerTranslation {
public:
    struct FileFormatError : public ghoul::RuntimeError {
        explicit FileFormatError(std::string offense);
        std::string offense;
    };

    /**
     * Constructor for the TLETranslation class. The \p dictionary must contain a key for
     * the file that contains the TLE information. The ghoul::Dictionary will be tested
     * against the openspace::Documentation returned by Documentation.
     * \param The ghoul::Dictionary that contains the information for this TLETranslation
     (*/
    TLETranslation(const ghoul::Dictionary& dictionary = ghoul::Dictionary());

    /**
     * Method returning the openspace::Documentation that describes the ghoul::Dictinoary
     * that can be passed to the constructor.
     * \return The openspace::Documentation that describes the ghoul::Dicitonary that can
     * be passed to the constructor
     */
    static documentation::Documentation Documentation();

private:
    /**
     * Reads the provided TLE file and calles the KeplerTranslation::setKeplerElments
     * method with the correct values. If \p filename is a valid TLE file but contains
     * disallowed values (see KeplerTranslation::setKeplerElements), a
     * KeplerTranslation::RangeError is thrown.
     *
     * \param filename The path to the file that contains the TLE file.
     * \param lineNum The line number in the file where the set of 3 TLE lines starts
     *
     * \throw std::system_error if the TLE file is malformed (does not contain at least
     *        two lines that start with \c 1 and \c 2.
     * \throw KeplerTranslation::RangeError If the Keplerian elements are outside of
     *        the valid range supported by Kepler::setKeplerElements
     * \pre The \p filename must exist
     */
    void readTLEFile(const std::string& filename, int lineNum);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___TLETRANSLATION___H__
