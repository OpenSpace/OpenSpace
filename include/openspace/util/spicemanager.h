/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_CORE___SPICEMANAGER___H__
#define __OPENSPACE_CORE___SPICEMANAGER___H__

#include <ghoul/fmt.h>
#include <ghoul/glm.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/boolean.h>
#include <ghoul/misc/exception.h>
#include <array>
#include <map>
#include <string>
#include <vector>
#include <set>

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wold-style-cast"
#elif defined(__GNUC__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wold-style-cast"
#endif

#include "SpiceUsr.h"
#include "SpiceZpr.h"

#ifdef __clang__
#pragma clang diagnostic pop
#elif defined(__GNUC__)
#pragma GCC diagnostic pop
#endif

namespace openspace {

namespace scripting { struct LuaLibrary; }

void throwSpiceError(const std::string& errorMessage);

class SpiceManager {
public:
    BooleanType(UseException);

    using TransformMatrix = std::array<double, 36>;
    using KernelHandle = unsigned int;

    struct SpiceException : public ghoul::RuntimeError {
        explicit SpiceException(std::string msg);
    };

    /**
     * Specifies the aberration correction method for the #targetPosition function.
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/spkpos_c.html
     */
    struct AberrationCorrection {
    public:
        /**
         * The type of the aberration correction.
         */
        enum class Type {
            /// No correction (`NONE`)
            None = 0,
            /// One-way light time (`LT`)
            LightTime,
            /// One-way light time and stellar (`LT+S`)
            LightTimeStellar,
            /// Converged newtonian light time (`CN`)
            ConvergedNewtonian,
            /// Converged newtonian+stellar (`CN+S`)
            ConvergedNewtonianStellar
        };
        /**
         * The direction of the aberration correct.
         */
        enum class Direction {
            Reception = 0,
            Transmission
        };

        /**
         * Default constructor initializing the AberrationCorrection to Type::None with a
         * Direction::Reception.
         */
        AberrationCorrection() = default;

        /**
         * Constructor initializing the AberrationCorrection to the provided \p type and
         * \p direction.
         *
         * \param t The type of the aberration correction (AberrationCorrection::Type)
         * \param d The used direction (AberrationCorrection::Direction)
         */
        AberrationCorrection(Type t, Direction d);

        /**
         * Converts one of the valid aberration correction strings into its enumeration
         * format. The valid strings are:
         *   - `NONE`
         *   - `LT`
         *   - `LT+S`
         *   - `CN`
         *   - `CN+S`
         *   - `XLT`
         *   - `XLT+S`
         *   - `XCN`
         *   - XCN+S
         *
         * \param identifier The identifier that should be converted into the enumeration
         *        Type and Direction
         *
         * \pre The \p identifier must not be empty and be of one of the valid strings
         */
        explicit AberrationCorrection(const std::string& identifier);

        /**
         * Returns the string representation of this Aberration Correction.
         *
         * \return The string representation of this Aberration correction
         */
        operator const char*() const;

        /// The type of aberration correction
        Type type = Type::None;
        /// The direction of the aberration correction
        Direction direction = Direction::Reception;
    };

    /**
     * The possible values for the method parameter of the targetInFieldOfView method.
     */
    enum class FieldOfViewMethod {
        Ellipsoid = 0,
        Point
    };

    /**
     * Returns the FieldOfViewMethod for the passed string. The allowed strings are
     * `ELLIPSOID` and `POINT`. All other values will result in an exception.
     *
     * \param method The field of view method
     * \return The field of view method enum
     *
     * \throw std::out_of_range if \p method is not a valid string
     * \pre \p method must not be empty
     */
    static FieldOfViewMethod fieldOfViewMethodFromString(const std::string& method);

    /**
     * The possible values for terminator type method of the terminatorEllipse method.
     */
    enum class TerminatorType {
        Umbral = 0,
        Penumbral
    };

    /**
     * Returns the TerminatorType for the passed string. The allowed strings are `UMBRAL`
     * and `PENUMBRAL`. All other values will result in an exception.
     *
     * \param type The terminator type
     * \return The terminator type enum
     *
     * \throw std::out_of_range if \p type is not a valid string
     * \pre \p type must not be empty
     */
    static TerminatorType terminatorTypeFromString(const std::string& type);

    static void initialize();
    static void deinitialize();
    static bool isInitialized();
    static SpiceManager& ref();

    /**
     * Loads one or more SPICE kernels into a program. The provided path can either be a
     * binary, text-kernel, or meta-kernel which gets loaded into the kernel pool. The
     * loading is done by passing the \p filePath to the `furnsh_c` function. Kernels can
     * safely be loaded multiple times and are reference counted.
     *
     * \param filePath The path to the kernel that should be loaded. This path will be
     *        passed to `absPath` to convert a relative path to an absolute path before
     *        usage
     * \return The loaded kernel's unique identifier that can be used to unload the kernel
     *
     * \throw SpiceException If the loading of the kernel \p filePath failed if, for
     *        example, \p filePath is not a valid SPICE kernel
     * \pre \p filePath must not be empty.
     * \pre \p filePath must be an absolute or relative path pointing to an existing file.
     * \post The kernel is loaded or has its reference counter incremented and the handle
     *       to the kernel is returned. The returned value is never equal to
     *       `KernelHandle(0)`.
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/furnsh_c.html
     */
    KernelHandle loadKernel(std::string filePath);

    /**
     * Unloads a SPICE kernel identified by the \p kernelId which was returned by the
     * loading call to #loadKernel. The unloading is done by calling the `unload_c`
     * function.
     *
     * \param kernelId The unique identifier that was returned from the call to
     *        #loadKernel which loaded the kernel
     *
     * \pre \p kernelId must be a valid handle.
     * \pre \p kernelId cannot be equal to `KernelHandle(0)`.
     * \post The kernel identified by \p kernelId is unloaded.
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/unload_c.html
     */
    void unloadKernel(KernelHandle kernelId);

    /**
     * Returns a list of all loaded kernels in the kernel pool that have been loaded
     * through the SpiceManager. The kernels are reported in order of their loading.
     *
     * \return The list of all loaded kernels that have been loaded through this manager
     */
    std::vector<std::string> loadedKernels() const;

    /**
     * Unloads a SPICE kernel identified by the \p filePath which was used in the
     * loading call to #loadKernel. The unloading is done by calling the `unload_c`
     * function.
     *
     * \param filePath The path of the kernel that should be unloaded
     *
     * \throw SpiceException If the \p filePath has not been previously used to
     *        successfully load a kernel.
     * \pre \p filePath must not be empty.
     * \post The kernel identified by \p filePath is unloaded.
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/unload_c.html
     */
    void unloadKernel(std::string filePath);

    /**
     * Returns whether a given \p target has an SPK kernel covering it at the designated
     * \p et ephemeris time.
     *
     * \param target The body to be examined. The target has to name a valid SPICE object
     *        with respect to the kernels that have been loaded
     * \param et The time for which the coverage should be checked
     * \return `true` if SPK kernels have been loaded to cover \p target at the time
     *         \p et, `false` otherwise
     *
     * \throw SpiceException If \p target does not name a valid SPICE object
     * \pre \p target must not be empty
     */
    bool hasSpkCoverage(const std::string& target, double et) const;

    /**
     * Returns a list of loaded SPK coverage intervals for \p target.
     *
     * \param target The body to be examined. The target has to name a valid SPICE object
     *        with respect to the kernels that have been loaded
     * \return `list` of SPK kernels for \p target, `empty` list if none loaded
     *
     * \throw SpiceException If \p target does not name a valid SPICE object
     * \pre \p target must not be empty.
     */
    std::vector<std::pair<double, double>> spkCoverage(const std::string& target) const;

    /**
     * Returns whether a given \p frame has a CK kernel covering it at the designated
     * \p et ephemeris time.
     *
     * \param frame The frame to be examined. The \p frame has to name a valid frame with
     *        respect to the kernels that have been loaded
     * \param et The time for which the coverage should be checked
     * \return `true` if SPK kernels have been loaded to cover \p target at the time
     *         \p et, false otherwise
     *
     * \throw SpiceException If \p target does not name a valid SPICE object or \p frame
     *        is not a valid frame
     * \pre \p target must not be empty
     */
    bool hasCkCoverage(const std::string& frame, double et) const;

    /**
     * Returns a list of loaded CK coverage intervals for \p target.
     *
     * \param target The body to be examined. The target has to name a valid SPICE object
     *        with respect to the kernels that have been loaded
     * \return `list` of CK kernels for \p target, `empty` list if none loaded
     *
     * \throw SpiceException If \p target does not name a valid SPICE object
     * \pre \p target must not be empty.
     */
    std::vector<std::pair<double, double>> ckCoverage(const std::string& target) const;

    /**
     * Returns a list of loaded spice frames.
     *
     * \param builtInFrames Boolean representing if builtIn or LoadedFrames should be used
     * \return `list` of Spice frames with ID(int) and Name(string)
     */
    std::vector<std::pair<int, std::string>> spiceBodies(bool builtInFrames) const;

    /**
     * Determines whether values exist for some \p item for any body, identified by its
     * \p naifId, in the kernel pool by passing it to the `bodfnd_c` function.
     *
     * \param naifId NAIF ID code of body
     * \param item The item to find
     * \return `true` if the function succeeded, `false` otherwise
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodfnd_c.html
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html
     */
    bool hasValue(int naifId, const std::string& item) const;

    /**
     * Determines whether values exist for some \p item for any \p body in the kernel pool
     * by passing it to the `bodfnd_c` function.
     *
     * \param body The name of the body that should be sampled
     * \param item The item to find in the \p body
     * \return `true` if the function succeeded, `false` otherwise
     *
     * \throw SpiceException If \p body does not name a valid SPICE object
     * \pre \p body must not be empty
     * \pre \p item must not be empty
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodfnd_c.html
     */
    bool hasValue(const std::string& body, const std::string& item) const;

    /**
     * Returns the NAIF ID for a specific \p body using the `bods2c_c` function.
     *
     * \param body The body name that should be retrieved
     * \return The ID of the `body` will be stored in this variable. The value will only
     *         be changed if the retrieval was successful
     *
     * \throw SpiceException If \p body does not name a valid SPICE object
     * \pre \p body must not be empty
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bods2c_c.html
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html
     */
    int naifId(const std::string& body) const;

    /**
     * Checks whether the specified \p body has a valid NAIF ID using the currently loaded
     * Spice kernels.
     *
     * \param body The body for which the presence of a valid ID should be checked
     * \return `true` if the \p body has a NAIF ID, `false` otherwise
     *
     * \pre \p body must not be empty.
     */
    bool hasNaifId(const std::string& body) const;

    /**
     * Returns the NAIF ID for a specific frame using `namfrm_c`.
     *
     * \param frame The frame name that should be retrieved
     * \return The NAIF ID of the \p frame
     *
     * \throw SpiceException If \p frame is not a valid frame
     * \pre \p frame must not be empty
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/namfrm_c.html
     */
    int frameId(const std::string& frame) const;

    /**
     * Checks whether the specified \p frame has a valid NAIF ID using the currently
     * loaded Spice kernels.
     *
     * \param frame The frame for which the presence of a valid ID should be checked
     * \return `true` if the \p frame has a NAIF ID, `false` otherwise
     *
     * \pre \p frame must not be empty
     */
    bool hasFrameId(const std::string& frame) const;

    /**
     * Retrieves a single \p value for a certain \p body. This method succeeds iff \p body
     * is the name of a valid body, \p value is a value associated with the body, and the
     * value consists of only a single `double` value. If all conditions are true, the
     * value is retrieved using the method `bodvrd_c` and stored in \p v.
     *
     * \param body The name of the body whose value should be retrieved or the NAIF ID of
     *        this body
     * \param value The value that should be retrieved, this value is case-sensitive
     * \param v The destination for the retrieved value
     *
     * \throw SpiceException If the \p body does not name a valid body, \p value is not a
     *        valid item for the \p body or the retrieved value is not a single value
     * \pre \p body must not be empty
     * \pre \p value must not be empty
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodvrd_c.html
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html
     */
    void getValue(const std::string& body, const std::string& value, double& v) const;

    /**
     * Retrieves a \p value with two components for a certain \p body. This method
     * succeeds iff \p body is the name of a valid body, \p value is a value associated
     * with the body, and the value consists of two `double` values. If all conditions
     * are true, the value is retrieved using the method `bodvrd_c` and stored in \p v.
     *
     * \param body The name of the body whose value should be retrieved or the NAIF ID of
     *        this body
     * \param value The value that should be retrieved, this value is case-sensitive
     * \param v The destination for the retrieved value
     *
     * \throw SpiceException If the \p body does not name a valid body, \p value is not a
     *        valid item for the \p body or the retrieved value is not a two-component
     *        value
     * \pre \p body must not be empty
     * \pre \p value must not be empty
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodvrd_c.html
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html
     */
    void getValue(const std::string& body, const std::string& value, glm::dvec2& v) const;

    /**
     * Retrieves a \p value with three components for a certain \p body. This method
     * succeeds iff \p body is the name of a valid body, \p value is a value associated
     * with the body, and the value consists of three `double` values. If all conditions
     * are true, the value is retrieved using the method `bodvrd_c` and stored in \p v.
     *
     * \param body The name of the body whose value should be retrieved or the NAIF ID of
     *        this body
     * \param value The value that should be retrieved, this value is case-sensitive
     * \param v The destination for the retrieved value
     *
     * \throw SpiceException If the \p body does not name a valid body, \p value is not a
     *        valid item for the \p body or the retrieved value is not a three-component
     *        value
     * \pre \p body must not be empty
     * \pre \p value must not be empty
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodvrd_c.html
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html
     */
    void getValue(const std::string& body, const std::string& value, glm::dvec3& v) const;

    /**
     * Retrieves a \p value with four components for a certain \p body. This method
     * succeeds iff \p body is the name of a valid body, \p value is a value associated
     * with the body, and the value consists of four `double` values. If all conditions
     * are true, the value is retrieved using the method `bodvrd_c` and stored in \p v.
     *
     * \param body The name of the body whose value should be retrieved or the NAIF ID of
     *        this body
     * \param value The value that should be retrieved, this value is case-sensitive
     * \param v The destination for the retrieved value
     *
     * \throw SpiceException If the \p body does not name a valid body, \p value is not a
     *        valid item for the \p body or the retrieved value is not a four-component
     *        value
     * \pre \p body must not be empty.
     * \pre \p value must not be empty.
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodvrd_c.html
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html
     */
    void getValue(const std::string& body, const std::string& value, glm::dvec4& v) const;

    /**
     * Retrieves a \p value with an arbitrary number of components for a certain \p body.
     * This method succeeds iff \p body is the name of a valid body, \p value is a value
     * associated with the body, and the value consists of the correct number of `double`
     * values. If all conditions are true, the value is retrieved using the method
     * `bodvrd_c` and stored in \p v.
     *
     * \param body The name of the body whose value should be retrieved or the NAIF ID of
     *        this body
     * \param value The value that should be retrieved, this value is case-sensitive
     * \param v The destination for the retrieved value. The `vector` must be
     *          preallocated to the correct size of components that should be retrieved
     *
     * \throw SpiceException If the \p body does not name a valid body, \p value is not a
     *        valid item for the \p body or the retrieved value does not contain the
     *        correct number of components
     * \pre \p body must not be empty.
     * \pre \p value must not be empty.
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodvrd_c.html
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html
     */
    void getValue(const std::string& body, const std::string& value,
        std::vector<double>& v) const;

    /**
     * Converts the value \p craftTicks of the internal clock for the spacecraft
     * identified by \p craft into standard ephemeris time and returns the value.
     *
     * \param craft The NAIF ID of the craft for which the time should be converted
     * \param craftTicks The internal clock ticks for the specified craft
     * \return The converted ephemeris time
     *
     * \throw SpiceException If the name \p craft is not a valid name available through
     *        all loaded kernels, if the craft is not supported by any of the loaded
     *        kernel, or if the provided \p craftTicks is not a valid tick time for the
     *        specific spacecraft
     * \pre \p craft must not be empty
     */
    double spacecraftClockToET(const std::string& craft, double craftTicks) const;

    /**
     * Converts the \p timeString representing a date to a double precision value
     * representing the ephemeris time; that is the number of TDB seconds past the J2000
     * epoch.
     *
     * \param timeString A string representing the time to be converted
     * \return The converted time; the number of TDB seconds past the J2000 epoch,
     *         representing the passed \p timeString
     *
     * \throw SpiceException If \p timeString is not a valid timestring according to the
     *        `str2et_c` function (see the Particulars section of the linked webpage)
     * \pre \p timeString must not be empty
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/str2et_c.html
     */
    double ephemerisTimeFromDate(const std::string& timeString) const;
    double ephemerisTimeFromDate(const char* timeString) const;

    /**
     * Converts the passed \p ephemerisTime into a human-readable date string with a
     * specific \p formatString.
     *
     * \param ephemerisTime The ephemeris time, that is the number of TDB seconds past the
     *        J2000 epoch
     * \param format The format string describing the output format
     * \return The destination for the converted date
     *
     * \pre \p format must not be empty
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/timout_c.html
     */
    template <int N = 31>
    std::string dateFromEphemerisTime(double ephemerisTime,
        const char (&format)[N] = "YYYY MON DDTHR:MN:SC.### ::RND") const
    {
        static_assert(N != 0, "Format must not be empty");

        std::string res;
        res.resize(N);
        dateFromEphemerisTime(ephemerisTime, res.data(), N, format);
        return res;
    }

    template <int N>
    void dateFromEphemerisTime(double ephemerisTime, char* outBuf, int bufferSize,
        const char (&format)[N] = "YYYY MON DDTHR:MN:SC.### ::RND") const
    {
        static_assert(N != 0, "Format must not be empty");
        ghoul_assert(N >= bufferSize - 1, "Buffer size too small");

        timout_c(ephemerisTime, format, bufferSize, outBuf);
        if (failed_c()) {
            throwSpiceError(std::format(
                "Error converting ephemeris time '{}' to date with format '{}'",
                    ephemerisTime, format
            ));
        }

        if (outBuf[0] == '*') {
            // The conversion failed and we need to use et2utc
            constexpr int SecondsPrecision = 3;
            et2utc_c(ephemerisTime, "C", SecondsPrecision, bufferSize, outBuf);
        }
    }

    std::string dateFromEphemerisTime(double ephemerisTime, const char* format);

    /**
     * Returns the \p position of a \p target body relative to an \p observer in a
     * specific \p referenceFrame, optionally corrected for \p lightTime (planetary
     * aberration) and stellar aberration (\p aberrationCorrection).
     *
     * \param target The target body name or the target body's NAIF ID
     * \param observer The observing body name or the observing body's NAIF ID
     * \param referenceFrame The reference frame of the output position vector
     * \param aberrationCorrection The aberration correction used for the position
     *        calculation
     * \param ephemerisTime The time at which the position is to be queried
     * \param lightTime If the \p aberrationCorrection is different from
     *        AbberationCorrection::Type::None, this variable will contain the light time
     *        between the observer and the target
     * \return The position of the \p target relative to the \p observer in the specified
     *         \p referenceFrame
     *
     * \throw SpiceException If the \p target or \p observer do not name a valid NAIF
     *        object, \p referenceFrame does not name a valid reference frame or if there
     *        is not sufficient data available to compute the position or neither the
     *        target nor the observer have coverage.
     * \pre \p target must not be empty
     * \pre \p observer must not be empty
     * \pre \p referenceFrame must not be empty
     * \post If an exception is thrown, \p lightTime will not be modified
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/spkpos_c.html
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html
     */
    glm::dvec3 targetPosition(const std::string& target, const std::string& observer,
        const std::string& referenceFrame, AberrationCorrection aberrationCorrection,
        double ephemerisTime, double& lightTime) const;

    /**
     * Returns the \p position of a \p target body relative to an \p observer in a
     * specific \p referenceFrame, optionally corrected for \p lightTime (planetary
     * aberration) and stellar aberration (\p aberrationCorrection).
     *
     * \param target The target body name or the target body's NAIF ID
     * \param observer The observing body name or the observing body's NAIF ID
     * \param referenceFrame The reference frame of the output position vector
     * \param aberrationCorrection The aberration correction used for the position
     *        calculation
     * \param ephemerisTime The time at which the position is to be queried
     * \return The position of the \p target relative to the \p observer in the specified
     *         \p referenceFrame
     *
     * \throw SpiceException If the \p target or \p observer do not name a valid NAIF
     *        object, \p referenceFrame does not name a valid reference frame or if there
     *        is not sufficient data available to compute the position or neither the
     *        target nor the observer have coverage
     * \pre \p target must not be empty
     * \pre \p observer must not be empty
     * \pre \p referenceFrame must not be empty
     * \post If an exception is thrown, \p lightTime will not be modified
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/spkpos_c.html
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html
     */
    glm::dvec3 targetPosition(const std::string& target,
        const std::string& observer, const std::string& referenceFrame,
        AberrationCorrection aberrationCorrection, double ephemerisTime) const;

    /**
     * This method returns the transformation matrix that defines the transformation from
     * the reference frame \p from to the reference frame \p to. As both reference frames
     * may be non-inertial, the \p ephemerisTime has to be specified.
     *
     * \param from The frame to be converted from
     * \param to The frame to be converted to
     * \param ephemerisTime Time at which to get the transformation matrix
     * \return The transformation matrix
     *
     * \throw SpiceException If the transformation matrix between \p from and \p to
     *        cannot be determined
     * \pre \p from must not be empty
     * \pre \p to must not be empty
     */
    glm::dmat3 frameTransformationMatrix(const std::string& from,
        const std::string& to, double ephemerisTime) const;

    /**
     * Struct that is used as the return value from the #surfaceIntercept method.
     */
    struct SurfaceInterceptResult {
        /// The closest surface intercept point on the target body in Cartesian
        /// Coordinates relative to the reference frame
        glm::dvec3 surfaceIntercept = glm::dvec3(0.0);

        /// If the aberration correction is not AberrationCorrection::Type::None, this
        /// value contains the time for which the intercept was computed. Otherwise it is
        /// the same as the ephemerisTime
        double interceptEpoch = 0.0;

        /// The vector from the observer's position to the \p surfaceIntercept position in
        /// the provided reference frame
        glm::dvec3 surfaceVector = glm::dvec3(0.0);

        /// `true` if the ray intersects the body, `false` otherwise
        bool interceptFound = false;
    };

    /**
     * Given an \p observer and a probing direction vector \p directionVector defining a
     * ray, compute the surface intercept of the ray on a \p target body at a specified
     * \p targetEpoch, optionally corrected for aberrations (\p aberrationCorrection).
     *
     * \param target The name of target body
     * \param observer The name of the observer
     * \param fovFrame Reference frame of the ray's direction vector
     * \param referenceFrame The reference frame in which the surface intercept and the
     *        surface vector are expressed
     * \param aberrationCorrection The aberration correction method that is applied to
     *        compute the intercept
     * \param ephemerisTime Intercept time in ephemeris seconds past J2000 TDB
     * \param directionVector Probing ray's direction
     * \return A SurfaceInterceptResult structure that contains all information about the
     *         intercept, including whether an intercept was found
     *
     * \throw SpiceException If the \p target or \p observer do not name the same NAIF
     *        object, the \p target or \p observer name the same NAIF object or are in the
     *        same location, the \p referenceFrame or \p fovFrame are not recognized,
     *        insufficient kernel information has been loaded.
     * \pre \p target must not be empty
     * \pre \p observer must not be empty
     * \pre \p The \p target and \p observer must be different strings
     * \pre \p fovFrame must not be empty
     * \pre \p referenceFrame must not be empty
     * \pre \p directionVector must not be the null vector
     * \post The SurfaceInterceptResult does not contain any uninitialized values
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/sincpt_c.html
     */
    SurfaceInterceptResult surfaceIntercept(const std::string& target,
        const std::string& observer, const std::string& fovFrame,
        const std::string& referenceFrame, AberrationCorrection aberrationCorrection,
        double ephemerisTime, const glm::dvec3& directionVector) const;

    /**
     * Determine whether a specific \p target is in the field-of-view of a specified
     * \p instrument or an \p observer at a given time, using the reference frame
     * \p referenceFrame.
     *
     * \param target The name or NAIF ID code string of the target
     * \param observer The name or NAIF ID code string of the observer
     * \param referenceFrame Body-fixed, body-centered frame for target body
     * \param instrument The name or NAIF ID code string of the instrument
     * \param method The type of shape model used for the target
     * \param aberrationCorrection The aberration correction method
     * \param ephemerisTime Time of the observation (seconds past J2000)
     * \return `true` if the target is visible, `false` otherwise
     *
     * \throw SpiceException If the \p target or \p observer do not name valid NAIF
     *        objects, the \p target or \p observer name the same NAIF object, the
     *        \p instrument does not name a valid NAIF object, or insufficient kernel
     *        information has been loaded
     * \pre \p target must not be empty
     * \pre \p observer must not be empty
     * \pre \p target and \p observer must not be different strings
     * \pre \p referenceFrame must not be empty
     * \pre \p instrument must not be empty
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/fovtrg_c.html
     */
    bool isTargetInFieldOfView(const std::string& target, const std::string& observer,
        const std::string& referenceFrame, const std::string& instrument,
        FieldOfViewMethod method, AberrationCorrection aberrationCorrection,
        double& ephemerisTime) const;

    /**
     * Struct that is used as the return value from the #targetState method.
     */
    struct TargetStateResult {
        /// The target position
        glm::dvec3 position = glm::dvec3(0.0);

        /// The target velocity
        glm::dvec3 velocity = glm::dvec3(0.0);

        /// One-way light time between `target` and `observer` if the aberration
        /// correction is enabled
        double lightTime = 0.0;
    };

    /**
     * Returns the state vector (position and velocity) of a \p target body relative to an
     * \p observer in a specific \p referenceFrame, optionally corrected for aberration
     * (\p aberrationCorrection).
     *
     * \param target The target body name or the target body's NAIF ID
     * \param observer The observing body name or the observing body's NAIF ID
     * \param referenceFrame The reference frame of the output position vector
     * \param aberrationCorrection The aberration correction method
     * \param ephemerisTime The time at which the position is to be queried
     * \return A TargetStateResult object that contains the `position`, containing the
     *         position of the target; the `velocity`, containing the velocity of the
     *         target; and the `lightTime`, containing the one-way light time between the
     *         \p target and the \p observer. This method is only set if the
     *         \p aberrationCorrection is set to a valid different from
     *         AberrationCorrection::None
     *
     * \throw SpiceException If the \p target or \p observer do not name a valid NAIF
     *        object, the \p referenceFrame is not a valid frame, or if there is
     *        insufficient kernel information
     * \pre \p target must not be empty
     * \pre \p observer must not be empty
     * \pre \p referenceFrame must not be empty
     * \post The resulting TargetStateResult is set to valid values; the `lightTime` is
     *       only set to a valid different from `0.0` if the \p aberrationCorrection is
     *       not AberrationCorrection::None
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/spkezr_c.html
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/naif_ids.html
     */
    TargetStateResult targetState(const std::string& target,
        const std::string& observer, const std::string& referenceFrame,
        AberrationCorrection aberrationCorrection, double ephemerisTime) const;

    /**
     * Returns the state transformation matrix used to convert from the \p sourceFrame to
     * the \p destinationFrame at a specific \p ephemerisTime.
     *
     * \param sourceFrame The name of the source reference frame
     * \param destinationFrame The name of the destination reference frame
     * \param ephemerisTime The time for which the transformation matrix should be
     *        returned
     * \return The TransformMatrix containing the transformation matrix that defines the
     *         transformation from the \p sourceFrame to the \p destinationFrame
     *
     * \throw SpiceException If the \p sourceFrame or the \p destinationFrame is
     *        not a valid frame
     * \pre \p sourceFrame must not be empty
     * \pre \p destinatoinFrame must not be empty
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/sxform_c.html
     */
    TransformMatrix stateTransformMatrix(const std::string& sourceFrame,
        const std::string& destinationFrame, double ephemerisTime) const;

    /**
     * Returns the matrix that transforms position vectors from the source reference frame
     * \p sourceFrame to the destination reference frame \p destinationFrame at the
     * specific \p ephemerisTime.
     *
     * \param sourceFrame The name of the source reference frame
     * \param destinationFrame The name of the destination reference frame
     * \param ephemerisTime The time at which the transformation matrix is to be queried
     * \return The transformation matrix that defines the transformation from the
     *         \p sourceFrame to the \p destinationFrame
     *
     * \throw SpiceException If there is no coverage available for the specified
     *        \p sourceFrame, \p destinationFrame, \p ephemerisTime combination
     * \pre \p sourceFrame must not be empty
     * \pre \p destinationFrame must not be empty
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/pxform_c.html
     */
    glm::dmat3 positionTransformMatrix(const std::string& sourceFrame,
        const std::string& destinationFrame, double ephemerisTime) const;

    /**
     * Returns the transformation matrix that transforms position vectors from the
     * \p sourceFrame at the time \p ephemerisTimeFrom to the \p destinationFrame at the
     * time \p ephemerisTimeTo.
     *
     * \param sourceFrame The name of the source reference frame
     * \param destinationFrame The name of the destination reference frame
     * \param ephemerisTimeFrom The time for the source reference frame
     * \param ephemerisTimeTo The time for the destination reference frame
     * \return The transformation matrix that maps between the \p sourceFrame at time
     *         \p ephemerisTimeFrom to the \p destinationFrame at the time
     *         \p ephemerisTimeTo
     *
     * \throw SpiceException If there is no coverage available for the specified
     *        \p sourceFrame and \p destinationFrame
     * \pre \p sourceFrame must not be empty
     * \pre \p destinationFrame must not be empty
     */
    glm::dmat3 positionTransformMatrix(const std::string& sourceFrame,
        const std::string& destinationFrame, double ephemerisTimeFrom,
        double ephemerisTimeTo) const;

    /// The structure returned by the #fieldOfView methods
    struct FieldOfViewResult {
        /**
         * The rough shape of the returned field of view.
         */
        enum class Shape {
            /// The shape is a pyramedal polyhedron
            Polygon = 0,
            /// The shape is a rectangular pyramid
            Rectangle,
            /// The shape is circular
            Circle,
            /// The shape is an ellipse
            Ellipse
        };

        /// The shape of the returned field of view
        Shape shape = Shape::Rectangle;

        /// The name of the reference frame in which the #bounds are defined
        std::string frameName;

        /// The direction towards the center of the field of view
        glm::dvec3 boresightVector = glm::dvec3(0.0);

        /// The corners of the field of view's bounding box, not necessarily unit vectors
        std::vector<glm::dvec3> bounds;
    };

    /**
     * This method returns the field-of-view (FOV) parameters for a specified
     * \p instrument.
     *
     * \param instrument The name of the instrument for which the FOV is to be retrieved
     * \return The FieldOfViewResult structure that contains information about the field
     *         of view
     *
     * \throw SpiceException If \p instrument does not name a valid NAIF object
     * \pre \p instrument must not be empty
     * \post The returned structure has all its values initialized
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/getfov_c.html
     */
    FieldOfViewResult fieldOfView(const std::string& instrument) const;

    /**
     * This method returns the field-of-view (FOV) parameters for a specified
     * \p instrument. The instrument must be a valid NAIF object index as returned by the
     * #naifId method.
     *
     * \param instrument The name of the instrument for which the FOV is to be retrieved
     * \return The FieldOfViewResult structure that contains information about the field
     *         of view
     *
     * \throw SpiceException If \p instrument does not name a valid NAIF object
     * \post The returned structure has all its values initialized
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/getfov_c.html
     */
    FieldOfViewResult fieldOfView(int instrument) const;

    /**
     * The structure retuned by the #terminatorEllipse method.
     */
    struct TerminatorEllipseResult {
        /// The vector from the target body at #targetEphemerisTime to the observer at
        /// the original time
        glm::dvec3 observerPosition = glm::dvec3(0.0);

        /// The full list of terminator points specified in the original reference frame
        std::vector<glm::dvec3> terminatorPoints;

        /// The local ephemeris time at the target, determined by the original
        /// `aberrationCorrection` factor
        double targetEphemerisTime = 0.0;
    };

    /**
     * This method computes a set of points on the umbral or penumbral terminator of
     * a specified \p target, where SPICE models the target shape as an ellipsoid.
     *
     * \param target The name of target body
     * \param observer The name of the observing body
     * \param frame The name of the reference frame relative to which the output
     *        terminator points are expressed
     * \param lightSource The name of body acting as light source
     * \param terminatorType Indicates the type of terminator to compute.
     *        TerminatorType::Umbral is the boundary of the portion of the ellipsoid
     *        surface in total shadow. TerminatorType::Penumbral is the boundary of the
     *        portion of the surface that is completely illuminated. Note that in
     *        astronomy references, the unqualified word "terminator" refers to the umbral
     *        terminator
     * \param aberrationCorrection The aberration correction method that is used
     * \param ephemerisTime The time at which the terminator ellipse shall be computed
     * \param numberOfTerminatorPoints The number of points along terminator that should
     *        be computed by this method
     * \return A TerminatorEllipseResult structure that contains all outputs of this
     *         function
     *
     * \throw SpiceException If the \p target, \p observer, or \p lightSource are not
     *        valid NAIF names, the \p frame is not a valid NAIF frame or there is
     *        insufficient kernel data loaded
     * \pre \p target must not be empty
     * \pre \p observer must not be empty
     * \pre \p frame must not be empty
     * \pre \p lightSource must not be empty
     * \pre \p numberOfTerminatorPoints must be bigger or equal to 1
     * \post The returned structure has all its values initialized
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/edterm_c.html
     */
    TerminatorEllipseResult terminatorEllipse(const std::string& target,
        const std::string& observer, const std::string& frame,
        const std::string& lightSource, TerminatorType terminatorType,
        AberrationCorrection aberrationCorrection, double ephemerisTime,
        int numberOfTerminatorPoints);

    /**
     * Sets the SpiceManager's exception handling. If UseException::No is passed to this
     * function, all subsequent calls will not throw an error, but fail silently instead.
     * If set to UseException::Yes, a SpiceException is thrown whenever an error occurs.
     *
     * \param useException The new exeception handling method that the SpiceManager should
     *        use
     */
    void setExceptionHandling(UseException useException);

    /**
     * Returns the current SpiceManager's exception strategy. See #setExceptionHandling.
     *
     * \return The current exception handling strategy.
     */
    UseException exceptionHandling() const;

    static scripting::LuaLibrary luaLibrary();

private:
    /**
     * Struct storing the information about all loaded kernels.
     */
    struct KernelInformation {
        /// The path from which the kernel was loaded
        std::string path;
        /// A unique identifier for each kernel
        KernelHandle id;
        /// How many parts loaded this kernel and are interested in it
        int refCount;
    };

    /**
     * Default constructor setting values for SPICE to not terminate on error.
     */
    SpiceManager();
    SpiceManager(const SpiceManager& c) = delete;
    SpiceManager& operator=(const SpiceManager& r) = delete;
    SpiceManager(SpiceManager&& r) = delete;

    /**
     * Default destructor that resets the SPICE settings.
     */
    ~SpiceManager();

    /**
     * Function to find and store the intervals covered by a ck file, this is done
     * by using mainly the `ckcov_c` and `ckobj_c` functions.
     *
     * \param path The path to the kernel that should be examined
     *
     * \pre \p path must be nonempty and be an existing file
     * \post Coverage times are stored only if loading was successful
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/ckobj_c.html
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/ckcov_c.html
     */
    void findCkCoverage(const std::string& path);

    /**
     * Function to find and store the intervals covered by a spk file, this is done
     * by using mainly the `spkcov_c` and `spkobj_c` functions.
     *
     * \param path The path to the kernel that should be examined
     *
     * \pre \p path must be nonempty and be an existing file
     * \post Coverage times are stored only if loading was successful
     *
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/spkobj_c.html
     * \see http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/spkcov_c.html
     */
    void findSpkCoverage(const std::string& path);

    /**
     * If a position is requested for an uncovered time in the SPK kernels, this function
     * will return an estimated position. If the coverage has not yet started, the first
     * position will be retrieved. If the coverage has ended, the last position will be
     * retrieved. If \p time is in a coverage gap, the position will be interpolated.
     *
     * \param target The body which is missing SPK data for this time
     * \param observer The observer. The position will be retrieved in relation to this
     *        body
     * \param referenceFrame The reference frame of the output position vector
     * \param aberrationCorrection The aberration correction used for the position
     *        calculation
     * \param ephemerisTime The time for which an estimated position is desirable
     * \param lightTime If the \p aberrationCorrection is different from
     *        AbberationCorrection::Type::None, this variable will contain the light time
     *        between the observer and the target.
     * \return The position of the \p target relative to the \p origin
     *
     * \throw SpiceException If the \p target or \p origin are not valid NAIF
     *        objects or if there is no position for the \p target at any time
     * \pre \p target must not be empty
     * \pre \p observer must not be empty
     * \pre \p referenceFrame must not be empty
     * \pre \p target and \p observer must be different
     * \post If an exception is thrown, \p lightTime will not be modified
     */
    glm::dvec3 getEstimatedPosition(const std::string& target,
        const std::string& observer, const std::string& referenceFrame,
        AberrationCorrection aberrationCorrection, double ephemerisTime,
        double& lightTime) const;

    /**
     * If a transform matrix is requested for an uncovered time in the CK kernels, this
     * function will an estimated matrix. If the coverage has not yet started, the first
     * transform matrix will be retrieved. If the coverage has ended, the last transform
     * matrix will be retrieved. If \p time is in a coverage gap, the transform matrix
     * will be interpolated.
     *
     * \param fromFrame The transform matrix will be retrieved in relation to this frame
     * \param toFrame The reference frame into which the resulting matrix will transformed
     * \param time The time for which an estimated transform matrix is requested
     * \return The estimated transform matrix of the frame
     *
     * \throw SpiceException If there is no coverage available for the specified
     *        \p sourceFrame and \p destinationFrame or the reference frames do not name a
     *        valid NAIF frame.
     * \pre \p fromFrame must not be empty
     * \pre \p toFrame must not be empty
     */
    glm::dmat3 getEstimatedTransformMatrix(const std::string& fromFrame,
        const std::string& toFrame, double time) const;

    /**
     * Loads pre defined leap seconds time kernel (naif00012.tls).
     */
    void loadLeapSecondsSpiceKernel();

    /**
     * Loads pre defined geophysical constants kernel (geophysical.ker)
     */
    void loadGeophysicalConstantsKernel();


    /// A list of all loaded kernels
    std::vector<KernelInformation> _loadedKernels;

    // Map: id, vector of pairs. Pair: Start time, end time;
    std::map<int, std::vector< std::pair<double, double>>> _ckIntervals;
    std::map<int, std::vector< std::pair<double, double>>> _spkIntervals;
    std::map<int, std::set<double>> _ckCoverageTimes;
    std::map<int, std::set<double>> _spkCoverageTimes;

    /// Stores whether the SpiceManager throws exceptions (Yes) or fails silently (No)
    UseException _useExceptions = UseException::Yes;

    /// The last assigned kernel-id, used to determine the next free kernel id
    KernelHandle _lastAssignedKernel = KernelHandle(0);

    static SpiceManager* _instance;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___SPICEMANAGER___H__
