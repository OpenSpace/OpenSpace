/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/fieldlinessequence/util/movingfieldlinehelper.h>

#include <modules/fieldlinessequence/util/commons.h>
#include <modules/fieldlinessequence/util/fieldlinesstate.h>
#include <ghoul/logging/logmanager.h>

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED

#include <ccmc/Kameleon.h>
#include <ccmc/KameleonInterpolator.h>
#include <modules/kameleon/include/kameleonhelper.h>

#endif // OPENSPACE_MODULE_KAMELEON_ENABLED
#pragma optimize("", off)

constexpr const char* _loggerCat = "MovingFiledlinesHelper[ Kameleon ]";

namespace openspace::fls {
#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
    bool traceAndAddLinesToState(ccmc::Kameleon* kameleon,
        const std::vector<glm::vec3>& seeds, const std::string& tracingVar,
        FieldlinesState& state, const size_t nPointsOnPathLine,
        const size_t nPointsOnFieldlines);
    std::vector<glm::vec3> traceAndCreateFieldline(const glm::vec3& seedPoint,
        ccmc::Kameleon* kameleon, float innerbounds, size_t nPointsOnFieldlines);
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

    float computeVelocity(glm::vec3 point, ccmc::Kameleon* kameleon) {
        float velocity;
        std::unique_ptr<ccmc::Interpolator> interpolator =
            std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);
        //compute u_perp_b with variables u and b
            //normalized b vector
        const glm::vec3 normBVec = glm::normalize(glm::vec3(
            interpolator->interpolate("bx", point.x, point.y, point.z),
            interpolator->interpolate("by", point.x, point.y, point.z),
            interpolator->interpolate("bz", point.x, point.y, point.z)));

        const glm::vec3 uVec = glm::vec3(
            interpolator->interpolate("ux", point.x, point.y, point.z),
            interpolator->interpolate("uy", point.x, point.y, point.z),
            interpolator->interpolate("uz", point.x, point.y, point.z));

        float u_dot_b = glm::dot(normBVec, uVec);

        //multiply by 1000 since the data is in km/s and openspace uses m/s
        glm::vec3 u_perp_b = (uVec - (normBVec * u_dot_b)) * 1000.0f;

        return glm::length(u_perp_b);
    }

    float computeTime(glm::vec3 vertex, glm::vec3 nextVertex, ccmc::Kameleon* kameleon) {
        float velocity = computeVelocity(vertex, kameleon);
        //distance to next interpolation
        float distance = glm::length(nextVertex * fls::ReToMeter - vertex * fls::ReToMeter);
        //time to next interpolation = distance / velocity = (m / (m/s)) = s
        return distance / velocity;
    }

    /** Traces field lines from the provided cdf file using kameleon and stores the data in
    * the provided FieldlinesState.
    * Returns `false` if it fails to create a valid state. Requires the kameleon module to
    * be activated!
    * \param state, FieldlineState which should hold the extracted data
    * \param cdfPath, std::string of the absolute path to a .cdf file
    * \param seedPoints, vector of seed points from which to trace field lines
    * \param tracingVar, which quantity to trace lines from. Typically "b" for magnetic field
    *        lines and "u" for velocity flow lines
    * \param extraVars, extra scalar quantities to be stored in the FieldlinesState; e.g. "T"
    *        for temperature, "rho" for density or "P" for pressure
    * \param extraMagVars, variables which should be used for extracting magnitudes, must be
    *        a multiple of 3; e.g. "ux", "uy" & "uz" to get the magnitude of the velocity
    *        vector at each line vertex
    */
    bool convertCdfToMovingFieldlinesState(FieldlinesState& state, const std::string& cdfPath,
        const std::vector<glm::vec3>& seedPoints,
        double manualTimeOffset,
        const std::string& tracingVar,
        std::vector<std::string>& extraVars,
        std::vector<std::string>& extraMagVars,
        const size_t nPointsOnPathLine,
        const size_t nPointsOnFieldLines)
    {
#ifndef OPENSPACE_MODULE_KAMELEON_ENABLED
        LERROR("CDF inputs provided but Kameleon module is deactivated");
        return false;
#else // OPENSPACE_MODULE_KAMELEON_ENABLED
        std::unique_ptr<ccmc::Kameleon> kameleon =
            kameleonHelper::createKameleonObject(cdfPath);
        state.setModel(fls::stringToModel(kameleon->getModelName()));

        // get time as string.
        state.setTriggerTime(kameleonHelper::getTime(kameleon.get(), manualTimeOffset));
        //std::string cdfStringTime = 
        //    SpiceManager::ref().dateFromEphemerisTime(cdfDoubleTime, "YYYYMMDDHRMNSC::RND");

        // use time as string for picking seedpoints from seedMap
        //std::vector<glm::vec3> seedPoints = seedMap.at(cdfStringTime);
        bool success = traceAndAddLinesToState(kameleon.get(), seedPoints, tracingVar, state,
            nPointsOnPathLine, nPointsOnFieldLines);

        return success;
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED
    }

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
    bool traceAndAddLinesToState(ccmc::Kameleon* kameleon,
        const std::vector<glm::vec3>& seedPoints,
        const std::string& tracingVar, FieldlinesState& state,
        const size_t nPointsOnPathLine,
        const size_t nPointsOnFieldlines)
    {
        bool success = false;

        float innerBoundaryLimit;
        switch (state.model()) {
        case fls::Model::Batsrus:
            innerBoundaryLimit = 0.5f;// TODO specify in Lua?
            break;
        default:
            LERROR(
                "OpenSpace's moving fieldlines currently only supports CDFs "
                "from the BATSRUS model"
            );
            return success;
        }

        // For each seedpoint, one line gets created, tracked with u perpendicular b.
        // then for each, and at each, vertex on that pathline, fieldlines are tracked
        if (tracingVar != "u_perp_b") {
            return success;
        }
        if (!kameleon->loadVariable("b")) {
            LERROR("Failed to load tracing variable: b");
            return success;
        }
        if (!kameleon->loadVariable("u")) {
            LERROR("Failed to load tracing variable: u");
            return success;
        }

        int i = 0;
        for (const glm::vec3& seed : seedPoints) {
            std::unique_ptr<ccmc::Interpolator> interpolator =
                std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);
            ccmc::Tracer tracer(kameleon, interpolator.get());
            tracer.setInnerBoundary(innerBoundaryLimit);

            ccmc::Fieldline uPerpBPathLine;
            uPerpBPathLine = tracer.unidirectionalTrace(
                tracingVar,
                seed.x,
                seed.y,
                seed.z
            );

            uPerpBPathLine.getDs();
            uPerpBPathLine.measure();
            uPerpBPathLine.integrate();

            ccmc::Fieldline mappedPath = uPerpBPathLine.interpolate(1, nPointsOnPathLine);
            std::vector<ccmc::Point3f> pathPositions = mappedPath.getPositions();
            //const size_t nPathLinePoints = pathPositions.size();

            std::vector<glm::vec3> pathLine;
            for (const ccmc::Point3f& p : pathPositions) {
                pathLine.emplace_back(p.component1, p.component2, p.component3);
            }

            //std::vector<float> velocities = computeVelocities(pathLine, kameleon);
            //std::vector<float> times = computeTimes(pathLine, velocities);
            // Elon: optimizing trimming could go here
            // seed? - trimPathFindLastVertex(pathLine, times, velocities, cdfLength);

            // Here all points on the pathLine will be used at seedpoints for 
            // the actual fieldlines (traced with "b" by default)
            state.addPathLine(pathLine, i);
            for (std::vector<glm::vec3>::iterator it = pathLine.begin();
                it != pathLine.end() - 1;
                std::advance(it, 1))
            {
                glm::vec3 thisVertex = *it;
                glm::vec3 nextVertex = *(it + 1);

                std::vector<glm::vec3> fieldline = traceAndCreateFieldline(
                    thisVertex, kameleon, innerBoundaryLimit, nPointsOnFieldlines
                );
                float time = computeTime(thisVertex, nextVertex, kameleon);
                state.addFieldLine(fieldline, time, i);
            }
            if (state.allPathLines().size() > 0) {
                success = true;
            }
            ++i;
        }
        return success;
    }

    // Creates a fieldline key frame for a vertex in the path line
    std::vector<glm::vec3> traceAndCreateFieldline(const glm::vec3& seedPoint,
        ccmc::Kameleon* kameleon,
        const float innerBoundaryLimit,
        const size_t nPointsOnFieldlines)
    {
        std::unique_ptr<ccmc::Interpolator> newInterpolator =
            std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);
        ccmc::Tracer tracer2(kameleon, newInterpolator.get());
        tracer2.setInnerBoundary(innerBoundaryLimit);

        //Elon: replace "secondary trace var"
        std::string secondaryTraceVar = "b";
        ccmc::Fieldline fieldline = tracer2.bidirectionalTrace(
            secondaryTraceVar,
            seedPoint.x,
            seedPoint.y,
            seedPoint.z
        );

        fieldline.getDs();
        fieldline.measure();
        fieldline.integrate();

        ccmc::Fieldline mappedFieldline =
            fieldline.interpolate(1, nPointsOnFieldlines);
        const std::vector<ccmc::Point3f>& fieldlinePositions =
            mappedFieldline.getPositions();
        std::vector<glm::vec3> vertices;
        for (const ccmc::Point3f& pt : fieldlinePositions) {
            vertices.emplace_back(pt.component1, pt.component2, pt.component3);
        }
        return vertices;
    }
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

} // namespace openspace::fls
