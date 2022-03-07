#ifndef OPENSPACE_MODULE_KAMELEON_ENABLED
#error "CDF inputs provided but Kameleon module is deactivated"
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

#include <modules/fieldlinessequence/util/matchingfieldlineshelper.h>

#include <modules/fieldlinessequence/util/movingfieldlinehelper.cpp>
#include <modules/fieldlinessequence/util/commons.h>
#include <modules/fieldlinessequence/util/fieldlinesstate.h>
#include <modules/kameleon/include/kameleonhelper.h>

#include <ccmc/Kameleon.h>
#include <ccmc/KameleonInterpolator.h>


namespace openspace::fls {

    // ALIASES

    using seedPointPair = std::pair<glm::vec3, glm::vec3>;

    // DECLARATIONS

    bool traceAndAddMatchingLinesToState(ccmc::Kameleon* kameleon,
        const std::vector<seedPointPair>& seeds, const std::string& tracingVar,
        FieldlinesState& state, const size_t nPointsOnPathLine,
        const size_t nPointsOnFieldlines);

    void traceAndCreateKeyFrame(std::vector<glm::vec3>& keyFrame,
        const glm::vec3& seedPoint,
        ccmc::Kameleon* kameleon,
        float innerbounds,
        size_t nPointsOnFieldlines);

    // DEFINITIONS

    bool openspace::fls::convertCdfToMatchingFieldlinesState(FieldlinesState& state,
        const std::string& cdfPath,
        const std::vector<glm::vec3>& seedPoints,
        double manualTimeOffset,
        const std::string& tracingVar,
        std::vector<std::string>& extraVars,
        std::vector<std::string>& extraMagVars,
        const size_t nPointsOnPathLine,
        const size_t nPointsOnFieldLines) {

        std::unique_ptr<ccmc::Kameleon> kameleon =
            kameleonHelper::createKameleonObject(cdfPath);
        state.setModel(fls::stringToModel(kameleon->getModelName()));

        // get time as string.
        state.setTriggerTime(kameleonHelper::getTime(kameleon.get(), manualTimeOffset));
        //std::string cdfStringTime = 
        //    SpiceManager::ref().dateFromEphemerisTime(cdfDoubleTime, "YYYYMMDDHRMNSC::RND");

                // use time as string for picking seedpoints from seedMap
            //std::vector<glm::vec3> seedPoints = seedMap.at(cdfStringTime);

        // TODO: Check if an even amount of seed points
        std::vector<seedPointPair> matchingSeedPoints;
        for (size_t i = 0; i < seedPoints.size(); i += 2) {
            matchingSeedPoints.push_back({ seedPoints[i], seedPoints[i + 1] });
        }

        bool isSuccessful = openspace::fls::traceAndAddMatchingLinesToState(kameleon.get(), matchingSeedPoints, tracingVar, state,
            nPointsOnPathLine, nPointsOnFieldLines);

        return isSuccessful;
    }

    bool traceAndAddMatchingLinesToState(ccmc::Kameleon* kameleon,
        const std::vector<seedPointPair>& matchingSeedPoints,
        const std::string& tracingVar, FieldlinesState& state,
        const size_t nPointsOnPathLine,
        const size_t nPointsOnFieldlines) {

        float innerBoundaryLimit;   // Defines the endpoint distance from Earth's center
        switch (state.model()) {
        case fls::Model::Batsrus:
            innerBoundaryLimit = 0.5f;  // TODO specify in Lua?
            break;
        default:
            LERROR(
                "OpenSpace's moving fieldlines currently only supports CDFs "
                "from the BATSRUS model"
            );
            return false;
        }

        // For each seedpoint, one line gets created, tracked with u perpendicular b.
        // then for each, and at each, vertex on that pathline, fieldlines are tracked
        if (tracingVar != "u_perp_b") {
            return false;
        }
        if (!kameleon->loadVariable("b")) {
            LERROR("Failed to load tracing variable: b");
            return false;
        }
        if (!kameleon->loadVariable("u")) {
            LERROR("Failed to load tracing variable: u");
            return false;
        }

        for (size_t i = 0; i < matchingSeedPoints.size(); ++i) {
            std::unique_ptr<ccmc::Interpolator> interpolator =
                std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);
            ccmc::Tracer tracer(kameleon, interpolator.get());
            tracer.setInnerBoundary(innerBoundaryLimit);

            // Create pathlines for matching fieldlines
            ccmc::Fieldline uPerpBPathLine1;
            uPerpBPathLine1 = tracer.unidirectionalTrace(
                tracingVar,
                matchingSeedPoints[i].first.x,
                matchingSeedPoints[i].first.y,
                matchingSeedPoints[i].first.z//,
                //ccmc::Tracer::Direction::REVERSE
            );// .reverseOrder();

            ccmc::Fieldline uPerpBPathLine2;
            uPerpBPathLine2 = tracer.unidirectionalTrace(
                tracingVar,
                matchingSeedPoints[i].second.x,
                matchingSeedPoints[i].second.y,
                matchingSeedPoints[i].second.z//,
                //ccmc::Tracer::Direction::REVERSE
            );// .reverseOrder();

            uPerpBPathLine1.getDs();
            uPerpBPathLine1.measure();
            uPerpBPathLine1.integrate();

            uPerpBPathLine2.getDs();
            uPerpBPathLine2.measure();
            uPerpBPathLine2.integrate();

            ccmc::Fieldline mappedPath1 = uPerpBPathLine1.interpolate(1, nPointsOnPathLine);
            std::vector<ccmc::Point3f> pathPositions1 = mappedPath1.getPositions();

            ccmc::Fieldline mappedPath2 = uPerpBPathLine2.interpolate(1, nPointsOnPathLine);
            std::vector<ccmc::Point3f> pathPositions2 = mappedPath2.getPositions();

            std::vector<glm::vec3> pathLine1;
            for (const ccmc::Point3f& p : pathPositions1) {
                pathLine1.emplace_back(p.component1, p.component2, p.component3);
            }

            std::vector<glm::vec3> pathLine2;
            for (const ccmc::Point3f& p : pathPositions2) {
                pathLine2.emplace_back(p.component1, p.component2, p.component3);
            }

            // Trim to get make matching fieldines equally long
            if (pathLine1.size() > pathLine2.size()) {
                pathLine1.resize(pathLine2.size());
            }
            else if (pathLine1.size() < pathLine2.size()) {
                pathLine2.resize(pathLine1.size());
            }

            //std::vector<float> velocities = computeVelocities(pathLine, kameleon);
            //std::vector<float> times = computeTimes(pathLine, velocities);
            // Elon: optimizing trimming could go here
            // seed? - trimPathFindLastVertex(pathLine, times, velocities, cdfLength);

            // Here all points on the pathLine will be used at seedpoints for 
            // the actual fieldlines (traced with "b" by default)
            state.addMatchingPathLines(std::move(pathLine1), std::move(pathLine2));

            //std::vector<glm::vec3>::iterator it = pathLine1.begin();
            //for (; it != pathLine1.end(); ++it) {
            for (size_t j = 0; j < pathLine1.size(); ++j) {

                std::vector<glm::vec3> keyFrame1;
                traceAndCreateKeyFrame(keyFrame1, pathLine1[j], kameleon, innerBoundaryLimit, nPointsOnFieldlines);

                std::vector<glm::vec3> keyFrame2;
                traceAndCreateKeyFrame(keyFrame2, pathLine2[j], kameleon, innerBoundaryLimit, nPointsOnFieldlines);

                // timeToNextKeyFrame is -1 if at last path line vertex
                float timeToNextKeyFrame1 = j + 1 == pathLine1.size() ?
                    -1.f : openspace::fls::computeTime(pathLine1[j], pathLine1[j + 1], kameleon);
                float timeToNextKeyFrame2 = j + 1 == pathLine2.size() ?
                    -1.f : openspace::fls::computeTime(pathLine2[j], pathLine2[j + 1], kameleon);

                state.addMatchingKeyFrames(std::move(keyFrame1), std::move(keyFrame2), 
                    timeToNextKeyFrame1, timeToNextKeyFrame2, i);
            }

        }
        bool isSuccessful = state.getAllMatchingFieldlines().size() > 0;
        return isSuccessful;
    }

    void traceAndCreateKeyFrame(std::vector<glm::vec3>& keyFrame,
        const glm::vec3& seedPoint,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t nPointsOnFieldlines) {
        std::unique_ptr<ccmc::Interpolator> newInterpolator =
            std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);

        ccmc::Tracer tracer(kameleon, newInterpolator.get());
        tracer.setInnerBoundary(innerBoundaryLimit);

        //Elon: replace "secondary trace var"
        std::string tracingVar = "b";
        ccmc::Fieldline fieldline = tracer.bidirectionalTrace(
            tracingVar,
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
        for (const ccmc::Point3f& pt : fieldlinePositions) {
            keyFrame.emplace_back(pt.component1, pt.component2, pt.component3);
        }
    }



    glm::vec3 openspace::fls::moveSeedpointInEigenvectorDirection(const glm::vec3& const pointInSpace, const glm::vec3& const eigenvector, const float& direction) {
        glm::vec3 movedPoint = pointInSpace + (eigenvector * FLT_EPSILON * direction);

        return movedPoint;
    }

} // openspace::fls
