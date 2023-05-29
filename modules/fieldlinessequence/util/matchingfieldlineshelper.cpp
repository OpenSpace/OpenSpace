#ifndef OPENSPACE_MODULE_KAMELEON_ENABLED
#error "CDF inputs provided but Kameleon module is deactivated"
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

#include <modules/fieldlinessequence/util/matchingfieldlineshelper.h>

#include <modules/fieldlinessequence/util/movingfieldlinehelper.cpp>
#include <modules/fieldlinessequence/util/commons.h>
#include <modules/fieldlinessequence/util/fieldlinesstate.h>

#include <iostream>


namespace openspace::fls {

    // ALIASES

    using seedPointPair = std::pair<glm::vec3, glm::vec3>;

    // DECLARATIONS

    ccmc::Fieldline traceAndCreateMappedPathLine(const std::string& tracingVar,
        ccmc::Tracer& tracer,
        const glm::vec3& seedPoint,
        const size_t nPointsOnPathLine,
        ccmc::Tracer::Direction direction = ccmc::Tracer::Direction::FOWARD);

    std::vector<glm::vec3> concatenatePathLines(
        const std::vector<ccmc::Point3f>& firstPart,
        const std::vector<ccmc::Point3f>& secondPart);

    std::vector<glm::vec3> convertPoint3fToVec3(
        const std::vector<ccmc::Point3f>& point3f);

    bool traceAndAddMatchingLinesToState(ccmc::Kameleon* kameleon,
        const std::vector<seedPointPair>& matchingSeedPoints,
        const std::vector<double>& birthTimes,
        const std::string& tracingVar,
        FieldlinesState& state,
        const size_t nPointsOnPathLine,
        const size_t nPointsOnFieldlines
    );

    void traceAndCreateKeyFrame(std::vector<glm::vec3>& keyFrame,
        std::vector<float>& lengths,
        const glm::vec3& seedPoint,
        ccmc::Kameleon* kameleon,
        float innerbounds,
        size_t nPointsOnFieldlines
    );

    std::vector <std::vector<glm::vec3>> traceSeedPointRecursive(
        int i,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        std::vector<std::pair<glm::vec3, std::string>>& seedPoint,
        const size_t nPointsOnPathLine,
        std::vector <std::vector<glm::vec3>> seedPointFieldlinePositions,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        float& accuracy,
        size_t _nPointsOnFieldLine
    );

    bool checkIfFieldlineIsOpen(std::vector<glm::vec3> fieldlinePositions, std::string topology);

    bool keepCheckingFlowlinesFieldline(std::vector<glm::vec3> flowlinePos, int flowlineIndex);

    std::pair<glm::vec3, std::string> moveSeedpointDown(
        std::pair<glm::vec3, std::string>& seedPoint, float& stepLength);

        std::pair<glm::vec3, std::string> moveSeedpointDown(
            std::pair<glm::vec3, std::string>& seedPoint, float& stepLength);

    std::vector<glm::vec3> getPositionsFromLine(ccmc::Fieldline seedPointFlowline);

    std::vector <std::vector<glm::vec3>> addFieldLinePositionsToVector(
        std::vector <std::vector<glm::vec3>>& seedPointFieldlinePositions,
        std::vector<glm::vec3> seedPointFlowlinePositionsVec3,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t _nPointsOnFieldLine);

    glm::vec3 modifySeedpoint(
        std::pair<glm::vec3, std::string>& seedPoint,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        const size_t nPointsOnPathLine,
        std::vector <std::vector<glm::vec3>>& seedPointFieldlinePositions,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t _nPointsOnFieldLine,
        int& amountOfPreviousFieldlines,
        float& prevZ,
        float& stepLength,
        float& accuracy,
        bool up);

    // DEFINITIONS

    bool openspace::fls::convertCdfToMatchingFieldlinesState(
        FieldlinesState& state,
        ccmc::Kameleon* kameleon,
        std::vector<std::pair<glm::vec3, std::string>>& seedPoints,
        const std::vector<double>& birthTimes,
        double manualTimeOffset,
        const std::string& tracingVar,
        std::vector<std::string>& extraVars,
        std::vector<std::string>& extraMagVars,
        const size_t nPointsOnPathLine,
        const size_t nPointsOnFieldLines)
    {

        // TODO: Check if an even amount of seed points
        std::vector<seedPointPair> matchingSeedPoints;
        for (size_t i = 0; i < seedPoints.size(); i += 2) {
            matchingSeedPoints.push_back({ seedPoints[i].first, seedPoints[i + 1].first });
        }

        bool isSuccessful = openspace::fls::traceAndAddMatchingLinesToState(
            kameleon,
            matchingSeedPoints,
            birthTimes,
            tracingVar,
            state,
            nPointsOnPathLine,
            nPointsOnFieldLines
        );

        return isSuccessful;
    }

    /*
    * Traces the field line of a given seedpoint and returns the
    * points postions of that fieldline
    */
    std::vector<std::vector<glm::vec3>> getAllFieldlinesPositionsOfSeedPoints(
        std::vector<std::pair<glm::vec3, std::string>>& seedPoints,
        const std::string& tracingVar,
        ccmc::Kameleon* kameleon,
        const size_t nPointsOnPathLine,
        float& accuracy)
    {
        if (tracingVar != "u_perp_b") {
            std::cout << "aint working " << std::endl;
        }
        if (!kameleon->loadVariable("b")) {
            LERROR("Failed to load tracing variable: b");
            std::cout << "aint working " << std::endl;
        }
        if (!kameleon->loadVariable("u")) {
            LERROR("Failed to load tracing variable: u");
            std::cout << "aint working " << std::endl;
        }

        std::unique_ptr<ccmc::Interpolator> interpolator =
            std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);

        ccmc::Tracer tracer(kameleon, interpolator.get());

        std::vector<std::vector<glm::vec3>> seedPointFieldlinePositions;

        size_t _nPointsOnFieldLine = 2;
        float innerBoundaryLimit = 0.5f;
        int i = 0;

        seedPointFieldlinePositions = traceSeedPointRecursive(
            i,
            tracingVar,
            tracer,
            seedPoints,
            nPointsOnPathLine,
            seedPointFieldlinePositions,
            kameleon,
            innerBoundaryLimit,
            accuracy,
            _nPointsOnFieldLine
        );

        return seedPointFieldlinePositions;
    }


    std::vector <std::vector<glm::vec3>> traceSeedPointRecursive(
        int i,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        std::vector<std::pair<glm::vec3, std::string>>& seedPoints,
        const size_t nPointsOnPathLine,
        std::vector <std::vector<glm::vec3>> seedPointFieldlinePositions,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        float& accuracy,
        size_t _nPointsOnFieldLine
        )
    {
        if (i >= seedPoints.size())
        {
            return seedPointFieldlinePositions;
        }
        else if (seedPoints[i].second == "IMF" || seedPoints[i].second == "CLOSED")
        {
            seedPointFieldlinePositions = traceSeedPointRecursive(
                i + 1,
                tracingVar,
                tracer,
                seedPoints,
                nPointsOnPathLine,
                seedPointFieldlinePositions,
                kameleon,
                innerBoundaryLimit,
                accuracy,
                _nPointsOnFieldLine
            );
        }
        else
        {
            int counter = 1;
            ccmc::Fieldline flowline = traceAndCreateMappedPathLine(
                tracingVar,
                tracer,
                seedPoints[i].first,
                nPointsOnPathLine,
                ccmc::Tracer::Direction::FOWARD);

            std::vector<glm::vec3> flowlinePositions
                = getPositionsFromLine(flowline);
            std::vector<std::vector<glm::vec3>> testFieldlinePositions;

            for (int i = 0; i < flowlinePositions.size(); i++)
            {
                std::vector<glm::vec3> fieldlinePositions2 = fls::getFieldlinePositions(
                    flowlinePositions[i],
                    kameleon,
                    innerBoundaryLimit,
                    _nPointsOnFieldLine
                );
                testFieldlinePositions.push_back(fieldlinePositions2);
            }

            std::ofstream output_file("C:/Dev/OpenSpaceLocalData/NotWorking.txt");

            if (output_file.is_open()) {
                for (const auto& subvec : testFieldlinePositions) {
                    for (const auto& vec : subvec) {
                        output_file << vec.x << " " << vec.y << " " << vec.z << " " << std::endl;
                    }
                    output_file << std::endl << std::endl << "NEW FLOW LINE " << std::endl;
                }
                output_file.close();
            }
            else {
                std::cerr << "Unable to open file" << std::endl;
            }

            while(keepCheckingFlowlinesFieldline(flowlinePositions, counter))
            {
                std::vector<glm::vec3> fieldlinePositions = fls::getFieldlinePositions(
                    flowlinePositions[counter],
                    kameleon,
                    innerBoundaryLimit,
                    _nPointsOnFieldLine
                );

                if (!checkIfFieldlineIsOpen(fieldlinePositions, seedPoints[i].second))
                {
                    int amountOfPreviousFieldlines = 0;
                    float prevZ = 0;
                    float stepLength = 2;

                    glm::vec3 modifiedSeedpoint = modifySeedpoint(
                        seedPoints[i],
                        tracingVar,
                        tracer,
                        nPointsOnPathLine,
                        seedPointFieldlinePositions,
                        kameleon,
                        innerBoundaryLimit,
                        _nPointsOnFieldLine,
                        amountOfPreviousFieldlines,
                        prevZ,
                        stepLength,
                        accuracy,
                        true
                    );

                    seedPoints[i].first = modifiedSeedpoint;

                    break;
                }
                counter++;
            }

            seedPointFieldlinePositions = traceSeedPointRecursive(
                i + 1,
                tracingVar,
                tracer,
                seedPoints,
                nPointsOnPathLine,
                seedPointFieldlinePositions,
                kameleon,
                innerBoundaryLimit,
                accuracy,
                _nPointsOnFieldLine
            );
        }
    return seedPointFieldlinePositions;
    }

    bool checkIfFieldlineIsOpen(std::vector<glm::vec3> fieldlinePositions, std::string topology)
    {
        std::pair<double, double> threshold_start_z = { std::make_pair(25.0, -25.0) };
        std::pair<double, double> threshold_end_z = { std::make_pair(-0.6, 0.6) };

        if (topology == "OPEN_NORTH")
        {
            if (fieldlinePositions[0].z > threshold_start_z.first || fieldlinePositions[0].z < threshold_start_z.second &&
                fieldlinePositions[fieldlinePositions.size() - 1].z > threshold_end_z.first &&
                fieldlinePositions[fieldlinePositions.size() - 1].z < threshold_end_z.second) {

                return true;
            }

            return false;
        }
        else if (topology == "OPEN_SOUTH")
        {
            if (fieldlinePositions[fieldlinePositions.size() - 1].z > threshold_start_z.first
                || fieldlinePositions[fieldlinePositions.size() - 1].z < threshold_start_z.second
                && fieldlinePositions[0].z > threshold_end_z.first
                && fieldlinePositions[0].z < threshold_end_z.second) {

                return true;
            }

            return false;
        }
    }

    /*
    * Move seed z-coordinate down by stepLength
    */
    std::pair<glm::vec3, std::string> moveSeedpointDown(
        std::pair<glm::vec3, std::string>& seedPoint, float& stepLength)
    {
        try {
            if (seedPoint.second == "OPEN_NORTH")
            {
                seedPoint.first.z = seedPoint.first.z - stepLength;

                std::cout << "Modifying.... Down by:" << stepLength << std::endl;
                std::cout << "Top " << seedPoint.second << ", z value: " << seedPoint.first.z << std::endl;
            }
            else if (seedPoint.second == "OPEN_SOUTH")
            {
                seedPoint.first.z = seedPoint.first.z + stepLength;
                std::cout << "Modifying.... Down by: " << stepLength << std::endl;
                std::cout << "Top " << seedPoint.second << ", z value: " << seedPoint.first.z << std::endl;
            }
        }
        catch (const ghoul::RuntimeError& e) {
            std::cerr << "Error: " << e.message << std::endl;
        }
        return seedPoint;
    }

    /*
    * Move seed z-coordinate up by stepLength
    */
    std::pair<glm::vec3, std::string> moveSeedpointUp(
        std::pair<glm::vec3, std::string>& seedPoint, float& stepLength)
    {
        try {
            if (seedPoint.second == "OPEN_NORTH")
            {
                seedPoint.first.z = seedPoint.first.z + stepLength;

                std::cout << "Modifying.... Up by: " << stepLength << std::endl;
                std::cout << "Top " << seedPoint.second << ", z value: " << seedPoint.first.z << std::endl;
            }
            else if (seedPoint.second == "OPEN_SOUTH")
            {
                seedPoint.first.z = seedPoint.first.z - stepLength;

                std::cout << "Modifying.... Up by: " << stepLength << std::endl;
                std::cout << "Top " << seedPoint.second << ", z value: " << seedPoint.first.z << std::endl;
            }
        }
        catch (const ghoul::RuntimeError& e) {
            std::cerr << "Error: " << e.message << std::endl;
        }
        return seedPoint;
    }

    /*
    * Checks if the position of the flowLine has gone past set criteria
    */
    bool keepCheckingFlowlinesFieldline(std::vector<glm::vec3> flowlinePos, int flowlineIndex) {
        if (flowlinePos[flowlineIndex].x > -0.5)
        {
            return true;
        }
        return false;
    }

    /*
    * Adds all coordinates for fieldline positions to a vector from a given flowline
    */
    std::vector <std::vector<glm::vec3>> addFieldLinePositionsToVector(
        std::vector <std::vector<glm::vec3>>& seedPointFieldlinePositions,
        std::vector<glm::vec3> seedPointFlowlinePositionsVec3,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t _nPointsOnFieldLine)
    {
        seedPointFieldlinePositions.clear();

        std::vector<glm::vec3> fieldlinePositions2;

        for (int i = 0; i < seedPointFlowlinePositionsVec3.size(); i++)
        {
            fieldlinePositions2 = fls::getFieldlinePositions(
                seedPointFlowlinePositionsVec3[i],
                kameleon,
                innerBoundaryLimit,
                _nPointsOnFieldLine
            );

            seedPointFieldlinePositions.push_back(fieldlinePositions2);
        }
        return seedPointFieldlinePositions;
    }

    std::vector<glm::vec3> getPositionsFromLine(ccmc::Fieldline seedPointFlowline)
    {
        std::vector<ccmc::Point3f> seedPointFlowlinePositions
            = seedPointFlowline.getPositions();
        std::vector<glm::vec3> seedPointFlowlinePositionsVec3
            = convertPoint3fToVec3(seedPointFlowlinePositions);

        return seedPointFlowlinePositionsVec3;
    }

    glm::vec3 modifySeedpoint(
        std::pair<glm::vec3, std::string>& seedPoint,
        const std::string& tracingVar,
        ccmc::Tracer& tracer,
        const size_t nPointsOnPathLine,
        std::vector <std::vector<glm::vec3>>& seedPointFieldlinePositions,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t _nPointsOnFieldLine,
        int& amountOfPreviousFieldlines,
        float& prevZ,
        float& stepLength,
        float& accuracy,
        bool up)
    {
        std::vector<glm::vec3> fieldlinePositions;

        // TODO: FIX: First traced fieldline is an IMF... therefor we skip it
        int flowlineIndex = 1;

        if (up)
        {
            seedPoint = moveSeedpointUp(seedPoint, stepLength);
        }
        else if (!up)
        {
            seedPoint = moveSeedpointDown(seedPoint, stepLength);
        }

        stepLength = stepLength / 2;

        ccmc::Fieldline seedPointFlowline = traceAndCreateMappedPathLine(
            tracingVar,
            tracer,
            seedPoint.first,
            nPointsOnPathLine,
            ccmc::Tracer::Direction::FOWARD);

        std::vector<glm::vec3> seedPointFlowlinePositions
            = getPositionsFromLine(seedPointFlowline);


        while (keepCheckingFlowlinesFieldline(seedPointFlowlinePositions, flowlineIndex))
        {
            fieldlinePositions = fls::getFieldlinePositions(
                seedPointFlowlinePositions[flowlineIndex],
                kameleon,
                innerBoundaryLimit,
                _nPointsOnFieldLine
            );

            if (!checkIfFieldlineIsOpen(fieldlinePositions, seedPoint.second))
            {
                std::cout << "Bad fieldline! - Move seed point" << std::endl;
                glm::vec3 modifiedSeedpoint = modifySeedpoint(
                    seedPoint,
                    tracingVar,
                    tracer,
                    nPointsOnPathLine,
                    seedPointFieldlinePositions,
                    kameleon,
                    innerBoundaryLimit,
                    _nPointsOnFieldLine,
                    amountOfPreviousFieldlines,
                    prevZ,
                    stepLength,
                    accuracy,
                    true
                );
            }
            flowlineIndex++;
        }

        if (stepLength > accuracy)
        {
            std::cout << "Good fieldline - Not enough accuracy - Move seed point" << std::endl;
            glm::vec3 modifiedSeedpoint = modifySeedpoint(
                seedPoint,
                tracingVar,
                tracer,
                nPointsOnPathLine,
                seedPointFieldlinePositions,
                kameleon,
                innerBoundaryLimit,
                _nPointsOnFieldLine,
                amountOfPreviousFieldlines,
                prevZ,
                stepLength,
                accuracy,
                false
            );
        }

        seedPointFieldlinePositions = addFieldLinePositionsToVector(seedPointFieldlinePositions,
            seedPointFlowlinePositions,
            kameleon,
            innerBoundaryLimit,
            _nPointsOnFieldLine);

        return seedPoint.first;
    }

    /**
    * Uses the tracer to trace and create a ccmc::Fieldline and returns.
    * Default direction is forward tracing
    */
    ccmc::Fieldline traceAndCreateMappedPathLine(const std::string& tracingVar,
        ccmc::Tracer &tracer,
        const glm::vec3& seedPoint,
        const size_t nPointsOnPathLine,
        ccmc::Tracer::Direction direction) {


        ccmc::Fieldline uPerpBPathLine;
        uPerpBPathLine = tracer.unidirectionalTrace(
            tracingVar,
            seedPoint.x,
            seedPoint.y,
            seedPoint.z,
            direction
        );

        if (direction == ccmc::Tracer::Direction::REVERSE) {
            uPerpBPathLine = uPerpBPathLine.reverseOrder();
        }

        uPerpBPathLine.getDs();
        uPerpBPathLine.measure();
        uPerpBPathLine.integrate();

        ccmc::Fieldline mappedPath = uPerpBPathLine.interpolate(1, nPointsOnPathLine);

        return mappedPath;
    }

    /**
    * Concatenates the two vectors of pathline vertices into a new vector.
    * Converts from point3f to glm::vec3.
    */
    std::vector<glm::vec3> concatenatePathLines(
        const std::vector<ccmc::Point3f>& firstPart,
        const std::vector<ccmc::Point3f>& secondPart) {

        std::vector<glm::vec3> concatenated;
        for (const ccmc::Point3f& p : firstPart) {
            concatenated.emplace_back(p.component1, p.component2, p.component3);
        }
        for (const ccmc::Point3f& p : secondPart) {
            concatenated.emplace_back(p.component1, p.component2, p.component3);
        }

        return concatenated;
    }

    /**
    * Concatenates the two vectors of pathline vertices into a new vector.
    * Converts from point3f to glm::vec3.
    */
    std::vector<glm::vec3> convertPoint3fToVec3(
        const std::vector<ccmc::Point3f>& point3f) {

        std::vector<glm::vec3> vec3;
        for (const ccmc::Point3f& p : point3f) {
            vec3.emplace_back(p.component1, p.component2, p.component3);
        }

        return vec3;
    }

    bool traceAndAddMatchingLinesToState(ccmc::Kameleon* kameleon,
        const std::vector<seedPointPair>& matchingSeedPoints,
        const std::vector<double>& birthTimes,
        const std::string& tracingVar,
        FieldlinesState& state,
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


        for (size_t i = 0; i < matchingSeedPoints.size() / 2; i++) {
            std::unique_ptr<ccmc::Interpolator> interpolator =
                std::make_unique<ccmc::KameleonInterpolator>(kameleon->model);
            ccmc::Tracer tracer(kameleon, interpolator.get());
            tracer.setInnerBoundary(innerBoundaryLimit);

            // Create pathlines (IMF and CF) for matching fieldlines
            // 11 is first part of first path line, 12 is the second part.
            // same for the second path line with 21 and 22
            size_t firstSeedId = i * 2;
            size_t secondSeedId = i * 2 + 1;
            ccmc::Fieldline mappedPath11 = traceAndCreateMappedPathLine(tracingVar, tracer,
                matchingSeedPoints[firstSeedId].first, nPointsOnPathLine, ccmc::Tracer::Direction::REVERSE);
            ccmc::Fieldline mappedPath12 = traceAndCreateMappedPathLine(tracingVar, tracer,
                matchingSeedPoints[secondSeedId].first, nPointsOnPathLine);

            ccmc::Fieldline mappedPath21 = traceAndCreateMappedPathLine(tracingVar, tracer,
                matchingSeedPoints[firstSeedId].second, nPointsOnPathLine, ccmc::Tracer::Direction::REVERSE);
            ccmc::Fieldline mappedPath22 = traceAndCreateMappedPathLine(tracingVar, tracer,
                matchingSeedPoints[secondSeedId].second, nPointsOnPathLine);


            // Get the vertex positions from the mapped pathlines
            std::vector<ccmc::Point3f> pathPositions11 = mappedPath11.getPositions();
            std::vector<ccmc::Point3f> pathPositions12 = mappedPath12.getPositions();

            std::vector<ccmc::Point3f> pathPositions21 = mappedPath21.getPositions();
            std::vector<ccmc::Point3f> pathPositions22 = mappedPath22.getPositions();

            // compute time
            size_t lengthToConcatenation1 = pathPositions11.size();
            size_t lengthToConcatenation2 = pathPositions21.size();

            // Here we concatenate the pathline pairs 11 + 12 and 21 + 22
            std::vector<glm::vec3> pathLine1 = concatenatePathLines(pathPositions11, pathPositions12);
            std::vector<glm::vec3> pathLine2 = concatenatePathLines(pathPositions21, pathPositions22);

            std::vector<glm::vec3>::const_iterator concatenationPointPathLine1 =
                pathLine1.begin() + lengthToConcatenation1;

            std::vector<glm::vec3>::const_iterator concatenationPointPathLine2 =
                pathLine2.begin() + lengthToConcatenation2;

            double birthTime = birthTimes[i];

            // Here all points on the pathLine will be used at seedpoints for
            // the actual fieldlines (traced with "b" by default)
            // - 1 because arrays start at 0
            state.addMatchingPathLines(std::move(pathLine1), lengthToConcatenation1 - 1,
                std::move(pathLine2), lengthToConcatenation2 - 1, birthTime);

            double timeToDaysideReconnection1 = 0.0;
            double timeToDaysideReconnection2 = 0.0;
            for (size_t j = 0; j < pathLine1.size(); ++j) {

                std::vector<glm::vec3> keyFrame1;
                std::vector<float> keyFrameLength1;
                traceAndCreateKeyFrame(keyFrame1, keyFrameLength1, pathLine1[j], kameleon, innerBoundaryLimit, nPointsOnFieldlines);
                std::vector<glm::vec3> keyFrame2;
                std::vector<float> keyFrameLength2;
                traceAndCreateKeyFrame(keyFrame2, keyFrameLength2, pathLine2[j], kameleon, innerBoundaryLimit, nPointsOnFieldlines);

                // timeToNextKeyFrame is -1 if at last pathline vertex
                // Vi vill tracea tid baklänges för den pathline del som räknats ut baklänges
                // Vi tror vi kan byta plats på this och next vertex
                float timeToNextKeyFrame1;
                float timeToNextKeyFrame2;

                timeToNextKeyFrame1 = j + 1 == pathLine1.size() ?
                    -1.f : openspace::fls::computeTime(pathLine1[j + 1], pathLine1[j], kameleon);

                bool isBeforeReconnection = j < (pathLine1.size() / 2 - 1);
                if (isBeforeReconnection)
                    timeToDaysideReconnection1 += timeToNextKeyFrame1;

                timeToNextKeyFrame2 = j + 1 == pathLine2.size() ?
                    -1.f : openspace::fls::computeTime(pathLine2[j + 1], pathLine2[j], kameleon);

                if (isBeforeReconnection)
                    timeToDaysideReconnection2 += timeToNextKeyFrame2;

                state.addMatchingKeyFrames(std::move(keyFrame1), std::move(keyFrame2),
                    timeToNextKeyFrame1, timeToNextKeyFrame2, std::move(keyFrameLength1), std::move(keyFrameLength2), i);
            }

            double deathTime = 0;
            double lifeTimeAfterReconnection = 10;
            double lifeTime = timeToDaysideReconnection1 < timeToDaysideReconnection2 ?
                timeToDaysideReconnection1 + lifeTimeAfterReconnection :
                timeToDaysideReconnection2 + lifeTimeAfterReconnection;

            // for the sake of the smurfsaft
            // match the death of dayside to the birth of nigthside
            if (i < matchingSeedPoints.size() / 4) {
                deathTime = birthTime + 700;
            }
            else {
                deathTime = birthTime + 2000;
            }

            // TODO: Make it work dynamically
            state.setDeathTimes(deathTime, deathTime, i);
            //state.setDeathTimes(birthTime + lifeTime, birthTime + lifeTime, i);
        }
        bool isSuccessful = state.getAllMatchingFieldlines().size() > 0;
        return isSuccessful;
    }

    /// <summary>
    /// Will trace and create a keyframe
    /// </summary>
    /// <param name="keyFrame">The keyframe that will be constructed</param>
    /// <param name="seedPoint">From where the trace starts</param>
    /// <param name="kameleon"></param>
    /// <param name="innerBoundaryLimit"></param>
    /// <param name="nPointsOnFieldlines">how many points to be created in the trace</param>
    void traceAndCreateKeyFrame(std::vector<glm::vec3>& keyFrame,
        std::vector<float>& lengths,
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

    std::vector<glm::vec3> getFieldlinePositions(
        glm::vec3& seedPoint,
        ccmc::Kameleon* kameleon,
        float innerBoundaryLimit,
        size_t nPointsOnFieldlines) {

        std::vector<glm::vec3> keyFrame;

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

        return keyFrame;
    }
} // openspace::fls
