#ifndef OPENSPACE_MODULE_KAMELEON_ENABLED
#error "CDF inputs provided but Kameleon module is deactivated"
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

#include <modules/fieldlinessequence/util/matchingfieldlinehelper.h>

#include <modules/fieldlinessequence/util/commons.h>
#include <modules/fieldlinessequence/util/fieldlinesstate.h>
#include <ccmc/Kameleon.h>
#include <ccmc/KameleonInterpolator.h>
#include <modules/kameleon/include/kameleonhelper.h>


//bool traceAndAddLinesToState();

bool openspace::fls::convertCdfToMatchingFieldlinesState(FieldlinesState& state, 
    const std::string& cdfPath,
    const std::vector<glm::vec3>& seedPoints,
    double manualTimeOffset,
    const std::string& tracingVar,
    std::vector<std::string>& extraVars,
    std::vector<std::string>& extraMagVars,
    const size_t nPointsOnPathLine,
    const size_t nPointsOnFieldLines)
{
    // TEMPORARY HARD-CODED MATCHING SEED POINTS
    constexpr const float eps = 0.1f;

    const glm::vec3 criticalPoint{ 10.019400000000000084f, -3.967299999999999827f, -0.02289000000000000062f };
    const glm::vec3 eigenVector = glm::normalize( glm::vec3{ 9.025006184649818408f, -5.271561957959811195, -0.2821642266899985763f });

    glm::vec3 imfSeed = criticalPoint + eigenVector * eps;
    glm::vec3 cfSeed = criticalPoint - eigenVector * eps;
    //******************************************

    std::unique_ptr<ccmc::Kameleon> kameleon =
        kameleonHelper::createKameleonObject(cdfPath);
    state.setModel(fls::stringToModel(kameleon->getModelName()));

    // get time as string.
    state.setTriggerTime(kameleonHelper::getTime(kameleon.get(), manualTimeOffset));
    //std::string cdfStringTime = 
    //    SpiceManager::ref().dateFromEphemerisTime(cdfDoubleTime, "YYYYMMDDHRMNSC::RND");
    
    return false;
}
