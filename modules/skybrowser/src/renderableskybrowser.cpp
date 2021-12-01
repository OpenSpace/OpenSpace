#include <modules/skybrowser/include/renderableskybrowser.h>

#include <modules/skybrowser/include/utility.h>
#include <modules/skybrowser/include/wwtdatahandler.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/globals.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryjsonformatter.h> // formatJson
#include <deque>


namespace {

    constexpr const char* _loggerCat = "RenderableSkyBrowser";

    const openspace::properties::Property::PropertyInfo DimensionsInfo = {
        "Dimensions",
        "Browser Dimensions",
        "Set the dimensions of the web browser windows."
    };
    const openspace::properties::Property::PropertyInfo UrlInfo = {
        "Url",
        "URL",
        "The URL to load"
    };

    const openspace::properties::Property::PropertyInfo ReloadInfo = {
        "Reload",
        "Reload",
        "Reload the web browser"
    };


    struct [[codegen::Dictionary(RenderableSkyBrowser)]] Parameters {

        // [[codegen::verbatim(DimensionsInfo.description)]]
        std::optional<glm::vec2> browserDimensions;

        // [[codegen::verbatim(UrlInfo.description)]]
        std::optional<std::string> url;
    };

#include "renderableskybrowser_codegen.cpp"
} // namespace

namespace openspace {


    RenderableSkyBrowser::RenderableSkyBrowser(const ghoul::Dictionary& dictionary)
        : RenderablePlane(dictionary),
        WwtCommunicator(dictionary)
    {
        std::string identifier;
        if (dictionary.hasValue<std::string>(KeyIdentifier)) {
            identifier = dictionary.value<std::string>(KeyIdentifier);
        }
        else {
            identifier = "RenderableSkyBrowser";
        }
        setIdentifier(identifier);

        const Parameters p = codegen::bake<Parameters>(dictionary);
        _url = p.url.value_or(_url);

        // Ensure the texture is a square for now
        // Maybe change later
        glm::vec2 windowDimensions = global::windowDelegate->currentSubwindowSize();
        float maxDimension = std::max(windowDimensions.x, windowDimensions.y);
        _dimensions = { maxDimension, maxDimension };

        addProperty(_url);
        addProperty(_dimensions);
        addProperty(_reload);
        addProperty(_verticalFov);
        addProperty(_borderColor);
        addProperty(_equatorialAim);
    }

    RenderableSkyBrowser::~RenderableSkyBrowser() {

    }

    void RenderableSkyBrowser::initializeGL() {
        Browser::initializeGL();
        RenderablePlane::initializeGL();
    }

    void RenderableSkyBrowser::deinitializeGL() {
       
        RenderablePlane::deinitializeGL();
        Browser::deinitializeGL();
    }

    void RenderableSkyBrowser::update(const UpdateData& data) {
        Browser::update();
        RenderablePlane::update(data);
    }

    void RenderableSkyBrowser::setIdInBrowser()
    {
        WwtCommunicator::setIdInBrowser(identifier());
    }

	void RenderableSkyBrowser::placeAt3dPosition(
        const glm::dvec3& positionSpeck, float verticalFov)
	{
        std::string renderableId = dynamic_cast<SceneGraphNode*>(
            this)->renderable()->identifier();
        // Uris for properties
        std::string sizeUri = "Scene." + _identifier + "." + renderableId + ".Size";
        std::string positionUri = "Scene." + _identifier + ".Translation.Position";
        std::string rotationUri = "Scene." + _identifier + ".Rotation.Rotation";
        std::string cameraAim = "NavigationHandler.OrbitalNavigator.Aim";
        glm::dvec3 position = positionSpeck * distanceconstants::Parsec;
        // Calculate the size of the plane with trigonometry
        // Calculate in equatorial coordinate system since the FOV is from Earth
        //  /|
        // /_|    Adjacent is the horizontal line, opposite the vertical 
        // \ |    Calculate for half the triangle first, then multiply with 2
        //  \|
        glm::dvec3 j2000 = skybrowser::galacticToEquatorial(position);
        double adjacent = glm::length(j2000);
        double opposite = 2 * adjacent * glm::tan(glm::radians(verticalFov * 0.5));

        // Calculate rotation to make the plane face the solar system barycenter
        glm::dvec3 normal = glm::normalize(-position);
        glm::dvec3 newRight = glm::normalize(
            glm::cross(glm::dvec3(0.0, 0.0, 1.0), normal)
        );
        glm::dvec3 newUp = glm::cross(normal, newRight);
        // Face the Solar System Barycenter
        glm::dmat3 rotation = glm::dmat3(1.0);
        rotation[0] = newRight;
        rotation[1] = newUp;
        rotation[2] = normal;

        std::string setValue = "openspace.setPropertyValueSingle('";

        openspace::global::scriptEngine->queueScript(
            setValue + sizeUri + "', " + std::to_string(opposite) + ");",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        openspace::global::scriptEngine->queueScript(
            setValue + positionUri + "', " + ghoul::to_string(position) + ");",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
        openspace::global::scriptEngine->queueScript(
            setValue + rotationUri + "', " + ghoul::to_string(rotation) + ");",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
	}
} // namespace
