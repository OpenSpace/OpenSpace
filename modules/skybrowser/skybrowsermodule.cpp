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

#include <modules/skybrowser/skybrowsermodule.h>
#include <modules/skybrowser/tinyxml2/tinyxml2.h>

 //#include <modules/webbrowser/webbrowsermodule.h>
 //#include <modules/webbrowser/include/screenspacebrowser.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/rendering/renderable.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/scene/scene.h>
#include <openspace/util/factorymanager.h>
#include "skybrowsermodule_lua.inl"
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <cmath> // For atan2
#include <glm/gtx/string_cast.hpp> // For printing glm data
#include <ghoul/filesystem/filesystem.h>
#include <openspace/util/httprequest.h> // For downloading files from url
#include <fstream>    
#include <filesystem> // To iterate through files in directory

namespace {
    struct [[codegen::Dictionary(ScreenSpaceSkyBrowser)]] Parameters {

    };
    
    #include "skybrowsermodule_codegen.cpp"
    
    
    
} // namespace

namespace openspace {
  
    scripting::LuaLibrary SkyBrowserModule::luaLibrary() const {

        scripting::LuaLibrary res;
        res.name = "skybrowser";
        res.functions = {
            {
                "create",
                &skybrowser::luascriptfunctions::createBrowser,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "move",
                &skybrowser::luascriptfunctions::moveBrowser,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "follow",
                &skybrowser::luascriptfunctions::followCamera,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            },
            {
                "loadCollection",
                &skybrowser::luascriptfunctions::loadImgCollection,
                {},
                "string or list of strings",
                "Add one or multiple exoplanet systems to the scene, as specified by the "
                "input. An input string should be the name of the system host star"
            }
        };

        return res;
    }


SkyBrowserModule::SkyBrowserModule()
    : OpenSpaceModule(SkyBrowserModule::Name)
    , _mouseOnObject(nullptr)
    , currentlyResizingBrowser(false)
    , currentlyDraggingObject(false)
    , resizeVector(0.f, 0.f)
    , shouldInitialize(true)
{
    global::callback::mousePosition->emplace_back(
        [&](double x, double y) {    
            // Quick fix to make all renderables find its corresponding partner
            if (shouldInitialize) {
                std::for_each(renderables.begin(), renderables.end(), [&](ScreenSpaceRenderable* obj) {
                    if (to_target(obj)) {
                        to_target(obj)->setConnectedBrowser();
                    }
                    else if (to_browser(obj)) {
                        to_browser(obj)->setConnectedTarget();
                    }
                    });
                shouldInitialize = false;
            }
            
            glm::vec2 pos = glm::vec2(static_cast<float>(x), static_cast<float>(y));
            _mousePosition = getMousePositionInScreenSpaceCoords(pos);

            if (currentlyDraggingObject) {
                _mouseOnObject->translate(_mousePosition - startDragMousePos, startDragObjectPos);
            }
            else if (currentlyResizingBrowser) {
                // Calculate scaling factor
                glm::vec2 mouseDragVector = (_mousePosition - startDragMousePos);
                glm::vec2 scalingVector = mouseDragVector * resizeVector;
                glm::vec2 newSizeRelToOld = (startResizeBrowserSize + (scalingVector)) / startResizeBrowserSize;
                // Scale the browser
                to_browser(_mouseOnObject)->scale(newSizeRelToOld);

                // For dragging functionality, translate so it looks like the browser isn't moving
                // Make sure the browser doesn't move in directions it's not supposed to 
                _mouseOnObject->translate(mouseDragVector * abs(resizeVector) / 2.f, startDragObjectPos);
            }
            // If there is no dragging or resizing, look for new objects
            else {
                // Save old selection for removing highlight
                ScreenSpaceRenderable* lastObj = _mouseOnObject;

                // Find and save what mouse is currently hovering on
                auto currentlyOnObject = std::find_if(renderables.begin(), renderables.end(), [&](ScreenSpaceRenderable* obj) {
                    return obj->coordIsInsideCornersScreenSpace(_mousePosition);
                    });
                _mouseOnObject = currentlyOnObject != renderables.end() ? *currentlyOnObject : nullptr;

                // Selection has changed
                if (lastObj != _mouseOnObject) {
                    glm::ivec3 highlightAddition{ 35, 35, 35 };
                    // Remove highlight
                    if (to_browser(lastObj)) {
                        to_browser(lastObj)->setBorderColor(to_browser(lastObj)->getColor() - highlightAddition);
                    }
                    else if (to_target(lastObj)) {
                        to_target(lastObj)->setBorderColor(to_target(lastObj)->getColor() - highlightAddition);
                    }

                    // Add highlight
                    if (to_browser(_mouseOnObject)) {
                        to_browser(_mouseOnObject)->setBorderColor(to_browser(_mouseOnObject)->getColor() + highlightAddition);
                    }
                    else if (to_target(_mouseOnObject)) {
                        to_target(_mouseOnObject)->setBorderColor(to_target(_mouseOnObject)->getColor() + highlightAddition);
                    }
                }
                
            }
        }
    );

    global::callback::mouseScrollWheel->emplace_back(
        [&](double, double scroll) -> bool {
            // If mouse is on browser, apply zoom
            if (to_browser(_mouseOnObject)) {
                to_browser(_mouseOnObject)->scrollZoom(scroll);
                return true;
            }
            else if (to_target(_mouseOnObject) && to_target(_mouseOnObject)->getSkyBrowser()) {
                to_target(_mouseOnObject)->getSkyBrowser()->scrollZoom(scroll);
            }

            return false;
        }
    );

    global::callback::mouseButton->emplace_back(
        [&](MouseButton button, MouseAction action, KeyModifier modifier) -> bool {

            if (action == MouseAction::Press) {

                if (_mouseOnObject && button == MouseButton::Left) {
                    startDragMousePos = _mousePosition;
                    startDragObjectPos = _mouseOnObject->getScreenSpacePosition();

                    // If current object is browser, check for resizing
                    if (to_browser(_mouseOnObject)) {
                        // Resize browser if mouse is over resize button
                        resizeVector = to_browser(_mouseOnObject)->coordIsOnResizeArea(_mousePosition);
                        if (resizeVector != glm::vec2{ 0 }) {
                            to_browser(_mouseOnObject)->saveResizeStartSize();
                            startResizeBrowserSize = to_browser(_mouseOnObject)->getScreenSpaceDimensions();
                            currentlyResizingBrowser = true;
                            return true;
                        }
                    }
                    currentlyDraggingObject = true;

                    return true;
                }
                else if (to_browser(_mouseOnObject) && button == MouseButton::Right) {

                    //startDragMousePos = _mousePosition;
                    //startDragObjectPos = dynamic_cast<ScreenSpaceSkyBrowser*>(_mouseOnObject)->->getScreenSpacePosition();
                    //currentlyDraggingObject = true;
                    return true;
                }
            }
            else if (action == MouseAction::Release) {
                if (currentlyDraggingObject) {
                    currentlyDraggingObject = false;
                    return true;
                }
                if (currentlyResizingBrowser) {
                    currentlyResizingBrowser = false;
                    to_browser(_mouseOnObject)->updateBrowserSize();
                    return true;
                }
            }

            return false;
        }
    );
} 
SkyBrowserModule::~SkyBrowserModule() {
    // Call destructor of all allocated xmls
    xmls.clear();
}

void SkyBrowserModule::internalDeinitialize() {
     

}

void SkyBrowserModule::internalInitialize(const ghoul::Dictionary& dict) {
    
    const Parameters p = codegen::bake<Parameters>(dict);

    // register ScreenSpaceBrowser
    auto fScreenSpaceRenderable = FactoryManager::ref().factory<ScreenSpaceRenderable>();
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");
    fScreenSpaceRenderable->registerClass<ScreenSpaceSkyBrowser>("ScreenSpaceSkyBrowser");

    // register ScreenSpaceTarget
    ghoul_assert(fScreenSpaceRenderable, "ScreenSpaceRenderable factory was not created");
    fScreenSpaceRenderable->registerClass<ScreenSpaceSkyTarget>("ScreenSpaceSkyTarget");

}

glm::vec2 SkyBrowserModule::getMousePositionInScreenSpaceCoords(glm::vec2& mousePos) {
    glm::vec2 size = global::windowDelegate->currentWindowSize();
    // Change origin to middle of the window
    glm::vec2 screenSpacePos = glm::vec2((mousePos - (size / 2.0f)));
    // Ensure the upper right corner is positive on the y axis
    screenSpacePos *= glm::vec2(1.0f, -1.0f);
    // Transform pixel coordinates to screen space coordinates [-1,1][-ratio, ratio]
    screenSpacePos /= (0.5f*size.y);
    return screenSpacePos;
}

void SkyBrowserModule::addRenderable(ScreenSpaceRenderable* object) {
    renderables.push_back(object);
    // Sort on z coordinate, objects closer to camera are in beginning of list
    std::sort(renderables.begin(), renderables.end());
}

ScreenSpaceSkyBrowser* SkyBrowserModule::to_browser(ScreenSpaceRenderable* ptr) {
    return dynamic_cast<ScreenSpaceSkyBrowser*>(ptr);
}
ScreenSpaceSkyTarget* SkyBrowserModule::to_target(ScreenSpaceRenderable* ptr) {
    return dynamic_cast<ScreenSpaceSkyTarget*>(ptr);
}

bool SkyBrowserModule::downloadFile(std::string& url, std::string& fileDestination) {
    // Get the webpage and save to file
    HttpRequest::RequestOptions opt{ 5 };
    SyncHttpFileDownload wtml_root(url, fileDestination, HttpFileDownload::Overwrite::Yes);
    wtml_root.download(opt);
    return wtml_root.hasSucceeded();
}

void SkyBrowserModule::loadWTMLCollectionsFromURL(std::string url, std::string fileName) {
    // Get file
    std::string fileDestination = absPath("${MODULE_SKYBROWSER}/WWTimagedata/") + fileName + ".aspx";
    if (!downloadFile(url, fileDestination)) {
        LINFO("Couldn't download file " + url);
        return;
    }    
    // Parse to XML
    using namespace tinyxml2;
    tinyxml2::XMLDocument* doc = new tinyxml2::XMLDocument();
    doc->LoadFile(fileDestination.c_str());

    XMLElement* root = doc->RootElement();
    XMLElement* element = root->FirstChildElement(std::string("Folder").c_str());
    // If there are no folders, or there are folder but without urls, stop recursion
    if (!element || (element && !element->FindAttribute("Url"))) {

        imageUrls.push_back(url);
        xmls.push_back(doc);
        LINFO("Saving " + url);
       
        return;
    }
    // Iterate through all the folders
    while (element && std::string(element->Value()) == "Folder") {
        // Get all attributes for the <Folder>
        std::string subUrl = element->FindAttribute("Url") ? element->FindAttribute("Url")->Value() : "";
        std::string subName = element->FindAttribute("Name") ? element->FindAttribute("Name")->Value() : "";
        if (subUrl != "" && subName != "") {
            loadWTMLCollectionsFromURL(subUrl, subName);
        }
        element = element->NextSiblingElement();
    }
}

void SkyBrowserModule::loadWTMLCollectionsFromDirectory(std::string directory) {

    for (const auto& entry : std::filesystem::directory_iterator(directory)) {
        tinyxml2::XMLDocument* doc = new tinyxml2::XMLDocument();
        std::cout << entry.path().u8string().c_str() << std::endl;
        if (doc->LoadFile(entry.path().u8string().c_str()) == tinyxml2::XMLError::XML_SUCCESS) {
            xmls.push_back(doc);
        }
    }
}

std::ostream& operator<<(std::ostream& os, const ImageData& img) {
    os << "Name: " << img.name << " Coords: ra: " << img.celestCoords.x << " dec: " << img.celestCoords.y << std::endl;
    os << "Thumbnail: " << img.thumbnailUrl << std::endl;
    os << "Collection: " << img.collection << std::endl << std::endl;
    return os;
}


int SkyBrowserModule::loadAllImagesFromXMLs() {
    for(tinyxml2::XMLDocument* doc : xmls) {
        tinyxml2::XMLElement* root = doc->FirstChildElement();
        std::string collectionName = root->FindAttribute("Name") ? root->FindAttribute("Name")->Value() : "";
        loadImagesFromXML(root, collectionName);
    }

    for (ImageData img : images) {
        std::cout << img;
    }
    return images.size();
}

void SkyBrowserModule::printAllUrls() {
    for (auto it = imageUrls.begin(); it != imageUrls.end(); it++) {
        LINFO(*it);
    }
}

void SkyBrowserModule::loadImagesFromXML(tinyxml2::XMLElement* node, std::string collectionName) {
    // Get direct child of node called "Place"
    using namespace tinyxml2;
    XMLElement* folder = getChildNode(node->FirstChildElement(), "Folder");
    
    // Terminate recursion if no folders
    if (!folder) {
        // When we are at leaf folder
        // Iterate through all the places and load as images
        // Prefer places over Image Sets as they contain same info
        XMLElement* place = getChildNode(node->FirstChildElement(), "Place");
        // No place found - look for images instead
        if (!place) {
            XMLElement* imageSet = getChildNode(node->FirstChildElement(), "ImageSet");
            while (imageSet) {
                loadImageSet(imageSet, collectionName);
                imageSet = imageSet->NextSiblingElement();
            }
        }
        // Place found - look through places
        while (place) {
            loadPlace(place, collectionName);
            place = place->NextSiblingElement();
        }
    }
    else {
        // Open all folders at same level
        while (folder) {
            std::string newCollectionName = collectionName + "/";
            if (folder->FindAttribute("Name")) {
                newCollectionName += std::string(folder->FindAttribute("Name")->Value());
            }
            loadImagesFromXML(folder, newCollectionName);
            folder = folder->NextSiblingElement();
        }
    }
}

int SkyBrowserModule::loadPlace(tinyxml2::XMLElement* place, std::string collectionName) {
    // Only load "Sky" type images
    if (std::string(place->FindAttribute("DataSetType")->Value()) != "Sky")
        return - 1;

    std::string url = "";
    // If the place doesn't have a thumbnail url data attribute,
    // Load the containing image set instead
    if (!place->FindAttribute("Thumbnail")) {
        // Traverse the children and look at all their first child to find ImageSet
        tinyxml2::XMLElement* child = place->FirstChildElement();
        tinyxml2::XMLElement* imageSet = nullptr;
        while (child) {
            imageSet = getChildNode(child, "ImageSet");
            if (imageSet) break;
            child = child->NextSiblingElement();
        }
        // If the place doesn't contain an image, nothing to add
        if (!imageSet) return -1;

        // Collect thumbnail url from ImageSet
        url = getURLFromImageSet(imageSet);
        if (url == "") return -1;
    }
    ImageData image;
    // Get attributes for the image
    image.name = place->FindAttribute("Name") ? place->FindAttribute("Name")->Value() : "";
    image.celestCoords.x = place->FindAttribute("RA") ? std::stof(place->FindAttribute("RA")->Value()) : 0.f;
    image.celestCoords.y = place->FindAttribute("Dec") ? std::stof(place->FindAttribute("Dec")->Value()) : 0.f;
    image.thumbnailUrl = url == "" ? place->FindAttribute("Thumbnail")->Value() : url;
    image.collection = collectionName;

    images.push_back(image);
    // Return index of image in vector
    return images.size();
}
int SkyBrowserModule::loadImageSet(tinyxml2::XMLElement* imageSet, std::string collectionName) {
    std::string type = imageSet->FindAttribute("DataSetType") ? imageSet->FindAttribute("DataSetType")->Value() : "";
    // Only load "Sky" type images
    if (type != "Sky")
        return - 1;

    ImageData image;

    // Get attributes for the image
    image.name = imageSet->FindAttribute("Name") ? imageSet->FindAttribute("Name")->Value() : "";
    image.celestCoords.x = imageSet->FindAttribute("RA") ? std::stof(imageSet->FindAttribute("RA")->Value()) : 0.f;
    image.celestCoords.y = imageSet->FindAttribute("Dec") ? std::stof(imageSet->FindAttribute("Dec")->Value()) : 0.f;
    image.thumbnailUrl = getURLFromImageSet(imageSet);
    image.collection = collectionName;
   
    images.push_back(image);
    // Return index of image in vector
    return images.size();
}

std::string SkyBrowserModule::getURLFromImageSet(tinyxml2::XMLElement* imageSet) {
    // FInd the thumbnail image url
   // The thumbnail is the last node so traverse backwards for speed
    tinyxml2::XMLElement* imageSetChild = imageSet->FirstChildElement("ThumbnailUrl");
    return imageSetChild ? imageSetChild->GetText() ? imageSetChild->GetText() : "" : "";
}

tinyxml2::XMLElement* SkyBrowserModule::getChildNode(tinyxml2::XMLElement* node, std::string name) {
    while (node && std::string(node->Name()) != name) {
        node = node->FirstChildElement();
    }
    return node;
}


/*
std::vector<documentation::Documentation> SkyBrowserModule::documentations() const {
    return {
        ExoplanetsDataPreparationTask::documentation(),
        RenderableOrbitDisc::Documentation()
    };
}
*/
} // namespace openspace
