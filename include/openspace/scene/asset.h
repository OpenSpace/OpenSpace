/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_CORE___ASSET___H__
#define __OPENSPACE_CORE___ASSET___H__

#include <openspace/properties/property.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalarproperty.h>

#include <ghoul/filesystem/filesystem.h>
#include <string>
#include <vector>

namespace openspace {

class AssetLoader;

class Asset : public properties::PropertyOwner {
public:
    class Optional : public properties::PropertyOwner {
    public:
        Optional(Asset* asset, Asset* owner, bool enabled = false);
    private:
        properties::BoolProperty _enabled;
        Asset* _asset;
        Asset* _owner;
    };

    enum class ReadyState : unsigned int {
        Loaded,
        Synchronizing,
        Synchronized,
        Initialized
    };

    /**
     * Root asset constructor
     */
    Asset(AssetLoader* loader, ghoul::filesystem::Directory directory);

    /**
    * Dependency or Optional constructor
    */
    Asset(AssetLoader* loader, ghoul::filesystem::Directory baseDirectory, std::string assetPath);

    std::string id() const;
    std::string assetFilePath() const;
    std::string assetName() const;
    std::string assetDirectory() const;
    AssetLoader* loader() const;
    std::string syncDirectory() const;
    ReadyState readyState() const;
    bool isInitReady() const;
    void initialize();
    void deinitialize();

    bool hasDependency(const Asset* asset) const;
    void addDependency(Asset* asset);
    void removeDependency(Asset* asset);
    void removeDependency(const std::string& assetId);

    bool hasDependants() const;
    bool hasInitializedDependants() const;

    bool hasOptional(Asset* asset) const;
    bool optionalIsEnabled(Asset* asset) const;
    void setOptionalEnabled(Asset* asset, bool enabled);
    void addOptional(Asset* asset, bool enabled);
    void removeOptional(Asset* asset);

    void dependantDidInitialize(Asset* dependant);
    void dependantWillDeinitialize(Asset* dependant);

    void optionalDidInitialize(Asset* optional);
    void optionalWillDeinitialize(Asset* optional);

    bool shouldSynchronize();
    bool shouldInitialize();

    std::string resolveLocalResource(std::string resourceName);
    std::string resolveSyncedResource(std::string resourceName);

    static std::string generateAssetId(std::string directory, std::string name);
private:


    ReadyState _readyState;
    AssetLoader* _loader;

    // Base name of .asset file
    std::string _assetName;

    // Asbolute path to directory with the .asset file
    std::string _assetDirectory;

    // Asset id
    std::string _id;

    // Asset dependencies
    std::vector<Asset*> _dependencies;

    // Asset dependants
    std::vector<Asset*> _dependants;

    // Optional sub-assets
    std::vector<Optional> _optionalAssets;

    // Assets that refers to this asset as an optional
    std::vector<Asset*> _optionalOwners;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___ASSET__H__
