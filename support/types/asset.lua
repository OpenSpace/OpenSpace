---@meta

-- Definitions for the `asset` table that is available in every asset file. This is kept
-- in sync by hand with AssetManager::setUpAssetLuaTable in src/scene/assetmanager.cpp

---@class AssetMeta
---@field Name string? The user-facing name of the asset
---@field Version string? A version number for this asset, SemVer is recommended
---@field Description string? A user-facing description of the asset
---@field Author string? The name of the author of the asset file
---@field URL string? A representative URL for this asset
---@field License string? The license under which the asset is released
---@field Identifiers string[]? All identifiers that are exposed by this asset

---@class Asset
---@field meta AssetMeta Meta information about the asset
---@field directory string The directory that contains this asset file
---@field filePath string The full path to this asset file
---@field enabled boolean Whether this asset was explicitly enabled by its parent
asset = {}

--- Returns the path to a resource. If called with a table, the resource is synchronized
--- and the path to the synchronized directory is returned. If called with a string, the
--- path is relative to the directory of this asset. Without argument, the asset's
--- directory is returned.
---@param resource? string|table
---@return string
function asset.resource(resource) end

---@deprecated Use `asset.resource` instead
---@param path? string
---@return string
function asset.localResource(path) end

---@deprecated Use `asset.resource` instead
---@param resource table
---@return string
function asset.syncedResource(resource) end

--- Loads another asset and returns the table of values it exported.
---@param path string
---@param explicitEnable? boolean
---@return table
function asset.require(path, explicitEnable) end

--- Returns whether an asset file exists at the provided path.
---@param path string
---@return boolean
function asset.exists(path) end

--- Exports a value to assets that require this asset. If only a table with an
--- `Identifier` key is provided, that identifier is used as the key.
---@overload fun(value: table)
---@param key string
---@param value any
function asset.export(key, value) end

--- Registers a function that is called when the asset is initialized.
---@param initializationFunction fun()
function asset.onInitialize(initializationFunction) end

--- Registers a function that is called when the asset is deinitialized.
---@param deinitializationFunction fun()
function asset.onDeinitialize(deinitializationFunction) end

---@type AssetMeta
asset.meta = {}
