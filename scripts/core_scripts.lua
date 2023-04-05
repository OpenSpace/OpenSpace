openspace.documentation = {
    {
        Name = "markInterestingNodes",
        Arguments = { sceneGraphNodes = "String[]" },
        Documentation = "This function marks the scene graph nodes identified by name " ..
        "as interesting, which will provide shortcut access to focus buttons and " ..
        "featured properties"
    },
    {
        Name = "markInterestingTimes",
        Arguments = { times = "Table[]" },
        Documentation = "This function marks interesting times for the current scene, " ..
        "which will create shortcuts for a quick access"
    },
    {
        Name = "removeInterestingNodes",
        Arguments = { sceneGraphNodes = "String[]" },
        Documentation = "This function removes unmarks the scene graph nodes " ..
        "identified by name as interesting, thus removing the shortcuts from the " ..
        "features properties list"
    },
    {
        Name = "setDefaultGuiSorting",
        Arguments = {},
        Documentation = "This function sets the default GUI sorting for the space " ..
        "environment to increasing size, from solar system, through Milky Way, " ..
        "Universe and finishing with other elements"
    },
    {
        Name = "setDefaultDashboard",
        Arguments = {},
        Documentation = "This function sets the default values for the dashboard " ..
        "consisting of 'DashboardItemDate', 'DashboardItemSimulationIncrement', " ..
        "'DashboardItemDistance', 'DashboardItemFramerate', and " ..
        "'DashboardItemParallelConnection'"
    },
    {
        Name = "rebindKey",
        Arguments = { oldKey = "String", newKey = "String" },
        Documentation = "Rebinds all scripts from the old key (first argument) to the " ..
        "new key (second argument)"
    },
    {
        Name = "appendToListProperty",
        Arguments = { identifier = "String", value = "any" },
        Documentation = "Add a value to the list property with the given identifier. " ..
        "The value can be any type, as long as it is the correct type for the given " ..
        "property. Note that a number will be converted to a string automatically."
    },
    {
        Name = "addToPropertyValue",
        Arguments = { identifier = "String", value = "String | Number" },
        Documentation = "Add a value to the property with the given identifier. " ..
        "Works on both numerical and string properties, where adding to a string " ..
        "property means appending the given string value to the existing string value."
    },
    {
        Name = "invertBooleanProperty",
        Arguments = { identifier = "String" },
        Documentation = "Inverts the value of a boolean property with the given "..
        "identifier"
    },
    {
        Name = "fadeIn",
        Arguments = {
            identifier = "String",
            fadeTime = "Number?",
            endScript = "String?"
        },
        Documentation = "Fades in the node(s) with the given identifier over the given " ..
        "time in seconds. The identifier can contain a tag and/or a wildcard to target " ..
        "several nodes. If the fade time is not provided then the " ..
        "'OpenSpaceEngine.FadeDuration' property will be used instead. If the third " ..
        "argument (endScript) is provided then that script will be run after the fade " ..
        "is finished."
    },
    {
        Name = "fadeOut",
        Arguments = {
            identifier = "String",
            fadeTime = "Number?",
            endScript = "String?"
        },
        Documentation = "Fades out the node(s) with the given identifier over the given " ..
        "time in seconds. The identifier can contain a tag and/or a wildcard to target " ..
        "several nodes. If the fade time is not provided then the " ..
        "'OpenSpaceEngine.FadeDuration' property will be used instead. If the third " ..
        "argument (endScript) is provided then that script will be run after the fade " ..
        "is finished."
    }
}

openspace.markInterestingNodes = function(nodes)
    for _, n in pairs(nodes) do
        if openspace.hasSceneGraphNode(n) then
            openspace.addTag(n, "GUI.Interesting")
        end
    end
end

openspace.markInterestingTimes = function(times)
    for _, n in pairs(times) do
        local name = n["Name"] or n[1]
        local time = n["Time"] or n[2]
        openspace.addInterestingTime(name, time)
    end
end

openspace.removeInterestingNodes = function(nodes)
    for _, n in pairs(nodes) do
        if openspace.hasSceneGraphNode(n) then
            openspace.removeTag(n, "GUI.Interesting")
        end
    end
end

openspace.setDefaultGuiSorting = function()
    openspace.setPropertyValueSingle(
        'Modules.ImGUI.Scene.Ordering',
        {
            "Solar System", "Milky Way", "Universe", "Other"
        }
    )
end

openspace.rebindKey = function(oldKey, newKey)
    local t = openspace.keyBindings(oldKey)
    openspace.clearKey(oldKey)
    for _, v in pairs(t) do
        openspace.bindKey(newKey, v)
    end
end

openspace.appendToListProperty = function(propertyIdentifier, newItem)
    local list = openspace.getPropertyValue(propertyIdentifier)
    if type(list) ~= 'table' then
        openspace.printError(
            "Error when calling script 'openspace.appendToListProperty': " ..
            "Could not append to non-list property '" .. propertyIdentifier .. "'"
        )
        return;
    end
    table.insert(list, newItem)
    openspace.setPropertyValueSingle(propertyIdentifier, list)
end

openspace.addToPropertyValue = function(propertyIdentifier, valueToAdd)
    local value = openspace.getPropertyValue(propertyIdentifier)
    if type(value) == 'string' then
        value = value .. valueToAdd;
    else
        value = value + valueToAdd;
    end
    openspace.setPropertyValueSingle(propertyIdentifier, value)
end

openspace.invertBooleanProperty = function(propertyIdentifier)
    local value = openspace.getPropertyValue(propertyIdentifier)
    if type(value) ~= 'boolean' then
        openspace.printError(
            "Error when calling script 'openspace.invertBooleanProperty': " ..
            "Could not invert non-boolean property '" .. propertyIdentifier .. "'"
        )
        return;
    end
    openspace.setPropertyValueSingle(propertyIdentifier, not value)
end

openspace.fadeIn = function(identifier, fadeTime, endScript)
    -- Set default values for optional arguments
    if endScript == nil then
        endScript = ""
    end

    if fadeTime == nil then
        fadeTime = openspace.getPropertyValue("OpenSpaceEngine.FadeDuration")
    end

    local nodeIdentifier = ""
    local enabledProperty = ""
    local fadeProperty = ""

    -- Assume that we need to enable the node(s) as we fade it/them in
    local isEnabled = false

    -- Is the identifier a single node or a regex for several nodes
    local hasTag, _ = string.find(identifier, "{")
    local hasWild, _ = string.find(identifier, "*")

    if hasTag ~= nil or hasWild ~= nil then
        -- Regex, several nodes
        nodeIdentifier = identifier
        enabledProperty = nodeIdentifier .. ".Renderable.Enabled"
        fadeProperty = nodeIdentifier .. ".Renderable.Fade"
    else
        -- Literal, single node
        -- Check if node exists
        local exists = openspace.hasSceneGraphNode(identifier)
        if not exists then
            openspace.printError(
                "Error when calling script 'openspace.fadeIn': " ..
                "Could not find node '" .. identifier .. "'"
            )
            return
        end

        nodeIdentifier = "Scene." .. identifier
        enabledProperty = nodeIdentifier .. ".Renderable.Enabled"
        fadeProperty = nodeIdentifier .. ".Renderable.Fade"

        -- Check if node has a Renderable (Renderable always has an Enabled property)
        exists = openspace.hasProperty(enabledProperty)
        if not exists then
            openspace.printError(
                "Error when calling script 'openspace.fadeIn': " ..
                "Could not find Renderable for node '" .. identifier .. "'"
            )
            return
        end

        isEnabled = openspace.getPropertyValue(enabledProperty)
    end

    -- If node is already enabled we only have to fade it
    if not isEnabled then
        openspace.setPropertyValue(fadeProperty, 0.0)
        openspace.setPropertyValue(enabledProperty, true)
    end

    openspace.setPropertyValue(fadeProperty, 1.0, fadeTime, "Linear", endScript)
end

openspace.fadeOut = function(identifier, fadeTime, endScript)
    -- Set default values for optional arguments
    if endScript == nil then
        endScript = ""
    end

    if fadeTime == nil then
        fadeTime = openspace.getPropertyValue("OpenSpaceEngine.FadeDuration")
    end

    local nodeIdentifier = ""
    local enabledProperty = ""
    local fadeProperty = ""

    -- Assume that the node(s) are enabled and that we need to fade it/them out
    local isEnabled = true

    -- Is the identifier a single node or a regex for several nodes
    local hasTag, _ = string.find(identifier, "{")
    local hasWild, _ = string.find(identifier, "*")

    if hasTag ~= nil or hasWild ~= nil then
        -- Regex, several nodes
        nodeIdentifier = identifier
        enabledProperty = nodeIdentifier .. ".Renderable.Enabled"
        fadeProperty = nodeIdentifier .. ".Renderable.Fade"
    else
        -- Literal, single node
        -- Check if node exists
        local exists = openspace.hasSceneGraphNode(identifier)
        if not exists then
            openspace.printError(
                "Error when calling script 'openspace.fadeOut': " ..
                "Could not find node '" .. identifier .. "'"
            )
            return;
        end

        nodeIdentifier = "Scene." .. identifier
        enabledProperty = nodeIdentifier .. ".Renderable.Enabled"
        fadeProperty = nodeIdentifier .. ".Renderable.Fade"

        -- Check if node has a Renderable (Renderable always has an Enabled property)
        exists = openspace.hasProperty(enabledProperty)
        if not exists then
            openspace.printError(
                "Error when calling script 'openspace.fadeOut': " ..
                "Could not find Renderable for node '" .. identifier .. "'"
            )
        end

        isEnabled = openspace.getPropertyValue(enabledProperty)
    end

    -- If node is already disabled we don't have to do anything
    if isEnabled then
        openspace.setPropertyValue(
            fadeProperty,
            0.0,
            fadeTime,
            "Linear",
            endScript
        )
    end
end
