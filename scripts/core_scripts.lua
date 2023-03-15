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
