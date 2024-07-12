openspace.documentation = {
    {
        Name = "markInterestingNodes",
        Arguments = {{ "sceneGraphNodes", "String[]" }},
        Documentation = "This function marks the scene graph nodes identified by name " ..
        "as interesting, which will provide shortcut access to focus buttons and " ..
        "featured properties"
    },
    {
        Name = "markInterestingTimes",
        Arguments = {{ "times", "Table[]" }},
        Documentation = "This function marks interesting times for the current scene, " ..
        "which will create shortcuts for a quick access"
    },
    {
        Name = "removeInterestingNodes",
        Arguments = {{ "sceneGraphNodes", "String[]" }},
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
        Arguments = {{ "oldKey", "String" }, { "newKey", "String" }},
        Documentation = "Rebinds all scripts from the old key (first argument) to the " ..
        "new key (second argument)"
    },
    {
        Name = "appendToListProperty",
        Arguments = {{ "identifier","String" }, { "value", "any" }},
        Documentation = "Add a value to the list property with the given identifier. " ..
        "The value can be any type, as long as it is the correct type for the given " ..
        "property. Note that a number will be converted to a string automatically."
    },
    {
        Name = "addToPropertyValue",
        Arguments = {{ "identifier", "String" } , { "value", "String | Number" }},
        Documentation = "Add a value to the property with the given identifier. " ..
        "Works on both numerical and string properties, where adding to a string " ..
        "property means appending the given string value to the existing string value."
    },
    {
        Name = "invertBooleanProperty",
        Arguments = {{ "identifier", "String" }},
        Documentation = "Inverts the value of a boolean property with the given "..
        "identifier"
    },
    {
        Name = "fadeIn",
        Arguments = {
            { "identifier", "String" },
            { "fadeTime", "Number?" },
            { "endScript", "String?" }
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
            { "identifier", "String" },
            { "fadeTime", "Number?" },
            { "endScript", "String?" }
        },
        Documentation = "Fades out the node(s) with the given identifier over the given " ..
        "time in seconds. The identifier can contain a tag and/or a wildcard to target " ..
        "several nodes. If the fade time is not provided then the " ..
        "'OpenSpaceEngine.FadeDuration' property will be used instead. If the third " ..
        "argument (endScript) is provided then that script will be run after the fade " ..
        "is finished."
    },
    {
        Name = "toggleFade",
        Arguments = {
            { "identifier", "String" },
            { "fadeTime", "Number?" },
            { "endScript", "String?" }
        },
        Documentation = [[Toggles the fade state of the node(s) with the given identifier over the given
          time in seconds. The identifier can contain a tag and/or a wildcard to target
          several nodes. If the fade time is not provided then the
          "OpenSpaceEngine.FadeDuration" property will be used instead. If the third
          argument (endScript) is provided then that script will be run after the fade
          is finished.]]
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
    local list = openspace.propertyValue(propertyIdentifier)
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
    local value = openspace.propertyValue(propertyIdentifier)
    if type(value) == 'string' then
        value = value .. valueToAdd;
    else
        value = value + valueToAdd;
    end
    openspace.setPropertyValueSingle(propertyIdentifier, value)
end

openspace.invertBooleanProperty = function(propertyIdentifier)
    local value = openspace.propertyValue(propertyIdentifier)
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
    endScript = endScript or ""
    fadeTime = fadeTime or openspace.propertyValue("OpenSpaceEngine.FadeDuration")

    local enabledProperty = identifier .. ".Enabled"
    local fadeProperty = identifier .. ".Fade"

    -- Assume that we need to enable the node(s) as we fade it/them in
    local isEnabled = false

    -- Check if the identifier is a single node or a regex for several nodes
    local hasTag, _ = identifier:find('{')
    local hasWild, _ = identifier:find('*')

    if hasTag ~= nil or hasWild ~= nil then
        -- Regex, several nodes
        local enabledPropertyList = openspace.property(enabledProperty)
        if next(enabledPropertyList) == nil then
            -- List is empty, no matches found
            openspace.printError(
                "Error when calling script 'openspace.fadeIn': " ..
                "Could not find any property matching '" .. enabledProperty .. "'"
            )
            return
        end

        local fadePropertyList = openspace.property(fadeProperty)
        if next(fadePropertyList) == nil then
            -- List is empty, no matches found
            openspace.printError(
                "Error when calling script 'openspace.fadeIn': " ..
                "Could not find any property matching '" .. fadeProperty .. "'"
            )
            return
        end
    else
        -- Literal, single node
        local hasEnabled = openspace.hasProperty(enabledProperty)
        local hasFade = openspace.hasProperty(fadeProperty)

        if not hasEnabled then
            openspace.printError(
                "Error when calling script 'openspace.fadeIn': " ..
                "Could not find property '" .. enabledProperty .. "'"
            )
            return
        elseif not hasFade then
            openspace.printError(
                "Error when calling script 'openspace.fadeIn': " ..
                "Could not find property '" .. fadeProperty .. "'"
            )
            return
        else
            isEnabled = openspace.propertyValue(enabledProperty)
        end
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
    endScript = endScript or ""
    fadeTime = fadeTime or openspace.propertyValue("OpenSpaceEngine.FadeDuration")

    local enabledProperty = identifier .. ".Enabled"
    local fadeProperty = identifier .. ".Fade"

    -- Assume that the node(s) are enabled and that we need to fade it/them out
    local isEnabled = true

    -- Is the identifier a single node or a regex for several nodes
    local hasTag, _ = identifier:find('{')
    local hasWild, _ = identifier:find('*')

    if hasTag ~= nil or hasWild ~= nil then
        -- Regex, several nodes
        local enabledPropertyList = openspace.property(enabledProperty)
        if next(enabledPropertyList) == nil then
            -- List is empty, no matches found
            openspace.printError(
                "Error when calling script 'openspace.fadeIn': " ..
                "Could not find any property matching '" .. enabledProperty .. "'"
            )
            return
        end

        local fadePropertyList = openspace.property(fadeProperty)
        if next(fadePropertyList) == nil then
            -- List is empty, no matches found
            openspace.printError(
                "Error when calling script 'openspace.fadeIn': " ..
                "Could not find any property matching '" .. fadeProperty .. "'"
            )
            return
        end
    else
        -- Literal, single node
        local hasEnabled = openspace.hasProperty(enabledProperty)
        local hasFade = openspace.hasProperty(fadeProperty)

        if not hasEnabled then
            openspace.printError(
                "Error when calling script 'openspace.fadeIn': " ..
                "Could not find property '" .. enabledProperty .. "'"
            )
            return
        elseif not hasFade then
            openspace.printError(
                "Error when calling script 'openspace.fadeIn': " ..
                "Could not find property '" .. fadeProperty .. "'"
            )
            return
        else
            isEnabled = openspace.propertyValue(enabledProperty)
        end
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

openspace.toggleFade = function(renderable, fadeTime, endScript)
  if (fadeTime == nil) then
    fadeTime = openspace.propertyValue("OpenSpaceEngine.FadeDuration")
  end
  local enabled = openspace.propertyValue(renderable .. ".Enabled")
  local fadeState = openspace.propertyValue(renderable .. ".Fade")
  if (enabled) then
    if (fadeState < 0.5) then
      openspace.fadeIn(renderable, fadeTime-(fadeTime*fadeState), endScript)
    else
      openspace.fadeOut(renderable, fadeTime*fadeState, endScript)
    end
  else
    openspace.fadeIn(renderable, fadeTime, endScript)
  end
end
