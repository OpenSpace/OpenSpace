openspace.documentation = {
    {
        Name = "markInterestingNodes",
        Arguments = { sceneGraphNode = "[ String ]" },
        Documentation = "This function marks the scene graph nodes identified by name " ..
        "as interesting, which will provide shortcut access to focus buttons and " .. 
        "featured properties"
    },
    {
        Name = "markInterestingTimes",
        Arguments = { times = "[ Table ]" },
        Documentation = "This function marks interesting times for the current scene, " ..
        "which will create shortcuts for a quick access"
    },
    {
        Name = "removeInterestingNodes",
        Arguments = { sceneGraphNode = "[ String ]" },
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
