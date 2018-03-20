openspace.documentation = {
    {
        Name = "markInteratingNodes",
        Arguments = "List of nodes",
        Documentation = "This function marks the scene graph nodes identified by name " ..
        "as interesting, which will provide shortcut access to focus buttons and " .. 
        "featured properties."
    },
    {
        Name = "removeInterestingNodes",
        Arguments = "List of nodes",
        Documentation = "This function removes unmarks the scene graph nodes " ..
        "identified by name as interesting, thus removing the shortcuts from the " ..
        "features properties list."
    },
    {
        Name = "setDefaultGuiSorting",
        Arguments = "",
        Documentation = "This function sets the default GUI sorting for the space " ..
        "environment to increasing size, from solar system, through Milky Way, " ..
        "Universe and finishing with other elements"
    },
    {
        Name = "setDefaultDashboard",
        Arguments = "",
        Documentation = "This function sets the default values for the dashboard " ..
        "consisting of 'DashboardItemDate', 'DashboardItemSimulationIncrement', " ..
        "'DashboardItemDistance', 'DashboardItemFramerate', and " ..
        "'DashboardItemParallelConnection'."
    },
    {
        Name = "rebindKey",
        Arguments = "string, string",
        Documentation = "Rebinds all scripts from the old key (first argument) to the " ..
        "new key (second argument)."
    }
}

openspace.markInterestingNodes = function(tag, nodes)
    for _, n in pairs(nodes) do
        if openspace.hasSceneGraphNode(n) then
            openspace.addTag(n, tag)
        end
    end
end

openspace.removeInterestingNodes = function(tag, nodes)
    for _, n in pairs(nodes) do
        if openspace.hasSceneGraphNode(n) then
            openspace.removeTag(n, tag)
        end
    end
end

openspace.setDefaultDashboard = function()
    openspace.dashboard.addDashboardItem({
        Type = "DashboardItemDate"
    })

    openspace.dashboard.addDashboardItem({
        Type = "DashboardItemSimulationIncrement"
    })

    openspace.dashboard.addDashboardItem({
        Type = "DashboardItemDistance"
    })

    openspace.dashboard.addDashboardItem({
        Type = "DashboardItemFramerate"
    })

    openspace.dashboard.addDashboardItem({
        Type = "DashboardItemParallelConnection"
    })
end

openspace.setDefaultGuiSorting = function()
    openspace.setPropertyValueSingle(
        'Modules.ImGUI.Main.SceneProperties.Ordering',
        {
            "Solar System", "Milky Way", "Universe", "Other"
        }
    )
end

openspace.rebindKey = function(oldKey, newKey)
    local t = openspace.getKeyBinding(oldKey)
    openspace.clearKey(oldKey)
    for _, v in pairs(t) do
        if v["Remote"] then
            openspace.bindKey(newKey, v["Command"])
        else
            openspace.bindKeyLocal(newKey, v["Command"])
        end
    end
end
