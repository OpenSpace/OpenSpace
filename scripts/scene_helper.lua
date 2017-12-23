openspace.documentation = {
    {
        Name = "mark_interating_nodes",
        Arguments = "List of nodes",
        Documentation = "This function marks the scene graph nodes identified by name " ..
        "as interesting, which will provide shortcut access to focus buttons and " .. 
        "featured properties."
    },
    {
        Name = "remove_interesting_nodes",
        Arguments = "List of nodes",
        Documentation = "This function removes unmarks the scene graph nodes " ..
        "identified by name as interesting, thus removing the shortcuts from the " ..
        "features properties list."
    },
    {
        Name = "set_default_gui_sorting",
        Arguments = "",
        Documentation = "This function sets the default GUI sorting for the space " ..
        "environment to increasing size, from solar system, through Milky Way, " ..
        "Universe and finishing with other elements"
    },
    {
        Name = "set_default_dashboard",
        Arguments = "",
        Documentation = "This function sets the default values for the dashboard " ..
        "consisting of 'DashboardItemDate', 'DashboardItemSimulationIncrement', " ..
        "'DashboardItemDistance', 'DashboardItemFramerate', and " ..
        "'DashboardItemParallelConnection'."
    }
}

openspace.mark_interesting_nodes = function(nodes)
    for _, n in pairs(nodes) do
        if openspace.hasSceneGraphNode(n) then
            openspace.addTag(n, "GUI.Interesting")
        end
    end
end

openspace.remove_interesting_nodes = function(nodes)
    for _, n in pairs(nodes) do
        if openspace.hasSceneGraphNode(n) then
            openspace.removeTag(n, "GUI.Interesting")
        end
    end
end

openspace.set_default_dashboard = function()
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

openspace.set_default_gui_sorting = function()
    openspace.setPropertyValueSingle(
        'Global Properties.ImGUI.Main.Properties.Ordering',
        {
            "Solar System", "Milky Way", "Universe", "Other"
        }
    )
end
