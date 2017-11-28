mark_interesting_nodes = function(nodes)
    for _, n in pairs(nodes) do
        if openspace.hasSceneGraphNode(n) then
            openspace.addTag(n, "GUI.Interesting")
        end
    end
end

set_default_dashboard = function()
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
