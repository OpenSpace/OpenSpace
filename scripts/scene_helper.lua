mark_interesting_nodes = function(nodes)
    for _, n in pairs(nodes) do
        if openspace.hasSceneGraphNode(n) then
            openspace.addTag(n, "GUI.Interesting")
        end
    end
end

set_default_gui_sorting = function()
    openspace.setPropertyValueSingle(
        'Global Properties.ImGUI.Main.Properties.Ordering',
        {
            "Solar System", "Milky Way", "Universe", "Other"
        }
    )
end