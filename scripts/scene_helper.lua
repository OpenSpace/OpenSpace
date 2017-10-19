mark_interesting_nodes = function(nodes)
    for _, n in pairs(nodes) do
        openspace.addTag(n, "GUI.Interesting")
    end
end
