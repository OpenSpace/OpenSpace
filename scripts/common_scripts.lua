openspace.documentation = {
    {
        Name = "rebindKey",
        Arguments = "string, string",
        Documentation = "Rebinds all scripts from the old key (first argument) to the " ..
        "new key (second argument)."
    }
}

openspace.rebindKey = function(old_key, new_key)
    local t = openspace.getKeyBinding(old_key)
    openspace.clearKey(old_key)
    for _, v in pairs(t) do
        if v["Remote"] then
            openspace.bindKey(new_key, v["Command"])
        else
            openspace.bindKeyLocal(new_key, v["Command"])
        end
    end
end
