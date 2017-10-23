return {
    -- Earth barycenter module
    {
        Name = "Lua Transformation",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Rotation = {
                Type = "LuaRotation",
                Script = "rotation.lua"
            },
            Scale = {
                Type = "LuaScale",
                Script = "scale.lua"
            },
            Translation = {
                Type = "LuaTranslation",
                Script = "translate.lua"
            }
        }
    }
}
