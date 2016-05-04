return {
    -- PlanetCoordinates module
    {   
        Name = "PlanetCoordinates",
        Parent = "JupiterBarycenter",
        Static = true,
        Renderable = {
            Type = "RenderableSphericalGrid",
            GridType = "GALACTIC",
            GridColor = { 0.2, 0.2, 0.2, 1},
            ParentsRotation = "IAU_JUPITER", 
            GridSegments = 36,
            GridRadius = { 0.72, 8 },
        }, 
        Ephemeris = {
            Type = "Static" -- for now, might change.
        },
    
        --[[
        Ephemeris = {
            Type = "Spice",
            Body = "EARTH",
            Reference = "ECLIPJ2000",
            Observer = "EARTH BARYCENTER",
            Kernels = {
                "kernels/earth.bsp"
            }
        },    
        --]]
        GuiName = "/Grid/Galactic"
    }
}