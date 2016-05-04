return {
    -- gridGalactic module
    {   
        Name = "gridGalactic",
        Parent = "SolarSystem",
        Static = true,
        Renderable = {
            Type = "RenderableSphericalGrid",
            GridType = "GALACTIC",
            GridColor = { 0.0, 0.4, 0.4, 1},
            GridMatrix = { 1.0,  0.0, 0.0, 0.0, 
                           0.0,  1.0, 0.0, 0.0, 
                           0.0,  0.0, 1.0, 0.0, 
                           0.0,  0.0, 0.0, 1.0 },
            GridSegments = 36,
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