return {
    -- gridGalactic module
    {   
        Name = "gridGalactic",
        Parent = "SolarSystem",
        Renderable = {
            Type = "RenderableSphericalGrid",
            GridType = "GALACTIC",
            GridColor = { 0.0, 0.4, 0.4, 1},
            GridMatrix = { 1.0,  0.0, 0.0, 0.0,
                           0.0,  1.0, 0.0, 0.0,
                           0.0,  0.0, 1.0, 0.0,
                           0.0,  0.0, 0.0, 1.0 },
            GridSegments = 36,
        }
    }
}