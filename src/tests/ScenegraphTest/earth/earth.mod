{
    -- Earth barycenter module
    {
        Name = "EarthBarycenter",
        Parent = "SolarSystem",
        Static = "true",
        Position = {
            Type = "Kepler",
            Inclination = 0.00041,
            AscendingNode = 349.2,
            Perihelion = 102.8517,
            SemiMajorAxis = 1.00002,
            DailyMotion = 0.9855796,     
            Eccentricity = 0.0166967,
            MeanLongitude = 328.40353
        }
    },

    -- dummy earth module
    --[[
    {   
        Name = "DummyEarth",
        Parent = "Root",
        Renderable = {
            Type = "RenderablePlanet",
            Geometry = {
                Type = "SimpleSphere",
                --Radius = { 1.0, 1 } -- not sure if correct; supposed 6371km in meters in pss
                Radius = { 1.0, 1} -- not sure if correct; supposed 6371km in meters in pss
            },
            Textures = {
                Type = "simple",
                Color = "textures/earth_nasa_lowres.png",
                Depth = "textures/earth_depth.png"
            },
        },
        Position = {
            Type = "Static",
            Position = { 0, 0, -1, 1}
        }
    },
    ]]--

    -- Earth module
    {   
        Name = "Earth",
        Parent = "EarthBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Geometry = {
                Type = "SimpleSphere",
                --Radius = { 6.371, 6 },
                Radius = { 1.0, 1},
                Segments = 10
            },
            Textures = {
                Type = "simple",
                Color = "textures/earth_nasa_lowres.png",
                Depth = "textures/earth_depth.png"
            },
            Atmosphere = {
                Type = "Nishita", -- for example, values missing etc etc
                MieFactor = 1.0,
                MieColor = {1.0, 1.0, 1.0}
            }
        },
        Position = {
            Type = "Spice",
            Body = "EARTH",
            Reference = "ECLIPJ2000",
            Observer = "EARTH BARYCENTER",
            Kernels = {
                "kernels/earth.bsp"
            }
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_EARTH",
            Reference = "ECLIPJ2000"
        },
        GuiName = "/Solar/Planets/Earth"
    }
}