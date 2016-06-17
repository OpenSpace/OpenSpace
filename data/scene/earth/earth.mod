return {
    -- Earth barycenter module
    {
        Name = "EarthBarycenter",
        Parent = "SolarSystemBarycenter",
        Static = true,
        Ephemeris = {
            Type = "Spice",
            Body = "EARTH BARYCENTER",
            Reference = "ECLIPJ2000",
            Observer = "SUN",
            Kernels = {
                "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            }
        },
    },
    -- Earth module
    {   
        Name = "Earth",
        Parent = "EarthBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_EARTH",
			Body = "EARTH",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 6.371, 6 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/earth_bluemarble.jpg",
				Night = "textures/earth_night.jpg",
                --Height = "textures/earth_bluemarble_height.jpg",                
                -- Depth = "textures/earth_depth.png",
                Reflectance = "textures/earth_reflectance.png",
                Clouds = "textures/earth_clouds.jpg"
            }
        },
        
        GuiName = "/Solar/Planets/Earth"
    },
    -- EarthTrail module
    {   
        Name = "EarthTrail",
        Parent = "EarthBarycenter",
        Renderable = {
            Type = "RenderableTrail",
            Body = "EARTH",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = { 0.5, 0.8, 1.0},
            TropicalOrbitPeriod = 365.242,
            EarthOrbitRatio = 1,
            DayLength = 24
        },
        GuiName = "/Solar/EarthTrail"
    },
    {
        Name = "EarthMarker",
        Parent = "Earth",
        Renderable = {
            Type = "RenderablePlane",
            Size = {3.0, 11.0},
            Origin = "Center",
            Billboard = true,
            Texture = "textures/marker.png"
        },
		Ephemeris = {
            Type = "Static",
            Position = {0, 0, 0, 5}
        }
    }
 -- Plane
    -- {   
    --     Name = "EarthPlane",
    --     Parent = "Earth",
    --     Renderable = {
    --         Type = "RenderablePlane",
    --         Billboard = true,
    --         Size = { 6.371, 6 },
    --         Texture = "textures/graph.jpg",
    --         Atmosphere = {
    --             Type = "Nishita", -- for example, values missing etc etc
    --             MieFactor = 1.0,
    --             MieColor = {1.0, 1.0, 1.0}
    --         }
    --     },
    --     Ephemeris = {
    --         Type = "Static",
    --         Position = { 6.371*2, 0, 0, 6},
    --     },
    -- }
}
