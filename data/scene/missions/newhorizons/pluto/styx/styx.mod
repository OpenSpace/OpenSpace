local styx_radius = 0.75E4

NewHorizonsKernels = {
    "${SPICE}/new_horizons/spk/NavSE_plu047_od122.bsp"
}

return {
    -- Styx module
    {   
        Name = "Styx",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_PLUTO",
            Body = "STYX",
            Radius = styx_radius,
            Geometry = {
                Type = "SimpleSphere",
                Radius = styx_radius,
                Segments = 100
            },
            ColorTexture = "textures/gray.jpg",
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "STYX",
                Observer = "PLUTO BARYCENTER",
                Kernels = NewHorizonsKernels
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_PLUTO",
                DestinationFrame = "ECLIPJ2000"
            },
        },
    },
    {
        Name = "StyxText",
        Parent = "Styx",
        Renderable = {
            Type = "RenderablePlane",
            Size = 10^6.3,
            Origin = "Center",
            Billboard = true,
            Texture = "textures/Styx-Text.png",
            BlendMode = "Additive"
        },
        Transform = {
            Translation = {
                Type = "StaticTranslation",
                Position = {1000000, 0, 1000000}
            },
        },
    },
    -- StyxTrail module
    {   
        Name = "StyxTrail",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "STYX",
                Observer = "PLUTO BARYCENTER",
            },
            Color = {0.00, 0.62, 1.00},
            Period = 20.16155,
            Resolution = 1000
        },
    }
}
