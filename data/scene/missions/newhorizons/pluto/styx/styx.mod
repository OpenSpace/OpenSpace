if UseAccurateNewHorizonsKernels then
    NewHorizonsKernels = {
        "${SPICE}/nh_kernels/spk/NavSE_plu047_od122.bsp"
    }
else
    NewHorizonsKernels = {
        "${SPICE}/NewHorizonsKernels/nh_p4p5_revised.bsp"
    }
end

return {
    -- Styx module
    {   
        Name = "Styx",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_PLUTO",
            Body = "STYX",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 0.75 , 4 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = "textures/gray.jpg",
            }
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "STYX",
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
            Size = {1.0, 6.3},
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
                Body = "STYX",
                Observer = "PLUTO BARYCENTER",
            },
            Color = {0.00, 0.62, 1.00},
            Period = 20.16155,
            Resolution = 1000
        },
    }
}
