local kerberos_radius = 0.1E5

if UseAccurateNewHorizonsKernels then
    NewHorizonsKernels = {
        "${SPICE}/nh_20170126/spk/NavSE_plu047_od122.bsp"
    }
else
    NewHorizonsKernels = {
        "${SPICE}/NewHorizonsKernels/nh_p4p5_revised.bsp"
    }
end

return {
    -- Styx module
    {   
        Name = "Kerberos",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_PLUTO",
            Body = "KERBEROS",
            Radius = kerberos_radius,
            Geometry = {
                Type = "SimpleSphere",
                Radius = kerberos_radius,
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
                Target = "KERBEROS",
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
        Name = "KerberosText",
        Parent = "Kerberos",
        Renderable = {
            Type = "RenderablePlane",
            Size = 10^6.3,
            Origin = "Center",
            Billboard = true,
            Texture = "textures/Kerberos-Text.png"
        },
        Transform = {
            Translation = {
                Type = "StaticTranslation",
                Position = {1000000, 0, 1000000},
            },
        },
    },    
    -- KerberosTrail module
    {   
        Name = "KerberosTrail",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "KERBEROS",
                Observer = "PLUTO BARYCENTER",
            },
            Color = {0.00, 0.62, 1.00},
            Period = 32.16756,
            Resolution = 1000
        },
    }
    
}
