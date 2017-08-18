local nix_radius = 0.45E5

local NewHorizonsKernels = {
    "${SPICE}/new_horizons/spk/NavSE_plu047_od122.bsp",
}

return {
    -- Nix module
    {   
        Name = "Nix",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderablePlanet",
            Frame = "IAU_PLUTO",
            Body = "NIX",
            Radius = nix_radius,
            Geometry = {
                Type = "SimpleSphere",
                Radius = nix_radius,
                Segments = 100
            },
            ColorTexture = "textures/gray.jpg",
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "NIX",
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
        Name = "NixText",
        Parent = "Nix",
        Renderable = {
            Type = "RenderablePlane",
            Size = 10^6.3,
            Origin = "Center",
            Billboard = true,
            Texture = "textures/Nix-Text.png"
        },
    },
    -- NixTrail module
    {   
        Name = "NixTrail",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "NIX",
                Observer = "PLUTO BARYCENTER",
            },
            Color = {0.00, 0.62, 1.00},
            Period = 24.85463,
            Resolution = 1000
        },
    }
    
}
