local charon_radius = 6.035E5

local NewHorizonsKernels = {
    "${SPICE}/new_horizons/spk/NavSE_plu047_od122.bsp",
    "${SPICE}/new_horizons/spk/NavPE_de433_od122.bsp",
}

-- Loading the smaller version for higher compatability
-- if we have a good way to measure GPU memory, we can make this dynamic
local ColorTexture = "textures/NH_Charon_mosaic_8192.png"
local HeightTexture = "textures/NH_Charon_DTM_8192.png"


return {
    -- CharonProjection module
    {   
        Name = "Charon",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderablePlanetProjection",
            Radius = charon_radius,
            Geometry = {
                Type = "SimpleSphere",
                Radius = charon_radius,
                Segments = 100
            },
            ColorTexture = ColorTexture,
            HeightTexture = HeightTexture,
            Projection = {
                Observer   = "NEW HORIZONS",
                Target     = "CHARON",
                Aberration = "NONE",
                AspectRatio = 2,

                Instrument = {
                    Name       = "NH_LORRI",
                    Method     = "ELLIPSOID",
                    Aberration = "NONE",
                    Fovy       = 0.2907,
                    Aspect     = 1,
                    Near       = 0.2,
                    Far        = 10000,
                },
                
                PotentialTargets = {
                    "PLUTO",
                    "CHARON"
                }
            }
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "CHARON",
                Observer = "PLUTO BARYCENTER",
                Kernels = NewHorizonsKernels
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_CHARON",
                DestinationFrame = "GALACTIC"
            },
        },
    },
    {
        Name = "CharonText",
        Parent = "Charon",
        Renderable = {
            Type = "RenderablePlane",
            Size = 10^6.3,
            Origin = "Center",
            Billboard = true,
            Texture = "textures/Charon-Text.png",
            BlendMode = "Additive"
        },
        Transform = {
            Translation = {
                Type = "StaticTranslation",
                Position = {0, -1000000, 0}
            },
        },
    },
    {
        Name = "CharonShadow",
        Parent = "Charon",
        Renderable = {
            Type = "RenderableShadowCylinder",
            TerminatorType = "PENUMBRAL", 
            LightSource = "SUN",
            Observer = "NEW HORIZONS",
            Body = "CHARON",
            BodyFrame = "IAU_CHARON",
            Aberration = "NONE",
        },
    },    
    -- CharonTrail module
    {   
        Name = "CharonTrail",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "CHARON",
                Observer = "PLUTO BARYCENTER",
            },
            Color = {0.00, 0.62, 1.00},
            Period = 6.38725,
            Resolution = 1000
        },
    }
}
