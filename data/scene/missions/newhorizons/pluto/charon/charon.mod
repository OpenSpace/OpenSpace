if UseAccurateNewHorizonsKernels then
    NewHorizonsKernels = {
        "${SPICE}/nh_kernels/spk/NavSE_plu047_od122.bsp"
    }
else
    NewHorizonsKernels = {
        "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
    }
end

Files = {
    low = "textures/charon_highres.jpg",
    med = "textures/charon_highres.jpg",
    high = "textures/cpmap_cyl_HR_0e.jpg"
}
ColorTexture = Files[TextureResolution]

return {
    -- CharonProjection module
    {   
        Name = "Charon",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderablePlanetProjection",
            Geometry = {
                Type = "SimpleSphere",
                Radius = 6.035E5,
                Segments = 100
            },
            Textures = {
                Color = ColorTexture,
                Height = "textures/cpdem-Mcolor2-MLorriCA-lr-5_ZMfs-cyl.jpg",
            },
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
                Body = "CHARON",
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
            Size = 1.0E6.3,
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
            MainFrame = "GALACTIC",
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
                Body = "CHARON",
                Observer = "PLUTO BARYCENTER",
            },
            Color = {0.00, 0.62, 1.00},
            Period = 6.38725,
            Resolution = 1000
        },
    }
}
