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
            Frame = "IAU_CHARON", 
            Body = "CHARON",
            Geometry = {
                Type = "SimpleSphere",
                Radius = { 6.035 , 5 },
                Segments = 100
            },
            Textures = {
                Type = "simple",
                Color = ColorTexture,
                Height = "textures/cpdem-Mcolor2-MLorriCA-lr-5_ZMfs-cyl.jpg",
                Project = "textures/defaultProj.png",
                Sequencing = "true",
            },
            Projection = {
                Observer   = "NEW HORIZONS",
                Target     = "CHARON",
                Aberration = "NONE",
                AspectRatio = 2
            },
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
        },
        --[[
        Ephemeris = {
            Type = "Spice",
            Body = "CHARON",
            Reference = "ECLIPJ2000",
            Observer = "PLUTO BARYCENTER",
            Kernels = NewHorizonsKernels
        },
        Rotation = {
            Type = "Spice",
            Frame = "IAU_CHARON",
            Reference = "ECLIPJ2000"
        },
        ]]
        Transform = {
            Translation = {
                Type = "SpiceEphemeris",
                Body = "CHARON",
                Reference = "GALACTIC",
                Observer = "PLUTO BARYCENTER",
                Kernels = NewHorizonsKernels
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "IAU_CHARON",
                DestinationFrame = "GALACTIC"
            },
        },
        GuiName = "/Solar/Planets/Charon"
    },
    {
        Name = "CharonText",
        Parent = "Charon",
        Renderable = {
            Type = "RenderablePlane",
            Size = {1.0, 6.3},
            Origin = "Center",
            Billboard = true,
            Texture = "textures/Charon-Text.png",
            BlendMode = "Additive"
        },
        Transform = {
            Translation = {
                Type = "StaticEphemeris",
                Position = {0, -1000000, 0}
            },
        },
        --[[
        Ephemeris = {
            Type = "Static",
             Position = {0, -10, 0, 5}
        }
        ]]
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
            Type = "RenderableTrail",
            Body = "CHARON",
            Frame = "GALACTIC",
            Observer = "PLUTO BARYCENTER",
            RGB = {0.00,0.62,1.00},
            TropicalOrbitPeriod = 120 ,
            EarthOrbitRatio = 0.03,
            DayLength = 1,
            Textures = {
                Type = "simple",
                Color = "${COMMON_MODULE}/textures/glare_blue.png",
                -- need to add different texture
            },  
        },
        GuiName = "/Solar/CharonTrail"
    }
}
