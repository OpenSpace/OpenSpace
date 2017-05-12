return {
    -- NewHorizonsFov main module
    {   
        Name = "NH_LORRI",
        Parent = "NewHorizonsPosition",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "NEW HORIZONS",
            Frame = "NH_SPACECRAFT",
            RGB   = { 0.8, 0.7, 0.7 },
            Textures = {
                Type  = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },
            Instrument = {                
                Name       = "NH_LORRI",
                Method     = "ELLIPSOID",
                Aberration = "NONE",      
            },
            PotentialTargets = {
                "Pluto",
                "Charon",
                "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
            }
        },
    },
    -- NewHorizonsFov module NH_RALPH_LEISA
    {   
        Name = "NH_RALPH_LEISA",
        Parent = "NewHorizonsPosition",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "NEW HORIZONS",
            Frame = "NH_SPACECRAFT",
            RGB   = { 0.8, 0.7, 0.7 },
            Textures = {
                Type  = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },
            Instrument = {               
                Name       = "NH_RALPH_LEISA",
                Method     = "ELLIPSOID",
                Aberration = "NONE",     
            },
            PotentialTargets = {
                "Pluto",
                "Charon",
                "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
            }
        },
    },

    -- NewHorizonsFov module NH_RALPH_MVIC_PAN1
    {   
        Name = "NH_RALPH_MVIC_PAN1",
        Parent = "NewHorizonsPosition",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "NEW HORIZONS",
            Frame = "NH_SPACECRAFT",
            RGB   = { 0.8, 0.7, 0.7 },
            Textures = {
                Type  = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },
            Instrument = {               
                Name       = "NH_RALPH_MVIC_PAN1",
                Method     = "ELLIPSOID",
                Aberration = "NONE",     
            },
            PotentialTargets = {
                "Pluto",
                "Charon",
                "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
            }
        },
    },
    -- NewHorizonsFov module NH_RALPH_MVIC_PAN2
    {   
        Name = "NH_RALPH_MVIC_PAN2",
        Parent = "NewHorizonsPosition",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "NEW HORIZONS",
            Frame = "NH_SPACECRAFT",
            RGB   = { 0.8, 0.7, 0.7 },
            Textures = {
                Type  = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },
            Instrument = {               
                Name       = "NH_RALPH_MVIC_PAN2",
                Method     = "ELLIPSOID",
                Aberration = "NONE",      
            },
            PotentialTargets = {
                "Pluto",
                "Charon",
                "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
            }

        },
    },
    -- NewHorizonsFov module NH_RALPH_MVIC_RED
    {   
        Name = "NH_RALPH_MVIC_RED",
        Parent = "NewHorizonsPosition",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "NEW HORIZONS",
            Frame = "NH_SPACECRAFT",
            RGB   = { 0.8, 0.7, 0.7 },
            Textures = {
                Type  = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },
            Instrument = {               
                Name       = "NH_RALPH_MVIC_RED",
                Method     = "ELLIPSOID",
                Aberration = "NONE",      
            },
            PotentialTargets = {
                "Pluto",
                "Charon",
                "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
            }

        },
    },
    -- NewHorizonsFov module NH_RALPH_MVIC_BLUE
    {   
        Name = "NH_RALPH_MVIC_BLUE",
        Parent = "NewHorizonsPosition",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "NEW HORIZONS",
            Frame = "NH_SPACECRAFT",
            RGB   = { 0.8, 0.7, 0.7 },
            Textures = {
                Type  = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },
            Instrument = {               
                Name       = "NH_RALPH_MVIC_BLUE",
                Method     = "ELLIPSOID",
                Aberration = "NONE",     
            },
            PotentialTargets = {
                "Pluto",
                "Charon",
                "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
            }
        },
    },
    -- NewHorizonsFov module NH_RALPH_MVIC_FT
    {   
        Name = "NH_RALPH_MVIC_FT",
        Parent = "NewHorizonsPosition",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "NEW HORIZONS",
            Frame = "NH_SPACECRAFT",
            RGB   = { 0.8, 0.7, 0.7 },
            Textures = {
                Type  = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },
            Instrument = {                
                Name       = "NH_RALPH_MVIC_FT",
                Method     = "ELLIPSOID",
                Aberration = "NONE",     
            },
            PotentialTargets = {
                "Pluto",
                "Charon",
                "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
            }
        },
    },
    -- NewHorizonsFov module NH_RALPH_MVIC_METHANE
    {   
        Name = "NH_RALPH_MVIC_METHANE",
        Parent = "NewHorizonsPosition",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "NEW HORIZONS",
            Frame = "NH_SPACECRAFT",
            RGB   = { 0.8, 0.7, 0.7 },
            Textures = {
                Type  = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },
            Instrument = {                
                Name       = "NH_RALPH_MVIC_METHANE",
                Method     = "ELLIPSOID",
                Aberration = "NONE",      
            },
            PotentialTargets = {
                "Pluto",
                "Charon",
                "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
            }
        },
    },
    -- NewHorizonsFov module NH_RALPH_MVIC_NIR
    {   
        Name = "NH_RALPH_MVIC_NIR",
        Parent = "NewHorizonsPosition",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "NEW HORIZONS",
            Frame = "NH_SPACECRAFT",
            RGB   = { 0.8, 0.7, 0.7 },
            Textures = {
                Type  = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },
            Instrument = {                
                Name       = "NH_RALPH_MVIC_NIR",
                Method     = "ELLIPSOID",
                Aberration = "NONE",      
            },
            PotentialTargets = {
                "Pluto",
                "Charon",
                "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
            }
        },
    },
    -- NewHorizonsFov module NH_ALICE_AIRGLOW
    {   
        Name = "NH_ALICE_AIRGLOW",
        Parent = "NewHorizonsPosition",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "NEW HORIZONS",
            Frame = "NH_SPACECRAFT",
            RGB   = { 0.8, 0.7, 0.7 },
            Textures = {
                Type  = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },
            Instrument = {                
                Name       = "NH_ALICE_AIRGLOW",
                Method     = "ELLIPSOID",
                Aberration = "NONE",      
            },
            PotentialTargets = {
                "Pluto",
                "Charon",
                "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
            }
        },
    },
    -- NewHorizonsFov module NH_ALICE_SOC
    {   
        Name = "NH_ALICE_SOC",
        Parent = "NewHorizonsPosition",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "NEW HORIZONS",
            Frame = "NH_SPACECRAFT",
            RGB   = { 0.8, 0.7, 0.7 },
            Textures = {
                Type  = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },
            Instrument = {                
                Name       = "NH_ALICE_SOC",
                Method     = "ELLIPSOID",
                Aberration = "NONE",      
            },
            PotentialTargets = {
                "Pluto",
                "Charon",
                "Jupiter", "Io", "Europa", "Ganymede", "Callisto"
            }
        },
    },
    {
        Name = "NH_REX",
        Parent = "NewHorizonsPosition",
        Renderable = {
            Type  = "RenderableCrawlingLine",
            Source = "NH_REX",
            Target = "EARTH",
            -- Body = "NEW HORIZONS",
            Frame = "GALACTIC",
            Color = { 1.0, 0.7, 0.0 },
            Instrument = "NH_REX"
        },
    }
}