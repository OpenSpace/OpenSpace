return {
    -- New Horizons Body module
    {   
        Name = "NewHorizons",
        -- Parent = "PlutoBarycenter", 
        -- Parent = "JupiterBarycenter", 
        Parent = "SolarSystemBarycenter", 
        Renderable = {
            Type = "RenderableModel",
            Body = "NEW HORIZONS",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "${OPENSPACE_DATA}/scene/newhorizons/models/NewHorizonsCleanModel.obj",
                Magnification = 4,
            }, 
            Textures = {
                Type = "simple",
                Color =  "textures/NHTextureFlipCol.jpg",
            },
            Rotation = {
                Source = "NH_SPACECRAFT",
                Destination = "GALACTIC"
            },
            Shading = {
                PerformShading = true,
                Fadeable = false,
                Ghosting = false,
            },
        },
        Ephemeris = {
                Type = "Spice",
                Body = "NEW HORIZONS",
                -- Reference = "ECLIPJ2000",
                Reference = "GALACTIC",
                -- Observer = "PLUTO BARYCENTER",
                Observer = "SUN",
                -- Observer = "JUPITER BARYCENTER",
                Kernels = {
                    --SCLK
                    
                    "${SPICE}/NewHorizonsKernels/new_horizons_413.tsc",
                    
                    "${SPICE}/NewHorizonsKernels/nhops_CORE_v9g_cdh2.bc",
                    "${SPICE}/NewHorizonsKernels/nh_ref_20080710_20160101_od059B.bsp",
                    
                    "${SPICE}/de430_1850-2150.bsp",
                    -- CK
                    "${SPICE}/NewHorizonsKernels/merged_nhpc_2006_v011.bc",
                    "${SPICE}/NewHorizonsKernels/merged_nhpc_2007_v006.bc",
                    -- FK       
                    "${SPICE}/NewHorizonsKernels/nh_v200.tf",
                    -- IK       
                    "${SPICE}/NewHorizonsKernels/nh_alice_v110.ti",
                    "${SPICE}/NewHorizonsKernels/nh_lorri_v100.ti",
                    "${SPICE}/NewHorizonsKernels/nh_pepssi_v110.ti",
                    "${SPICE}/NewHorizonsKernels/nh_ralph_v100.ti",
                    "${SPICE}/NewHorizonsKernels/nh_rex_v100.ti",
                    "${SPICE}/NewHorizonsKernels/nh_sdc_v101.ti",
                    "${SPICE}/NewHorizonsKernels/nh_swap_v100.ti",
                    -- PCK      
                    
                    -- SPK      
                    "${SPICE}/jup260.bsp",
                    "${SPICE}/NewHorizonsKernels/de413.bsp",
                    "${SPICE}/NewHorizonsKernels/nh_nep_ura_000.bsp",
                    "${SPICE}/NewHorizonsKernels/nh_recon_e2j_v1.bsp",
                    "${SPICE}/NewHorizonsKernels/nh_recon_j2sep07_prelimv1.bsp",
                    "${SPICE}/NewHorizonsKernels/sb_2002jf56_2.bsp",
                    "${SPICE}/NewHorizonsKernels/nh_plu017.bsp",
                }
            },
        GuiName = "/Solar/NewHorizons"
    },
    --NewHorizonsTrail module
    --[[{   
        Name = "NewHorizonsTrail",
        Parent = "Root",
        Renderable = {
            Type = "RenderableTrail",
            Body = "NEW HORIZONS",
            Frame = "GALACTIC",
            Observer = "SUN",
            -- Observer = "PLUTO BARYCENTER",
            RGB = {1.00,0.80,0.00},
            TropicalOrbitPeriod = 39425.0,
            EarthOrbitRatio = 4.5,
            DayLength = 9.9259,
            TimeStamps = true,
            Textures = {
                Type = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },  
        },
        GuiName = "/Solar/NewHorizonsTrail"
    },--]]

    -- New Horizons Labels Elements module
    {   
        Name = "Labels",
        Parent = "NewHorizons", 
        Renderable = {
            Type = "RenderableModel",
            Body = "NEW HORIZONS",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "${OPENSPACE_DATA}/scene/newhorizons/models/Labels.obj",
                Magnification = 4,
            }, 
            Textures = {
                Type = "simple",
                Color =  "textures/labels.png",
                BumpMap = "textures/goldfoilbump.tif"
            },
            Rotation = {
                Source = "NH_SPACECRAFT",
                Destination = "GALACTIC"
            },
            Shading = {
                PerformShading = true,
                Fadeable = false,
                Ghosting = false,
            },
        },
        Ephemeris = {
                Type = "Static",
            },
        GuiName = "/Solar/NewHorizons"
    },

    -- NewHorizonsPath module
    {   
        Name = "NewHorizonsPathPluto",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderablePath",
            Body = "NEW HORIZONS",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = { 1.0, 0.8, 0.4 },
            TimeSteps = 900,
            Textures = {
                Type = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },  
            DrawLine = true;
            
            StartTime = "2015 JUL 07 12:00:00",
            EndTime = "2015 JUL 17 12:00:00"
        },
        GuiName = "/Solar/NewHorizonsPathPluto"
    },
    
        -- NewHorizonsPath module
    {   
        Name = "NewHorizonsPathJupiter",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderablePath",
            Body = "NEW HORIZONS",
            Frame = "GALACTIC",
            Observer = "SUN",
            RGB = { 1.0, 0.8, 0.2 },
            TimeSteps = 900,
            Textures = {
                Type = "simple",
                Color = "textures/glare_blue.png",
                -- need to add different texture
            },  
            DrawLine = true;
            
            StartTime = "2007 FEB 07 12:00:00",
            EndTime = "2007 MAR 15 12:00:00"
        },
        GuiName = "/Solar/NewHorizonsPathJupiter"
    }, 
}
