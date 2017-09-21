NewHorizonsKernels = {
    "${SPICE}/new_horizons/spk/nh_pred_20141201_20190301_od122.bsp",
    "${SPICE}/new_horizons/spk/NavSE_plu047_od122.bsp",
    "${SPICE}/new_horizons/spk/NavPE_de433_od122.bsp",

    "${SPICE}/jup260.bsp",

    "${SPICE}/new_horizons/ck/nh_scispi_2015_pred.bc",
    "${SPICE}/new_horizons/ck/nh_scispi_2015_recon.bc",
    "${SPICE}/new_horizons/ck/nh_lorri_wcs.bc",
    
    "${SPICE}/new_horizons/smithed_pc_and_sp/PLU_LORRI_ALL_161216.bc",

    "${SPICE}/new_horizons/sclk/new-horizons_1121.tsc",

    "${SPICE}/new_horizons/pck/nh_targets_v001.tpc",
    "${SPICE}/new_horizons/pck/nh_pcnh_005.tpc",

    "${SPICE}/new_horizons/fk/nh_v220.tf",
    "${SPICE}/new_horizons/ik/nh_allinstruments_v002.ti",
    "${SPICE}/new_horizons/ik/nh_alice_v200.ti",
    "${SPICE}/new_horizons/ik/nh_lorri_v201.ti",
    "${SPICE}/new_horizons/ik/nh_pepssi_v110.ti",
    "${SPICE}/new_horizons/ik/nh_ralph_v100.ti",
    "${SPICE}/new_horizons/ik/nh_rex_v100.ti",
    "${SPICE}/new_horizons/ik/nh_sdc_v101.ti",
    "${SPICE}/new_horizons/ik/nh_swap_v100.ti",
    "${SPICE}/new_horizons/ik/nh_astr_v000.ti",
    "${SPICE}/new_horizons/ik/nh_fss_v000.ti",
    "${SPICE}/new_horizons/fk/nh_soc_misc_v001.tf",
    "${SPICE}/new_horizons/spk/nh_stars.bsp",
}

return {
    {
        Name = "NewHorizonsPosition",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "NEW HORIZONS",
                Observer = "SUN",
                Kernels = NewHorizonsKernels
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "NH_SPACECRAFT",
                DestinationFrame = "GALACTIC",
            },
        }
    },
    {
        Name = "NewHorizons",
        Parent = "NewHorizonsPosition", 
        Renderable = {
            Type = "RenderableModel",
            Body = "NEW HORIZONS",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "models/NewHorizonsCleanModel.obj",
                -- Magnification = 4,
            }, 
            ColorTexture = "textures/NHTexture.jpg",
            Shading = {
                PerformShading = true,
                Fadeable = false,
                Ghosting = false,
            },
        },
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
                GeometryFile = "models/Labels.obj",
                -- Magnification = 4,
            }, 
            ColorTexture =  "textures/labels.png",
            Textures = {
                Type = "simple",
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
        }
    },
    {
        Name = "NewHorizonsTrailPluto",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "NEW HORIZONS",
                Observer = "PLUTO BARYCENTER"
            },
            Color = { 1.0, 0.8, 0.4 },
            ShowFullTrail = true,
            StartTime = "2015 JUL 07 12:00:00",
            EndTime = "2015 JUL 17 12:00:00",
            PointSize = 5,
            SampleInterval = 3600,
            TimeStampSubsampleFactor = 4,
            EnableFade = false,
            Rendering = "Lines+Points"
        },
    },
    --[[
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
    ]]
}
