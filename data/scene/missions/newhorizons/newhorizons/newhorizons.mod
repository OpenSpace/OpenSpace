if UseAccurateNewHorizonsKernels then
    NewHorizonsKernels = {
        -- SCLK
        "${SPICE}/nh_kernels/sclk/new-horizons_0976.tsc",
        -- "${SPICE}/NewHorizonsKernels/new_horizons_413.tsc",

        -- SPK
        "${SPICE}/nh_kernels/spk/nh_pred_20141201_20190301_od122.bsp",
        -- "${SPICE}/nh_kernels/spk/nh_pred_20120501_20160913_od093.bsp",
        "${SPICE}/nh_kernels/spk/plu_all_mvi.spk",
        "${SPICE}/jup260.bsp",
        -- "${SPICE}/NewHorizonsKernels/de413.bsp",

        -- CK
        "${SPICE}/nh_kernels/ck/nh_scispi_2015_pred.bc",
        "${SPICE}/nh_kernels/ck/nh_scispi_2015_recon.bc",
        "${SPICE}/nh_kernels/ck/nh_lorri_wcs.bc",
        "${SPICE}/nh_kernels/ck/plutonet_PS104.bc",

        -- FK
        "${SPICE}/nh_kernels/fk/nh_soc_misc_v001.tf",
        "${SPICE}/nh_kernels/fk/nh_v220.tf",

        -- IK
        "${SPICE}/nh_kernels/ik/nh_alice_v120.ti",
        "${SPICE}/nh_kernels/ik/nh_allinstruments_v002.ti",
        "${SPICE}/nh_kernels/ik/nh_astr_v000.ti",
        "${SPICE}/nh_kernels/ik/nh_fss_v000.ti",
        "${SPICE}/nh_kernels/ik/nh_lorri_v100.ti",
        "${SPICE}/nh_kernels/ik/nh_pepssi_v110.ti",
        "${SPICE}/nh_kernels/ik/nh_ralph_v100.ti",
        "${SPICE}/nh_kernels/ik/nh_rex_v100.ti",
        "${SPICE}/nh_kernels/ik/nh_sdc_v101.ti",
        "${SPICE}/nh_kernels/ik/nh_swap_v100.ti",

        -- LSK
        "${SPICE}/nh_kernels/lsk/naif0011.tls",

        -- PCK
        "${SPICE}/nh_kernels/pck/nh_targets_v001.tpc",
        "${SPICE}/nh_kernels/pck/pck00010.tpc",
        "${SPICE}/nh_kernels/pck/nh_pcnh_002.tpc"
    };
else
    NewHorizonsKernels = {
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
end

return {
    {
        Name = "NewHorizonsPosition",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "NEW HORIZONS",
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
        -- Parent = "PlutoBarycenter", 
        -- Parent = "JupiterBarycenter", 
        Parent = "NewHorizonsPosition", 
        Renderable = {
            Type = "RenderableModel",
            Body = "NEW HORIZONS",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "models/NewHorizonsCleanModel.obj",
                -- Magnification = 4,
            }, 
            Textures = {
                Type = "simple",
                Color =  "textures/NHTexture.jpg",
            },
            Shading = {
                PerformShading = true,
                Fadeable = false,
                Ghosting = false,
            },
        },
        Transform = {
            Scale = {
                Type = "StaticScale",
                Scale = 100
            }
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
        }
    },
    {
        Name = "NewHorizonsTrailPluto",
        Parent = "PlutoBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Body = "NEW HORIZONS",
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
