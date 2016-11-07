local BENNU_BODY = "2101955"

local CaseDependentKernels
if KernelCase == 2 then
    CaseDependentKernels = {
        "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/525m_Sortie_v2/ORX_Recon_525mSortie_Case02.bsp",
        "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/525m_Sortie_v2/Recon_525mSortie_Case02_0Latitude.bc",
        "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/525m_Sortie_v2/Recon_525mSortie_Case02_atl_19145_04.atf",

        "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/225m_Sortie_v2/ORX_Recon_225mSortie_Case02.bsp",
        "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/225m_Sortie_v2/Recon_225mSortie_Case02_0Latitude.bc"
    }
elseif KernelCase == 5 then
    CaseDependentKernels = {
        "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/525m_Sortie_v2/ORX_Recon_525mSortie_Case05.bsp",
        "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/525m_Sortie_v2/Recon_525mSortie_Case05_20negLatitude.bc",
        "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/525m_Sortie_v2/Recon_525mSortie_Case05_atl_19145_04.atf",
        "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/525m_Sortie_v2/Recon_525mSortie_Case05_NominalProfile.bc",

        "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/225m_Sortie_v2/ORX_Recon_225mSortie_Case05.bsp",
        "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/225m_Sortie_v2/Recon_225mSortie_Case05_20negLatitude.bc"
    }
elseif KernelCase == 8 then
    CaseDependentKernels = {
        "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/525m_Sortie_v2/Recon_525mSortie_Case08_NominalProfile.bc",
        "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/225m_Sortie_v2/ORX_Recon_225mSortie_Case08.bsp",
        "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/225m_Sortie_v2/Recon_225mSortie_Case08_40negLatitude.bc"
    }
elseif KernelCase == 11 then
    CaseDependentKernels = {
        "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/225m_Sortie_v2/ORX_Recon_225mSortie_Case11.bsp",
        "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/225m_Sortie_v2/Recon_225mSortie_Case11_60negLatitude.bc"
    }
end

local OsirisRexKernels = {
    -- background 
    -- SCLK kernels needs to be loaded before CK kernels (and generally first)
    "${SPICE}/OsirisRexKernels/background/sclk/ORX_SCLKSCET.00000.tsc",

    -- This cannot be loaded correctly for some reason!
    --openspace.spice.loadKernel("${SPICE}/OsirisRexKernels/background/dsk/RQ36mod.oct12_CCv0001.bds")
    
    "${SPICE}/OsirisRexKernels/background/fk/orx_v04.tf",
    "${SPICE}/OsirisRexKernels/background/ik/orx_lidar_v00.ti",
    "${SPICE}/OsirisRexKernels/background/ik/orx_ocams_v03.ti",
    "${SPICE}/OsirisRexKernels/background/ik/orx_otes_v00.ti",
    "${SPICE}/OsirisRexKernels/background/ik/orx_rexis_v00.ti",
    "${SPICE}/OsirisRexKernels/background/ik/orx_struct_v00.ti",
    "${SPICE}/OsirisRexKernels/background/ik/orx_navcam_v00.ti",
    "${SPICE}/OsirisRexKernels/background/ik/orx_ola_v00.ti",
    "${SPICE}/OsirisRexKernels/background/ik/orx_ovirs_v00.ti",
    "${SPICE}/OsirisRexKernels/background/ik/orx_stowcam_v00.ti",
    "${SPICE}/OsirisRexKernels/background/lsk/naif0011.tls",
    "${SPICE}/OsirisRexKernels/background/pck/bennu_SPH250m.tpc",
    "${SPICE}/OsirisRexKernels/background/pck/bennu_v10.tpc",

    -- Low res SPK
    "${SPICE}/OsirisRexKernels/background/spk/orx_160917_231024_pgaa3_day15m60_v1.bsp",
    "${SPICE}/OsirisRexKernels/background/spk/orx_160914_231024_pgaa3_day12m60_v1.bsp",
    
    "${SPICE}/OsirisRexKernels/background/spk/orx_160908_231024_pgaa3_day06m60_v1.bsp",
    "${SPICE}/OsirisRexKernels/background/spk/spk_orx_160908_231024_pgaa2_day06m60_v3.bsp",
    "${SPICE}/OsirisRexKernels/background/spk/orx_160908_231024_pgaa2_day06m60.bsp",

    "${SPICE}/OsirisRexKernels/background/spk/OREX_20160908_M60_complete.bsp",
    "${SPICE}/OsirisRexKernels/background/spk/OREX_20160904_M45_complete.bsp",

    -- SPK
    "${SPICE}/OsirisRexKernels/background/spk/de421.bsp",
    "${SPICE}/OsirisRexKernels/background/spk/sb-101955-76.bsp",

    -- Nominal_Profile_LowRes
    "${SPICE}/OsirisRexKernels/Nominal_Profile_LowRes/Approach_600s_20180816T230000_20181119T010000.bsp",
    "${SPICE}/OsirisRexKernels/Nominal_Profile_LowRes/Approach_NominalProfile_600s_20180816T230000_20181119T010000.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Profile_LowRes/DetailedSurvey_600s_20190108T000000_20190317T000000.bsp",
    "${SPICE}/OsirisRexKernels/Nominal_Profile_LowRes/OrbitalA_600s_20181203T230000_20190109T000000.bsp",
    "${SPICE}/OsirisRexKernels/Nominal_Profile_LowRes/OrbitalA_NominalProfile_600s_20181203T230000_20190109T000000.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Profile_LowRes/OrbitalB_600s_20190316T000000_20190521T000000.bsp",
    "${SPICE}/OsirisRexKernels/Nominal_Profile_LowRes/DetailedSurvey_NominalProfile_600s_20190108T000000_20190317T000000.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Profile_LowRes/OrbitalB_NominalProfile600s_20190316T000000_20190521T000000.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Profile_LowRes/PrelimSurvey_600s_20181119T230000_20181204T010000.bsp",
    "${SPICE}/OsirisRexKernels/Nominal_Profile_LowRes/PrelimSurvey_NominalProfile_600s_20181119T230000_20181204T010000.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Profile_LowRes/Recon_600s_20190519T000000_20190830T000000.bsp",
    "${SPICE}/OsirisRexKernels/Nominal_Profile_LowRes/Recon_NominalProfile_600s_20190519T000000_20190830T000000.bc",
    
    -- Nominal_Observations_Science
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/03_Approach/DustSearch_v1/Phase03_AP_DustSearch_1.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/03_Approach/LightCurve_v1/Phase03_AP_LightCurve_1.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/03_Approach/LightCurve_v1/Phase03_AP_LightCurve_2.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/03_Approach/NatSatSearch_v1/Phase03_AP_SatSearch_1.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/03_Approach/NatSatSearch_v1/Phase03_AP_SatSearch_2.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/03_Approach/PhaseFunction_v1/Phase03_AP_PhaseFunction_1.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/03_Approach/ShapeModel_v1/Phase03_AP_ShapeModel_1.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/03_Approach/ShapeModel_v1/Phase03_AP_ShapeModel_2.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/03_Approach/ShapeModel_v1/Phase03_AP_ShapeModel_3.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/03_Approach/ShapeModel_v1/Phase03_AP_ShapeModel_4.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/03_Approach/ShapeModel_v1/Phase03_AP_ShapeModel_5.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/03_Approach/ShapeModel_v1/Phase03_AP_ShapeModel_6.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/03_Approach/ShapeModel_v1/Phase03_AP_ShapeModel_7.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/03_Approach/ShapeModel_v1/Phase03_AP_ShapeModel_8.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/03_Approach/ShapeModel_v1/Phase03_AP_ShapeModel_9_Forced4x4.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/03_Approach/SpectraMap_v1/Phase03_AP_SpectraMap_1.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/04_PrelimSurvey/MapCamOLA_v1/Phase04_PS_MC_1_v1_1a.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/04_PrelimSurvey/MapCamOLA_v1/Phase04_PS_MC_2_v1_1a.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/04_PrelimSurvey/OLA_v1/Phase04_PS_OLA_Nominal_1.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/04_PrelimSurvey/OLA_v1/Phase04_PS_OLA_Nominal_2.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/04_PrelimSurvey/OLA_v1/Phase04_PS_OLA_Nominal_3.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/04_PrelimSurvey/OLA_v1/Phase04_PS_OLA_Nominal_4.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/04_PrelimSurvey/PolyCam_v1/Phase04_PS_PolyCam_1.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/04_PrelimSurvey/PolyCam_v1/Phase04_PS_PolyCam_2.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/04_PrelimSurvey/PolyCam_v1/Phase04_PS_PolyCam_3.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/04_PrelimSurvey/PolyCam_v1/Phase04_PS_PolyCam_4.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/04_PrelimSurvey/PolyCam_v1/Phase04_PS_PolyCam_5.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/04_PrelimSurvey/PolyCam_v1/Phase04_PS_PolyCam_6.bc",

    --openspace.spice.loadKernel("${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/BaseballDiamond_v2/atl_19013_18_BBD1_info.TXT")
    --openspace.spice.loadKernel("${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/BaseballDiamond_v2/atl_19014_16_BBD2_info.TXT")
    --openspace.spice.loadKernel("${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/BaseballDiamond_v2/atl_19020_18_BBD3_info.TXT")
    --openspace.spice.loadKernel("${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/BaseballDiamond_v2/atl_19021_19_BBD4_info.TXT")
    --openspace.spice.loadKernel("${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/BaseballDiamond_v2/README.txt")
    
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/BaseballDiamond_v2/atl_19013_18_BBD1_v2.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/BaseballDiamond_v2/atl_19014_16_BBD2_v2.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/BaseballDiamond_v2/atl_19020_18_BBD3_v2.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/BaseballDiamond_v2/atl_19021_19_BBD4_v2.bc",
    

    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/EquatorialStations_v1/Phase06_DS_Equatorial_Stations_1.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/EquatorialStations_v1/Phase06_DS_Equatorial_Stations_2.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/EquatorialStations_v1/Phase06_DS_Equatorial_Stations_3.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/EquatorialStations_v1/Phase06_DS_Equatorial_Stations_4.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/EquatorialStations_v1/Phase06_DS_Equatorial_Stations_5.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/EquatorialStations_v1/Phase06_DS_Equatorial_Stations_6.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/EquatorialStations_v1/Phase06_DS_Equatorial_Stations_7.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/PlumeSearch_v1/Phase06_DS_Plume_Search_1.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/06_DetailedSurvey/PlumeSearch_v1/Phase06_DS_Plume_Search_2.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/07_OrbitalB/CandidateSampleSite_v1/Phase07_OB_CSS_Mapping_1.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/07_OrbitalB/CandidateSampleSite_v1/Phase07_OB_CSS_Mapping_2.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/07_OrbitalB/CandidateSampleSite_v1/Phase07_OB_CSS_Mapping_3.bc",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/07_OrbitalB/CandidateSampleSite_v2/CSS_Mapping_1.a",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/07_OrbitalB/CandidateSampleSite_v2/CSS_Mapping_2.a",
    "${SPICE}/OsirisRexKernels/Nominal_Observations_Science/07_OrbitalB/CandidateSampleSite_v2/CSS_Mapping_3.a",

    --openspace.spice.loadKernel("${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/225m_Sortie_v2/Case02_0Latitude.wmv")
    --openspace.spice.loadKernel("${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/225m_Sortie_v2/Case05_20negLatitude.wmv")
    --openspace.spice.loadKernel("${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/225m_Sortie_v2/Case08_40negLatitude.wmv")
    --openspace.spice.loadKernel("${SPICE}/OsirisRexKernels/Nominal_Observations_Science/08_Recon/225m_Sortie_v2/Case11_60negLatitude.wmv")

    KernelCase
}


return {
    ------------------------
    --     Osiris Rex     --
    ------------------------
    {
        Name = "OsirisRex",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableModel",
            Body = "OSIRIS-REX",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "models/orx_base_resized_12_sep_2016.obj",
                Magnification = 0,
            },
            Textures = {
                Type = "simple",
                Color =  "textures/osirisTex.png",
            },
            Shading = {
                PerformShading = true,
                Fadeable = false,
                Ghosting = false,
            },
        },
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "OSIRIS-REX",
                Observer = "SUN",
                Kernels = OsirisRexKernels
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "ORX_SPACECRAFT",
                DestinationFrame = "GALACTIC",
            },
        },
    },
    {
        Name = "ORX_OCAMS_POLYCAM",
        Parent = "OsirisRex",
        Renderable = {
            Type = "RenderableModel",
            Body = "OSIRIS-REX",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "models/orx_polycam_resized_12_sep_2016.obj",
                Magnification = 0,
            }, 
            Textures = {
                Type = "simple",
                Color =  "textures/osirisTex.png",
            },
            Shading = {
                PerformShading = true,
                Fadeable = false,
                Ghosting = false,
            },
        },
        Transform = {
            Translation = {
                Type = "StaticTranslation",
                Position = {-0.2476, 0.2710, 0.3364},
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "ORX_OCAMS_POLYCAM",
                DestinationFrame = "ORX_SPACECRAFT",
            },
        },
    },
    {
        Name = "ORX_REXIS",
        Parent = "OsirisRex",
        Renderable = {
            Type = "RenderableModel",
            Body = "OSIRIS-REX",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "models/orx_rexis_resized_12_sep_2016.obj",
                Magnification = 0,
            }, 
            Textures = {
                Type = "simple",
                Color =  "textures/osirisTex.png",
            },
            Shading = {
                PerformShading = true,
                Fadeable = false,
                Ghosting = false,
            },
        },
        Transform = {
            Translation = {
                Type = "StaticTranslation",
                Position = {0, 0.3371, 0.2712},
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "ORX_REXIS",
                DestinationFrame = "ORX_SPACECRAFT",
            },
        },
    },
    {
        Name = "POLYCAM FOV",
        Parent = "ORX_OCAMS_POLYCAM",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "OSIRIS-REX",
            Frame = "ORX_OCAMS_POLYCAM",
            RGB   = { 0.8, 0.7, 0.7 },
            Instrument = {
                Name       = "ORX_OCAMS_POLYCAM",
                Method     = "ELLIPSOID",
                Aberration = "NONE",
            },
            PotentialTargets = {
                BENNU_BODY -- Bennu
            }
        },
    },
    { 
        Name = "REXIS FOV",
        Parent = "ORX_REXIS",
        Renderable = {
            Type  = "RenderableFov",
            Body  = "OSIRIS-REX",
            Frame = "ORX_REXIS",
            RGB   = { 0.8, 0.7, 0.7 },
            Instrument = {
                Name       = "ORX_REXIS",
                Method     = "ELLIPSOID",
                Aberration = "NONE",
            },
            PotentialTargets = {
                BENNU_BODY -- Bennu
            },
            FrameConversions = {
                [BENNU_BODY] = "IAU_BENNU"
            }
        },
    },
    --[[
    -- Latest image taken by POLYCAM
    { 
        Name = "ImagePlaneOsirisRex",
        Parent = "Bennu2",
        Renderable = {
            Type = "RenderablePlaneProjection",
            Frame = "IAU_BENNU",
            DefaultTarget = BENNU_BODY,
            Spacecraft = "OSIRIS-REX",
            Instrument = "ORX_OCAMS_POLYCAM",
            Moving = false,
            Texture = "textures/defaultProj.png",
        },
        Ephemeris = {
            Type = "Static",
            Position = {0, 0, 0, 1}
        }, 
    },
    -- POLYCAM FoV square
    {
        Name = "FovImagePlane",
        Parent = "Bennu2",
        Renderable = {
            Type = "RenderablePlaneProjection",
            Frame = "IAU_BENNU",
            DefaultTarget = BENNU_BODY,
            Spacecraft = "OSIRIS-REX",
            Instrument = "ORX_OCAMS_POLYCAM",
            Moving = true,
            Texture = "textures/defaultProj.png",
        },
        Ephemeris = {
            Type = "Static",
            Position = {0, 0, 0, 1}
        },
    },
    ]]

    -- Trail relative to Earth
    {   
        Name = "OsirisRexTrailEarth",
        Parent = "Earth",
        Renderable = {
            Type = "RenderableTrailNew",
            -- Spice
            Body = "OSIRIS-REX",
            Frame = "IAU_EARTH",
            Observer = "EARTH",
            -- Optional rendering properties
            LineColor = { 0.9, 0.9, 0.0 },
            PointColor = { 0.9, 0.9, 0.0 },
            LineFade = 0.0, -- [0,1]
            RenderPart = 1,
            LineWidth = 2,
            ShowTimeStamps = false,
            RenderFullTrail = false,
            -- Time interval
            TimeRange = {
                Start = "2016 SEP 8 23:05:00.50",
                End = "2016 SEP 9 00:05:00",
            },
            SampleDeltaTime = 60, -- Seconds between each point
            SubSamples = 59, 
        },
    },

    -- Trail relative to solar system barycenter
    {   
        Name = "OsirisRexTrailSolarSystem",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailNew",
            -- Spice
            Body = "OSIRIS-REX",
            Frame = "GALACTIC",
            Observer = "SUN",
            -- Optional rendering properties
            LineColor = { 0.2, 0.9, 0.2 },
            PointColor = { 0.2, 0.9, 0.2 },
            LineFade = 0.0, -- [0,1]
            RenderPart = 0.13,
            LineWidth = 2,
            ShowTimeStamps = false,
            RenderFullTrail = false,
            -- Time interval
            TimeRange = {
                Start = "2016 SEP 8 23:05:00.50",
                End = "2023 SEP 24 12:00:00",
            },
            SampleDeltaTime = 3600, -- Seconds between each point
            SubSamples = 0, 
        },
    },

    -- Trail relative to Bennu
    {   
        Name = "OsirisRexTrailBennu",
        Parent = "BennuBarycenter",
        Renderable = {
            Type = "RenderableTrailNew",
            -- Spice
            Body = "OSIRIS-REX",
            Frame = "GALACTIC",
            Observer = BENNU_BODY,
            -- Optional rendering properties
            LineColor = { 0.9, 0.2, 0.9 },
            PointColor = { 0.9, 0.2, 0.9 },
            LineFade = 0.5, -- [0,1]
            RenderPart = 0.06,
            LineWidth = 2,
            ShowTimeStamps = false,
            RenderFullTrail = false,
            -- Time interval
            TimeRange = {
                Start = "2016 SEP 8 23:05:00.50",
                End = "2023 SEP 24 12:00:00",
            },
            SampleDeltaTime = 3600, -- Seconds between each point
            SubSamples = 3, 
        },
    },


}
