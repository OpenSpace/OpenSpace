return {
    -- Comet Vesta Body module
    {   
        Name = "Vesta",
        Parent = "SolarSystemBarycenter", 
        Renderable = {
            Type = "RenderableModelProjection",
            Frame = "IAU_VESTA",
            Body = "VESTA",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "${OPENSPACE_DATA}/scene/vestaprojection_2/obj/VestaComet_5000.obj",
            },
            Textures = {
                Type = "simple",
                Color = "textures/dummy.jpg",
                Project = "textures/projectMe.png",
                Default = "textures/defaultProj_backup.png"
            },
            Projection = {
                Sequence   = "${OPENSPACE_DATA}/scene/vestaprojection_2/vestaimages",
                SequenceType = "image-sequence",
                Observer   = "DAWN",
                Target     = "VESTA",
                Aberration = "NONE",
            },
            DataInputTranslation = {
                Instrument = {
                    --[[FC1 = {
                        DetectorType  = "Camera",
                        Spice = {"DAWN_FC1"},
                    },  --]]
                    FC2 = {
                        DetectorType  = "Camera",
                        Spice = {"DAWN_FC2"},
                    },  
                },                  
                Target = { 
                    Read  = {
                        "TARGET_NAME",
                        "INSTRUMENT_HOST_NAME",
                        "INSTRUMENT_ID", 
                        "START_TIME", 
                        "STOP_TIME", 
                        --"DETECTOR_TYPE",
                        --"SEQUENCE_ID",
                    },
                    Convert = {
                        VESTA           = {"VESTA"   },
                        DAWN            = {"DAWN"   },
                        --FRAMINGCAMERA1  = {"DAWN_FC1"},
                        FRAMINGCAMERA2  = {"DAWN_FC2"},
                        --FC1           = {"DAWN_FC1"},
                        FC2             = {"DAWN_FC2"},
                    },
                },
            },
            Instrument = {                
                Name       = "DAWN_FC2",
                Method     = "ELLIPSOID",
                Aberration = "NONE",
                Fovy       = 5.46,
                Aspect     = 1,
                Near       = 0.2,
                Far        = 10000,
            },
            
            --[[ Instrument = {                
                Name       = "DAWN_FC1",
                Method     = "ELLIPSOID",
                Aberration = "NONE",
                Fovy       = 5.46,
                Aspect     = 1,
                Near       = 0.2,
                Far        = 10000,
            },--]]
            PotentialTargets = {
                "VESTA"
            },
            Rotation = {
                Source = "IAU_VESTA",
                Destination = "GALACTIC"
            },
            
            StartTime = "2007 JUL 20 12:00:00",
            EndTime = "2018 JAN 22 12:00:00"
        },
        Ephemeris = {
            Type = "Spice",
            Body = "VESTA",
            Observer = "SUN",
            Kernels = {
                --"${OPENSPACE_DATA}/spice/DAWN_KERNELS/pck/dawn_vesta_v06.tpc",
                "${OPENSPACE_DATA}/spice/DawnKernels/spk/sb_vesta_071107.bsp",
            }
        }
    },
    -- Vesta Trail Module
    {   
        Name = "VestaTrail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Target = "VESTA",
                Observer = "SUN",
            },
            Color = { 0.7, 0.8, 0.7 },
            StartTime = "2007 JUL 20 12:00:00",
            EndTime = "2018 JAN 22 12:00:00",
            SampleInterval = 3600 * 24
        }
    }
}
