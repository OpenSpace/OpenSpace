local Kernels = {
    '${SPICE}/voyager/vg1_v02.tf',
    '${SPICE}/voyager/vg100019.tsc',
    '${SPICE}/voyager/Voyager_1.a54206u_V0.2_merged.bsp',
    '${SPICE}/voyager/voyager_1.ST+1991_a54418u.merged.bsp',
    '${SPICE}/voyager/vgr1_jup230.bsp',
    '${SPICE}/voyager/vgr1_sat337.bsp',
    '${SPICE}/voyager/vgr1_super.bc',
    '${SPICE}/voyager/vgr1_super_v2.bc'
}

local RotationMatrix = {
    -1, 0, 0,
    0, 0, -1,
    0, -1, 0
}

return {
    {
        Name = "Voyager 1",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Target = "VOYAGER 1",
                Observer = "SUN",
                Kernels = Kernels
            },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "VG1_SC_BUS",
                DestinationFrame = "GALACTIC"
            }
        },
        GuiPath = "/Solar System/Missions/Voyager 1"
    },
    {
        Name = "Voyager 1 Main",
        Parent = "Voyager 1",
        Renderable = {
            Type = "RenderableModel",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "voyager-main.obj"
            },
            ColorTexture = "voyager-main.jpg",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Voyager 1"
    },
    {
        Name = "Voyager 1 Antanna",
        Parent = "Voyager 1",
        Renderable = {
            Type = "RenderableModel",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "voyager-antenna.obj"
            },
            ColorTexture = "voyager-antenna.png",
            ModelTransform = RotationMatrix
        },
        GuiPath = "/Solar System/Missions/Voyager 1"
    },

    -- The trails are organized as follows.  The cruise phases can be resolved in relatively
    -- low resolution since they are just straight lines
    -- The encounter phases should be much higher resolution or otherwise artifacts would appear
    {
        Name = "Voyager 1 Trail Cruise Earth-Jupiter",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "VOYAGER 1",
                Observer = "SUN",
                Kernels = Kernels
            },
            Color = { 0.70,0.50,0.20 },
            StartTime = "1977 SEP 05",
            EndTime = "1979 MAR 04",
            SampleInterval = 545 * 2  -- 545 is the number of days between the Start and End time
        },
        GuiName = "/Solar System/Missions/Voyager 1"
    },
    {
        Name = "Voyager 1 Trail Encounter Jupiter",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "VOYAGER 1",
                Observer = "SUN",
                Kernels = Kernels
            },
            Color = { 0.70,0.50,0.20 },
            EnableFade = false,
            StartTime = "1979 MAR 03 23:24:00", -- @TODO: Probably an off-by-one bug in RenderableTrailTrajectory?
            EndTime = "1979 MAR 09",
            SampleInterval = 100
        },
        GuiName = "/Solar System/Missions/Voyager 1"
    },
    {
        Name = "Voyager 1 Trail Cruise Jupiter-Saturn",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "VOYAGER 1",
                Observer = "SUN",
                Kernels = Kernels
            },
            EnableFade = false,
            Color = { 0.70,0.50,0.20 },
            StartTime = "1979 MAR 09",
            EndTime = "1980 NOV 11",
            SampleInterval = 618 * 2  -- 618 is the number of days between the Start and End time
        },
        GuiName = "/Solar System/Missions/Voyager 1"
    },
    {
        Name = "Voyager 1 Trail Encounter Saturn",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "VOYAGER 1",
                Observer = "SUN",
                Kernels = Kernels
            },
            EnableFade = false,
            Color = { 0.70,0.50,0.20 },
            StartTime = "1980 NOV 10 23:08:30",  -- @TODO: Probably an off-by-one bug in RenderableTrailTrajectory?
            EndTime = "1980 NOV 16",
            SampleInterval = 100
        },
        GuiName = "/Solar System/Missions/Voyager 1"
    },
    {
        Name = "Voyager 1 Trail Cruise Saturn-Inf",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailTrajectory",
            Translation = {
                Type = "SpiceTranslation",
                Target = "VOYAGER 1",
                Observer = "SUN",
                Kernels = Kernels
            },
            EnableFade = false,
            Color = { 0.70,0.50,0.20 },
            StartTime = "1980 NOV 16",
            EndTime = "2021 JAN 01",
            SampleInterval = 14656 * 2  -- 14656 is the number of days between the Start and End time
        },
        GuiName = "/Solar System/Missions/Voyager 1"
    }
}
