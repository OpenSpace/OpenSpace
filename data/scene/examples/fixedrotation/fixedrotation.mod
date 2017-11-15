return {
    -- Earth barycenter module
    {
        Name = "Fixed Rotation",
        Parent = "EarthBarycenter",
        Transform = {
            Rotation = {
                Type = "FixedRotation",
                Attached = "Fixed Rotation",
                XAxis = { 0.0, 1.0, 0.0 },
                XAxisOrthogonal = true,
                YAxis = "EarthBarycenter"
            },
            Translation = {
                Type = "SpiceTranslation",
                Target = "MOON",
                Observer = "EARTH BARYCENTER",
                Kernels = "${OPENSPACE_DATA}/spice/de430_1850-2150.bsp"
            },
            Scale = {
                Type = "StaticScale",
                Scale = 10000000
            }
        },
        Renderable = {
            Type = "RenderableModel",
            Geometry = {
                Type = "MultiModelGeometry",
                GeometryFile = "teapot.obj",
                -- Magnification = 4,
            },
            ColorTexture = "placeholder.png",
        }
    }
}
