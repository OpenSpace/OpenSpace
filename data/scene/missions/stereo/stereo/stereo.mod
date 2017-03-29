return {
    -- MODEL PLAY - NOT USED
    -- {
    --     Name = "Stereo",
    --     Parent = "SolarSystemBarycenter",
    --     Renderable = {
    --         Type = "RenderableModel",
    --         Body = "STEREO AHEAD",
    --         Geometry = {
    --             Type = "MultiModelGeometry",
    --             GeometryFile = "Stereo-2016-comp.lwo"
    --         },
    --         Textures = {
    --             Type = "simple",
    --             Color = "textures/tex_01.png"
    --         },
    --     },
    --     Transform = {
    --         Translation = {
    --             Type = "SpiceTranslation",
    --             Body = "STEREO AHEAD",
    --             Observer = "SUN",
    --             Kernels = "${OPENSPACE_DATA}/spice/STEREO-A_merged.bsp"
    --         }
    --     }
    -- },
    -- Stereo A Trail
    {
        Name = "StereoA",
        Parent = "SolarSystemBarycenter",
        Transform = {
            Translation = {
                Type = "SpiceTranslation",
                Body = "STEREO AHEAD",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/STEREO-A_merged.bsp"
            },
            -- Using internal reference frame
            -- Rotation = {
            --     Type = "SpiceRotation",
            --     SourceFrame = "STASCPNT",
            --     DestinationFrame = "GALACTIC",
            --     Kernels = "${OPENSPACE_DATA}/spice/stereo_rtn.tf"
            -- },
            Rotation = {
                Type = "SpiceRotation",
                SourceFrame = "ECLIPJ2000",
                DestinationFrame = "GALACTIC",
                Kernels = "${OPENSPACE_DATA}/spice/stereo_rtn.tf"
            },
        }
    },
    {
        Name = "Stereo A trail",
        Parent = "SolarSystemBarycenter",
        Renderable = {
            Type = "RenderableTrailOrbit",
            Translation = {
                Type = "SpiceTranslation",
                Body = "STEREO AHEAD",
                Observer = "SUN",
                Kernels = "${OPENSPACE_DATA}/spice/STEREO-A_merged.bsp"
            },
            Color = { 1.0, 1.0, 1.0 },
            Period = 365.242,
            Resolution = 1000
        }
    },
    -- Plane in front of STEREO
    {
        Name = "Stereo Plane",
        Parent = "StereoA",
        Renderable = {
            Type = "RenderableSpacecraftCameraPlane",
            Target = "Sun",
            Size = {23.454, 9.949},
            Origin = "Center",
            -- Dummy texture
            Texture = "images/stereo2.png",
        },
        -- Transform = {
        --     Translation = {
        --          Type = "StaticTranslation",
        --          Position = {0, -300000000, 0}
        --     },
        -- }
    }
}
