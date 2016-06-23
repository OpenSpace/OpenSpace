local kiloparsec = 3.086 * 10^19;

return {
    {   
        Name = "Volumetric Milky Way",
        Parent = "Root",
        Ephemeris = {
            Type = "Static",
            Position = {0, 0, 0, 0}
        },
        Renderable = {
             Type = "RenderableGalaxy",
             Translation = {0, 0, 0},
             Volume = {
                 Filename = "${OPENSPACE_DATA}/scene/volumetricmilkyway/milkyway/milkyway_512_512_64_RGBA32F.volume",
                 Dimensions = {512, 512, 64},
                 Size = {50 * kiloparsec, 50 * kiloparsec, 12.5 * kiloparsec},
             },
             Points = {
                  Filename = "${OPENSPACE_DATA}/scene/volumetricmilkyway/milkyway/milkyway_points.binary",
                  Scaling = {kiloparsec, kiloparsec, kiloparsec}
             }
        },
        GuiName = "/VolumetricMilkyWay"
    }
}
