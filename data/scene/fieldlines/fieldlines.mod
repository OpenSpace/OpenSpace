local file1 = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_closed_seeds_all.txt';
local file2 = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_open_north_all.txt';
local file3 = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_open_south_all.txt';
local file4 = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_solar_wind_all.txt';
local file5 = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_separatrix_seeds_all.txt';

local volumeFile = '${OPENSPACE_DATA}/batsrus.cdf';

return {
    {
        Name = "Fieldlines1",
        Parent = "Earth",
        Renderable = {
            Type = "RenderableFieldlines",
            VectorField = {
                Type = "VolumeKameleon",
                File = volumeFile,
                Model = "BATSRUS",
                Variables = {"bx", "by", "bz"}
            },
            Fieldlines = {
                Stepsize = 1,
                Classification = true,
            },
            SeedPoints = {
                Type = "File",
                File = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_closed_seeds_all.txt';
            }
        }
    },
    {   
        Name = "Fieldlines2",
        Parent = "Earth",
        Renderable = {
            Type = "RenderableFieldlines",
            VectorField = {
                Type = "VolumeKameleon",
                File = volumeFile,
                Model = "BATSRUS",
                Variables = {"bx", "by", "bz"}
            },
            Fieldlines = {
                Stepsize = 1,
                Classification = true
            },
            SeedPoints = {
                Type = "File",
                File = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_open_north_all.txt'
            }
        }
    },
    {   
        Name = "Fieldlines3",
        Parent = "Earth",
        Renderable = {
            Type = "RenderableFieldlines",
            VectorField = {
                Type = "VolumeKameleon",
                File = volumeFile,
                Model = "BATSRUS",
                Variables = {"bx", "by", "bz"}
            },
            Fieldlines = {
                Stepsize = 1,
                Classification = true
            },
            SeedPoints = {
                Type = "File",
                File = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_open_south_all.txt'
            }
        }
    },
    {   
        Name = "Fieldlines4",
        Parent = "Earth",
        Renderable = {
            Type = "RenderableFieldlines",
            VectorField = {
                Type = "VolumeKameleon",
                File = volumeFile,
                Model = "BATSRUS",
                Variables = {"bx", "by", "bz"}
            },
            Fieldlines = {
                Stepsize = 1,
                Classification = true
            },
            SeedPoints = {
                Type = "File",
                File = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_solar_wind_all.txt'
            }
        }
    },
    {   
        Name = "Fieldlines5",
        Parent = "Earth",
        Renderable = {
            Type = "RenderableFieldlines",
            VectorField = {
                Type = "VolumeKameleon",
                File = volumeFile,
                Model = "BATSRUS",
                Variables = {"bx", "by", "bz"}
            },
            Fieldlines = {
                Stepsize = 1,
                Classification = true
            },
            SeedPoints = {
                Type = "File",
                File = '${OPENSPACE_DATA}/scene/fieldlines/bats_seeds/BATS_R_US_separatrix_seeds_all.txt'
            }
        }
    }
}