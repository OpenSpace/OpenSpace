-- This file contains a list of default servers for globes that can be used in the GUI
-- to easily add layers

return {
    Earth = {
        {
            Name = "GIBS",
            URL  = "https://gibs.earthdata.nasa.gov/twms/epsg4326/best/twms.cgi?request=GetTileService"
        },
    },
    Moon = {
        {
            Name = "OnMoon LMMP",
            URL  = "https://onmoon.lmmp.nasa.gov/wms.cgi?request=GetCapabilities"
        },
    },
    Mercury = {
        {
            Name = "USGS Mercury",
            URL  = "https://planetarymaps.usgs.gov/cgi-bin/mapserv?map=/maps/mercury/mercury_simp_cyl.map&service=WMS&request=GetCapabilities"
        },
    },
    Callisto = {
        {
            Name = "USGS Callisto",
            URL  = "https://planetarymaps.usgs.gov/cgi-bin/mapserv?map=/maps/jupiter/callisto_simp_cyl.map&service=WMS&request=GetCapabilities"
        },
    },
    Deimos = {
        {
            Name = "USGS Deimos",
            URL  = "https://planetarymaps.usgs.gov/cgi-bin/mapserv?map=/maps/mars/deimos_simp_cyl.map&service=WMS&request=GetCapabilities"
        },
    },
    Pluto = {
        {
            Name = "USGS Pluto",
            URL = "https://planetarymaps.usgs.gov/cgi-bin/mapserv?map=/maps/pluto/pluto_simp_cyl.map&service=WMS&request=GetCapabilities"
        }
    },
    Charon = {
        {
            Name = "USGS Charon",
            URL = "https://planetarymaps.usgs.gov/cgi-bin/mapserv?map=/maps/pluto/charon_simp_cyl.map&service=WMS&request=GetCapabilities"
        }
    }
}


-- https://astrowebmaps.wr.usgs.gov/webmapatlas/Layers/maps.html