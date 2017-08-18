-- This file contains a list of default servers for globes that can be used in the GUI
-- to easily add layers

return {
    Earth = {
        "https://gibs.earthdata.nasa.gov/twms/epsg4326/best/twms.cgi?request=GetTileService"
    },
    Moon = {
        "https://onmoon.lmmp.nasa.gov/wms.cgi?request=GetCapabilities"
    },
    Mercury = {
        "https://planetarymaps.usgs.gov/cgi-bin/mapserv?map=/maps/mercury/mercury_simp_cyl.map&service=WMS&request=GetCapabilities"
    },
    Callisto = {
        "https://planetarymaps.usgs.gov/cgi-bin/mapserv?map=/maps/jupiter/callisto_simp_cyl.map&service=WMS&request=GetCapabilities"
    },
    Deimos = {
        "https://planetarymaps.usgs.gov/cgi-bin/mapserv?map=/maps/mars/deimos_simp_cyl.map&service=WMS&request=GetCapabilities"
    }
}


-- https://astrowebmaps.wr.usgs.gov/webmapatlas/Layers/maps.html