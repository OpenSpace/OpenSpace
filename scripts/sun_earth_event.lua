-- Load the common helper functions
dofile(openspace.absPath('${SCRIPTS}/common.lua'))

openspace.clearKeys()
helper.setCommonKeys()
helper.setDeltaTimeKeys({
--  1           2           3           4           5           6           7           8           9           0
--------------------------------------------------------------------------------------------------------------------------
--  1s          2s          5s          10s         30s         1m          2m          5m          10m         30m
    1,          2,          5,          10,         30,         60,         120,        300,        600,        1800,

--  1h          2h          3h          6h          12h         1d          2d          4d          1w          2w
    3600,       7200,       10800,      21600,      43200,      86400,      172800,     345600,     604800,     1209600,

--  1mo         2mo         3mo         6mo         1yr         2y          5y          10y         20y         50y
    2592000,    5184000,    7776000,    15552000,   31536000,   63072000,   157680000,  315360000,  630720000,  1576800000
})
--  OBS: One month (1mo) is approximated by 30 days.


-- Spacecraft imagery stuff
openspace.bindKey("a", "openspace.setPropertyValue('Interaction.origin', 'Sun')", "Sets the focus of the camera to the Sun")
openspace.bindKey("s", "openspace.setPropertyValue('Interaction.origin', 'SDO')", "Sets the focus of the camera to SDO")
--openspace.bindKey("d", "openspace.setPropertyValue('Interaction.origin', 'Stereo A')", "Sets the focus of the camera to Stereo A")
--openspace.bindKey("f", "openspace.setPropertyValue('Interaction.origin', 'SOHO')", "Sets the focus of the camera to SOHO")
--openspace.bindKey("g", "openspace.setPropertyValue('Interaction.origin', 'ISS')", "Sets the focus of the camera to ISS")
-- Time
openspace.bindKey("h", "openspace.time.setDeltaTime(1500)", "Set delta time to 1500")
openspace.bindKey("j", "openspace.time.setTime('2012 JUL 01 00:00:00.000')", "Sets time to 2012 07 01 00:00:00.000")
openspace.bindKey("k", "openspace.time.setTime('2012 JUL 04 00:00:00.000')", "Sets time to 2012 07 04 00:00:00.000")
openspace.bindKey("l", "openspace.time.setTime('2012 JUL 12 19:10:15.000')", "Sets time to 2012 07 12 19:10:15.000")

openspace.bindKey("Ctrl+d",
    "openspace.setPropertyValue('SolarImagery_Stereo_StereoA_Image_EUV.renderable.currentActiveInstrumentProperty', 'SECCHI_EUVI_195');" ..
    "openspace.setPropertyValue('SolarImagery_Stereo_StereoB_Image_EUV.renderable.currentActiveInstrumentProperty', 'SECCHI_EUVI_195');" ..
    "openspace.setPropertyValue('SolarImagery_SDO_Image_AIA.renderable.currentActiveInstrumentProperty', 'AIA_AIA_193');",
    "Sets all EUV to 195"
    --"openspace.setPropertyValue('Pluto.renderable.clearAllProjections', true);",
)

openspace.bindKey("Alt+d",
    "openspace.setPropertyValue('SolarImagery_Stereo_StereoA_Image_EUV.renderable.currentActiveInstrumentProperty', 'SECCHI_EUVI_304');" ..
    "openspace.setPropertyValue('SolarImagery_Stereo_StereoB_Image_EUV.renderable.currentActiveInstrumentProperty', 'SECCHI_EUVI_304');" ..
    "openspace.setPropertyValue('SolarImagery_SDO_Image_AIA.renderable.currentActiveInstrumentProperty', 'AIA_AIA_304');",
    "Sets all EUV to 304"
    --"openspace.setPropertyValue('Pluto.renderable.clearAllProjections', true);",
)

-- Activate Stereo A + B EUV + Orbits + Labels + Frustums
openspace.bindKey("Shift+d",
    --"openspace.setPropertyValue('SolarImagery_Stereo_O*.renderable.enabled', true);",
    --"openspace.setPropertyValue('SolarImagery_Stereo_L*.renderable.enabled', true);",
    --helper.renderable.toggle('SolarImagery_Stereo_*_Image_*')
    helper.renderable.toggle('SolarImagery_Stereo_O_StereoA_Trail') ..
    helper.renderable.toggle('SolarImagery_Stereo_O_StereoB_Trail') ..
    helper.renderable.toggle('SolarImagery_Stereo_L_StereoA_Marker') ..
    helper.renderable.toggle('SolarImagery_Stereo_L_StereoB_Marker') ..
    "openspace.setPropertyValue('SolarImagery_Stereo_StereoA_Image_EUV.renderable.currentActiveInstrumentProperty', 'SECCHI_EUVI_195');" ..
    "openspace.setPropertyValue('SolarImagery_Stereo_StereoB_Image_EUV.renderable.currentActiveInstrumentProperty', 'SECCHI_EUVI_195');" ..
    "openspace.setPropertyValue('SolarImagery_*_Image*.renderable.moveFactor', 0.9)" ..
    helper.renderable.toggle('SolarImagery_Stereo_StereoA_Image_EUV') ..
    helper.renderable.toggle('SolarImagery_Stereo_StereoB_Image_EUV') ..
    helper.property.invert('SolarImagery_Stereo_StereoA_Image_EUV.renderable.enableFrustum') ..
    helper.property.invert('SolarImagery_Stereo_StereoB_Image_EUV.renderable.enableFrustum'),
    --helper.renderable.toggle('SolarImagery_Stereo_StereoA_Image_COR'),
    --helper.rendearble.toggle('SolarImagery_Stereo_StereoB_Image_COR'),
    "Activate Stereo A + B + EUV + Orbits + Labels + Frustums"
)

-- PFSS ON
-- Turn off SDO Frustum, stereo frustums, switch to HMI magnetogram, set plane opacity to 1
openspace.bindKey("Ctrl+d",
    helper.property.invert('SolarImagery_SDO_Image_AIA.renderable.enableFrustum') ..
    helper.property.invert('SolarImagery_Stereo_StereoA_Image_EUV.renderable.enableFrustum') ..
    helper.property.invert('SolarImagery_Stereo_StereoB_Image_EUV.renderable.enableFrustum') ..
    helper.renderable.toggle('SolarImagery_Stereo_StereoA_Image_EUV') ..
    helper.renderable.toggle('SolarImagery_Stereo_StereoB_Image_EUV') ..
    "openspace.setPropertyValue('SolarImagery_SDO_Image_AIA.renderable.currentActiveInstrumentProperty', 'HMI_HMI_magnetogram');" ..
    "openspace.setPropertyValue('SolarImagery_*_Image*.renderable.planeOpacity', 100)",
    "Turn off SDO Frustum, stereo frustums, switch to HMI magnetigram, set plane opacity to 1"
)

-- Frustums
openspace.bindKey("f",
    helper.property.invert('SolarImagery_SDO_Image_AIA.renderable.enableFrustum') ..
    helper.property.invert('SolarImagery_Stereo_StereoA_Image_EUV.renderable.enableFrustum') ..
    helper.property.invert('SolarImagery_Stereo_StereoA_Image_COR.renderable.enableFrustum') ..
    helper.property.invert('SolarImagery_Stereo_StereoB_Image_EUV.renderable.enableFrustum') ..
    helper.property.invert('SolarImagery_Stereo_StereoB_Image_COR.renderable.enableFrustum') ..
    helper.property.invert('SolarImagery_Soho_Image_C2.renderable.enableFrustum') ..
    helper.property.invert('SolarImagery_Soho_Image_C3.renderable.enableFrustum'),
    "Invert display frustums"
)

-- SDO
openspace.bindKey("Shift+s",
    --helper.renderable.toggle('SolarImagery_SDO_Image_AIA') ..
    helper.renderable.toggle('SolarImagery_SDO_Trail') ..
    helper.renderable.toggle('SolarImagery_SDO_Marker'),
    "Toggle SDO Trail and marker SDO"
)

-- Toggle SDO Marker, turn on SDO Frustum, move out planes, turn on plane opacity,  set to continuum
openspace.bindKey("Ctrl+s",
    helper.renderable.toggle('SolarImagery_SDO_Marker') ..
    helper.renderable.toggle('SolarImagery_EarthMarker_Marker') ..
    helper.property.invert('SolarImagery_SDO_Image_AIA.renderable.enableFrustum') ..
    --"openspace.setPropertyValue('SolarImagery_*_Image*.renderable.moveFactor', 0.85)" ..
    helper.renderable.toggle('SolarImagery_Stereo_StereoA_Image_EUV') ..
    helper.renderable.toggle('SolarImagery_Stereo_StereoB_Image_EUV') ..
    "openspace.setPropertyValue('SolarImagery_*_Image*.renderable.planeOpacity', 100)" ..
    "openspace.setPropertyValue('SolarImagery_SDO_Image_AIA.renderable.currentActiveInstrumentProperty', 'HMI_HMI_continuum');",
    "Toggle SDO Marker"
)

-- Toggle SOHO
openspace.bindKey("Shift+f",
    helper.renderable.toggle('SolarImagery_Soho_Image_C2') ..
    helper.renderable.toggle('SolarImagery_Soho_Image_C3') ..
    helper.property.invert('SolarImagery_Soho_Image_C2.renderable.enableFrustum') ..
    helper.property.invert('SolarImagery_Soho_Image_C3.renderable.enableFrustum') ..
    helper.property.invert('SolarImagery_SDO_Image_AIA.renderable.enableFrustum') ..
    helper.renderable.toggle('SolarImagery_SDO_Marker') ..
    helper.renderable.toggle('SolarImagery_Soho_Marker') ..
    helper.renderable.toggle('SolarImagery_Soho_Trail'),
    "Toggle SOHO"
)

openspace.bindKey("Ctrl+g",
    helper.renderable.toggle('SolarImagery_Stereo_O_StereoA_Trail') ..
    helper.renderable.toggle('SolarImagery_Stereo_O_StereoB_Trail') ..
    helper.renderable.toggle('SolarImagery_Soho_Marker') ..
    helper.renderable.toggle('SolarImagery_Soho_Trail') ..
    "openspace.setPropertyValue('SolarImagery_SDO_Image_AIA.renderable.currentActiveInstrumentProperty', 'HMI_HMI_magnetogram');" ..
    "openspace.time.setTime('2012 JUL 12 19:10:15.000')" ..
    -- TURN ON ENLIL!!!!!!!!!
    helper.renderable.toggle('SolarImagery_Soho_Image_C3') ..
    "openspace.setPropertyValue('FL_PFSS.renderable.enabled', true);",
    "Turn off stereo a, b, soho trails, turn off soho marker and switch to hmi continuum and turn on PFSS, set to PFSS Time"
)

-- Make hot KEY
openspace.bindKey("Shift+p",
    --"openspace.time.setTime('2012 JUL 12 19:10:15.000')" ..
    helper.renderable.toggle('FL_PFSS'),
    "Toggle PFSS"
)

openspace.bindKey("Ctrl+e",
    helper.renderable.toggle('SolarImagery_EarthMarker_Marker') ..
    helper.renderable.toggle('SolarImagery_SDO_Marker'),
    "Toggle Earth and SDO Marker"
)

openspace.bindKey("Ctrl+1",
    "openspace.setPropertyValue('Sun_Projection.renderable.loopId', 0);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', false);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', true);",
    "Activate Loop 1"
)

openspace.bindKey("Ctrl+2",
    "openspace.setPropertyValue('Sun_Projection.renderable.loopId', 1);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', false);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', true);",
    "Activate Loop 2"
)

openspace.bindKey("Ctrl+3",
    "openspace.setPropertyValue('Sun_Projection.renderable.loopId', 2);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', false);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', true);",
    "Activate Loop 3"
)

openspace.bindKey("Ctrl+4",
    "openspace.setPropertyValue('Sun_Projection.renderable.loopId', 3);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', false);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', true);",
    "Activate Loop 4"
)

openspace.bindKey("Ctrl+5",
    "openspace.setPropertyValue('Sun_Projection.renderable.loopId', 4);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', false);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', true);",
    "Activate Loop 5"
)

openspace.bindKey("Ctrl+6",
    "openspace.setPropertyValue('Sun_Projection.renderable.loopId', 5);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', false);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', true);",
    "Activate Loop 6"
)
openspace.bindKey("Ctrl+7",
    "openspace.setPropertyValue('Sun_Projection.renderable.loopId', 6);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', false);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', true);",
    "Activate Loop 7"
)

openspace.bindKey("Ctrl+8",
    "openspace.setPropertyValue('Sun_Projection.renderable.loopId', 7);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', false);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', true);",
    "Activate Loop 8"
)

openspace.bindKey("Ctrl+9",
    "openspace.setPropertyValue('Sun_Projection.renderable.loopId', 8);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', false);" ..
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', true);",
    "Activate Loop 9"
)

openspace.bindKey("Shift+g",
    "openspace.setPropertyValue('Sun_Projection.renderable.activateLooping', false);",
    "Stop Looping"
)
