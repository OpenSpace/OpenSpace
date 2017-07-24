dofile(openspace.absPath('${SCRIPTS}/config_mrv.lua'))
openspace.setPropertyValue(MRV_CONFIG_ENLIL_RENDERABLE .. ".memoryBudget", 128)
openspace.setPropertyValue(MRV_CONFIG_ENLIL_RENDERABLE .. ".streamingBudget", 512)

openspace.setPropertyValue(MRV_CONFIG_ENLIL_RENDERABLE .. ".scalingExponent", 12)
openspace.setPropertyValue(MRV_CONFIG_ENLIL_RENDERABLE .. ".scaling", {0.7, 0.7, 0.7})


