-- Load the common helper functions
dofile(openspace.absPath('${SCRIPTS}/common.lua'))

return
{
	helper.scheduledScript.enable("2016 SEP 08 23:05:01", "OsirisRexTrailEarth", true),
	helper.scheduledScript.enable("2016 SEP 09 00:00:00", "OsirisRexTrailSolarSystem", true),
	helper.scheduledScript.enable("2016 SEP 09 02:00:00", "OsirisRexTrailEarth", false),
	helper.scheduledScript.enable("2018 OCT 11 00:00:00", "OsirisRexTrailBennu", true),
	helper.scheduledScript.enable("2018 OCT 15 00:00:00", "OsirisRexTrailSolarSystem", false),
	helper.scheduledScript.enable("2019 AUG 01 00:00:00", "OsirisRexTrailSolarSystem", true),
	helper.scheduledScript.enable("2019 AUG 01 00:00:00", "OsirisRexTrailBennu", false),
}