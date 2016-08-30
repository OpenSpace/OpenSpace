-- Load the common helper functions
dofile(openspace.absPath('${SCRIPTS}/common.lua'))

return
{
	helper.scheduledScript.setEnabled("2016 SEP 08 23:05:00", "OsirisRexTrailSolarSystem", false),
	helper.scheduledScript.setEnabled("2016 SEP 08 23:05:00", "OsirisRexTrailBennu", false),

	helper.scheduledScript.reversible.setEnabled("2016 SEP 08 23:05:01", "OsirisRexTrailEarth", true),
	helper.scheduledScript.reversible.setEnabled("2016 SEP 09 00:00:00", "OsirisRexTrailSolarSystem", true),
	helper.scheduledScript.reversible.setEnabled("2016 SEP 09 02:00:00", "OsirisRexTrailEarth", false),
	helper.scheduledScript.reversible.setEnabled("2018 OCT 11 00:00:00", "OsirisRexTrailBennu", true),
	helper.scheduledScript.reversible.setEnabled("2018 OCT 15 00:00:00", "OsirisRexTrailSolarSystem", false),
	helper.scheduledScript.reversible.setEnabled("2019 AUG 01 00:00:00", "OsirisRexTrailSolarSystem", true),
	helper.scheduledScript.reversible.setEnabled("2019 AUG 01 00:00:00", "OsirisRexTrailBennu", false),
}