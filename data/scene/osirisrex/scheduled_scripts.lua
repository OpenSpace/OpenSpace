function enable(name, enabled)
	return "openspace.setPropertyValue('" .. name .. ".renderable.enabled', " .. (enabled and "true" or "false") .. ");";
end

function scheduleEnabled(time, name, enabled)
	return 
	{
		Time = time,
		ReversibleLuaScript = {
			Forward = enable(name, enabled),
			Backward = enable(name, not enabled)
		}
	}
end

return 
{
	scheduleEnabled("2016 SEP 08 23:05:01", "OsirisRexTrailEarth", true),
	scheduleEnabled("2016 SEP 09 00:00:00", "OsirisRexTrailSolarSystem", true),
	scheduleEnabled("2016 SEP 09 02:00:00", "OsirisRexTrailEarth", false),
	scheduleEnabled("2018 OCT 11 00:00:00", "OsirisRexTrailBennu", true),
	scheduleEnabled("2018 OCT 15 00:00:00", "OsirisRexTrailSolarSystem", false),
	scheduleEnabled("2019 AUG 01 00:00:00", "OsirisRexTrailSolarSystem", true),
	scheduleEnabled("2019 AUG 01 00:00:00", "OsirisRexTrailBennu", false),
}