asset.meta = {  Name = [[Default]],  Version = [[1.0]],  Description = [[Default OpenSpace Profile. Adds Earth satellites not contained in other profiles.]],  Author = [[OpenSpace Team]],  URL = [[https://www.openspaceproject.com]],  License = [[MIT License]],}asset.require("base");
asset.require("scene/solarsystem/planets/earth/earth");
asset.require("scene/solarsystem/planets/earth/fieldlinesCDF");
asset.onInitialize(function()
openspace.bindKey("S", [[local list = openspace.getProperty('{earth_satellites}.Renderable.Enabled'); for _,v in pairs(list) do openspace.setPropertyValueSingle(v, not openspace.getPropertyValue(v)) end]], [[Toggle trails on or off for satellites around Earth]], [[Toggle satellite trails]], [[/Earth]]);
openspace.bindKey("I", [[openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'ISS');openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);]], [[Refocuses the camera on the ISS]], [[Focus ISS]], [[/Earth]]);
openspace.bindKey("HOME", [[openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Aim', '');openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.Anchor', 'Earth')openspace.setPropertyValueSingle('NavigationHandler.OrbitalNavigator.RetargetAnchor', nil);]], [[Retargets the camera on Earth]], [[Focus on Earth]], [[/Earth]]);
openspace.time.setTime("2000-01-01T08:00:00")
openspace.time.setDeltaTimeSteps({ 1 ,5 ,30 ,60 ,300 ,1800 ,3600 ,43200 ,86400 ,604800 ,1209600 ,2592000 ,5184000 ,7776000 ,15552000 ,31536000 ,63072000 ,157680000 ,315360000 ,630720000 , });
openspace.markInterestingNodes({ [[Earth]],[[Moon]],[[Sun]], });
openspace.setPropertyValue("{earth_satellites}.Renderable.Enabled", false);
openspace.globebrowsing.goToGeo([[Earth]], 58.5877, 16.1924, 17000000);

end)
