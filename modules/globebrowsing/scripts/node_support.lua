openspace.globebrowsing.documentation = {
  {
    Name = "setNodePosition",
    Arguments = {{ "nodeIdentifer", "String" }, { "globeIdentifier", "String" }, { "latitude", "Number" }, { "longitude", "Number" }, { "altitude", "Number" }},
    Documentation =
        "Sets the position of a SceneGraphNode that has GlobeTranslation/GlobeRotations. " ..
        "Usage: openspace.globebrowsing.setNodePosition(" ..
        "\"Scale_StatueOfLiberty\", \"Earth\", 40.000, -117.5, optionalAltitude)"
  },
  {
    Name = "setNodePositionFromCamera",
    Arguments = {{ "nodeIdentifer", "String" }, { "useAltitude", "Boolean" }},
    Documentation =
        "Sets the position of a SceneGraphNode that has GlobeTranslation/GlobeRotations" ..
        " to match the camera. Only uses camera position not rotation. If useAltitude" ..
        " is true, then the position will also be updated to the camera's altitude." ..
        "Usage: openspace.globebrowsing.setNodePositionFromCamera(" ..
        "\"Scale_StatueOfLiberty\", optionalUseAltitude)"
  }
}

openspace.globebrowsing.setNodePosition = function (node_identifer, globe_identifier, lat, lon, altitude)
    openspace.setParent(node_identifer, globe_identifier)
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Globe', globe_identifier);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Latitude', lat);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Longitude', lon);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Rotation.Globe', globe_identifier);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Rotation.Latitude', lat);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Rotation.Longitude', lon);
    if (altitude) then
        openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Altitude', altitude);
        openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Altitude', altitude);
    end
end

openspace.globebrowsing.setNodePositionFromCamera = function (node_identifer, use_altitude)
    local lat, lon, alt = openspace.globebrowsing.geoPositionForCamera();
    local camera = openspace.navigation.getNavigationState();
    openspace.setParent(node_identifer, camera.Anchor)
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Globe', camera.Anchor);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Latitude', lat);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Longitude', lon);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Rotation.Globe', camera.Anchor);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Rotation.Latitude', lat);
    openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Rotation.Longitude', lon);
    if (use_altitude) then
        openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Altitude', alt);
        openspace.setPropertyValueSingle('Scene.' .. node_identifer .. '.Translation.Altitude', alt);
    end
end
