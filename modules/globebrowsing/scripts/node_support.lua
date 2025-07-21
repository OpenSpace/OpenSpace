openspace.globebrowsing.documentation = {
  {
    Name = "setNodePosition",
    Arguments = {
      { "nodeIdentifier", "String" },
      { "globeIdentifier", "String" },
      { "latitude", "Number" },
      { "longitude", "Number" },
      { "altitude", "Number?" }
    },
    Documentation = [[
      Sets the position of a scene graph node that has a
      [GlobeTranslation](#base_translation_globetranslation) and/or
      [GlobeRotation](#base_rotation_globerotation).

      Usage:
      ```lua
      openspace.globebrowsing.setNodePosition(
        "Scale_StatueOfLiberty", "Earth", 40.000, -117.5, optionalAltitude
      )
      ```

      \\param nodeIdentifier The identifier of the scene graph node to move
      \\param globeIdentifier The identifier of the [RenderableGlobe](#globebrowsing_renderableglobe)
                              that the object should be put on
      \\param latitude The latitude value for the new position, in degrees
      \\param longitude The longitude value for the new position, in degrees
      \\param altitude An optional altitude value for the new position, in meters. If
                       excluded, an altitude of 0 will be used
    ]]
  },
  {
    Name = "setNodePositionFromCamera",
    Arguments = {
      { "nodeIdentifer", "String" },
      { "useAltitude", "Boolean?" }
    },
    Documentation = [[
      Sets the position of a scene graph node that has a
      [GlobeTranslation](#base_translation_globetranslation) and/or
      [GlobeRotation](#base_rotation_globerotation) to match the camera. Only
      uses camera position not rotation. If useAltitude is true, then the position
      will also be updated to the camera's altitude.

      Usage:
      ```lua
      openspace.globebrowsing.setNodePositionFromCamera(
        "Scale_StatueOfLiberty", optionalUseAltitude
      )
      ```

      \\param nodeIdentifier The identifier of the scene graph node to move
      \\param useAltitude If true, the camera's altitude will also be used for the new
                          positions. Otherwise, it will not.
    ]]
  }
}

openspace.globebrowsing.setNodePosition = function (node_identifer, globe_identifier, lat, lon, altitude)
    openspace.setParent(node_identifer, globe_identifier)
    openspace.setPropertyValueSingle("Scene." .. node_identifer .. ".Translation.Globe", globe_identifier);
    openspace.setPropertyValueSingle("Scene." .. node_identifer .. ".Translation.Latitude", lat);
    openspace.setPropertyValueSingle("Scene." .. node_identifer .. ".Translation.Longitude", lon);
    openspace.setPropertyValueSingle("Scene." .. node_identifer .. ".Rotation.Globe", globe_identifier);
    openspace.setPropertyValueSingle("Scene." .. node_identifer .. ".Rotation.Latitude", lat);
    openspace.setPropertyValueSingle("Scene." .. node_identifer .. ".Rotation.Longitude", lon);
    if (altitude) then
        openspace.setPropertyValueSingle("Scene." .. node_identifer .. ".Translation.Altitude", altitude);
        openspace.setPropertyValueSingle("Scene." .. node_identifer .. ".Translation.Altitude", altitude);
    end
end

openspace.globebrowsing.setNodePositionFromCamera = function (node_identifer, use_altitude)
    local lat, lon, alt = openspace.globebrowsing.geoPositionForCamera();
    local camera = openspace.navigation.getNavigationState();
    openspace.setParent(node_identifer, camera.Anchor)
    openspace.setPropertyValueSingle("Scene." .. node_identifer .. ".Translation.Globe", camera.Anchor);
    openspace.setPropertyValueSingle("Scene." .. node_identifer .. ".Translation.Latitude", lat);
    openspace.setPropertyValueSingle("Scene." .. node_identifer .. ".Translation.Longitude", lon);
    openspace.setPropertyValueSingle("Scene." .. node_identifer .. ".Rotation.Globe", camera.Anchor);
    openspace.setPropertyValueSingle("Scene." .. node_identifer .. ".Rotation.Latitude", lat);
    openspace.setPropertyValueSingle("Scene." .. node_identifer .. ".Rotation.Longitude", lon);
    if (use_altitude) then
        openspace.setPropertyValueSingle("Scene." .. node_identifer .. ".Translation.Altitude", alt);
        openspace.setPropertyValueSingle("Scene." .. node_identifer .. ".Translation.Altitude", alt);
    end
end
