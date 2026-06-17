registerFunction(
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
      [GlobeTranslation](#base_translation_globe) and/or
      [GlobeRotation](#base_rotation_globe).

      Usage:
      ```lua
      openspace.globebrowsing.setNodePosition(
        "Scale_StatueOfLiberty", "Earth", 40.000, -117.5, optionalAltitude
      )
      ```

      \\param nodeIdentifier The identifier of the scene graph node to move
      \\param globeIdentifier The identifier of the
              [RenderableGlobe](#globebrowsing_renderable_globe) that the object should be
              put on
      \\param latitude The latitude value for the new position, in degrees
      \\param longitude The longitude value for the new position, in degrees
      \\param altitude An optional altitude value for the new position, in meters. If
              excluded, an altitude of 0 will be used
    ]],
    Function = function(node_identifier, globe_identifier, lat, lon, altitude)
      openspace.setParent(node_identifier, globe_identifier)
      openspace.setPropertyValueSingle("Scene." .. node_identifier .. ".Translation.Globe", globe_identifier);
      openspace.setPropertyValueSingle("Scene." .. node_identifier .. ".Translation.Latitude", lat);
      openspace.setPropertyValueSingle("Scene." .. node_identifier .. ".Translation.Longitude", lon);
      openspace.setPropertyValueSingle("Scene." .. node_identifier .. ".Rotation.Globe", globe_identifier);
      openspace.setPropertyValueSingle("Scene." .. node_identifier .. ".Rotation.Latitude", lat);
      openspace.setPropertyValueSingle("Scene." .. node_identifier .. ".Rotation.Longitude", lon);
      if (altitude) then
          openspace.setPropertyValueSingle("Scene." .. node_identifier .. ".Translation.Altitude", altitude);
      end
  end
  }
)

registerFunction(
  {
    Name = "setNodePositionFromCamera",
    Arguments = {
      { "nodeIdentifer", "String" },
      { "useAltitude", "Boolean?" }
    },
    Documentation = [[
      Sets the position of a scene graph node that has a
      [GlobeTranslation](#base_translation_globe) and/or
      [GlobeRotation](#base_rotation_globe) to match the camera. Only uses camera position
      not rotation. If useAltitude is true, then the position will also be updated to the
      camera's altitude.

      Usage:
      ```lua
      openspace.globebrowsing.setNodePositionFromCamera(
        "Scale_StatueOfLiberty", optionalUseAltitude
      )
      ```

      \\param nodeIdentifier The identifier of the scene graph node to move
      \\param useAltitude If true, the camera's altitude will also be used for the new
              positions. Otherwise, it will not
    ]],
    Function = function(node_identifier, use_altitude)
      local lat, lon, alt = openspace.globebrowsing.geoPositionForCamera();
      local camera = openspace.navigation.getNavigationState();
      openspace.setParent(node_identifier, camera.Anchor)
      openspace.setPropertyValueSingle("Scene." .. node_identifier .. ".Translation.Globe", camera.Anchor);
      openspace.setPropertyValueSingle("Scene." .. node_identifier .. ".Translation.Latitude", lat);
      openspace.setPropertyValueSingle("Scene." .. node_identifier .. ".Translation.Longitude", lon);
      openspace.setPropertyValueSingle("Scene." .. node_identifier .. ".Rotation.Globe", camera.Anchor);
      openspace.setPropertyValueSingle("Scene." .. node_identifier .. ".Rotation.Latitude", lat);
      openspace.setPropertyValueSingle("Scene." .. node_identifier .. ".Rotation.Longitude", lon);
      if (use_altitude) then
          openspace.setPropertyValueSingle("Scene." .. node_identifier .. ".Translation.Altitude", alt);
      end
  end
  }
)
