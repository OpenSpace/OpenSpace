set (OPENSPACE_DEPENDENCIES
  debugging
)

# Don't **actually** add it as a dependency. Only mark it as a dependency if it was already enabled anyway. We need to do this as the GlobeBrowsing partially depends on being able to detect if OpenSpace was compiled with Spout support or not
if (OPENSPACE_MODULE_SPOUT)
  set(OPENSPACE_DEPENDENCIES ${OPENSPACE_DEPENDENCIES} spout)
endif (OPENSPACE_MODULE_SPOUT)

set (DEFAULT_MODULE ON)
