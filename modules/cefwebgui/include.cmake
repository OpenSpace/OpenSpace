if(OS_MACOSX OR WIN32)
	set(DEFAULT_MODULE ON)
else()
	# CefWebGui is not available on Linux
	set(DEFAULT_MODULE OFF)
endif()

set(OPENSPACE_DEPENDENCIES
    webbrowser
    webgui
    server
)
