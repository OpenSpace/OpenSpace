if(OS_MACOSX OR WIN32)
	set(DEFAULT_MODULE ON)
else()
	# WebBrowser is not available on Linux
	set(DEFAULT_MODULE OFF)
endif()