##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2018                                                                #
#                                                                                        #
# Permission is hereby granted, free of charge, to any person obtaining a copy of this   #
# software and associated documentation files (the "Software"), to deal in the Software  #
# without restriction, including without limitation the rights to use, copy, modify,     #
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to     #
# permit persons to whom the Software is furnished to do so, subject to the following    #
# conditions:                                                                            #
#                                                                                        #
# The above copyright notice and this permission notice shall be included in all copies  #
# or substantial portions of the Software.                                               #
#                                                                                        #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,    #
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A          #
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT     #
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF   #
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE   #
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                          #
##########################################################################################

function(build_webgui_source target_name)
    set(NPM_COMMAND "npm" CACHE STRING "Location of NPM executable unless in PATH.")
    # copy webgui source
    add_custom_command(
        TARGET ${target_name} POST_BUILD
        COMMAND ${NPM_COMMAND} install
#        COMMAND ${NPM_COMMAND} run lint
#        COMMAND ${CMAKE_COMMAND} -E copy_directory ${WEBGUI_MODULE_PATH}/web/dist "$<TARGET_FILE_DIR:${CEF_TARGET}>/gui"
#        COMMAND ${CMAKE_COMMAND} -E copy_directory ${WEBGUI_MODULE_PATH}/web/dist/static "$<TARGET_FILE_DIR:${CEF_TARGET}>/gui/static"
        WORKING_DIRECTORY ${WEBGUI_MODULE_PATH}/web
        VERBATIM
    )
endfunction()
