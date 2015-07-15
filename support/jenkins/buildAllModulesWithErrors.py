import os
from subprocess import call

# To be called from the build folder in the OpenSpace
modules = os.listdir("../modules")

cmd = ["cmake"]
cmd.append("-DGHOUL_USE_DEVIL=OFF")
cmd.append("-OPENSPACE_WARNINGS_AS_ERRORS=ON")
for m in modules:
    cmd.append("-DOPENSPACE_MODULE_" + m.upper() + "=ON")

cmd.append("..")
call(cmd)