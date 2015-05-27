import os
from subprocess import call
from itertools import product, repeat

# To be called from the build folder in the OpenSpace
modules = os.listdir("../modules")
modules.remove("base")

# Get 2**len(modules) combinatorical combinations of ON/OFF
settings = []
for args in product(*repeat(("ON", "OFF"), len(modules))):
    settings.append(args)

# Create all commands 
cmds = []
for s in settings:
    cmd = ["cmake", "-DGHOUL_USE_DEVIL=NO", "-DOPENSPACE_MODULE_BASE=ON"]

    for m,s in zip(modules, s):
        cmd.append("-DOPENSPACE_MODULE_" + m.upper() + "=" + s)
    cmd.append("..")
    cmds.append(cmd)

# Build cmake and compile
for c in cmds:
    print "CMake:" , cmd
    call(cmd)
    call(["make", "-j4"])

