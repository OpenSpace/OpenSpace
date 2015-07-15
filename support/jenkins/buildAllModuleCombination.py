import os
from subprocess import call
from itertools import product, repeat
import shutil

# To be called from the main OpenSpace
modules = os.listdir("modules")
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
    shutil.rmtree("build", ignore_errors=True)
    shutil.rmtree("bin", ignore_errors=True)
    os.makedirs("build")
    os.chdir("build")
    call(cmd)
    call(["make", "-j4"])
    os.chdir("..")