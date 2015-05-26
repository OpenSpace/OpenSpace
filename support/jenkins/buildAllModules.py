import os
from subprocess import call

modules = os.listdir("../../modules")

cmd = ["cmake"]
cmd.append("-DGHOUL_USE_DEVIL=OFF")
for m in modules:
    cmd.append("-DOPENSPACE_MODULE_" + m.upper() + "=ON")

cmd.append(".")
call(cmd)