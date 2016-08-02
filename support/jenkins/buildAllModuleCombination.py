import os
from subprocess import call
from itertools import product, repeat
import shutil
import sys
import re
import random

DefaultEnabledModules = [
    "base"
]

# The class that represents a build command
# 'command': The CMake command that will get executed
# 'dependencies': A list of module dependencies for this command
# 'enabledModules': A list of modules that are enabled
class Command:
    def __init__(self):
        self.command = ""
        self.dependencies = []
        self.enabledModules = ["base"]


# Extracts the dependencies from a modules 'include.cmake' file
# This method uses a regex to look for OPENSPACE_DEPENDENCIES
# and adds all names to the list. Then, it will remove the modules
# that have been commented out.
# TODO: Right now, only the next module is removed when a comment
# symbol is found. This does not take into account that there might
# be multiple modules defined in a single commented out line, so we
# potentially have extra dependencies
def getDependencies(module):
    includeFile = "modules/{}/include.cmake".format(module)
    if os.path.isfile(includeFile):
        # The file exists
        dependencyList = []
        with open(includeFile) as f:
            # The file can be opened
            contents = f.read()
            p = re.compile("(\OPENSPACE_DEPENDENCIES(.*)\))", re.DOTALL)
            match = p.search(contents)
            if match is not None:
                string = match.group(match.lastindex)
                # Remove the OPENSPACE_DEPENDENCIES string that is part
                # of the matched string
                string = string.replace("OPENSPACE_DEPENDENCIES", "")
                # Remove the ) that is part of the matched string
                string = string.replace(")", "")

                dependencyList = string.split()

                for index, w in enumerate(dependencyList):
                    # If we find a comment symbol, remove the comment
                    # and the next command
                    if dependencyList[index] == "#":
                        del dependencyList[index]
                        del dependencyList[index]

        return dependencyList
    else:
        return []

percentage = 1.0
if len(sys.argv) > 1:
    percentage = float(sys.argv[1])
    print("Percentage of testing:", percentage)

# To be called from the main OpenSpace
modules = os.listdir("modules")
# modules.remove(".DS_Store")
for m in DefaultEnabledModules:
    modules.remove(m)

# Get 2**len(modules) combinatorical combinations of ON/OFF
settings = []
for args in product(*repeat(("ON", "OFF"), len(modules))):
    settings.append(args)

# Remove the first (all modules enabled) and the last (no modules enabled)
# as we have those  version separately
settings = settings[1:-1]

# Create all commands 
cmds = []
for s in settings:
    cmd = Command()
    cmd.command = ["cmake", "-DGHOUL_USE_DEVIL=NO"]

    for m in DefaultEnabledModules:
        cmd.command.append("-DOPENSPACE_MODULE_" + m.upper() + "=ON")

    for m,s in zip(modules, s):
        cmd.command.append("-DOPENSPACE_MODULE_" + m.upper() + "=" + s)

        if s == "ON":
            # If a module is enabled, we get its dependencies and add them
            # to the list
            deps = getDependencies(m)
            cmd.dependencies.append(deps)
            cmd.enabledModules.append(m)

    # First, we flatten the list of lists that we received before and then
    # remove the duplicates by going through a set
    cmd.dependencies = list(set([x for sl in cmd.dependencies for x in sl]))

    cmd.command.append("..")
    cmds.append(cmd)

for cmd in cmds:
    # If 'enabledModules' is not a strict superset of 'dependencies'
    # We have dependencies that are not enabled, so we shouldn't build
    # this version
    # if not set(cmd.enabledModules).issuperset(set(cmd.dependencies)):
    d = set(cmd.dependencies)
    m = set(cmd.enabledModules)

    if d < m:
        cmds.remove(cmd)

print("Number of builds:", len(cmds))
iter = 1
for cmd in cmds:
    if random.random() > percentage:
        print("Skipping build #", iter, "/", len(cmds))
        iter = iter + 1
        continue

    print("Build #", iter, "/", len(cmds))
    print("Cmd", cmd.command)
    print("Deps", cmd.dependencies)
    print("Mods", cmd.enabledModules)
    print("")
    iter = iter + 1

    shutil.rmtree("build", ignore_errors=True)
    shutil.rmtree("bin", ignore_errors=True)
    os.makedirs("build")
    os.chdir("build")
    call(cmd)
    call(["make"])
    os.chdir("..")