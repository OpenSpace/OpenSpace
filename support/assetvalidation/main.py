"""
OpenSpace

Copyright (c) 2014-2025

Permission is hereby granted, free of charge, to any person obtaining a copy of this
software and associated documentation files (the "Software"), to deal in the Software
without restriction, including without limitation the rights to use, copy, modify,
merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be included in all copies
or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

"""

import argparse
import os
import pathlib
from AssetValidation import runAssetValidation
import re


# Helper func because argparse does not handle boolean values nicely
# https://stackoverflow.com/questions/15008758/parsing-boolean-values-with-argparse
def str2bool(v):
    """Helper function to parse boolean arguments passed in cli"""
    if isinstance(v, bool):
        return v
    if v.lower() in ("yes", "true", "t", "y", "1"):
        return True
    elif v.lower() in ("no", "false", "f", "n", "0"):
        return False
    else:
        raise argparse.ArgumentTypeError("Boolean value expected")

def setupArgparse():
    """
    Creates and sets up a parser for commandline arguments. This function returns the parsed
    arguments as a dictionary.
    """

    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-d", "--dir",
        dest="dir",
        type=str,
        help="Specifies the OpenSpace directory in which to run the validation",
        required=True
    )

    parser.add_argument(
        "-f", "--filter",
        dest="filter",
        type=str,
        help="Specifies a regex expression to filter on a certain subset of .assets",
        required=False
    )

    parser.add_argument(
        "-v", "--verbose",
        dest="verbose",
        help="Prints verbose output",
        required=False,
        default=False,
    )

    parser.add_argument(
        "-s", "--start",
        dest="startOS",
        help="Specifies whether to start OpenSpace as a subprocess before running the "
             "validation",
        required=False,
        default=True,
    )

    parser.add_argument(
        "-a", "--at",
        dest="startAt",
        help="Start validating at asset nr #. Useful if there OpenSpace crashed during "
             "testing. Note the order in which assets are loaded is not guaranteed to be "
             "the same between runs.",
        required=False,
        default=0,
        type=int
    )

    args = parser.parse_args()
    return args


args = setupArgparse()

args.verbose = str2bool(args.verbose)
args.startOS = str2bool(args.startOS)

# Find the exectuable location and its name
if os.name == "nt":
    # Windows
    executable = f"{args.dir}/bin/RelWithDebInfo/OpenSpace.exe"
else:
    # Linux/Mac
    executable = f"{args.dir}/bin/OpenSpace"

if not os.path.exists(executable):
    raise FileNotFoundError(f"Could not find OpenSpace executable at '{executable}'")

files = list(pathlib.Path(f"{args.dir}/data/").rglob("*.asset"))
if args.filter:
    p = re.compile(args.filter)
    files = [x for x in files if re.search(p, str(x))]

# Sort files to make sure the files we've gotten are consistent between runs, this way we
# can start from a specific index
files.sort()
files = files[args.startAt:]

runAssetValidation(files, executable, args)
