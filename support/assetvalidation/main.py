"""
OpenSpace

Copyright (c) 2014-2024

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
        help="",
        required=False
    )

    parser.add_argument(
        "-v", "--verbose",
        dest="verbose",
        help="Prints verbose output",
        required=False,
        default=False,
    )

    args = parser.parse_args()
    return args


args = setupArgparse()

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

# files = glob.glob(f"{args.dir}/data/**/*.asset", recursive=True)

# Normalize path endings to forward slashes
# files = [file.replace(os.sep, "/") for file in files]

runAssetValidation(files, executable, args)