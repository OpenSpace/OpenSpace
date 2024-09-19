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

import asyncio
from openspace import Api
import time
import os
import subprocess
import shutil
import pathlib

# Global flag for verbose output
verbose = False

def log(*values: object):
    """Custom log function to log messages to file and optionally to console"""
    if verbose:
        print(" ".join(values))

    # Get timestamp of current run
    timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
    with open("log.txt", "a") as file:
        file.write(f"{timestamp}: {' '.join(values)}\n")


async def subscribeToErrorlog(api: Api, exit: asyncio.Event):
    topic = api.subscribeToError({"logLevel": "Warning"})

    log("Subscribed to error log")
    async for future in topic.iterator():
        message = await future
        log(message)
        if exit.is_set():
            log("Unsubscribing from error log")
            topic.talk({ "event": "stop_subscription" })
            topic.cancel()

def removeCache(osDir):
    """
    Clears the contents of the OpenSpace cache directory
    """
    try:
        cacheDir = os.path.join(osDir, "cache")
        with os.scandir(cacheDir) as entries:
            for entry in entries:
                if entry.is_file():
                    os.unlink(entry.path)
                else:
                    shutil.rmtree(entry.path)
    except OSError as e:
        log(f"Error removing cache: {e}")


async def subscribeToAssetLoadingFinishedEvent(api: Api):
    """
    Subscribe to the AssetLoadingFinished event, unsubscribe after the event has been
    received.
    """
    topic = api.subscribeToEvent("AssetLoadingFinished")
    await api.nextValue(topic) # Wait for the event to be fired
    log("Assets were loaded")
    topic.cancel() # Unsubscribe to the event

async def internalRun(openspace, assets: list[pathlib.Path], osDir: str, api: Api):
    """
    Logic for running the asset validation tests
    """
    assetCount = 1

    # TODO: subscribe to warnings and errors and report / log them
    unsubscribeToErrorLogEvent = asyncio.Event()
    # errorLog = asyncio.create_task(subscribeToErrorlog(api, unsubscribeToErrorLogEvent))

    async def unloadAssets():
        log("Getting root assets...")
        roots = await openspace.asset.rootAssets()
        log("Removing assets...")
        for key, root in roots.items():
            await openspace.asset.remove(root)

    async def validateEmptyScene():
        assets = await openspace.asset.allAssets()
        if assets:
            log(f"Error: {len(assets)} assets still loaded: {assets}")
            return False
        return True

    # Make sure we start on a completely empty scene
    await unloadAssets()

    log("Subscribing to AssetLoadingFinished event")
    assetLoadingEvent = api.subscribeToEvent("AssetLoadingFinished")

    for asset in assets:
        log(f"Handling asset {assetCount}/{len(assets)}")
        log(f"Asset: {asset}")

        # We want to start with a cleared cache to make sure assets load correctly from
        # scratch
        removeCache(osDir)

        # # Special asset rules TODO: solve these issues
        if asset.name == "temperature_land_highres.asset":
            continue

        if asset.name == "gaia_dr2_download_stars.asset": # This is 28Gb of download data
            continue

        # if asset.name == "videostretchedtotime.asset":
        #     openspace.time.setTime("2023-01-29T20:30:00")

        path = str(asset).replace(os.sep, "/")

        # Load asset
        log(f"Adding asset without cache")
        await openspace.asset.add(path)
        log("Waiting for AssetLoadingFinished event")
        await api.nextValue(assetLoadingEvent)
        await openspace.printInfo("Recieved the event")

        # Printscreen?

        # Unload asset
        log("Unloading assets")
        await unloadAssets()
        log("Validating empty scene")
        isSceneEmpty = await validateEmptyScene()
        # TODO: manually remove each asset that are still loaded?

        # Load asset using cache
        log(f"Adding asset from cache")
        await openspace.asset.add(path)
        log("Waiting for AssetLoadingFinished event")
        await api.nextValue(assetLoadingEvent)
        await openspace.printInfo("Recieved the event")

        # Printscreen?

        # Unload assets again
        log("Unloading assets")
        await unloadAssets()
        log("Validating empty scene")
        isSceneEmpty = await validateEmptyScene()

        assetCount += 1
        log("Waiting for 0.5 seconds before next asset")
        time.sleep(0.5) # Arbitrary sleep to let OpenSpace breathe

    unsubscribeToErrorLogEvent.set()
    assetLoadingEvent.cancel() # Unsubscribe to event
    # await errorLog

async def mainLoop(files, osDir):
    log("Connecting to OpenSpace...")
    api = Api("localhost", 4681)
    api.connect()
    openspace = await api.singleReturnLibrary()
    log("Connected to OpenSpace")

    await asyncio.create_task(internalRun(openspace, files, osDir, api))
    api.disconnect()

def runAssetValidation(files: list[pathlib.Path], executable: str, args):
    """Run the validation on the given files using OpenSpace executable provided by
    `executable`. This includes starting OpenSpace as a subprocess using a known
    configuration file and the empty profile, establishing a connection using the Python
    API to the OpenSpace instance, and then running the validation on the given files.

    - `files` a list of file paths to the assets to validate
    - `executable` the path to the OpenSpace executable that should be run for the
    validation
    """

    global verbose
    verbose = args.verbose

    startOpenSpace = args.startOS

    if startOpenSpace:
        log("Starting OpenSpace...")
        process = subprocess.Popen(
            [executable,
            "--bypassLauncher"
            ],
            cwd=os.path.dirname(executable),
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE
        )

    # We wait for OpenSpace to start before trying to connect
    if startOpenSpace:
        time.sleep(5)
    asyncio.new_event_loop().run_until_complete(mainLoop(files, args.dir))

    if startOpenSpace:
        process.kill()