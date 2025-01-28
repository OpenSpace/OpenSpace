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

from openspace import Api
import asyncio
import logging
import os
import pathlib
import subprocess
import shutil
import time

# Global flag for verbose output
verbose = False
# Logging object
logger = logging.getLogger(__name__)

def log(*values: object, logLevel = logging.DEBUG):
    """Custom log function to log messages to file and optionally to console"""
    msg = ' '.join(str(v) for v in values)
    if verbose:
        print(msg)

    match logLevel:
        case logging.DEBUG:
            logger.debug(msg)
        case logging.INFO:
            logger.info(msg)
        case logging.WARNING:
            logger.warning(msg)
        case logging.ERROR:
            logger.error(msg)
        case logging.CRITICAL:
            logger.critical(msg)

def incrementLogNames():
    """Keeps the last 5 logs and increment each log by one in each run"""
    scriptDirectory = pathlib.Path(__file__).parent.resolve()
    logs = list(pathlib.Path(scriptDirectory).rglob("log*.txt"))
    for log in reversed(logs):
        logname = log.name
        if "_" in logname:
            n = int(logname[logname.find("_") + 1])
            if n == 5:
                log.unlink()
                continue
            log.rename(f"{scriptDirectory}/log_{n + 1}.txt")
        else:
            log.rename(f"{scriptDirectory}/log_{1}.txt")

async def subscribeToErrorlog(api: Api, exit: asyncio.Event):
    topic = api.subscribeToLogMessages({
        "timeStamping": False,
        "dateStamping": False,
        "logLevel": "Warning"
    })
    log("Subscribed to error log", logLevel = logging.INFO)

    async for future in topic.iterator():
        message = await future

        level = logging.WARNING
        if ("Error" in message):
            level = logging.ERROR
        if( "Fatal" in message):
            level = logging.CRITICAL

        log(message, logLevel = level)


        if exit.is_set():
            log("Unsubscribing from error log...")
            topic.cancel()
            log("Unsubscribed to error log", logLevel = logging.INFO)
            break

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
        log(f"Error removing cache: {e}", logLevel = logging.ERROR)

async def ensureEmptyScene(openspace, loadedAsset: pathlib.Path):
    """ Make sure that the scene is empty of all assets, actions, and screenspace
    renderables. Unload and log any existing items
    """
    assets = await openspace.asset.allAssets()
    if assets:
        log(f"Handling asset: {loadedAsset}: {len(assets)} assets are still loaded",
            logLevel = logging.ERROR
        )
        for asset in assets.values():
            log(f"Removing asset: '{asset}'", logLevel = logging.ERROR)
            await openspace.asset.remove(asset)

    sceneGraphNodes = await openspace.sceneGraphNodes()
    if len(sceneGraphNodes) > 1: # Root is always returned
        log(f"Handling asset: {loadedAsset}: {len(sceneGraphNodes) - 1} scene graph" + \
            " nodes are still loaded",
            logLevel = logging.ERROR
        )
        for node in sceneGraphNodes.values():
            if node == "Root":
                continue
            log(f"Removing scene graph node: '{node}'", logLevel = logging.ERROR)
            await openspace.removeSceneGraphNode(node)

    actions = await openspace.action.actions()
    if actions:
        log(f"Handling asset: {loadedAsset}: {len(actions)} actions are still loaded",
            logLevel = logging.ERROR
        )
        for action in actions.values():
            log(f"Removing action: '{action}'", logLevel = logging.ERROR)
            await openspace.action.removeAction(action)

    screenSpaceRenderables = await openspace.screenSpaceRenderables()
    if screenSpaceRenderables:
        log(f"Handling asset: {loadedAsset}: {len(screenSpaceRenderables)} screen-space" +
            " renderables are still loaded",
            logLevel = logging.ERROR
        )
        for screenSpace in screenSpaceRenderables.values():
            log(f"Removing screen space renderable: '{screenSpace}'",
                logLevel = logging.ERROR
            )
            await openspace.removeScreenSpaceRenderable(screenSpace)

    dashboardItems = await openspace.dashboard.dashboardItems()
    if dashboardItems:
        log(f"Handling asset: {loadedAsset}: {len(dashboardItems)} dashboard items are" +
            " still loaded", logLevel = logging.ERROR)
        for dashboardItem in dashboardItems.values():
            log(f"Removing dashboard item: '{dashboardItem}'", logLevel = logging.ERROR)
            await openspace.dashboard.removeDashboardItem(dashboardItem)

async def internalRun(openspace, assets: list[pathlib.Path], osDir: str, api: Api):
    """
    Logic for running the asset validation tests
    """
    assetCount = 1

    logsettings = {
        "timeStamping": False,
        "dateStamping": False,
        "logLevel": "Warning"
    }
    def onMessage(message):
        level = logging.WARNING
        if ("Error" in message):
            level = logging.ERROR
        if( "Fatal" in message):
            level = logging.CRITICAL

        log(message, logLevel = level)

    cancelSubscriptionToErrorLog = api.subscribeToLogMessages(logsettings, onMessage)

    async def unloadAssets():
        log("Getting root assets")
        roots = await openspace.asset.rootAssets()
        log("Removing root assets")
        for root in roots.values():
            await openspace.asset.remove(root)

    # Make sure we start on a completely empty scene
    await unloadAssets()
    await ensureEmptyScene(openspace, "Pre-emptying scene")

    assetLoadingEvent = api.subscribeToEvent("AssetLoadingFinished")
    log("Subscribed to AssetLoadingFinished event", logLevel = logging.INFO)

    for asset in assets:
        log(f"Handling asset {assetCount}/{len(assets)}", logLevel = logging.INFO)
        log(f"Asset: {asset}")

        # We want to start with a cleared cache to make sure assets load correctly from
        # scratch
        removeCache(osDir)
        path = str(asset).replace(os.sep, "/")

        # Load asset
        log(f"Adding asset without cache")
        await openspace.asset.add(path)
        log("Waiting for AssetLoadingFinished event")
        await api.nextValue(assetLoadingEvent)
        log("AssetLoadingFinished event received")

        # Unload asset
        log("Unloading assets")
        await unloadAssets()
        log("Ensuring scene is empty")
        await ensureEmptyScene(openspace, asset)

        # Load asset using cache
        log(f"Adding asset from cache")
        await openspace.asset.add(path)
        log("Waiting for AssetLoadingFinished event")
        await api.nextValue(assetLoadingEvent)
        log("AssetLoadingFinished event received")

        # Unload assets again
        log("Unloading assets")
        await unloadAssets()
        log("Ensuring scene is empty")
        await ensureEmptyScene(openspace, asset)

        assetCount += 1
        log("Finished testing asset", logLevel = logging.INFO)
        time.sleep(0.5) # Arbitrary sleep to let OpenSpace breathe

    # eventUnsubscribeToErrorLog.set()
    assetLoadingEvent.cancel() # Unsubscribe to event
    await cancelSubscriptionToErrorLog()

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
    incrementLogNames()

    global verbose
    verbose = args.verbose
    scriptDirectory = pathlib.Path(__file__).parent.resolve()
    logging.basicConfig(filename=f"{scriptDirectory}/log.txt",
                    format='[%(asctime)s.%(msecs)03d] %(levelname)s: %(message)s',
                    datefmt='%Y-%m-%d | %H:%M:%S',
                    encoding='utf-8',
                    level=logging.DEBUG if verbose else logging.WARNING
                    )

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
