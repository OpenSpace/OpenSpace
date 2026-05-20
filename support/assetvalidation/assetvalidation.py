##########################################################################################
#                                                                                        #
# OpenSpace                                                                              #
#                                                                                        #
# Copyright (c) 2014-2026                                                                #
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

import asyncio
import logging
import os
import re
import subprocess
import shutil
import time
from openspace import Api
from pathlib import Path

# Global flag for verbose output
verbose = False
# Current asset path being validated, used in log message callbacks
path = ""
# OpenSpace root directory, used for computing relative paths in log messages
_rootDir: Path = Path()
# Logging object
logger = logging.getLogger(__name__)

# Regex matching any Windows absolute path (e.g. C:\foo\bar or D:/foo/bar)
_WIN_PATH_RE = re.compile(r'[A-Za-z]:[/\\][^\s\'"]*')

def _relPath(p: Path | str) -> str:
  """Return path relative to _rootDir with a leading slash, for display in logs.
  For paths outside _rootDir (e.g. sync/cache dirs), returns the original path unchanged."""
  try:
    return "/" + Path(p).relative_to(_rootDir).as_posix()
  except ValueError:
    return str(p)

def _sanitizePaths(text: str) -> str:
  """Replace all absolute Windows paths embedded in a string with display-safe relative paths"""
  return _WIN_PATH_RE.sub(lambda m: _relPath(m.group()), text)

class OpenSpaceCrashException(Exception):
  """
  Raised when OpenSpace crashes mid-run, carrying the number of assets that were
  successfully validated before the crash.
  """
  def __init__(self, completedCount: int, cause: Exception):
    super().__init__(str(cause))
    self.completedCount = completedCount
    self.cause = cause

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
  """Keeps the last 5 logs for each log file and increments each by one per run"""
  scriptDirectory = Path(__file__).parent.resolve()
  for baseName in ["log", "log_debug"]:
    for n in range(5, 0, -1):
      rotated = scriptDirectory / f"{baseName}_{n}.txt"
      if rotated.exists():
        if n == 5:
          rotated.unlink()
        else:
          rotated.rename(scriptDirectory / f"{baseName}_{n + 1}.txt")
    current = scriptDirectory / f"{baseName}.txt"
    if current.exists():
      current.rename(scriptDirectory / f"{baseName}_1.txt")

def removeCache(osDir: Path):
  """Clears the contents of the OpenSpace cache directory"""
  try:
    cacheDir = osDir / "cache"
    with os.scandir(cacheDir) as entries:
      for entry in entries:
        if entry.is_file():
          os.unlink(entry.path)
        else:
          shutil.rmtree(entry.path)
  except OSError as e:
    log(f"Error removing cache: {e}", logLevel = logging.ERROR)

async def ensureEmptyScene(openspace, loadedAsset: Path | str):
  """
  Make sure that the scene is empty of all assets, actions, and screenspace renderables.
  Unload and log any existing items
  """
  displayAsset = _relPath(loadedAsset)
  assets = await openspace.asset.allAssets()
  if assets:
    log(f"Handling asset: {displayAsset}: {len(assets)} assets are still loaded",
      logLevel = logging.ERROR
    )
    for asset in assets.values():
      log(f"Removing asset: '{asset}'", logLevel = logging.ERROR)
      await openspace.asset.remove(asset)

  sceneGraphNodes = await openspace.sceneGraphNodes()
  if len(sceneGraphNodes) > 1: # Root is always returned
    log(f"Handling asset: {displayAsset}: {len(sceneGraphNodes) - 1} scene graph" + \
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
    log(f"Handling asset: {displayAsset}: {len(actions)} actions are still loaded",
      logLevel = logging.ERROR
    )
    for action in actions.values():
      log(f"Removing action: '{action}'", logLevel = logging.ERROR)
      await openspace.action.removeAction(action)

  screenSpaceRenderables = await openspace.screenSpaceRenderables()
  if screenSpaceRenderables:
    log(f"Handling asset: {displayAsset}: {len(screenSpaceRenderables)} screen-space" +
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
    log(f"Handling asset: {displayAsset}: {len(dashboardItems)} dashboard items are" +
      " still loaded", logLevel = logging.ERROR)
    for dashboardItem in dashboardItems.values():
      log(f"Removing dashboard item: '{dashboardItem}'", logLevel = logging.ERROR)
      await openspace.dashboard.removeDashboardItem(dashboardItem)

async def waitForAssetState(eventTopic, path: str, expectedState: str) -> str:
  """
  Loop on AssetLoading events until the given asset path reaches the expected state.
  Returns the final state received. Exits early and logs an error on 'Error' state.
  """
  log(f"Waiting for asset '{_relPath(path)}' to reach state '{expectedState}'")
  while True:
    event = await eventTopic.next()
    assetPath = event.get("AssetPath", "").replace(os.sep, "/")
    state = event.get("State", "")
    log(f"Asset event: path='{_relPath(assetPath)}', state='{state}'")
    if assetPath == path:
      if state == expectedState:
        log(f"Asset reached expected state '{expectedState}'")
        return state
      if state == "Error":
        log(f"Asset '{_relPath(path)}' reached error state while waiting for '{expectedState}'",
          logLevel = logging.ERROR
        )
        return state

async def internalRun(openspace, assets: list[Path], osDir: Path, api: Api,
                      startIndex: int = 0, totalCount: int = 0, timeout: int = 180) -> int:
  """Logic for running the asset validation tests"""
  assetCount = 0
  if totalCount == 0:
    totalCount = len(assets)

  logsettings = {
    "timeStamping": False,
    "dateStamping": False,
    "logLevel": "Warning"
  }
  def onMessage(logMsg: dict[str, str]):
    logLevel = logMsg["level"]
    level = logging.WARNING
    if (logLevel == "Error"):
      level = logging.ERROR
    if (logLevel == "Fatal"):
      level = logging.CRITICAL

    message = f"['{_relPath(path)}'] - [{logMsg['category']}]: {_sanitizePaths(logMsg['message'])}"
    log(message, logLevel = level)

  cancelSubscriptionToErrorLog = api.subscribeToLogMessages(logsettings, onMessage)

  async def unloadAssets(newLine: bool = False):
    log("Getting root assets")
    roots = await openspace.asset.rootAssets()
    log(f"Removing root assets{"\n" if newLine else ""}")
    for root in roots.values():
      await openspace.asset.remove(root)

  eventTopic = api.subscribeToEvent("AssetLoading")
  log("Subscribed to AssetLoading event")

  try:
    # Make sure we start on a completely empty scene
    await unloadAssets(newLine = True)
    # Simulation time steps are added by C++ code and are not linked to asset, remove them
    # manually here to avoid them interfering with the asset count and logs
    actions = await openspace.action.actions()
    for action in actions.values():
      if "openspace.time.interpolateDeltaTime" not in action.get("Command", ""):
        log(f"Removing non-time-step action: '{action}'", logLevel = logging.ERROR)
      await openspace.action.removeAction(action)
    await ensureEmptyScene(openspace, "Pre-emptying scene")

    for asset in assets:
      globalIndex = startIndex + assetCount + 1
      log(f"Handling asset {globalIndex}/{totalCount}", logLevel = logging.INFO)
      log(f"Asset: '{_relPath(asset)}'", logLevel = logging.INFO)

      # We want to start with a cleared cache to make sure assets load correctly from
      # scratch
      removeCache(osDir)
      global path
      path = str(asset).replace(os.sep, "/")

      try:
        async with asyncio.timeout(timeout):
          # Load asset
          log(f"Adding asset without cache")
          await openspace.asset.add(path)
          log("Waiting for AssetLoading 'Loaded' event")
          await waitForAssetState(eventTopic, path, "Loaded")
          log("Asset loaded")

          # Unload asset
          log("Unloading assets")
          await unloadAssets()
          log("Waiting for AssetLoading 'Unloaded' event")
          await waitForAssetState(eventTopic, path, "Unloaded")
          log("Ensuring scene is empty")
          await ensureEmptyScene(openspace, asset)

          # Load asset using cache
          log(f"Adding asset from cache")
          await openspace.asset.add(path)
          log("Waiting for AssetLoading 'Loaded' event")
          await waitForAssetState(eventTopic, path, "Loaded")
          log("Asset loaded")

          # Unload assets again
          log("Unloading assets")
          await unloadAssets()
          log("Waiting for AssetLoading 'Unloaded' event")
          await waitForAssetState(eventTopic, path, "Unloaded")
          log("Ensuring scene is empty")
          await ensureEmptyScene(openspace, asset)

        assetCount += 1
        log("Finished testing asset\n", logLevel = logging.INFO)
        time.sleep(0.5) # Arbitrary sleep to let OpenSpace breathe

      except TimeoutError:
        log(
          f"Asset '{asset}' timed out after {timeout}s, skipping to next asset",
          logLevel = logging.ERROR
        )
        raise OpenSpaceCrashException(assetCount, TimeoutError(f"Timed out after {timeout}s"))

      except Exception as e:
        log(f"Unexpected error in '{_relPath(asset)}': {e}", logLevel = logging.ERROR)
        raise OpenSpaceCrashException(assetCount, e)
  finally:
    try:
      eventTopic.cancel()
    except Exception:
      pass
    try:
      await cancelSubscriptionToErrorLog()
    except Exception:
      pass

  return assetCount

async def mainLoop(files: list[Path], osDir: Path, startIndex: int = 0,
                   totalCount: int = 0, timeout: int = 180) -> int:
  """
  Connects to OpenSpace and runs the asset validation. Returns the number of
  successfully validated assets. Returns less than len(files) if a crash occurred.
  """
  log("Connecting to OpenSpace...")
  api = Api("localhost", 4681)
  await api.connect()
  openspace = await api.library()
  log("Connected to OpenSpace")

  try:
    return await internalRun(
      openspace, files, osDir, api, startIndex, totalCount, timeout
    )
  except OpenSpaceCrashException as e:
    return e.completedCount
  finally:
    try:
      api.disconnect()
    except Exception:
      pass

def runAssetValidation(files: list[Path], executable: Path, rootDir: Path, args):
  """
  Run the validation on the given files using the OpenSpace executable provided by
  `executable`. This establishes a connection using the Python API to the OpenSpace
  instance and then runs the validation on the given files.

  If `args.startOS` is True, OpenSpace is started using the `empty` profile as a
  subprocess before connecting and killed after validation completes. If False, an
  already-running OpenSpace instance is expected to be available.

  - `files` a list of file paths to the assets to validate
  - `executable` the path to the OpenSpace executable that should be run for the
  validation (only used when `args.startOS` is True)
  """
  incrementLogNames()

  global verbose, _rootDir
  verbose = args.verbose
  _rootDir = rootDir / "data" / "assets"

  logLevelNamesMapping = logging.getLevelNamesMapping()

  if args.logLevel not in logLevelNamesMapping:
    log(f"Invalid log level '{args.logLevel}' provided, defaulting to 'WARNING'",
      logLevel = logging.WARNING
    )
    args.logLevel = "WARNING"

  scriptDirectory = Path(__file__).parent.resolve()
  formatter = logging.Formatter(
    '[%(asctime)s.%(msecs)03d] %(levelname)s: %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
  )

  # log.txt: only messages at or above the requested log level
  warningsHandler = logging.FileHandler(
    f"{scriptDirectory}/log.txt", encoding='utf-8'
  )
  warningsHandler.setLevel(logLevelNamesMapping[args.logLevel])
  warningsHandler.setFormatter(formatter)

  # log_debug.txt: all messages, useful for tracing steps before an error
  debugHandler = logging.FileHandler(
    f"{scriptDirectory}/log_debug.txt", encoding='utf-8'
  )
  debugHandler.setLevel(logging.DEBUG)
  debugHandler.setFormatter(formatter)

  rootLogger = logging.getLogger()
  rootLogger.setLevel(logging.DEBUG)
  rootLogger.addHandler(warningsHandler)
  rootLogger.addHandler(debugHandler)

  startOpenSpace = args.startOS
  process = None
  remaining = list(files)
  totalCount = len(files)
  startIndex = 0

  while remaining:
    if startOpenSpace:
      log("Starting OpenSpace...", logLevel = logging.INFO)
      process = subprocess.Popen(
        [executable, "--bypassLauncher", "--profile", "empty"],
        cwd = executable.parent,
        stdout = subprocess.DEVNULL,
        stderr = subprocess.PIPE
      )
      time.sleep(5)

    loop = asyncio.new_event_loop()
    try:
      completed = loop.run_until_complete(
        mainLoop(remaining, rootDir, startIndex, totalCount, args.timeout)
      )
    finally:
      # Cancel any tasks left pending by the API (e.g. async generator athrow,
      # subscribeToLogMessages internal queue reader) before closing the loop to avoid
      # "Task was destroyed but it is pending!" warnings
      pending = asyncio.all_tasks(loop)
      if pending:
        for task in pending:
          task.cancel()
        loop.run_until_complete(asyncio.gather(*pending, return_exceptions=True))
      loop.close()

    if startOpenSpace and process:
      process.kill()
      process.wait()
      process = None

    if completed >= len(remaining):
      log("All assets validated", logLevel = logging.INFO)
      break

    # OpenSpace crashed - log the offending asset and continue with the next one
    crashedAsset = remaining[completed]
    log(
      f"OpenSpace crashed during validation of '{_relPath(crashedAsset)}', skipping to next asset\n",
      logLevel = logging.ERROR
    )
    startIndex += completed + 1
    remaining = remaining[completed + 1:]

    if not startOpenSpace:
      log(
        "Cannot restart OpenSpace automatically (--start=False). Stopping validation.",
        logLevel = logging.ERROR
      )
      break
