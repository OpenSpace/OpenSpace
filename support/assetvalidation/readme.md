# Description
This script validates all* assets files by loading them one at a time into an empty scene in OpenSpace. The assets are loaded from scratch and then from cache and any errors are logged.

## Setup
The project provides a `requirements.txt` with the necessary dependencies. Install using e.g., pip `pip install -r /path/to/requirements.txt`

## Running validation tests
| Parameter | Description |
| --------- | ----------- |
| `--dir` | Points to the base folder of the OpenSpace version that is used to execute the validation. There needs to be a compiled version of OpenSpace available in the folder such that `bin/RelWithDebInfo/OpenSpace.exe`(on Windows) or `bin/OpenSpace` (on Linux) exists and is executable. |
| `--filter` | Regex string to filter the assets to validate |
| `--verbose` | If set to True output messages will also be printed to the console. |
| `--logLevel` | Sets the log level of reported messages. One of `DEBUG`, `INFO`, `WARNING` (default), `ERROR`, or `CRITICAL`. |
| `--start` | If set to True (default), OpenSpace is started as a subprocess before running the validation. Set to False if OpenSpace is already running. |
| `--at` | Start validating at asset number #. Useful if OpenSpace crashed during testing. Note: the order in which assets are loaded is not guaranteed to be the same between runs. |

```sh
python main.py --dir "C:/Development/OpenSpace" --filter "examples/*"

python main.py --dir "C:/Development/OpenSpace" --verbose=t --start=false --filter "examples/*"

python main.py --dir "C:/Development/OpenSpace" --filter "examples/*" --at 10 --logLevel "DEBUG"
```
