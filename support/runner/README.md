# Runner
The _Runner_ is a Python script that will execute and possibly submit test results to a regression server. To execute the _Runner_, the `requests` and `openspace-api` PIP packages need to be installed, for example using `pip install requests && pip install openspace-api`. The `main.py` inside the `runner` folder can then be called to run individual tests. Executing `main.py --help` will return all commandline parameters that can be used to customize the program execution. The available commandline arguments are as follows:

| Parameter | Description |
| --------- | ----------- |
| `--test` | A comma-separated list of the group/name combination of the tests that should be run. The group of a test is all of the folders relative to the `visualtests` server concatenated with the name of the test being the filename. For example a test in `visualtests/mars/insight/landing.ostest` would have the group "mars/insight" and the name "landing". |
| `--overwrite` | This path can be provided to store commonly used files that can be useful to keep between test runs. Right now, this is only used for the Sync folder and the MRF cache used by OpenSpace. |

Example: `python main.py --test default/earth,rosetta/model default --overwrite C:/Development/TestCache`

Additionally, a `config.json` must be provided if tests are to be submitted to the regression server. The `config.sample.json` provides a stub that can be used as the starting point for configuring the JSON file.

If no `config.json` is found, all tests are run locally and are not submitted to the regression server. Instead all resulting images are stored in a `tests` folder whose subfolders mimick the folder structure found in the `visualtests` folder, resulting in images that can be manually inspected.

If a `config.json` is provided, it requires the specification of the URL at which the regression server is located, the hardware string under which the test images are submitted, and a runner id that has to be provided by the administrator of the regression test server. If all these values are correct, test images are directly submitted to the regression server and be can used to compare against a reference image.
