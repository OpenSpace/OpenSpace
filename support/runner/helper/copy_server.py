# This script will take the existing results from one server and submit them as new test
# results to a second server. This can be used to migrate existing test results between
# updates

import argparse
import json
import requests

parser = argparse.ArgumentParser()
parser.add_argument(
  "-s", "--source",
  dest="source",
  type=str,
  help="The URL of the server from which to copy the results",
  required=True
)
parser.add_argument(
  "-d", "--destination",
  dest="destination",
  type=str,
  help="The URL of the server to which to copy the results",
  required=True
)
parser.add_argument(
  "-r", "--runner",
  dest="runner",
  type=str,
  help="A valid runner ID for the destination server to be allowed to submit the "
    "existing test records as new tests",
  required=True
)
args = parser.parse_args()


# Request the records from the source server
res = requests.get(f"{args.source}/api/test-records")
if res.status_code != 200:
  print(f"Image submission failed with error {res.status_code}")
  print(res.text)
  exit(-1)
records = json.loads(res.text)

# and then resubmit it as a new test to the destination server
for record in records:
  group = record["group"]
  name = record["name"]
  hardware = record["hardware"]
  print(f"Processing {group}/{name}/{hardware}")

  i = 0
  for data in record["data"]:
    timestamp = data["timeStamp"]
    image_url = f"{args.source}/api/result/candidate/{group}/{name}/{hardware}/{timestamp}"
    log_url = f"{args.source}/api/result/log/{group}/{name}/{hardware}/{timestamp}"
    print(f"  {i}")

    print(f"    Downloading image: {image_url}")
    img = requests.get(image_url)
    if img.status_code != 200:
      print(f"Error with {res.status_code}")
      exit(-1)

    print(f"    Downloading log: {log_url}")
    log = requests.get(log_url)
    if log.status_code != 200:
      print(f"Error with {res.status_code}")
      exit(-1)


    print("    Submitting")
    res = requests.post(
      f"{args.destination}/api/submit-test",
      data = {
        "group": group,
        "name": name,
        "hardware": hardware,
        "runnerID": args.runner,
        "timestamp": timestamp,
        "timing": data["timing"],
        "commitHash": data["commitHash"]
      },
      files = {
        "file": img.content,
        "log": log.content
      }
    )

    i = i + 1
