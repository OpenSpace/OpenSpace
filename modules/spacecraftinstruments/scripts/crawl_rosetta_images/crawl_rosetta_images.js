'use strict';

let https = require('https');
var $ = require('cheerio');
let fs = require('fs');

let archiveUrl = 'https://planetgate.mps.mpg.de/Image_of_the_Day/public/IofD_archive.html';
let rootUrl = 'https://planetgate.mps.mpg.de/Image_of_the_Day/public/';

let missionName = '"ROSETTA"';
let targetName = '"67P"';
let instrumentHostName = '"ROSETTA-ORBITER"';
let instrumentId = '"NAVCAM"';
let detectorType = '"CAMERA"';
let downloadFolder = "rosettaimages";

function getHttps(url, cb) {
  https.get(url, (res) => {
    let body = '';
    res.on('data', (d) => {
      body += d;
    });
    res.on('end', () => {
      cb(body);
    });
  });
}

function padZeros(str, nZeros) {
  str = '' + str;
  while (str.length < nZeros) str = '0' + str;
  return str;
}

function formatDate(date) {
  return date.getFullYear() + '-' +
         padZeros(date.getMonth(), 2) + '-' +
         padZeros(date.getDate(), 2) + 'T' +
         padZeros(date.getHours(), 2) + ':' +
         padZeros(date.getMinutes(), 2) + ':' +
         padZeros(date.getSeconds(), 2) + '.' +
         padZeros(date.getMilliseconds(), 3);
}

function parseDate(str) {
  let year = str.substr(0, 4);
  let month = str.substr(5, 2);
  let day = str.substr(8, 2);
  let hours = str.substr(11, 2);
  let minutes = str.substr(14, 2);
  let seconds = str.substr(17, 2);
  let milliseconds = str.substr(20, 3);

  return new Date(year, month, day, hours, minutes, seconds, milliseconds);
}

function formatLbl(data) {
  let maxKeyLength = 0;
  Object.keys(data).forEach((key) => {
    maxKeyLength = Math.max(maxKeyLength, key.length);
  });
  let outString = '';
  Object.keys(data).forEach((key) => {
    let value = data[key];
    outString += key;
    outString += (new Array(maxKeyLength - key.length + 1)).join(' ');
    outString += ' = ';
    outString += value;
    outString += '\n';
  });
  outString += 'END';
  return outString;
}

getHttps(archiveUrl, (body) => {
  let $root = $.load(body);

  $root('tr td').map((i, td) => {
    let thumbnailName = $(td).find('img').attr('src');
    let imageUrl = '';
    if (thumbnailName) {
      let originalName = thumbnailName.replace('_tn', '');
      imageUrl = rootUrl + originalName;
    }


    let detailsName = $(td).find('a').attr('href');
    if (detailsName) {
      let detailsUrl = rootUrl + detailsName;

      getHttps(detailsUrl, (detailsBody) => {
        let $detailsRoot = $.load(detailsBody);

        let startTime = '';
        let id = '';
        let cam = '';
        let exposureTime = '';

        $detailsRoot('tr').map((i, detailsTr) => {
          let header = $(detailsTr).children('th').html();
          let cell = $(detailsTr).children('td').html();

          switch (header) {
            case 'ID': id = cell; break;
            case 'Date taken': startTime = cell; break;
            case 'Camera': cam = cell; break;
            case 'Exposure time': exposureTime = cell; break;
          }
        });

        let exposureSeconds = 0;
        if (exposureTime.substr(-2) === ' s') {
          exposureSeconds = +exposureTime.substr(0, exposureTime.length - 2);
        } else {
          throw "exposure time expressed in unexpeded format, " + exposureTime;
        }

        let startDate = parseDate(startTime);
        let stopDate = new Date(startDate.getTime() + exposureSeconds * 1000);

        let lblContents = formatLbl({
          MISSION_NAME: missionName,
          TARGET_NAME: targetName,
          INSTRUMENT_HOST_NAME: instrumentHostName,
          INSTRUMENT_ID: instrumentId,
          DETECTOR_TYPE: detectorType,
          START_TIME: formatDate(startDate),
          STOP_TIME: formatDate(stopDate)
        });

        let filenameWithoutSuffix = downloadFolder + '/' + id;
        fs.writeFile(filenameWithoutSuffix + '.lbl', lblContents, (err) => {
          if (err) {
            throw err;
          }
        });

        let suffix = imageUrl.substr(imageUrl.lastIndexOf('.'));
        let file = fs.createWriteStream(filenameWithoutSuffix + suffix);
        let request = https.get(imageUrl, (res) => {
          res.pipe(file);
        });
      });
    }
  });
});
