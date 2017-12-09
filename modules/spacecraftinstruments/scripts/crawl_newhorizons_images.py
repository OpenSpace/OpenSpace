from urllib.request import urlopen, urlretrieve
import re

def downloadPage(pageNumber):
    downloadURL = 'http://pluto.jhuapl.edu/soc/Pluto-Encounter/index.php?order=dateTaken&page='

    response = urlopen(downloadURL + str(pageNumber))
    source = str(response.read())

    searchStr = 'col-xs-2 thumbBox'
    imagePositions = [m.end() for m in re.finditer(searchStr, source)]

    # Entire line:
    # div class="col-xs-2 thumbBox"><A HREF="view_obs.php?image=029912/lor_0299127173_0x630_sci_3.jpg&utc_time=2015-07-13<br>21:00:54 UTC&description=&target=PLUTO&range=0.7M km&exposure=150 msec&imgType=approved"><IMG SRC="data/pluto/level2/lor/jpeg/thumbnails/029912/lor_0299127173_0x630_sci_3.jpg" alt="Thumbnail image of " style="margin-bottom:6px;"></A><span style="font-weight:bold;"><p>2015-07-13</p><p>21:00:54 UTC</p></span><p>Exp: 150 msec</p><p>Target: PLUTO</p><p>Range: 0.7M km</p></div>

    # Image URL
    #http://pluto.jhuapl.edu/soc/Pluto-Encounter/data/pluto/level2/lor/jpeg/029912/lor_0299124574_0x630_sci_4.jpg

    for p in imagePositions:
        try:
            beginOffset = len('"><A HREF="view_obs.php?image=029912/')
            imageLimiterEnd = '&utc_time='
            imageEnd = source[p:].find(imageLimiterEnd)
            # imageLength = len('lor_0299127173_0x630_sci_3.jpg')
            utcDateOffset = len('&utc_time=')
            utcDateLength = len('2015-07-13')
            utcTimeOffset = len('<br>')
            utcTimeLength = len('21:00:54')
            # targetOffset = len(' UTC&description=&')
            targetLimiterBegin = '&target='
            targetLimiterEnd = '&range='
            targetBegin = source[p:].find(targetLimiterBegin)
            targetEnd = source[p:].find(targetLimiterEnd)

            pos = p+beginOffset
            imageName = source[pos:p+imageEnd]
            imageLength = len('imageName')
            pos = p + imageEnd + utcDateOffset

            utcDate = source[pos:pos+utcDateLength]
            pos = pos + utcDateLength + utcTimeOffset

            utcTime = source[pos:pos+utcTimeLength]

            target = source[p+targetBegin+len(targetLimiterBegin):p+targetEnd]


            urlFirstPart = imageName[len('lor_'): len('lor_') + len('029912')]
            imageURL = 'http://pluto.jhuapl.edu/soc/Pluto-Encounter/data/pluto/level2/lor/jpeg/' + urlFirstPart + '/' + imageName

            print("ImageName: " + imageName)
            print("UTCDate: " + utcDate)
            print("UTCTime: " + utcTime)
            print("Target: " + target)
            print("URL: " + imageURL)

            # Download image
            urlretrieve(imageURL, imageName)

            # Create Label file
            with open(imageName[:-3] + 'lbl', 'w') as f:
                f.write('MISSION_NAME = "NEW HORIZONS"\n')
                f.write('SEQUENCE_ID = "UNUSED"\n')
                f.write('TARGET_NAME = "' + target + '"\n')
                f.write('START_TIME = ' + utcDate + 'T' + utcTime + '.000\n')
                f.write('STOP_TIME = ' + utcDate + 'T' + utcTime + '.005\n')
                f.write('INSTRUMENT_HOST_NAME = "NEW HORIZONS"\n')
                f.write('INSTRUMENT_ID = "LORRI"\n')
                f.write('DETECTOR_ID = "LORRI"\n')
                f.write('DETECTOR_TYPE = "CCD"\n')
                f.write('END\n')
        except Exception as inst:
            print('FAIL')
            print(inst)
            print(imageName)
            print(utcDate)
            print(utcTime)
            print(target)
            print(imageURL)


for i in range(0,165):
    print("Downloading Page: " + str(i))
    downloadPage(i)