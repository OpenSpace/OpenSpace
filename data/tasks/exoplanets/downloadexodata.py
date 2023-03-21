##
# Download most recent exoplanet data from NASA Exoplanet Archive using the TAP service
# More info at: https://exoplanetarchive.ipac.caltech.edu/docs/TAP/usingTAP.html
#
# The data table is the Planetary Systems Composite dataset, where multiple sources have
# been combined into one row per planet.
# https://exoplanetarchive.ipac.caltech.edu/cgi-bin/TblView/nph-tblView?app=ExoTbls&config=PSCompPars
#
# The script downloads the columns needed for the visualization in OpenSpace and for the
# exoplanets datapreparation task, but more columns can be added if needed.
##

import pandas as pd

dataFileName = 'downloaded_exo_data.csv'

# The columns we need for the visualization in OpenSpace
columns = 'pl_name,hostname,pl_letter,sy_snum,sy_pnum,pl_orbsmax,pl_orbsmaxerr1,pl_orbsmaxerr2,' \
          'pl_orbeccen,pl_orbeccenerr1,pl_orbeccenerr2,pl_orbincl,pl_orbinclerr1,pl_orbinclerr2,' \
          'pl_orblper,pl_orblpererr1,pl_orblpererr2,pl_orbper,pl_orbpererr1,pl_orbpererr2,' \
          'pl_radj,pl_radjerr1,pl_radjerr2,pl_tranmid,pl_tranmiderr1,pl_tranmiderr2,ra,dec,' \
          'sy_dist,st_rad,st_raderr1,st_raderr2,st_teff,st_tefferr1,st_tefferr2,' \
          'st_lum,st_lumerr1,st_lumerr2,cb_flag,disc_year'

# This may contain any extra conditions that one might want to fulfill. Start with a '+' sign
where = ''

###
## Download and save csv file
print("Downloading all confirmed planets from NExSci's Exoplanets Archive... (Planetary Systems Composite Data table)")

NEW_API = 'https://exoplanetarchive.ipac.caltech.edu/TAP/sync?query='
url = NEW_API + 'select+' + columns + '+from+pscomppars' + where + '&format=csv'
print(url)
df = pd.read_csv(url)

print("Writing data to file...")
df.to_csv(dataFileName)
print("Done!")
