# Based on implementation by Sebastian Zieba: https://github.com/sebastian-zieba/TSM
# 2021-11-03    Main (updated) repo: https://github.com/lkreidberg/TSM
#
# Downloads confirmed planet table from NExSci, aggregates data for ESM and TSM metrics, and saves
# the resulting data in a csv file

from tkinter import FALSE
import pandas as pd
import numpy as np
import os
from datetime import datetime
from astropy import constants as const
import math
import json

# T - temperature (Kelvin)
# lam - wavelength (meter)
# http://spiff.rit.edu/classes/phys317/lectures/planck.html
def Plancks_function(T, lam):
    h = const.h.value
    c = const.c.value
    kB = const.k_B.value

    # constant in the numerator of the equation
    # num = 2.0 * h * c * c / (lam ** 5)

    # Since we have a ratio, we don't actually care about the nominator.
    # We are rather interested in how the value related to the planet (here the temp)
    # affects the final value. So, ignore the constant, to get more understandable values
    num = 1.0

    res = num / (np.exp(h * c / (lam * kB * T)) - 1)
    return res

# The full Planck ratio used in the ESM computation. The division means that we can
# simplify the equation a bit, since some constants will take each other out. Also
# note that the division is flipped
# Ts - stellar temmp
# Tp - planet dayside temp
# lam - wavelength (meter)
# depth - (Rplanet / Rstar) ^2
def Planck_ratio(depth, Ts, Tp, lam):
    h = const.h.value
    c = const.c.value
    kB = const.k_B.value
    res = depth * 1e6 * (np.exp(h * c / (lam * kB * Ts)) - 1) / (np.exp(h * c / (lam * kB * Tp)) - 1)
    return res

Rjup = const.R_jup/const.R_earth
Mjup = const.M_jup/const.M_earth
ReRs = const.R_earth/const.R_sun
MeMs = const.M_earth/const.M_sun

NEW_API = 'https://exoplanetarchive.ipac.caltech.edu/TAP/sync?query='
# The "exoplanets" table includes all confirmed planets and hosts in the
# archive with parameters derived from a single, published reference

DATA_FOLDER = '../data/'
dataFileName = DATA_FOLDER + 'aggregated_data'

# Create data folder if not exists
if not os.path.exists(DATA_FOLDER):
    os.makedirs(DATA_FOLDER)
    print('Created data folder')

###
## Download new confirmed planets
###
print("Downloading all confirmed planets from NExSci's Exoplanets Archive..")

columns='pl_name,hostname,default_flag,sy_snum,sy_pnum,discoverymethod,disc_year,disc_pubdate,disc_facility,tran_flag,soltype,' \
        'pl_refname,pl_orbper,pl_orbpererr1,pl_orbpererr2,pl_orbperlim,pl_orbsmax,pl_orbsmaxerr1,pl_orbsmaxerr2,pl_orbsmaxlim,' \
        'pl_rade,pl_radeerr1,pl_radeerr2,pl_radelim,pl_radj,pl_radjerr1,pl_radjerr2,pl_radjlim,pl_bmasse,pl_bmasseerr1,pl_bmasseerr2,' \
        'pl_bmasselim,pl_bmassj,pl_bmassjerr1,pl_bmassjerr2,pl_bmassjlim,pl_bmassprov,pl_orbeccen,pl_orbeccenerr1,pl_orbeccenerr2,' \
        'pl_orbeccenlim,pl_insol,pl_insolerr1,pl_insolerr2,pl_insollim,pl_eqt,pl_eqterr1,pl_eqterr2,pl_eqtlim,' \
        'pl_orbincl,pl_orbinclerr1,pl_orbinclerr2,pl_orbincllim,ttv_flag,pl_trandur,pl_trandurerr1,pl_trandurerr2,pl_trandurlim,' \
        'pl_ratdor,pl_ratdorerr1,pl_ratdorerr2,pl_ratdorlim,pl_ratror,pl_ratrorerr1,pl_ratrorerr2,pl_ratrorlim,' \
        'gaia_id,disc_telescope,disc_instrument,pl_letter,pl_trandep,pl_trandeperr1,pl_trandeperr2,' \
        'st_nphot,st_nrvc,pl_ntranspec,pl_nespec,st_nspec,' \
        'st_age,st_ageerr1,st_ageerr2,st_dens,st_denserr1,st_denserr2,st_vsin,st_vsinerr1,st_vsinerr2,'\
        'st_rotp,st_rotperr1,st_rotperr2,st_radv,st_radverr1,st_radverr2,sy_plx,sy_plxerr1,sy_plxerr2,' \
        'st_refname,st_spectype,st_teff,st_tefferr1,st_tefferr2,st_tefflim,st_rad,st_raderr1,st_raderr2,st_radlim,' \
        'st_mass,st_masserr1,st_masserr2,st_masslim,st_met,st_meterr1,st_meterr2,st_metlim,st_metratio,st_lum,st_lumerr1,st_lumerr2,' \
        'st_logg,st_loggerr1,st_loggerr2,st_logglim,sy_refname,ra,dec,sy_dist,sy_disterr1,sy_disterr2,' \
        'sy_vmag,sy_vmagerr1,sy_vmagerr2,sy_jmag,sy_jmagerr1,sy_jmagerr2,sy_hmag,sy_hmagerr1,sy_hmagerr2,' \
        'sy_kmag,sy_kmagerr1,sy_kmagerr2,pl_pubdate,releasedate' \

print("Downloading default_flag=1")
where1 = 'where+default_flag=1+and+upper%28soltype%29+like+%27%25CONF%25%27'
full1 = NEW_API + 'select+' + columns + '+from+ps+' + where1 + '&format=csv'
df0 = pd.read_csv(full1, index_col=None)

print("Downloading default_flag=0")
where0 = 'where+default_flag=0+and+upper%28soltype%29+like+%27%25CONF%25%27'
full0 = NEW_API + 'select+' + columns + '+from+ps+' + where0 + '&format=csv'
df1 = pd.read_csv(full0, index_col=None)

with open(DATA_FOLDER + 'last_update_time.txt', 'w+') as ff:
    ff.write(str(datetime.now()))

print("Downloading positions from pscomppars table")
# Also want to get the position parameters from the comppars dataset, to use
# if the ps dataset don't have position values
positionColumns = 'pl_name,sy_dist,sy_disterr1,sy_disterr2,ra,dec'
full_comppars= NEW_API + 'select+' + positionColumns + '+from+pscomppars&format=csv'
df_comppars = pd.read_csv(full_comppars, index_col=None)

###
## Aggregate data
###
print("Aggregating data and computing additional parameters...")

# concatenates data sets
df = pd.concat([df0,df1])

# ## Convert the pubdate to a datetime object
# This is messy since there are three different datetime formats here.
# Must make sure to sort before filtering.

## OBS! Actually, it seems that by now the pubdate have the same formatting, so just use as is.
# This also avoids problems when the month in the format Y-M is zero, which it is for sme planets for some reason
df = df.sort_values(by='pl_pubdate', ascending=False)

# df['dt_obj'] = df['pl_pubdate']
# df['dt_obj'] = pd.to_datetime(df['pl_pubdate'], format="%Y-%m", errors='ignore')
# df['dt_obj'] = pd.to_datetime(df['pl_pubdate'], format="%Y-%m-%d", errors='ignore')
# df['dt_obj'] = pd.to_datetime(df['pl_pubdate'], format="%Y-%m-%d %H:%M", errors='ignore')
# df['dt_obj'] = pd.to_datetime(df['pl_pubdate'], format="%Y-%m", errors='raise')

# df = df.sort_values(by='dt_obj', ascending=False)

## Build up a filter to remove results from the Stassun et al. 2017 paper,
#  which are often more recent but less precise than previous publications
df.loc[df['pl_refname'].str.contains("STASSUN"), 'pl_refname'].unique()
data_filter = (
    (df['pl_refname'] != '<a refstr=STASSUN_ET_AL__2017 href=https://ui.adsabs.harvard.edu/abs/2017AJ....153..136S/abstract target=ref>Stassun et al. 2017</a>')
)

# ## Group by planet name and grab the most recent record
cols = df.columns.to_list()[1:]
agg_dict = dict(zip(cols, ['first'] * len(cols)))

# aggregate
df = df[data_filter].groupby('pl_name', as_index = False).agg(agg_dict)

# fill in columns where mass or radius are only in Jupiter units
df.fillna({'pl_rade': df.pl_radj*Rjup}, inplace=True)
df.fillna({'pl_bmasse': df.pl_bmassj*Mjup}, inplace=True)
df.fillna({'pl_bmasseerr1': df.pl_bmassjerr1*Mjup}, inplace=True)
df.fillna({'pl_bmasseerr2': df.pl_bmassjerr2*Mjup}, inplace=True)

# ## Initialize new column for aggregate temperature
# Try to use as few columns in the data as possible:
# first populate with insolation flux
df['pl_Teq'] = 278.*(df['pl_insol'])**0.25
# if insolation is unavailable, try equilibrium temperature
df.fillna({'pl_Teq': df.pl_eqt}, inplace=True)
# if equilibrium temperature is unavailable, calculate from a/Rs and Teff
df.fillna({'pl_Teq': 1/(np.sqrt(2*df['pl_ratdor']))*df['st_teff']}, inplace=True)
# if a/Rs is unavailable, calculate it from a [AU] and Rs [Rsun]
df.fillna({'pl_Teq': 1/(np.sqrt(2*215.*df['pl_orbsmax']/df['st_rad']))*df['st_teff']}, inplace=True)

# fill rprs if not given
df['pl_ratror'] = ReRs*df['pl_rade']/df['st_rad']
# initialize new column for (Rp/Rs)**2
df['pl_rprs2'] = df['pl_ratror']**2

# scale factor for TSM calculation https://arxiv.org/pdf/1805.03671.pdf
df['scale'] = 0.
df.loc[df['pl_rade'] <= 1.5, 'scale'] = 0.19
df.loc[df['pl_rade'] > 4.0, 'scale'] = 1.15
df.loc[(df['pl_rade'] <= 4.0)&(df['pl_rade'] >2.75), 'scale'] = 1.28
df.loc[(df['pl_rade'] <= 2.75)&(df['pl_rade'] > 1.5), 'scale'] = 1.26

# Initialize new column for TSM
print("Computing TSM")
df['TSM'] = df['pl_rade'] * df['pl_rprs2']/(ReRs**2) * df['pl_Teq']/df['pl_bmasse'] * 10.**(-0.2*df['sy_jmag']) * df['scale']

#calculates observational efficiency for HST (accounting for brightness of host star)
df['efficiency'] = 1.
df.loc[df['sy_jmag'] <= 8.8, 'efficiency'] = (1.375*df['sy_jmag']**2 - 1.214*df['sy_jmag'] - 26.68)/64.58
df.loc[df['sy_jmag'] <= 5., 'efficiency'] = 0.3

df['efficiency_kmag'] = 1.
df.loc[df['sy_kmag'] <= 8.8, 'efficiency_kmag'] = (1.375*df['sy_kmag']**2 - 1.214*df['sy_kmag'] - 26.68)/64.58
df.loc[df['sy_kmag'] <= 5., 'efficiency_kmag'] = 0.3

# option to correct TSM for observatinoal efficiency with HST/WFC3
#df['TSM'] = df['TSM']*np.sqrt(df['efficiency'])

# TODO: figure out if needed
# ## TODO: why are these here and not earlier? If computed earlier they could be used for computation
# # Fill ars if missing: a(AU)/Rs(Ro)*215
# df.pl_ratdor.fillna(df['pl_orbsmax']/df['st_rad']*215, inplace=True)

# # Fill insolation if missing: Ts^4/ars^2 * (215^2/5772^4) = Ts^4/ars^2 * 4.166e-11
# df.pl_insol.fillna(df['st_teff']**4/df['pl_ratdor']**2*4.166e-11, inplace=True)

print("Computing ESM")
# df['ed_ESM'] = Planck_ratio(df['pl_rprs2'], df['st_teff'], 1.1*df['pl_Teq'], 7.5e-6)
# df['ESM'] = 4.29 * df['pl_rprs2'] * df['ed_ESM'] * 10**(-0.2*df['sy_kmag'])

# Alteraitve verison from original script, to get the factors separately:
df['pl_Tday'] = 1.1*df['pl_Teq']
df['planck_ratio'] = Plancks_function(df['pl_Tday'], 7.5e-6) / Plancks_function(df['st_teff'], 7.5e-6)
df['ESM'] = 4.29 * 1e6 * df['pl_rprs2'] * df['planck_ratio'] * 10**(-0.2*df['sy_kmag'])

# TODO: propagate errors, so we get uncertainties for ESM and TSM

# Drop some irrelevant columns
columnsToDrop = ['pl_Tday', 'scale', 'pl_rprs2']
df.drop(columns=columnsToDrop, inplace=True)

## Fill nan position values with data from composite table, if there is any.
# (this solves the problem of Trappist-1 not having a position, for example)
temp_df = df[['pl_name']].merge(df_comppars, on='pl_name', how='left')
df.fillna({'sy_dist': temp_df.sy_dist}, inplace=True)
df.fillna({'sy_disterr1': temp_df.sy_disterr1}, inplace=True)
df.fillna({'sy_disterr2': temp_df.sy_disterr2}, inplace=True)
df.fillna({'ra': temp_df.ra}, inplace=True)
df.fillna({'dec': temp_df.dec}, inplace=True)

##################################################################
# Chemical Abundances
##################################################################

print("Reading data from prepared APOGEE and GALAH files...")

abundanceDatafolder = "https://data.openspaceproject.com/release/ExoplanetExplorer/misc/exo_star_abundances/"
apogeePath = abundanceDatafolder + "abundances_apogee.csv"
galahPath = abundanceDatafolder + "abundances_galah.csv"

apogee = pd.read_csv(apogeePath)
apogee = apogee.add_suffix('_apogee')
apogee.rename(columns={'gaia_id_apogee':'gaia_id'}, inplace=True) # remove suffix from id column

galah = pd.read_csv(galahPath)
galah = galah.add_suffix('_galah')
galah.rename(columns={'gaia_id_galah':'gaia_id'}, inplace=True) # remove suffix from id column

# Drop any unnamed columns
apogee = apogee.loc[:, ~apogee.columns.str.contains('^Unnamed')]
galah = galah.loc[:, ~galah.columns.str.contains('^Unnamed')]

print(apogee.columns)
print(galah.columns)

# Drop duplicates (just keep first entry)
apogee = apogee.drop_duplicates('gaia_id')
galah = galah.drop_duplicates('gaia_id')

# Add apogee and galah columns
df = df.merge(apogee, on='gaia_id', how='left')
df = df.merge(galah, on='gaia_id', how='left')

##################################################################
# IAC Exoplanet Atmospheres
# Dataset from: http://research.iac.es/proyecto/exoatmospheres/
##################################################################
IAC_COLUMNS = ['name', 'molecules']
csv_url = 'http://research.iac.es/proyecto/muscat/exoatm/export'
df_iac_data = pd.read_csv(
    csv_url,
    sep=';',
    usecols=IAC_COLUMNS,
    quotechar="'" # keep all the double quotes around, to not break the json column
)

# Remove all outer quotes from the strings
def strip_quotes(item):
    return item.str.strip('"')

df_iac_data = df_iac_data.apply(strip_quotes)

# Remove duplicates and empty observations
df_iac_data = df_iac_data[df_iac_data.molecules != '[]']
df_iac_data.drop_duplicates()

# Drop a faulty row (something must be wrong with the formatting...
# it gets part of the publication data in the molecules list) # 2022-06-28
df_iac_data = df_iac_data[df_iac_data['molecules'].str.contains("author") == False]

# Convert molcecules column to actual json
def molecules_to_json(item):
    return json.loads(item)

df_iac_data['molecules'] = df_iac_data['molecules'].map(molecules_to_json)

# print(df_iac_data)

def add_value(dict_obj, key, value):
    ''' Adds a key-value pair to the dictionary.
        If the key already exists in the dictionary,
        it will associate multiple values with that
        key instead of overwritting its value'''
    if key not in dict_obj:
        dict_obj[key] = value
    elif isinstance(dict_obj[key], list):
        dict_obj[key].append(value)
    else:
        dict_obj[key] = [dict_obj[key], value]

# The resulting dataset has multiple rows per planet (each row is one observation).
# Combine into one row per planet, in a python dictionary
dict_iac = {}
for index, row in df_iac_data.iterrows():
    key = row['name']
    values = row['molecules']
    if key not in dict_iac:
        dict_iac[key] = {}

    resultDict = dict_iac[key]

    for molecule, value in values.items():
        add_value(resultDict, molecule, value)

    dict_iac[key] = resultDict

# Check result
# print(dict_iac)

# Save as a json file, just in case
with open("../data/test_iac_csv.json", "w") as outfile:
    json.dump(dict_iac, outfile)


KEY_DETECTION = 'Detection'
KEY_UPPER_LIMIT = 'Upper limit'
KEY_NO_DETECTION = 'No detection'

# Flip data so that we have one column for each detection degree.
# Rebuild as dataframe, so we can merge with original dataset
rows = []
for name, molecules in dict_iac.items():
    detections = []
    upperLimits = []
    noDetections = []
    for molecule, value in molecules.items():
        if (value == KEY_DETECTION):
            detections.append(molecule)
        elif (value == KEY_UPPER_LIMIT):
            upperLimits.append(molecule)
        elif (value == KEY_NO_DETECTION):
            noDetections.append(molecule)

    # join with '&' signs
    rows.append([name, '&'.join(detections), '&'.join(upperLimits), '&'.join(noDetections)])

res_df_iac = pd.DataFrame(rows, columns=['name', 'molecule_detection', 'molecule_upperLimit', 'molecule_noDetection'])

print ('IAC Detected Molecules in Atmosphere: ')
print ('--------------------------------')
print (res_df_iac)

df = df.merge(res_df_iac, left_on='pl_name', right_on='name', how='left')
df.drop(columns=['name'], inplace=True) # drop extra name column

##################################################################
# Planned JWST observations
# Dataset from: https://tess.mit.edu/science/tess-acwg/
##################################################################

jwst_csv_url = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQfgw6NlQb3h86hlzo1zFZeLizKZ7ZzunZERWMP0l8LT-F1on-O5FobVlveeTEuEKq6iwSxG0rof326/pub?output=csv'
df_jwst_data = pd.read_csv(
    jwst_csv_url,
    sep=',',
    skiprows=7, # 7 rows of metadata before columns names start
    usecols=['Planet', 'Observation', 'Instrument', 'Mode', 'Data to be obtained'],
    index_col=False
)

# Combine observation technical details into one column and drop old columns
df_jwst_data['jwst_obs_ins_mode'] = '(' + df_jwst_data['Observation'] + ' / ' + df_jwst_data['Instrument'] + ' / ' + df_jwst_data['Mode'] + ')'
df_jwst_data.drop(columns=['Observation', 'Instrument', 'Mode'], inplace=True)
df_jwst_data = df_jwst_data.fillna('')
df_jwst_data.rename(columns = {'Data to be obtained':'jwst_dates'}, inplace = True)

# Remove commas from dates
df_jwst_data['jwst_dates'] = df_jwst_data['jwst_dates'].str.replace(',', '')

# # Create dataset with one row per planet
df_jwst_merged = df_jwst_data.groupby('Planet')['jwst_obs_ins_mode'].agg('; '.join).reset_index()
df_jwst_merged = df_jwst_merged.merge(df_jwst_data.groupby('Planet')['jwst_dates'].agg('; '.join).reset_index())

# Include number of observations
n_observation_counts = df_jwst_data['Planet'].value_counts().to_dict()
df_jwst_merged['jwst_n_obs'] = df_jwst_merged['Planet'].map(n_observation_counts)

print ('JWST Observations: ')
print ('--------------------------------')
print (df_jwst_merged)

# Add columns to big table
df = df.merge(df_jwst_merged, left_on='pl_name', right_on='Planet', how='left')
df.drop(columns=['Planet'], inplace=True) # drop extra name column


##################################################################
# Other custom attributes
##################################################################

#************************************************************************************
# Coeffcients to be used in the analytical expression to calculate habitable zone flux
# boundaries

# Coefficients from: https://depts.washington.edu/naivpl/sites/default/files/HZ_coefficients.dat
# Order:  Recent Venus (otimistic inner), Runaway Greenhouse (conservative inner),
#         Maximum Greenhouse (conservative outer), Early Mars (optimistic outer),
#         Runaway Greenhouse for 5 ME, Runaway Greenhouse for 0.1 ME
seff = [0,0,0,0,0,0]
seffsun  = [1.776,1.107, 0.356, 0.320, 1.188, 0.99]
a = [2.136e-4, 1.332e-4, 6.171e-5, 5.547e-5, 1.433e-4, 1.209e-4]
b = [2.533e-8, 1.580e-8, 1.698e-9, 1.526e-9, 1.707e-8, 1.404e-8]
c = [-1.332e-11, -8.308e-12, -3.198e-12, -2.874e-12, -8.968e-12, -7.418e-12]
d = [-3.097e-15, -1.931e-15, -5.575e-16, -5.011e-16, -2.084e-15, -1.713e-15]

## Habitable zone boundaries for the stars
df['hz_inner_opt'] = np.nan  # optimistic inner
df['hz_inner_cons'] = np.nan # conservative inner
df['hz_outer_opt'] = np.nan # conservative outer
df['hz_outer_opt'] = np.nan  # optimistic outer

# Only select valid Kopparupu planets, with teff in range [2600, 7200] K. But, be a bit more flexible
# TODO: include uncertainty?
validStarRows = (df['st_teff'] > 2000) & (df['st_teff'] < 8000)

# TODO: don't do these operations on the final dataframe
df['tstar'] = df['st_teff'] - 5780.0

# convert luminosity from log10 solar luminosity to just solar
df['st_lum_solar'] = 10.0 ** df['st_lum']

df.loc[validStarRows, 'hz_inner_opt'] =  (df['st_lum_solar'] / (seffsun[0] + a[0]*df['tstar'] + b[0]*df['tstar']**2 + c[0]*df['tstar']**3 + d[0]*df['tstar']**4)) ** 0.5
df.loc[validStarRows, 'hz_inner_cons'] = (df['st_lum_solar'] / (seffsun[1] + a[1]*df['tstar'] + b[1]*df['tstar']**2 + c[1]*df['tstar']**3 + d[1]*df['tstar']**4)) ** 0.5
df.loc[validStarRows, 'hz_outer_opt'] = (df['st_lum_solar'] / (seffsun[2] + a[2]*df['tstar'] + b[2]*df['tstar']**2 + c[2]*df['tstar']**3 + d[2]*df['tstar']**4)) ** 0.5
df.loc[validStarRows, 'hz_outer_opt'] =  (df['st_lum_solar'] / (seffsun[3] + a[3]*df['tstar'] + b[3]*df['tstar']**2 + c[3]*df['tstar']**3 + d[3]*df['tstar']**4)) ** 0.5

# Use the boundaries to compute a score based on the semimajor axis of the planet

## Compute radius to use for HZ check, based on average flux (see Bomont et al. paper)
df['hz_orbsmax'] = df['pl_orbsmax'] * ((1.0 - df['pl_orbeccen'] ** 2) ** 0.25)

# Inner optimistic range => score is computed by linearly interpolating to find value [0,1]
rows = (df['hz_orbsmax'] > df['hz_inner_opt']) & (df['hz_orbsmax'] < df['hz_inner_cons'])
df.loc[rows, 'hz_score'] = (df['hz_orbsmax'] - df['hz_inner_opt']) / (df['hz_inner_cons'] - df['hz_inner_opt'])

# Within conservative range => score = 1.0
rows = (df['pl_orbsmax'] > df['hz_inner_cons']) & (df['hz_orbsmax'] < df['hz_outer_opt'])
df.loc[rows, 'hz_score'] = 1.0

# Inner optimistic range => score is computed by linearly interpolating to find value [0,1]
rows = (df['hz_orbsmax'] > df['hz_outer_opt']) & (df['hz_orbsmax'] < df['hz_outer_opt'])
df.loc[rows, 'hz_score'] = 1.0 - (df['hz_orbsmax'] - df['hz_outer_opt']) / (df['hz_outer_opt'] - df['hz_outer_opt'])


# Account for high eccentricity and high luminosity, according to results from Bomont et al.
# https://www.aanda.org/articles/aa/pdf/2016/07/aa28073-16.pdf

# Just using the eccentricity scores based on the circular orbit that represents the average flux
# leads to "weird" results for highly eccentric orbits. Can we account for star luminosity and
# eccentricity based on the results from Bomont et al somehow?

# If under acceptable line => keep value. If over completely unecceptable ratio, multiply by zero
df['hz_score_scale'] = np.nan
df.loc[(df['st_lum_solar']  < -2.5 * df['pl_orbeccen'] + 2.25), 'hz_score_scale'] = 1.0;
df.loc[(df['st_lum_solar']  > -2.5 * df['pl_orbeccen'] + 2.5), 'hz_score_scale'] = 0.0;

# For everything in between, linearly interpolate between the lines to decide the scaling factor.
# => reduce value with highet eccentricity and luminosity

# Distance between the lines (simple approximating the graph in figure 16 in Bomont et al. paper)
# Compute distance to the zero-line and divide by full distance to get the factor
denom = math.sqrt(1 + 2.5**2)
dfull = 0.25 / denom
betweenLines = (df['st_lum_solar']  <= -2.5 * df['pl_orbeccen'] + 2.5)&(df['st_lum_solar']  >= -2.5 * df['pl_orbeccen'] + 2.25)
df.loc[betweenLines, 'hz_score_scale'] = (np.abs(df['st_lum_solar'] + 2.5 * df['pl_orbeccen'] - 2.5) / denom) / dfull

df['hz_score'] *= df['hz_score_scale']

# Drop some columns only used for computation
df.drop(columns=['hz_score_scale', 'hz_orbsmax'], inplace=True)

# Note about line computation:
# Bottom line (all under = 1.0) tilt is computed from (L, ecc) = (10^-4, 0.9) and (L, ecc) = (1.0 0.5).
# Then it was moved to be a little more pessimistic, so that (L, ecc) = (10^-4, 0.7)
# Upper line was computed with the same tilt, but computed to go through the point (L, ecc) = (1, 0.6)

## -------------------------------------------------------------##
## ADD ANY OTHER COMPUTATIONS YOU WANT HERE ##


##################################################################
# Write out data file
##################################################################

# Remove single quotes in names, etc.. Causes problems when saving to javascript
df.replace({'\'': ''}, regex=True, inplace=True)

# NaN values should be considered missing values. Replace with empty strings
df = df.fillna('')

print("Writing data to file...")
df.to_csv(dataFileName + ".csv", index=False)

# Also print json string as a variable in a javascript file
jsonString = df.to_json(orient='records')
with open(dataFileName + ".js", 'w') as f:
    f.write("let dataString = ")
    f.write("'" + jsonString + "'")
    f.write(";\n")
    f.write("const data = JSON.parse(dataString);")

print("Done!")


# Finally, print some statistics
print("Writing statistics file...")
nPlanets = df.shape[0]
nColumns = df.shape[1]

err_cols = [col for col in df if col.endswith('err1') or col.endswith('err2')]
lim_cols = [col for col in df if col.endswith('lim')]
n_value_cols = nColumns - len(err_cols) - len(lim_cols)

with open(DATA_FOLDER + 'statistics.txt', 'w+') as ff:
    ff.write("number of planets: " + str(nPlanets) + "\n")
    ff.write("number of columns: " + str(nColumns) + "\n")
    ff.write("number of value columns (parameters): " + str(n_value_cols) + "\n")
    ff.write("number of error columns: " + str(len(err_cols)) + " (2 per column with uncertainty info)\n")
    ff.write("number of error limit columns: " + str(len(lim_cols)) + "\n")

print("Wrote data on " + str(nPlanets) + " planets and " + str(nColumns) + " columns (" + str(n_value_cols) + " different parameters)")
