#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Sep  3 19:07:55 2019

@author: travis
"""
from dask.diagnostics import ProgressBar
import geopandas as gpd
import numpy as np
import os
import time
import xarray as xr

os.environ['HDF5_USE_FILE_LOCKING'] = 'False'
os.chdir('..')

def subset(aoi, urls, dst):
    start = time.time()
    huss = xr.open_mfdataset(urls)

    # Get bounding box of aoi
    minlon = aoi.bounds.minx.values + 360
    maxlon = aoi.bounds.maxx.values + 360
    minlat = aoi.bounds.miny.values
    maxlat = aoi.bounds.maxy.values

    # Get lons and lats of dataset
    lons = huss.lon.values
    lats = huss.lat.values
    lonbounds = lons[(lons <= maxlon) & (lons > minlon)]
    latbounds = lats[(lats <= maxlat) & (lats > minlat)]
    lonidx1 = np.where(lons == np.min(lonbounds))[0][0]
    lonidx2 = np.where(lons == np.max(lonbounds))[0][0]
    latidx1 = np.where(lats == np.min(latbounds))[0][0]
    latidx2 = np.where(lats == np.max(latbounds))[0][0]

    # Subset
    husslet = huss.specific_humidity[:, latidx1:latidx2, lonidx1:lonidx2]

    # Now simply save to file?
    husslet.to_netcdf(dst)
    duration = time.time() - start
    print("Completed in {} minutes".format(duration/60))


def subsetBar(aoi, urls, dst):
    start = time.time()
    huss = xr.open_mfdataset(urls)

    # Get bounding box of aoi
    minlon = aoi.bounds.minx.values + 360
    maxlon = aoi.bounds.maxx.values + 360
    minlat = aoi.bounds.miny.values
    maxlat = aoi.bounds.maxy.values

    # Get lons and lats of dataset
    lons = huss.lon.values
    lats = huss.lat.values
    lonbounds = lons[(lons <= maxlon) & (lons > minlon)]
    latbounds = lats[(lats <= maxlat) & (lats > minlat)]
    lonidx1 = np.where(lons == np.min(lonbounds))[0][0]
    lonidx2 = np.where(lons == np.max(lonbounds))[0][0]
    latidx1 = np.where(lats == np.min(latbounds))[0][0]
    latidx2 = np.where(lats == np.max(latbounds))[0][0]

    # Subset
    husslet = huss.specific_humidity[:, latidx1:latidx2, lonidx1:lonidx2]

    # Now simply save to file in parallel?
    delayed = husslet.to_netcdf("test.nc", compute=False)
    with ProgressBar():
        delayed.compute()
    duration = time.time() - start
    print("Completed in {} minutes".format(duration/60))


# CCSM4, rcp85, huss, 1950 to 2099
urls = open('data/macav2metdata_urls.txt', 'r').readlines()
urls = [str(u.replace(' \n', '')) for u in urls if 'http' in u]
urls = [u.replace('fileServer', 'dodsC') for u in urls]

# get bounding box of AOI 
parks = gpd.read_file('data/shapefiles/nps_boundary.shp')
aoi = parks[parks['UNIT_NAME'] == 'Death Valley National Park']

# Test functions
subsetBar(aoi, urls, 'data/dvnp_huss4.nc')
#subset(aoi, urls, 'data/dvnp_huss3.nc')
