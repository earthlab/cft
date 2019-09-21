#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Sep  3 19:07:55 2019

@author: travis
"""
import geopandas as gpd
import numpy as np
import time
import netCDF4 as nc


def subset(aoi, urls, dst):
    start = time.time()
    huss = nc.MFDataset(urls, aggdim='time')

    # Get bounding box of aoi
    minlon = aoi.bounds.minx.values + 360
    maxlon = aoi.bounds.maxx.values + 360
    minlat = aoi.bounds.miny.values
    maxlat = aoi.bounds.maxy.values

    # Get lons and lats of dataset
    lons = huss['lon'][:]
    lats = huss['lat'][:]
    lonbounds = lons[(lons <= maxlon) & (lons > minlon)]
    latbounds = lats[(lats <= maxlat) & (lats > minlat)]
    lonidx1 = np.where(lons == np.min(lonbounds))[0][0]
    lonidx2 = np.where(lons == np.max(lonbounds))[0][0]
    latidx1 = np.where(lats == np.min(latbounds))[0][0]
    latidx2 = np.where(lats == np.max(latbounds))[0][0]

    # Subset
    husslet = huss['specific_humidity'][:, latidx1:latidx2, lonidx1:lonidx2]

    # Now simply save to file?
    nco = nc.Dataset(dst, 'w')
    nco.createDimension('time', husslet.shape[0])
    nco.createDimension('lat', husslet.shape[1])
    nco.createDimension('lon', husslet.shape[2])
    lats = nco.createVariable('lats', 'd', ('lat',))
    lons = nco.createVariable('lons', 'd', ('lon',))
    var = nco.createVariable('huss', 'd', ('time', 'lat', 'lon'), zlib=True)
    lats[:] = huss['lat'][latidx1:latidx2]
    lons[:] = huss['lon'][lonidx1:lonidx2]
    var[:] = husslet
    nco.close()

    duration = time.time() - start
    print("Completed in {} minutes".format(duration/60))

# CCSM4, rcp85, huss, 1950 to 2099
urls = open('data/macav2metdata_urls.txt', 'r').readlines()
urls = [str(u.replace(' \n', '')) for u in urls if 'http' in u]
urls = [u.replace('fileServer', 'dodsC') for u in urls]

# get bounding box of AOI 
parks = gpd.read_file('data/shapefiles/nps_boundary.shp')
aoi = parks[parks['UNIT_NAME'] == 'Death Valley National Park']

# Test function
subset(aoi, urls, dst='data/dvnp_huss.nc')
