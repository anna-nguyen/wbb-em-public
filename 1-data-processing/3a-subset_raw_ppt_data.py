import netCDF4 as nc
import numpy as np
import os
import urllib.request
import xarray as xr

raw_ppt_data_folder = "/Users/annanguyen/Box Sync/WBB-mapping-Stanford/wbb-em-data/raw_ppt_data/"
subset_ppt_data_folder = "/Users/annanguyen/Box Sync/WBB-mapping-Stanford/wbb-em-dat/subset_ppt_data/"

def subset_data(filename): # example filename: 201202.nc
    rawdata = xr.open_dataset(raw_ppt_data_folder + filename)
    ppt = rawdata['precipitation']
    lat_bnds = [23.7, 25.0] 
    lon_bnds = [89.8, 90.9] 
    filtered = ppt.where((ppt.lon > lon_bnds[0]) & (ppt.lon < lon_bnds[1]) & (ppt.lat > lat_bnds[0]) & (ppt.lat < lat_bnds[1]), 
               drop=True)
    new_filename = 'subset_' + filename
    filtered.to_netcdf(subset_ppt_data_folder + new_filename)
    
filenames = os.listdir(raw_ppt_data_folder)
for filename in filenames:
    subset_data(filename)
