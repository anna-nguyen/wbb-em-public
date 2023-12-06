import netCDF4 as nc
import numpy as np
import os
import urllib.request
import xarray as xr
import datetime
import pandas as pd

data_folder = "/Users/annanguyen/Box Sync/WBB-mapping-Stanford/wbb-em-data/"
subset_ppt_data_folder = "/Users/annanguyen/Box Sync/WBB-mapping-Stanford/data/subset_ppt_data/"

all_ppt_data = pd.DataFrame(columns = ["year", "month", "day", "ppt_mean"])

for filename in os.listdir(subset_ppt_data_folder):
    rawdata = xr.open_dataset(subset_ppt_data_folder + filename)
    ppt = rawdata['precipitation']

    west_lon_bnds = [89.866, 90.058]
    west_lat_bnds = [24.460, 24.757]

    east_lon_bnds = [90.146, 90.819]
    east_lat_bnds = [23.789, 24.959]

    filtered = ppt.where(((ppt.lon > west_lon_bnds[0]) & (ppt.lon < west_lon_bnds[1]) &
                         (ppt.lat > west_lat_bnds[0]) & (ppt.lat < west_lat_bnds[1])) |
                         ((ppt.lon > east_lon_bnds[0]) & (ppt.lon < east_lon_bnds[1]) &
                         (ppt.lat > east_lat_bnds[0]) & (ppt.lat < east_lat_bnds[1])),
               drop=True)

    daily_means = filtered.resample(time='d').mean()
    num_days = len(daily_means)
    ppt_means = []
    for i in np.arange(num_days):
        ppt_means.append(float(np.mean(daily_means[i,])))

    dates = np.arange(1, num_days+1)
    month = np.repeat(int(filename.strip(".nc")[11:13]), num_days)
    year = np.repeat(int(filename.strip(".nc")[7:11]), num_days)

    ppt_data = pd.DataFrame({"year": year,
                             "month": month,
                             "day": dates,
                             "ppt_mean": ppt_means})

    all_ppt_data = all_ppt_data.append(ppt_data)
    
all_ppt_data.to_csv(data_folder + "full_ppt_averages.csv")
