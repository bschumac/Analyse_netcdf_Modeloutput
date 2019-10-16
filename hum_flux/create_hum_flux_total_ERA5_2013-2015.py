# -*- coding: utf-8 -*-
"""
Created on Thu Aug 22 18:09:51 2019

@author: benjamin.schumacher
"""

import numpy as np
import math as m
import os
from netCDF4 import Dataset
import matplotlib.pyplot as plt


yearlst = ["2013","2014", "2015"]

flds = "D:/Masterarbeit/Copernicus_Download/2013/"
fld_lst = os.listdir(flds)

scheme  =  flds+fld_lst[0]+"/hum_flux/"
humflux_file = "hum_flux_total.nc"

humflux_a = Dataset(scheme+humflux_file, 'r', format='NETCDF3_64BIT')


humflux_var = humflux_a.variables['length'][:]
mask = (humflux_var < -250000000)
humflux_np_a = np.ma.MaskedArray(humflux_var, mask=mask)

humflux_a.close()
del(humflux_var)


for year in yearlst:
    print(year)
    flds = "D:/Masterarbeit/Copernicus_Download/"+year+"/"
    #flds = "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/second_model/20km_gul_6_6/"
    fld_lst = os.listdir(flds)
    #
    if year == "2013":
        start_iterator = 1
    else:
        start_iterator = 0
        
    for i in range (start_iterator, len(fld_lst), 1):
        print(i)
        scheme  =  flds+fld_lst[i]+"/hum_flux/"
        humflux_file = "hum_flux_total.nc"
        
        humflux_b = Dataset(scheme+humflux_file, 'r', format='NETCDF3_64BIT')
        
    
        humflux_var = humflux_b.variables['length'][:]
        mask = (humflux_var < -250000000)
        humflux_np_b = np.ma.MaskedArray(humflux_var, mask=mask)
    
        
        humflux_b.close()
        del(humflux_var)
        
        humflux_np_a = np.append(humflux_np_a, humflux_np_b, axis=0)
        
nx = humflux_np_a.shape[1]; ny = humflux_np_a.shape[2]; nz = humflux_np_a.shape[0]
# open a new netCDF file for writing.
ncfile = Dataset(scheme+'/hum_flux_total_2013-2015.nc','w') 

# create the x and y dimensions.
ncfile.createDimension('x',nx)
ncfile.createDimension('y',ny)
ncfile.createDimension('z',nz)
# create the variable (4 byte integer in this case)
# first argument is name of variable, second is datatype, third is
# a tuple with the names of dimensions.

data = ncfile.createVariable('length','f4',('z','x','y'))
# write data to variable.
data[:] = humflux_np_a
# close the file.
ncfile.close()
print ('*** SUCCESS writing example file simple_xy.nc!')

