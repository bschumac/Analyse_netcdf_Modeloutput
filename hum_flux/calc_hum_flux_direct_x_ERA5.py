# -*- coding: utf-8 -*-
"""
Created on Mon Aug 19 22:25:55 2019

@author: dogbert
"""

#Calculate vertically integrated moisture flux in y direction after
# van der Ent 2010

import numpy as np
import math as m
import os
import re
from netCDF4 import Dataset
import matplotlib.pyplot as plt
#os.system("rm -rf /media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/2014_complete/Kiliman_20km_ERA_Aug2014_GrellFC01/hum_flux")

#flds = "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/second_model/20km_gul_6_6/"
flds = "D:/Masterarbeit/Copernicus_Download/2013/"

fld_lst = os.listdir(flds)


for i in range (0, len(fld_lst), 1):   
    act_fld = flds+fld_lst[i]
    #act_output = act_fld+"/output/"
    print(i)
    if not os.path.exists(act_fld+"/hum_flux"):
        os.mkdir(act_fld+"/hum_flux")
    out_dir = act_fld+"/hum_flux"
    #all_files = os.listdir(act_fld)
    

    act_atm_fl = act_fld+'/output_2013_mon_'+fld_lst[i]+'.nc'
    act_srf_pr = act_fld+'/output_2013_pressure_mon_'+fld_lst[i]+'.nc'

    act_nc_fl = Dataset(act_atm_fl, 'r', format='NETCDF3_64BIT')
    print ("Read UAS")
    uas_var = act_nc_fl.variables['u'][:]
    uas_np = np.array(uas_var)
    del(uas_var)
    
    print ("Read QAS")
    qas_var = act_nc_fl.variables['q'][:]
    qas_np = np.array(qas_var)
    del(qas_var)
    print(uas_np.shape)
    act_nc_fl.close()
    
    act_nc_fl = Dataset(act_srf_pr, 'r', format='NETCDF3_64BIT')
    print ("Read PS")
    ps_var  = act_nc_fl.variables['sp'][:]
    ps_np = np.array(ps_var)
    del(ps_var)
    act_nc_fl.close()
    
    
    sigma = [0.025, 0.075, 0.125, 0.175, 0.225, 0.275, 0.325, 0.375, 0.425,
                0.475, 0.525, 0.575, 0.625, 0.675, 0.725, 0.775,
                0.825, 0.870, 0.910, 0.945, 0.970, 0.985, 0.995]

    ##vas_var = act_nc_fl.variables['va'][:]
    ##mask = (vas_var < -250000000)
    ##vas_np = np.ma.MaskedArray(vas_var, mask=mask)
    ##

    
    ps_top = np.empty(ps_np[1].shape)
    ps_top.fill(50)
    print(ps_top.shape)
    timesteps = ps_np.shape[0]
    res_hum_flux_x = np.empty(ps_np.shape)
    W = np.empty(ps_np[1].shape)
    print(W.shape)
    
    g = np.empty(ps_np[1].shape)
    W.fill(20000)
    g.fill(9.81)
    print(timesteps)
    pro = 0
    #print(uas_np[1])
    for j in range (0,timesteps, 1):
            if (j%5 ==0):
                if (j/timesteps*100>pro):
                    print(pro)
                    pro = round(j/timesteps*100, 1)
            dp_act = np.empty(qas_np[1].shape)
            for k in range (0,len(sigma), 1):
                sigma_np = np.empty(ps_top.shape)
                sigma_np.fill(sigma[k])
                dp = np.add(np.multiply(sigma_np,np.subtract(ps_np[j],ps_top)),ps_top)         
                dp_act[k] = dp    

            y = np.multiply(uas_np[j],qas_np[j])
            x = dp_act
            Sa = np.trapz(y,x, axis=0)
            hum_flux = np.multiply(np.divide(W,g),Sa)
            #hum_flux = np.rot90(hum_flux,1)
            hum_flux = np.flipud(hum_flux)
            res_hum_flux_x[j] = hum_flux 
            
    
    plt.imshow(res_hum_flux_x[0])
    # the output array to write will be nx x ny
    nx = res_hum_flux_x.shape[1]; ny = res_hum_flux_x.shape[2]; nz = res_hum_flux_x.shape[0]
    # open a new netCDF file for writing.
    out_name = out_dir+'/hum_flux_x_direct.nc'
    ncfile = Dataset(out_name,'w') 

    # create the x and y dimensions.
    ncfile.createDimension('x',nx)
    ncfile.createDimension('y',ny)
    ncfile.createDimension('z',nz)
    # create the variable (4 byte integer in this case)
    # first argument is name of variable, second is datatype, third is
    # a tuple with the names of dimensions.
    data = ncfile.createVariable('length','f4',('z','x','y'))

    # write data to variable.
    data[:] = res_hum_flux_x

    # close the file.
    ncfile.close()
    print ('*** SUCCESS writing example file'+ out_name)
