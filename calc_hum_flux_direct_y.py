#Calculate vertically integrated moisture flux in y direction after
# van der Ent 2010

import numpy as np
import math as m
import os
import re
from netCDF4 import Dataset
#os.system("rm -rf /media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/2014_complete/Kiliman_20km_ERA_Aug2014_GrellFC01/hum_flux")

flds = "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/second_model/20km_gul_6_6/"
#flds = "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/2014_complete/"
fld_lst = os.listdir(flds)

#
for i in range (0, len(fld_lst), 1):
    act_fld = flds+fld_lst[i]
    act_output = act_fld+"/output/"
    print(i)
    #os.mkdir(act_fld+"/hum_flux")
    out_dir = act_fld+"/hum_flux"
    all_files = os.listdir(act_output)
    ATM_files = []
    for lst_item in all_files:
        if 'ATM' in lst_item:
           ATM_files.append(lst_item)

    act_fl = act_output+ATM_files[1]
 

    act_nc_fl = Dataset(act_fl, 'r', format='NETCDF3_64BIT')
    print ("Read VAS")
    #uas_var = act_nc_fl.variables['ua'][:]
    #uas_np = np.array(uas_var)
    vas_var = act_nc_fl.variables['va'][:]
    vas_np = np.array(vas_var)
    print ("Read PS")
    ps_var  = act_nc_fl.variables['ps'][:]
    ps_np = np.array(ps_var)
    print ("Read QAS")
    qas_var = act_nc_fl.variables['qas'][:]
    qas_np = np.array(qas_var)

    print(vas_np.shape)
    sigma = [0.025, 0.075, 0.125, 0.175, 0.225, 0.275, 0.325, 0.375, 0.425,
                0.475, 0.525, 0.575, 0.625, 0.675, 0.725, 0.775,
                0.825, 0.870, 0.910, 0.945, 0.970, 0.985, 0.995]

    act_nc_fl.close()
    ps_top = np.empty(ps_np[1].shape)
    ps_top.fill(50)
    print(ps_top.shape)
    timesteps = ps_np.shape[0]
    res_hum_flux_y = np.empty(ps_np.shape)
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

            y = np.multiply(vas_np[j],qas_np[j])
            x = dp_act
            Sa = np.trapz(y,x, axis=0)
            hum_flux = np.multiply(np.divide(W,g),Sa)
            hum_flux = np.rot90(hum_flux,1)
            hum_flux = np.fliplr(hum_flux)
            res_hum_flux_y[j] = hum_flux 
            
    # the output array to write will be nx x ny
    nx = res_hum_flux_y.shape[1]; ny = res_hum_flux_y.shape[2]; nz = res_hum_flux_y.shape[0]
    # open a new netCDF file for writing.
    out_name = out_dir+'/hum_flux_y_direct.nc'
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
    data[:] = res_hum_flux_y

    # close the file.
    ncfile.close()
    print ('*** SUCCESS writing example file'+ out_name)
