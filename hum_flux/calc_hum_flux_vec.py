import numpy as np
import math as m
import os
from netCDF4 import Dataset
import matplotlib.pyplot as plt
flds = "D:/Masterarbeit/Copernicus_Download/2013/"
#flds = "/media/dogbert/XChange/Masterarbeit/Analyse_Modeloutput/raster/second_model/20km_gul_6_6/"
fld_lst = os.listdir(flds)
#
for i in range (0, len(fld_lst), 1):
    i = 0
    scheme  =  flds+fld_lst[i]+"/hum_flux/"
    print(scheme)
    xdirect = "hum_flux_x_direct.nc"
    ydirect = "hum_flux_y_direct.nc"

    uas = Dataset(scheme+xdirect, 'r', format='NETCDF3_64BIT')
    vas = Dataset(scheme+ydirect, 'r', format='NETCDF3_64BIT')

    uas_var = uas.variables['length'][:]
    mask = (uas_var < -250000000)
    uas_np = np.ma.MaskedArray(uas_var, mask=mask)

    vas_var = vas.variables['length'][:]
    mask = (vas_var < -250000000)
    vas_np = np.ma.MaskedArray(vas_var, mask=mask)
    uas.close()
    vas.close()

    len_uas = uas_np.shape[0]

    erg_arr_direc = np.empty((uas_np.shape[0],uas_np.shape[2],uas_np.shape[1]))
    erg_arr_len = np.empty((uas_np.shape[0],uas_np.shape[2],uas_np.shape[1]))
    pro = 0

    for x in range(0, len_uas, 1):
        if (x/len_uas*100>pro):
            print(pro)
            pro = round(x/len_uas*100, 1) 
            
        act_mat_uas = np.matrix(uas_np[x])
        act_mat_vas = np.matrix(vas_np[x])
        erg_vlen_abs = np.sqrt(np.square(act_mat_uas)+np.square(act_mat_vas))
        erg_dir_rad = np.arctan2(act_mat_uas/erg_vlen_abs, act_mat_vas/erg_vlen_abs)
        erg_dir_deg = (erg_dir_rad * 180)/m.pi
        erg_dir_deg_pos = np.where(erg_dir_deg < 0.0, erg_dir_deg+360, erg_dir_deg)
        erg_arr_direc[x] = (np.rot90(erg_dir_deg_pos,1))
        erg_arr_len[x] = (np.rot90(erg_vlen_abs,1))
        erg_arr_direc[x] = (np.fliplr(erg_arr_direc[x]))
        erg_arr_len[x] = (np.fliplr(erg_arr_len[x]))

       
    # the output array to write will be nx x ny
    nx = vas_np.shape[1]; ny = vas_np.shape[2]; nz = vas_np.shape[0]
    # open a new netCDF file for writing.
    ncfile = Dataset(scheme+'/hum_flux_total.nc','w') 

    # create the x and y dimensions.
    ncfile.createDimension('x',nx)
    ncfile.createDimension('y',ny)
    ncfile.createDimension('z',nz)
    # create the variable (4 byte integer in this case)
    # first argument is name of variable, second is datatype, third is
    # a tuple with the names of dimensions.
    data = ncfile.createVariable('direct','f4',('z','x','y'))
    data1 = ncfile.createVariable('length','f4',('z','x','y'))
    # write data to variable.
    data[:] = erg_arr_direc
    data1[:] = erg_arr_len
    # close the file.
    ncfile.close()
    print ('*** SUCCESS writing example file simple_xy.nc!')


