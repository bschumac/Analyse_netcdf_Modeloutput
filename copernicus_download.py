# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import cdsapi

yearlst = ["2013","2014","2015"]




i = 12
year = "2013"
for year in yearlst:
    i = 1
    #for i in range(1,12):
#        print(i)
#        c = cdsapi.Client()
        # More complex request
#        #[latmax, lonmin, latmin, lonmax]
#        c.retrieve("reanalysis-era5-pressure-levels", {
#                "product_type":   "reanalysis",
#                "format":         "netcdf",
#                "area":           "8.70/26.50/-27.80/64.20",
#                "variable":       ["u", "v", "q"],
#                "pressure_level": ["30","70","125", "175", "225", "250", "300", "350", "400", "450", "500","550","600", "650", "700", 
#                                   "775", "825", "875", "900", "925", "950", "975", "1000"],
#                "year":           year,
#                "month":          ["{:02d}".format(i)],
#                "day":            ["01","02","03","04","05","06","07","08","09","10","11",
#                                   "12","13","14","15","16","17","18","19","20","21","22",
#                                   "23","24","25","26","27","28","29","30","31"],
#                "time":           ["00","01","02","03","04","05","06","07","08","09","10","11",
#                                   "12","13","14","15","16","17","18","19","20","21","22",
#                                   "23"]
#            }, "/home/dogbert/Copernicus_Download/output_"+year+"_mon_"+str("{:02d}".format(i))+".nc")
#            
        #,output_2015_mon_01.nc
        #,"05","06","07","08","09","10","11","12"/media/dogbert/data_meg2015/Copernicus_download/
        
        
        #for i in range(1,12):   
    print(i)
    c = cdsapi.Client()
    # More complex request
    #[latmax, lonmin, latmin, lonmax]
    c.retrieve("reanalysis-era5-single-levels", {
            "product_type":   "reanalysis",
            "format":         "netcdf",
            "area":           "8.70/26.50/-27.80/64.20",
            "variable":       ["tp"],
            "year":           year,
            "month":          ["{:02d}".format(i)],
            "day":            ["01","02","03","04","05","06","07","08","09","10","11",
                               "12","13","14","15","16","17","18","19","20","21","22",
                               "23","24","25","26","27","28","29","30","31"],
            "time":           ["00","01","02","03","04","05","06","07","08","09","10","11",
                               "12","13","14","15","16","17","18","19","20","21","22",
                               "23"]
        }, "/media/benjamin/XChange/Masterarbeit/Copernicus_Download/output_"+year+"_pr_mon_"+str("{:02d}".format(i))+".nc")
    
    
       
            
        #,/media/benjamin/XChange/Masterarbeit/Copernicus_Download