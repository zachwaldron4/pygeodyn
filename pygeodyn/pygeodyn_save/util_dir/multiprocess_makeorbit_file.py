import numpy as np
import pandas as pd

#### Computer/CL functions
import os
import os.path
import sys
import subprocess
import shutil
import time

#### modules for reading and converting data
import linecache
from   datetime import datetime,timedelta
import copy
import logging

def multiprocess_makeorbit_file(DEN_csv, fileorder_num, orbitcloud_path, model_data_path  ):
    import sys
    sys.path.insert(0,'/data/geodyn_proj/interface_kamodo_geodyn/Kamodo/kamodo/flythrough/')
    from SingleSatelliteFlythrough import SingleModelFlythrough
    sys.path.insert(0,'/data/geodyn_proj/interface_kamodo_geodyn/Kamodo/kamodo/flythrough/')
    from SatelliteFlythrough import ModelFlythrough

    count=0

    file_length = np.size(DEN_csv['Date'])
    index_half = int(file_length/2 )
    if fileorder_num == 0:
        index_file0 = 0 
        index_file1 = index_half
        file_name = orbitcloud_path+'0'
        file = open(file_name,  'w')

    elif fileorder_num == 1:
        index_file0 = index_half  
        index_file1 = -1
        file_name = orbitcloud_path+'1'
        file = open(file_name,  'w')


    for it,val in enumerate(DEN_csv['Date'][index_file0:index_file1]):

        date_index = DEN_csv['YYMMDD'][it] + DEN_csv['HHMMSS'][it]
        unix_time  = DEN_csv['sattime_utctimestamp'][it]
        print(f"**** {date_index} -- {fileorder_num}-{count} ****")

        count+=1

        ### Get the coordinates along the orbit:
        lon = float(DEN_csv['Lon'][it])
        lat = float(DEN_csv['Lat'][it])
        alt = float(DEN_csv['Height_kilometers'][it])
        center_coord = [lon, lat, alt]


        ### Find the coordinates of the cube surround the orbit point:
        delta_deg = 2    # degrees
        delta_m = 1000.*1e-3 # meters to kilometers
        A = [lon + delta_deg, lat+delta_deg, alt+delta_m]  # top,    front, left
        B = [lon + delta_deg, lat-delta_deg, alt+delta_m]  # top,    back,  Left
        C = [lon - delta_deg, lat+delta_deg, alt+delta_m]  # top,    front, right
        D = [lon - delta_deg, lat-delta_deg, alt+delta_m]  # top,    back,  right
        E = [lon + delta_deg, lat+delta_deg, alt-delta_m]  # bottom, front, left
        F = [lon + delta_deg, lat-delta_deg, alt-delta_m]  # bottom, back,  left
        G = [lon - delta_deg, lat+delta_deg, alt-delta_m]  # bottom, front, right
        H = [lon - delta_deg, lat-delta_deg, alt-delta_m]  # bottom, back,  right


        ### Store the cube's coordinates in the dictionary index
        cube_corners_and_center = []
        cube_corners_and_center.append(center_coord)
        cube_corners_and_center.append(A)
        cube_corners_and_center.append(B)
        cube_corners_and_center.append(C)
        cube_corners_and_center.append(D)
        cube_corners_and_center.append(E)
        cube_corners_and_center.append(F)
        cube_corners_and_center.append(G)
        cube_corners_and_center.append(H)

        #### Import Coordinates to Kamodo
        ##
        #### Kamodo static inputs:
        model          = 'TIEGCM'
        file_dir       = model_data_path+'/'
#                 logger.debug(f"Added a forward slash to path of {self.model_data_path} to input into Kamodo")
        variable_list  = ['rho','psi_O2', 'psi_O',  'psi_He', 'T_n']
        coord_type     = 'SPH'
        coord_grid     = 'sph'
        high_res       = 1.
        verbose        = False  
        csv_output     = '' 
        plot_output    = ''


        #### Extract the coordinates from each list to plug into Kamodo with vectorization
        lons_in = [item[0] for item in cube_corners_and_center]
        lats_in = [item[1] for item in cube_corners_and_center]
        alts_in = [item[2] for item in cube_corners_and_center]

        ## Gather inputs for Kamodo
        sat_time       = unix_time*np.ones(np.size(alts_in))
        c1             = lons_in
        c2             = lats_in
        c3             = alts_in
        ## Plug vectorized coordinates into Kamodo
        results = ModelFlythrough(model, file_dir, variable_list, sat_time, c1, c2, c3, 
                            coord_type, coord_grid, high_res=20., verbose=False, 
                            csv_output='', plot_output='')

        corners = ['0','1','2','3','4','5','6','7','8']

        for ii,iival in enumerate(results['rho']):
            file.write(f"{date_index}   {results['c1'][ii]:8.4f}   {results['c2'][ii]:8.4f}   {results['c3'][ii]:8.4f}   {iival:15.8e}   {corners[ii]} \n")

    file.close()
    return