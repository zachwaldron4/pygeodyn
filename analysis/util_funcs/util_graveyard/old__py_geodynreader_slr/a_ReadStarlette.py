import numpy as np
import pandas as pd
from datetime import datetime,timedelta
import os.path
import linecache
from sys import exit


def ReadStarlette(arc, sat_file, grav_id, local_path, path_to_data, AccelStatus):
    '''
    This function acts as an intermediary to call
    the other functions that read the starlette GEODYN output.
    This starlette data is tracked with SLR.
    '''
    
    
    ############################################
    #     Fixed Input parameters
    ############################################
    import sys  
    sys.path.insert(0, '/data/analysis/notebooks/util_funcs/py_geodynreader_slr/')
    SAT_ID = 7501001
    sat = 'st'
    file_name =  sat + arc + '.'+ grav_id
    print('The base file name for this arc is:',file_name,'\n' )
    import os

    # First check if the data exists in the place you think it does.
    iieout_file  = path_to_data + 'IIEOUT/'+ file_name
    ascii_xyz_file = path_to_data + 'XYZ_TRAJ/'+ file_name
    density_file = path_to_data + 'DENSITY/'+ file_name

#     print('\n')
    if os.path.isfile(iieout_file) == True:
        print('File exists: iieout \n       ',iieout_file)
    else:
        print('ERROR: Not the correct path for file: iieout\n',iieout_file )
        exit(1)

    if os.path.isfile(ascii_xyz_file) == True:
        print('File exists: ascii_xyz \n       ',ascii_xyz_file)
    else:
        print('ERROR: Not the correct path for file: ascii_xyz\n',ascii_xyz_file )
        exit(1)

    if os.path.isfile(density_file) == True:
        print('File exists: densityfil \n       ',density_file)
    else:
        print('ERROR: Not the correct path for file: densityfil\n',density_file )
        exit(1)
#     print('\n')




    print('\n Loading data... \n')

    ######################################
    ##    Read Adjusted Parameters      
    ######################################
    from b_ReadStarlette import Save_AdjustedParameters_starlette
    SatMain_AdjustedParams = Save_AdjustedParameters_starlette(SAT_ID, iieout_file, AccelStatus)

    print('Parameter adjustment data loaded')


    ######################################
    ##        Read Trajectory      
    ######################################
    from b_ReadStarlette import read_ascii_xyz
    read_ascii_xyz = read_ascii_xyz(ascii_xyz_file, iieout_file , SAT_ID )
    trajectory_dict = read_ascii_xyz.get_single_sat_data()
    # data_dict[SAT_ID].head()

    print('Trajectory data loaded')



    ######################################
    ##       Read Density Values
    ######################################
    from b_ReadStarlette import read_density_file
    den = read_density_file(density_file)
    # den.head()

    print('Density data loaded')



    ######################################
    ##       Read Residual Values
    ######################################
    from b_ReadStarlette import read_observed_resids_all
    resids = read_observed_resids_all(iieout_file)

    print('Residual data loaded')
    
    
    
    ######################################
    ##       Return collected Datasets
    ######################################

    return(SatMain_AdjustedParams, trajectory_dict, den, resids )



























