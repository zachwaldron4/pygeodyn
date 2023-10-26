import numpy as np
import pandas as pd
from datetime import datetime,timedelta
import os.path
import linecache
from sys import exit
import os
import datetime


def Read_GEODYN_func_MultipleArcs(geodyn_run_params):
    '''
    This function acts as an intermediary to call to the 
    primary functions that call the GEODYN Output.
    
    Input should be a dictionary containing the the following keys:
       
        (MSIS 2 Example below)
            
            arc_list = ['030914_2wk',
                        '030928_2wk']
            geodyn_run_params = {}
            geodyn_run_params['arc_list']        = arc_list
            geodyn_run_params['sat_file']        = 'st'
            geodyn_run_params['SAT_ID']          = 7501001
            geodyn_run_params['grav_id']         = 'goco05s' 
            geodyn_run_params['local_path']      = '/data/geodyn_proj/analysis/starlette_analysis/'
            geodyn_run_params['AccelStatus']     = 'acceloff'
            geodyn_run_params['den_model']       = 'msis2'        
            path_to_model = ('/data/geodyn_proj/runs_geodyn/results/'+
                     geodyn_run_params['sat_file'] +'/'+
                     geodyn_run_params['den_model']+'/'+  
                     geodyn_run_params['den_model']+'_'+ 
                     geodyn_run_params['AccelStatus'] +'/')
            geodyn_run_params['path_to_data']    = path_to_model
            geodyn_run_params['YR']              = 2003
            geodyn_run_params['DATA_TYPE']       = 'SLR'
            geodyn_run_params['verbose_loading'] = False
            geodyn_run_params['Verbose_Stats']   = False
    
    '''
 
    #fix naming convention to be downstream functions
    if geodyn_run_params['sat_file'] == 'st':
        sat_name = 'starlette'

        
        
    #Inititalize that we will save out data along each arc in arc list   
    SatMain_AdjustedParams_arcs = {}
    trajectory_df_arcs = {}
    den_arcs = {}
    resids_observed_arcs = {}
    resids_summary_station_arcs = {}
    resids_meas_summary_arcs = {}
    RunStats_arcs = {}
    
    # Using the input params, construct the filenames for this arc:
    for arc in geodyn_run_params['arc_list']:
        
        file_name =  geodyn_run_params['sat_file'] + arc + '.'+ geodyn_run_params['grav_id']
        print('              Loading ', geodyn_run_params['path_to_data'] ,file_name,'\n' ,sep = '')


        
        
        
        # First check if the arc data exists in the place you think it does.
        iieout_file  = geodyn_run_params['path_to_data'] + 'IIEOUT/'+ file_name
        ascii_xyz_file = geodyn_run_params['path_to_data'] + 'XYZ_TRAJ/'+ file_name
        density_file = geodyn_run_params['path_to_data'] + 'DENSITY/'+ file_name

        
        
        
        # Do some pretty printing if it is wanted:
        if geodyn_run_params['verbose_loading'] == True:
            if os.path.isfile(iieout_file) == True:
                print('       File exists: iieout \n              ',iieout_file)
            else:
                print('ERROR: Not the correct path for file: iieout\n',iieout_file )
                exit(1)
            if os.path.isfile(ascii_xyz_file) == True:
                print('       File exists: ascii_xyz \n              ',ascii_xyz_file)
            else:
                print('ERROR: Not the correct path for file: ascii_xyz\n',ascii_xyz_file )
                exit(1)
            if os.path.isfile(density_file) == True:
                print('       File exists: densityfil \n              ',density_file)
            else:
                print('ERROR: Not the correct path for file: densityfil\n',density_file )
                exit(1)
        else:
            print(' ')
        if geodyn_run_params['verbose_loading'] == True:
            print('\n       Loading data... \n')
        else:
            pass

        
        
        
        
        
        # ######################################
        # ##    Read Adjusted Parameters      
        # ######################################
        import sys  
        sys.path.insert(0, '/data/geodyn_proj/analysis/util_funcs/py_read_geodyn_output/')
        from b_ReadGEODYN import Save_AdjustedParameters_geodyn
        
        SatMain_AdjustedParams_arcs[arc] = Save_AdjustedParameters_geodyn(geodyn_run_params['SAT_ID'], 
                                                                iieout_file, 
                                                                geodyn_run_params['AccelStatus'], 
                                                                geodyn_run_params['DATA_TYPE'])
        if geodyn_run_params['verbose_loading'] == True:
            print('       Parameter adjustment data loaded')
        else:
            pass



        # #####################################
        # #        Read Trajectory      
        # #####################################
        from b_ReadGEODYN import read_ascixyz
        trajectory_df_arcs[arc] = read_ascixyz(ascii_xyz_file, geodyn_run_params['YR'] )

        
        if geodyn_run_params['verbose_loading'] == True:
            print('       Trajectory data loaded')
        else:
            pass




        ######################################
        ##       Read Density Values
        ######################################
        from b_ReadGEODYN import read_density_file
        den_arcs[arc] = read_density_file(density_file, geodyn_run_params['YR'] )
        
        
        
        if geodyn_run_params['verbose_loading'] == True:
            print('       Density data loaded')
        else:
            pass



        ######################################
        ##   -   Read Residual Values   -   ##
        ######################################
        from b_ReadGEODYN import read_observed_resids, read_residual_summarybystation, read_resid_measurement_summaries


        resids_observed_arcs[arc] = read_observed_resids(iieout_file, geodyn_run_params['YR'] , geodyn_run_params['DATA_TYPE'])
        resids_summary_station_arcs[arc] = read_residual_summarybystation(iieout_file)
        resids_meas_summary_arcs[arc] = read_resid_measurement_summaries(iieout_file)

        
        
        if geodyn_run_params['verbose_loading'] == True:
            print('       Observation Residuals \n       Summary by Station \n        Measurement Summary data loaded')
        else:
            pass
    
        ######################################
        ##    -   Read IIEOUT STATS    -    ##
        ######################################    
        from b_ReadGEODYN import save_stats_to_dict
        RunStats_arcs[arc] = save_stats_to_dict(iieout_file, geodyn_run_params['den_model'], sat_name, geodyn_run_params['DATA_TYPE'], Verbose_Stats=geodyn_run_params['Verbose_Stats'] )

    
    #     ######################################
    #     ##       Return collected Datasets
    #     ######################################
    return(SatMain_AdjustedParams_arcs,
            trajectory_df_arcs,
            den_arcs,
            resids_observed_arcs,
            resids_summary_station_arcs,
            resids_meas_summary_arcs,
            RunStats_arcs)

























