#### ----------------------------------------
#### Import modules:
#### -----------------
import numpy as np
import pandas as pd
    #### Computer function
import os
import os.path
import sys
import subprocess
import shutil
import time
    #### modules for reading and converting data
import linecache
from datetime import datetime,timedelta
import copy


#### ----------------------------------------
#### Plotting modules:
#### -----------------
import plotly.graph_objects as go
from plotly.offline import plot, iplot
from plotly.subplots import make_subplots
import plotly.express as px
#### ----------------------------------------
#### ----------------------------------------
#### ----------------------------------------

class UtilSetInputs:
    def __init__(self):  
        pass
           
        
#     def set_satellite_params(self, satellite):
#         if satellite == 'starlette':
#             self.SATELLITE_dir = 'st'
#             self.SATID         = '7501001'
#             self.YR            = 2003
#             self.DATA_TYPE     = 'SLR'
#             self.grav_id  = 'goco05s'
#             self.g2b_file = 'starlette_03_slrg2b.rm0'
#             self.atgrav_file = 'ATGRAV.glo-3HR_20030101-20080204_1860_AOD1B_0006.0090'
#             self.ephem_file = 'ephem1421.data'
            
#         elif satellite == 'icesat2':
#             self.SATELLITE_dir = 'icesat2'
#             self.SATID         = '1807001'
#             self.YR            = 2018
#             self.DATA_TYPE     = 'PCE'
#             self.grav_id = '' 
# #             self.g2b_file = 'g2b_pce'
#             self.g2b_file = 'icesat2_g2b_PCE_gpstraj'
#             self.atgrav_file = 'ATGRAV.glo-3HR_20160101-PRESENT_9999_AOD1B_0006.0090'
#             self.ephem_file = 'ephem1430.data_2025'
#             self.gravfield_file = 'eigen-6c.gfc_20080101_do_200_fix.grv'
#             self.external_attitude = 'EXAT01.2018.287'
#         else:
#             print('Satellites that are currently available: [starlette, icesat2]')
            
        
#     def set_density_model_setup_params(self, den_model):
#         if den_model == 'msis86':
#             self.DEN_DIR       = den_model
#             self.SETUP_DEN_DIR = 'msis'
#             self.iisset_den = '86'
# #             self.GDYN_version  = 'pygeodyn_MODS'
#         elif den_model == 'msis00':
#             self.DEN_DIR       = den_model
#             self.SETUP_DEN_DIR = 'msis'
#             self.iisset_den = '86'
# #             self.GDYN_version  = 'pygeodyn_MODS'
#         elif den_model == 'msis2':
#             self.DEN_DIR       = den_model
#             self.SETUP_DEN_DIR = 'msis'
#             self.iisset_den = '86'
# #             self.GDYN_version  = 'pygeodyn_MODS'
#         elif den_model == 'dtm87':
#             self.DEN_DIR       = den_model
#             self.SETUP_DEN_DIR = 'dtm87'
#             self.iisset_den = '87'
# #             self.GDYN_version  = 'pygeodyn_MODS'
#         elif den_model == 'jaachia71':
#             self.DEN_DIR       = den_model
#             self.SETUP_DEN_DIR = 'jaachia71'
#             self.iisset_den = '71'
# #             self.GDYN_version  = 'pygeodyn_MODS'
#         else:
#             print('Density model string formats: [msis86, msis00, msis2, dtm87, jaachia71]')   
    
    
#     def set_acceleration_params(self, emp_accels_bool):
#         if emp_accels_bool == False:
#             self.ACCELS = 'acceloff'
#         elif emp_accels_bool == True:
#             self.ACCELS = 'accelon'
#         else:
#             print('Please input the correct status of the empirically adjusted accelerations:')
#             print("        param['empirical_accels']= False if the empircal accererations are turned off.")


#     def set_parallelize_arcs(self, satellite):
#         self.set_satellite_params( self.satellite )
#         self.arcs_list =  [  '030914_2wk',
#                         '030928_2wk',
#                         '031012_2wk',
#                         '031026_2wk',
#                         '031109_2wk',
#                         '031123_2wk',
#                         '031207_2wk',
#                         '031221_2wk',
#                         ]
#         self.parallelize = True

        

#     def set_file_paths_for_multiple_arcs(self, arc_val):
# #             self.arc = arc_val
            
#             self.path_to_model = ('/data/data_geodyn/results/'+
#                                        self.SATELLITE_dir +'/'+
#                                        self.den_model+'/'+  
#                                        self.den_model+'_'+ self.ACCELS + self.SpecialRun_name +'/')
#             file_name =  str(self.SATELLITE_dir) + str(arc_val) + '.'+ str(self.grav_id)        
#             print('        ')
#             print('     File path: ')
#             print('     Loading ', self.path_to_model ,'.../',file_name,' ' ,sep = '')

#             ####  save the specific file names as "private members" with the _filename convention
#             self._asciixyz_filename = self.path_to_model + 'XYZ_TRAJ/'+ file_name
#             self._iieout_filename   = self.path_to_model + 'IIEOUT/'  + file_name
#             self._density_filename  = self.path_to_model + 'DENSITY/' + file_name     
        
        
#     def set_global_params(self, RunParams ):    

#         if RunParams['sat_file'] == 'st':
#             RunParams['SAT_ID']          = 7501001
#             RunParams['DATA_TYPE']       = 'SLR'
#             RunParams['AccelStatus']     = 'acceloff'

#             RunParams['grav_id']         = 'goco05s'     
#             path_to_model = ('/data/data_geodyn/results/'+
#                      RunParams['sat_file'] +'/'+
#                      RunParams['den_model']+'/'+  
#                      RunParams['den_model']+'_'+ 
#                      RunParams['AccelStatus']+ RunParams['SpecialRun_name'] +'/')
#             RunParams['path_to_data']    = path_to_model

#         return(RunParams)