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


class UtilControl_Tools:
    def __init__(self):  
        pass
    
    def make_directory_check_exist(self, directory, verbose=False):
        if verbose:
            def verboseprint(*args, **kwargs):
                    print(*args, **kwargs)
        else:
            verboseprint = lambda *a, **k: None # do-nothing function

        if os.path.exists(directory):
            verboseprint('Directory Exists: ',directory)
        else:
            os.makedirs(directory)
            verboseprint('Making Directory: ',directory)
        return

    def geodyn_modify_inputs(self, options_in, density_model):
        
        if options_in['DRHODZ_update']== True:
            drhodz_val = '1'
        elif options_in['DRHODZ_update']== False:
            drhodz_val = '0'
        else:
            sys.exit("DRHODZ option is in incorrect format")

            
        if density_model== 'msis86':
            model_val = '0'
        elif density_model== 'msis00':
            model_val = '1'
        elif density_model== 'msis2':
            model_val = '2'
        elif density_model== 'jaachia71':
            model_val = '0'        
        elif density_model== 'dtm87':
            model_val = '0'
        else:
            sys.exit("Density Model Option (DEN_DIR) is in incorrect format")
            
        file1 = open("/data/geodyn_proj/pygeodyn/geodyn_options.txt","w+")
        file1.writelines(drhodz_val+'\n') # first value is for DrhoDz
        file1.writelines(model_val +'\n') # 2nd values is for model switching
        file1.writelines('0'+'\n')
        file1.writelines('0'+'\n')
        file1.writelines('0'+'\n')
        file1.writelines('0'+'\n')
        file1.close()
        
        
    def verboseprint(self, *args, **kwargs):
        if self.verbose:
            print(*args, **kwargs)
        else:
            return( lambda *a, **k: None) # do-nothing function
        
        
        
    def help_get_started_with_run():
        pygeodyn_inputs = ['run_ID',
                           'arc',
                           'satellite',
                           'den_model',
                           'empirical_accels',
                           'SpecialRun_name',
                           'options_in',
                           'verbose']

        options_satellite = ['starlette', 'iss', '(please dont choose iss yet)']
        options_density_model = ['msis86', 'msis00', 'msis2', 'jaachia71', 'dtm87']
        options_arc = [ '030914_2wk','030928_2wk','031012_2wk','031026_2wk','031109_2wk','031123_2wk','031207_2wk', '(broken)031221_2wk' ]
        options_empirical_accels = ['True', 'False']
        options_SpecialRun_name = [' ']
        options_RunID = [' ']
        options_options_in =  ["True", "False"]
        options_verbose = ['True', 'False']
        tab ='      '

        ##############################################################

        print('----- Welcome to the pygeodyn help feature! -----')

        time.sleep(0.5)
        print(tab,"pygeodyn currently requires the following inputs in a dictionary: ")
        for i in pygeodyn_inputs:
            print(tab, tab, i)
        time.sleep(0.5)
        print()

        print('You can either see a pre-made example or make your own run with user inputs.')
        example = input(' Example? (True) or enter your own inputs (write: input):   ')


        if example == 'input':
            #-------------------------------------------------------------
            print(tab,'Please choose from the following optional inputs...')    

            #-----------SATELLITE--------------------------------------------------    
            print(tab,tab,'Satellite options: ',options_satellite,'.' )
            user___satellite = input('satellite:   ')
            while user___satellite not in options_satellite:
                print(tab, 'Bad input, please input one of the above options:')
                user___satellite = input('satellite:   ')


            #----------DENSITY MODEL---------------------------------------------------   
            print(tab,tab,'Density model options: ',options_density_model,'.' )
            user___den_model = input('den_model:   ')
            while user___satellite not in options_satellite:
                print(tab, 'Bad input, please input one of the above options:')
                user___den_model = input('den_model:   ')

            #----------EMPIRICAL-ACCELS----------------------------------------------------
            print(tab,tab,'Will empirically adjusted accelerations be ON (True) or OFF (False): ',options_empirical_accels )
            user___empirical_accels = input('empirical_accels:   ')
            while user___satellite not in options_satellite:
                print(tab, 'Bad input, please input one of the above options:')
                user___empirical_accels = input('empirical_accels:   ')


            #---------OPTIONS-IN----------------------------------------------------
            print(tab,tab,'Do you want to turn on the DRHODZ update?: ',options_options_in, )
            user___DrhodzOption = input('drhodz_update:   ')
            while user___satellite not in options_satellite:
                print(tab, 'Bad input, please input one of the above options:')
                user___DrhodzOption = input('drhodz_update:   ')

            #---------VERBOSE-----------------------------------------------------
            print(tab,tab,'Do you want a verbose run? this prints a LOT of text during the run: ',options_verbose,)
            user___verbose = input('verbose:   ')
            while user___satellite not in options_satellite:
                print(tab, 'Bad input, please input one of the above options:')
                user___verbose = input('verbose:   ')

            #--------SPECIAL-RUN-NAME-----------------------------------------------------
            print(tab,tab,'Do you want to give the save files a special name?')
            print(tab,tab,'This is recommended if you do not want to overwrite previously saved data.',options_SpecialRun_name,)
            user___SpecialRun_name = input('SpecialRun_name:   ')
            while user___satellite not in options_satellite:
                print(tab, 'Bad input, please input one of the above options:')
                user___SpecialRun_name = input('SpecialRun_name:   ')

            #-------RUN-ID------------------------------------------------------
            print(tab,tab,'Give this run an identifying tag.  This only shows up in the text while it runs: ',options_RunID, )
            user___run_ID = input('run_ID:   ')
            while user___satellite not in options_satellite:
                print(tab, 'Bad input, please input one of the above options:')
                user___run_ID = input('run_ID:   ')


            #-------Choose Arcs------------------------------------------------------
            print(tab,tab,'Choose the arc to run. ',options_arc, )
            user___arc = input('arc:   ')
            while user___satellite not in options_satellite:
                print(tab, 'Bad input, please input one of the above options:')
                user___arc = input('arc:   ')

            print()
            print()
            time.sleep(1)
            print('Here are your inputs.  Copy and paste the following into a cell:')

        else:
            user___run_ID           = 'Run_Arc_1'
            user___arc              = '030914_2wk'
            user___satellite        = 'starlette'
            user___den_model        = 'msis2'
            user___empirical_accels = 'False'
            user___SpecialRun_name  = '_developer_test'
            user___DrhodzOption     = 'True'
            user___verbose          = 'False'
            print()
            print()
            time.sleep(1)
            print('------------------------------------------------------------------')

            print('Example inputs to edit.  Copy and paste the following into a cell:')

        print()
        print()
        print("#------ A dictionary containing the run parameters ------  ")
        print("run_params = {} ")
        print("run_params['run_ID']           =  '",user___run_ID,"'  "                      ,sep='')
        print("run_params['arc']              =  '",user___arc,"'  "                         ,sep='')
        print("run_params['satellite']        =  '",user___satellite,"'  "                   ,sep='')
        print("run_params['den_model']        =  '",user___den_model,"'  "                   ,sep='')
        print("run_params['empirical_accels'] =  ",user___empirical_accels,"  "              ,sep='')
        print("run_params['SpecialRun_name']  =  '",user___SpecialRun_name,"'  "             ,sep='')
        print("run_params['options_in']       =  {'DRHODZ_update':",user___DrhodzOption,"}  ",sep='')
        print("run_params['verbose']          =  ",user___verbose,"  "                       ,sep='')

        print()
        print()

        print("#------ Initialize the run Object ------ ")
        print("Obj_run_geodyn = pygeodyn_CONTROL(run_params)")
        print()      
        print("#------ Call the run fuction ------ ")
        print("Obj_run_geodyn.RUN_GEODYN()")

        return

