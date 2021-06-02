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
# import plotly.graph_objects as go
# from plotly.offline import plot, iplot
# from plotly.subplots import make_subplots
# import plotly.express as px
#### ----------------------------------------
#### ----------------------------------------
#### ----------------------------------------


class UtilControl_Tools:
    def __init__(self):  
        pass
    
    
    
    def set_density_model_setup_params(self, den_model):
        if den_model == 'msis86':
            self.DEN_DIR       = den_model
            self.SETUP_DEN_DIR = 'msis'
            self.iisset_den = '86'
#             self.GDYN_version  = 'pygeodyn_MODS'
        elif den_model == 'msis00':
            self.DEN_DIR       = den_model
            self.SETUP_DEN_DIR = 'msis'
            self.iisset_den = '86'
#             self.GDYN_version  = 'pygeodyn_MODS'
        elif den_model == 'msis2':
            self.DEN_DIR       = den_model
            self.SETUP_DEN_DIR = 'msis'
            self.iisset_den = '86'
#             self.GDYN_version  = 'pygeodyn_MODS'
        elif den_model == 'dtm87':
            self.DEN_DIR       = den_model
            self.SETUP_DEN_DIR = 'dtm87'
            self.iisset_den = '87'
#             self.GDYN_version  = 'pygeodyn_MODS'
        elif den_model == 'jaachia71':
            self.DEN_DIR       = den_model
            self.SETUP_DEN_DIR = 'jaachia71'
            self.iisset_den = '71'
#             self.GDYN_version  = 'pygeodyn_MODS'
        else:
            print('Density model string formats: [msis86, msis00, msis2, dtm87, jaachia71]')   

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
    
    
    
    
    def clean_iisset_file(self):

       
        (path_to_setupfiles, setup_file_arc, SAT_ID, den_model_setupval) = ( self.INPUTDIR,  self.ARC, self.SATID, self.iisset_den)
        
        
        
        ORIG_iisset_file = path_to_setupfiles + '/' + setup_file_arc

        shutil.copyfile(ORIG_iisset_file, path_to_setupfiles +'/'+'cleaned_setup_file')
        iisset_file =       path_to_setupfiles +'/'+'cleaned_setup_file'


        cards_to_remove = [ 'ACCEL9',
                            'XEPHEM',
                            'REFRAC',
                            'GPSMOD',
                            'OFFSET',
                            'OFFADJ',
                            'ANTPHC',
                            'ANTPH2',
                            'CGMASS',
                          ] 

        ##### Grab the EPOCH start and end times
        EPOCH_lines = []
        with open(iisset_file, 'r') as f:
            for line_no, line_text in enumerate(f):
                if 'EPOCH         ' in line_text:
                    EPOCH_lines.append(line_no) 

        ##### Identify and save the EPOCH start and end times
        for i,val in enumerate(EPOCH_lines):
            satpar_line = linecache.getline(iisset_file,val) # Check the above SATPAR line get the correct satellite ID (i.e. NOT GPS)

            ##### only do this for the main satellite, so look for the correct SATID in the SATPAR card above EPOCH
            if SAT_ID in satpar_line:
                epoch_start = linecache.getline(iisset_file,val + 1)[20:40].strip() #181013210000.0000000
                epoch_start_YYMMDD = linecache.getline(iisset_file,val + 1)[20:26].strip()       # 181013
                epoch_start_HHMM = linecache.getline(iisset_file,val + 1)[26:30].strip()         # 210000.0000000
                epoch_start_SS_SSSSSSS = linecache.getline(iisset_file,val + 1)[30:40].strip()   # 00.0000000     

                epoch_end   = linecache.getline(iisset_file,val + 1)[60:80].strip() #1810160300 00.000
                epoch_end_YYMMDD = linecache.getline(iisset_file,val + 1)[60:66].strip()       # 181016
                epoch_end_HHMM = linecache.getline(iisset_file,val + 1)[66:70].strip()         # 210000.0000000
                epoch_end_SS_SSSSSSS = linecache.getline(iisset_file,val + 1)[70:80].strip()   # 00.0000000     

        ##### TO DO : Add conditional for month-day turnover
        epoch_start_minus2days = epoch_start_YYMMDD[:-2]+str(int(epoch_start_YYMMDD[-2:])-2)+epoch_start_HHMM+epoch_start_SS_SSSSSSS
        epoch_end_plus1days =  epoch_end_YYMMDD[:-2]+str(int(epoch_end_YYMMDD[-2:])+1)+epoch_end_HHMM+epoch_end_SS_SSSSSSS


        ##### Putting in the options is one of the hardest parts of using GEODYN
        #####    They require VERY specific inputs depending on the run type.  
        card_strings = {}
        card_strings['ORBFIL'] =  'ORBFIL2 31       '+SAT_ID+'     '+str(epoch_start)[:-6]+'  '+str(epoch_end)[:6]+' 24200.00 .100000D+01'
        card_strings['ORBTVU'] =  'ORBTVU1021       '+SAT_ID+'     '+str(epoch_start)[:-6]+'  '+str(epoch_end)[:6]+' 24200.00 .100000D+01'
        card_strings['RESID']  =  'RESIDU12'
        card_strings['OBSVU']  =  'OBSVU 4'
        card_strings['PRNTVU'] =  'PRNTVU55212222    22122'  
        card_strings['ATMDEN'] =  'ATMDEN  '+ den_model_setupval
        card_strings['ATGRAV']  =  'ATGRAV9090              '+epoch_start_minus2days +''+epoch_end_plus1days[:-1]   
        card_strings['I64G2E']  =  'I64G2E         25'  # using 30 like in st-SLR run maxed out the memory usage
        card_strings['SATPAR']  =  'SATPAR   13      '+SAT_ID+'          9.53000000       1514.000'
        card_strings['SIGMA           1']  =  '               10.0D+25            10.0D+25'
        card_strings['SIGMA           2']  =  '               10.0D+25            10.0D+25'
        card_strings['SIGMA           3']  =  '               10.0D+25            10.0D+25'
        card_strings['SIGMA          51']  =  '               10.0D+25             0.10'
        card_strings['SIGMA          85']  =  '               0.010000            0.010000'


        ##### read in all lines of the file and save them
        with open(iisset_file, "r") as f:
            lines_all = f.readlines()    
        ##### Re-write the file line-by-line WITHOUT the cards that we want to remove    
        with open(iisset_file, "w") as f:
            for line in lines_all:
                if any(card in line for card in cards_to_remove):
                    # IF the any of the cards in the list are in the line, dont add it
                    pass
                else:
                    f.write(line)                

        ##### DO IT AGAIN but with the above updates
        ##### read in all lines of the file and save them
        with open(iisset_file, "r") as f:
            lines_all = f.readlines()                

        ####------------------------------------------------
        #### Check to see if cards we want are in file first
        ####------------------------------------------------
        ##### card flags to see if certain cards are present in the file
        card_flag = {}
        for card in card_strings:
            ### Set the default flag to be False,  if the card is in the file, flip the flag to True
            card_flag[card] = False
            for line in lines_all:
                if card in line:
                    card_flag[card] = True


        ## Re-write the file line-by-line and EDIT the cards that need to be modified    
        with open(iisset_file, "w") as f:
            for line in lines_all:
        #         if (card in line for card in card_strings):
        #             line_replace = card_strings[card]
        #             f.write(line_replace+' \n')
        #         else:
        #             f.write(line)

                for card in card_strings:
                    line_replace = card_strings[card]

                    if card in line:
                        # Go ahead and re-write the line to be in the desired format
                        f.write(line_replace+' \n')
                        break
                    else:
                        f.write(line)
                        break

        ##### DO IT AGAIN but with the above updates
        ###    read in all lines of the file and save them
        with open(iisset_file, "r") as f:
            lines_all = f.readlines()                  
        ####----------------------------------------------------
        #### Add any cards that we want that are not in the file
        ####----------------------------------------------------
        for card in card_flag:
            if card_flag[card] == False:

                with open(iisset_file, "w") as f:
                    for line in lines_all:
                        if 'ALBEDO' in line:
                            f.write(line)
                            f.write(card_strings[card] + ' \n')                 
                        else:
                            f.write(line)

    def check_if_run_converged(self, iieout_filename):
        ''' 
        Check if the run converged properly. If it did not print to the console.
        
        Non-convergence options:

            ** ELEM **  CARTESIAN SPACECRAFT COORDINATES EQUIVALENT TO HYPERBOLIC TRAJECTORY.
                EXECUTION TERMINATING.


        
        '''
        self.convergence_flag = False
        
        with open(iieout_filename, 'r') as f:
            for line_no, line in enumerate(f):
                
                if 'CONVERGENCE' in line:
                    self.convergence_flag = True
#                     print('File converged... reading the file.')
                    break
                
                elif 'HYPERBOLIC TRAJECTORY' in line:
                    self.convergence_flag = False
#                     index_last_slash = self._iieout_filename.rfind('/')
#                     print('|',self.tab,'-----','File: ',self._iieout_filename[index_last_slash+1:]  )
                    
                    longest_line = '|'+' File:'+self._iieout_filename
                    print('+','—'*len(longest_line))
                    print('|',self.tab,'----------- Execution terminated in IIE before convergence -----------')
                    print('|',)
                    print('|', ' File:',self._iieout_filename )
                    print('|', ' Line number:',line_no )
                    print('',)
                    print('',line.rstrip("\n"))
                    print('',)
                    print('|',self.tab,'---------------- Continue to the next arc in the list ----------------')
                    print('+','—'*len(longest_line))
