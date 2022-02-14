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




class Util_Tools:
    
    def __init__(self):  
        pass





    def set_density_model_setup_params(self, den_model):
        
#         print('UTIL ---- set_density_model_setup_params ')

        
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
        elif den_model == 'ctipe':
            self.DEN_DIR       = den_model
            self.SETUP_DEN_DIR = 'ctipe'
            self.iisset_den = '86'
        #### We will have two methods for running a model in Kamodo: 
        ####                  1) Command Line kamodo call (model_cl)
        ####                  2) OrbitCloud method        (model_oc)
        elif den_model == 'tiegcm_cl':
            self.DEN_DIR       = den_model
            self.SETUP_DEN_DIR = 'tiegcm_cl'
            self.iisset_den = '86'
        elif den_model == 'tiegcm_oc':
            self.DEN_DIR       = den_model
            self.SETUP_DEN_DIR = 'tiegcm_oc'
            self.iisset_den = '86'
        elif den_model == 'gitm':
            self.DEN_DIR       = den_model
            self.SETUP_DEN_DIR = 'gitm'
            self.iisset_den = '86'

        else:
            print('Density model string formats: [msis86, msis00, msis2, dtm87, jaachia71]')   
            print('Dear zach, please add the remaining models you have added. --from zach')   

            
        from re import search

        if search('tiegcm', self.DEN_DIR):
            if self.satellite == 'icesat2':
                
#                 self.model_data_path ='/data/data_geodyn/atmos_models_data/tiegcm/2018/Lutz_Rastaetter_072319_IT_1' 
                self.model_data_path = self.run_settings['model_data_path']
                
                filemodels = open("/data/geodyn_proj/pygeodyn/pygeodyn_develop/geodyn_modelpaths.txt","w+")
                filemodels.writelines(self.model_data_path+'\n')
                filemodels.writelines('none'+'\n')
                filemodels.close()

            
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
        elif density_model== 'ctipe_cl':
            model_val = '3'
        elif density_model== 'tiegcm_cl':
            model_val = '4'
        elif density_model== 'gitm_cl':
            model_val = '5'
        
        elif density_model== 'tiegcm_oc':
            model_val = '6'
            
        elif density_model== 'jaachia71':
            model_val = '0'        
        elif density_model== 'dtm87':
            model_val = '0'
        else:
            sys.exit("Density Model Option (DEN_DIR) is in incorrect format")
            
        file1 = open("/data/geodyn_proj/pygeodyn/pygeodyn_develop/geodyn_options.txt","w+")
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
        
        
        
#     def help_get_started_with_run():
#         pygeodyn_inputs = ['run_ID',
#                            'arc',
#                            'satellite',
#                            'den_model',
#                            'empirical_accels',
#                            'SpecialRun_name',
#                            'options_in',
#                            'verbose']

#         options_satellite = ['starlette', 'iss', '(please dont choose iss yet)']
#         options_density_model = ['msis86', 'msis00', 'msis2', 'jaachia71', 'dtm87']
#         options_arc = [ '030914_2wk','030928_2wk','031012_2wk','031026_2wk','031109_2wk','031123_2wk','031207_2wk', '(broken)031221_2wk' ]
#         options_empirical_accels = ['True', 'False']
#         options_SpecialRun_name = [' ']
#         options_RunID = [' ']
#         options_options_in =  ["True", "False"]
#         options_verbose = ['True', 'False']
#         tab ='      '

#         ##############################################################

#         print('----- Welcome to the pygeodyn help feature! -----')

#         time.sleep(0.5)
#         print(tab,"pygeodyn currently requires the following inputs in a dictionary: ")
#         for i in pygeodyn_inputs:
#             print(tab, tab, i)
#         time.sleep(0.5)
#         print()

#         print('You can either see a pre-made example or make your own run with user inputs.')
#         example = input(' Example? (True) or enter your own inputs (write: input):   ')


#         if example == 'input':
#             #-------------------------------------------------------------
#             print(tab,'Please choose from the following optional inputs...')    

#             #-----------SATELLITE--------------------------------------------------    
#             print(tab,tab,'Satellite options: ',options_satellite,'.' )
#             user___satellite = input('satellite:   ')
#             while user___satellite not in options_satellite:
#                 print(tab, 'Bad input, please input one of the above options:')
#                 user___satellite = input('satellite:   ')


#             #----------DENSITY MODEL---------------------------------------------------   
#             print(tab,tab,'Density model options: ',options_density_model,'.' )
#             user___den_model = input('den_model:   ')
#             while user___satellite not in options_satellite:
#                 print(tab, 'Bad input, please input one of the above options:')
#                 user___den_model = input('den_model:   ')

#             #----------EMPIRICAL-ACCELS----------------------------------------------------
#             print(tab,tab,'Will empirically adjusted accelerations be ON (True) or OFF (False): ',options_empirical_accels )
#             user___empirical_accels = input('empirical_accels:   ')
#             while user___satellite not in options_satellite:
#                 print(tab, 'Bad input, please input one of the above options:')
#                 user___empirical_accels = input('empirical_accels:   ')


#             #---------OPTIONS-IN----------------------------------------------------
#             print(tab,tab,'Do you want to turn on the DRHODZ update?: ',options_options_in, )
#             user___DrhodzOption = input('drhodz_update:   ')
#             while user___satellite not in options_satellite:
#                 print(tab, 'Bad input, please input one of the above options:')
#                 user___DrhodzOption = input('drhodz_update:   ')

#             #---------VERBOSE-----------------------------------------------------
#             print(tab,tab,'Do you want a verbose run? this prints a LOT of text during the run: ',options_verbose,)
#             user___verbose = input('verbose:   ')
#             while user___satellite not in options_satellite:
#                 print(tab, 'Bad input, please input one of the above options:')
#                 user___verbose = input('verbose:   ')

#             #--------SPECIAL-RUN-NAME-----------------------------------------------------
#             print(tab,tab,'Do you want to give the save files a special name?')
#             print(tab,tab,'This is recommended if you do not want to overwrite previously saved data.',options_SpecialRun_name,)
#             user___SpecialRun_name = input('SpecialRun_name:   ')
#             while user___satellite not in options_satellite:
#                 print(tab, 'Bad input, please input one of the above options:')
#                 user___SpecialRun_name = input('SpecialRun_name:   ')

#             #-------RUN-ID------------------------------------------------------
#             print(tab,tab,'Give this run an identifying tag.  This only shows up in the text while it runs: ',options_RunID, )
#             user___run_ID = input('run_ID:   ')
#             while user___satellite not in options_satellite:
#                 print(tab, 'Bad input, please input one of the above options:')
#                 user___run_ID = input('run_ID:   ')


#             #-------Choose Arcs------------------------------------------------------
#             print(tab,tab,'Choose the arc to run. ',options_arc, )
#             user___arc = input('arc:   ')
#             while user___satellite not in options_satellite:
#                 print(tab, 'Bad input, please input one of the above options:')
#                 user___arc = input('arc:   ')

#             print()
#             print()
#             time.sleep(1)
#             print('Here are your inputs.  Copy and paste the following into a cell:')

#         else:
#             user___run_ID           = 'Run_Arc_1'
#             user___arc              = '030914_2wk'
#             user___satellite        = 'starlette'
#             user___den_model        = 'msis2'
#             user___empirical_accels = 'False'
#             user___SpecialRun_name  = '_developer_test'
#             user___DrhodzOption     = 'True'
#             user___verbose          = 'False'
#             print()
#             print()
#             time.sleep(1)
#             print('------------------------------------------------------------------')

#             print('Example inputs to edit.  Copy and paste the following into a cell:')

#         print()
#         print()
#         print("#------ A dictionary containing the run parameters ------  ")
#         print("run_params = {} ")
#         print("run_params['run_ID']           =  '",user___run_ID,"'  "                      ,sep='')
#         print("run_params['arc']              =  '",user___arc,"'  "                         ,sep='')
#         print("run_params['satellite']        =  '",user___satellite,"'  "                   ,sep='')
#         print("run_params['den_model']        =  '",user___den_model,"'  "                   ,sep='')
#         print("run_params['empirical_accels'] =  ",user___empirical_accels,"  "              ,sep='')
#         print("run_params['SpecialRun_name']  =  '",user___SpecialRun_name,"'  "             ,sep='')
#         print("run_params['options_in']       =  {'DRHODZ_update':",user___DrhodzOption,"}  ",sep='')
#         print("run_params['verbose']          =  ",user___verbose,"  "                       ,sep='')

#         print()
#         print()

#         print("#------ Initialize the run Object ------ ")
#         print("Obj_run_geodyn = pygeodyn_CONTROL(run_params)")
#         print()      
#         print("#------ Call the run fuction ------ ")
#         print("Obj_run_geodyn.RUN_GEODYN()")

#         return
    
    
    
    
#     def clean_iisset_file(self):

       
#         (path_to_setupfiles, setup_file_arc, SAT_ID, den_model_setupval) = ( self.INPUTDIR,  self.ARC, self.SATID, self.iisset_den)
        
        
        
#         ORIG_iisset_file = path_to_setupfiles + '/' + setup_file_arc

#         shutil.copyfile(ORIG_iisset_file, path_to_setupfiles +'/'+'cleaned_setup_file')
#         iisset_file =       path_to_setupfiles +'/'+'cleaned_setup_file'


#         cards_to_remove = [ 'ACCEL9',
#                             'XEPHEM',
#                             'REFRAC',
#                             'GPSMOD',
#                             'OFFSET',
#                             'OFFADJ',
#                             'ANTPHC',
#                             'ANTPH2',
#                             'CGMASS',
#                           ] 

#         ##### Grab the EPOCH start and end times
#         EPOCH_lines = []
#         with open(iisset_file, 'r') as f:
#             for line_no, line_text in enumerate(f):
#                 if 'EPOCH         ' in line_text:
#                     EPOCH_lines.append(line_no) 

#         ##### Identify and save the EPOCH start and end times
#         for i,val in enumerate(EPOCH_lines):
#             satpar_line = linecache.getline(iisset_file,val) # Check the above SATPAR line get the correct satellite ID (i.e. NOT GPS)

#             ##### only do this for the main satellite, so look for the correct SATID in the SATPAR card above EPOCH
#             if SAT_ID in satpar_line:
#                 epoch_start = linecache.getline(iisset_file,val + 1)[20:40].strip() #181013210000.0000000
#                 epoch_start_YYMMDD = linecache.getline(iisset_file,val + 1)[20:26].strip()       # 181013
#                 epoch_start_HHMM = linecache.getline(iisset_file,val + 1)[26:30].strip()         # 210000.0000000
#                 epoch_start_SS_SSSSSSS = linecache.getline(iisset_file,val + 1)[30:40].strip()   # 00.0000000     

#                 epoch_end   = linecache.getline(iisset_file,val + 1)[60:80].strip() #1810160300 00.000
#                 epoch_end_YYMMDD = linecache.getline(iisset_file,val + 1)[60:66].strip()       # 181016
#                 epoch_end_HHMM = linecache.getline(iisset_file,val + 1)[66:70].strip()         # 210000.0000000
#                 epoch_end_SS_SSSSSSS = linecache.getline(iisset_file,val + 1)[70:80].strip()   # 00.0000000     

#         ##### TO DO : Add conditional for month-day turnover
#         epoch_start_minus2days = epoch_start_YYMMDD[:-2]+str(int(epoch_start_YYMMDD[-2:])-2)+epoch_start_HHMM+epoch_start_SS_SSSSSSS
#         epoch_end_plus1days =  epoch_end_YYMMDD[:-2]+str(int(epoch_end_YYMMDD[-2:])+1)+epoch_end_HHMM+epoch_end_SS_SSSSSSS


#         ##### Putting in the options is one of the hardest parts of using GEODYN
#         #####    They require VERY specific inputs depending on the run type.  
#         card_strings = {}
#         card_strings['ORBFIL'] =  'ORBFIL2 31       '+SAT_ID+'     '+str(epoch_start)[:-6]+'  '+str(epoch_end)[:6]+' 24200.00 .100000D+01'
#         card_strings['ORBTVU'] =  'ORBTVU1021       '+SAT_ID+'     '+str(epoch_start)[:-6]+'  '+str(epoch_end)[:6]+' 24200.00 .100000D+01'
#         card_strings['RESID']  =  'RESIDU12'
#         card_strings['OBSVU']  =  'OBSVU 4'
#         card_strings['PRNTVU'] =  'PRNTVU55212222    22122'  
#         card_strings['ATMDEN'] =  'ATMDEN  '+ den_model_setupval
#         card_strings['ATGRAV']  =  'ATGRAV9090              '+epoch_start_minus2days +''+epoch_end_plus1days[:-1]   
#         card_strings['I64G2E']  =  'I64G2E         25'  # using 30 like in st-SLR run maxed out the memory usage
#         card_strings['SATPAR']  =  'SATPAR   13      '+SAT_ID+'          9.53000000       1514.000'
#         card_strings['SIGMA           1']  =  '               10.0D+25            10.0D+25'
#         card_strings['SIGMA           2']  =  '               10.0D+25            10.0D+25'
#         card_strings['SIGMA           3']  =  '               10.0D+25            10.0D+25'
#         card_strings['SIGMA          51']  =  '               10.0D+25             0.10'
#         card_strings['SIGMA          85']  =  '               0.010000            0.010000'


#         ##### read in all lines of the file and save them
#         with open(iisset_file, "r") as f:
#             lines_all = f.readlines()    
#         ##### Re-write the file line-by-line WITHOUT the cards that we want to remove    
#         with open(iisset_file, "w") as f:
#             for line in lines_all:
#                 if any(card in line for card in cards_to_remove):
#                     # IF the any of the cards in the list are in the line, dont add it
#                     pass
#                 else:
#                     f.write(line)                

#         ##### DO IT AGAIN but with the above updates
#         ##### read in all lines of the file and save them
#         with open(iisset_file, "r") as f:
#             lines_all = f.readlines()                

#         ####------------------------------------------------
#         #### Check to see if cards we want are in file first
#         ####------------------------------------------------
#         ##### card flags to see if certain cards are present in the file
#         card_flag = {}
#         for card in card_strings:
#             ### Set the default flag to be False,  if the card is in the file, flip the flag to True
#             card_flag[card] = False
#             for line in lines_all:
#                 if card in line:
#                     card_flag[card] = True


#         ## Re-write the file line-by-line and EDIT the cards that need to be modified    
#         with open(iisset_file, "w") as f:
#             for line in lines_all:
#         #         if (card in line for card in card_strings):
#         #             line_replace = card_strings[card]
#         #             f.write(line_replace+' \n')
#         #         else:
#         #             f.write(line)

#                 for card in card_strings:
#                     line_replace = card_strings[card]

#                     if card in line:
#                         # Go ahead and re-write the line to be in the desired format
#                         f.write(line_replace+' \n')
#                         break
#                     else:
#                         f.write(line)
#                         break

#         ##### DO IT AGAIN but with the above updates
#         ###    read in all lines of the file and save them
#         with open(iisset_file, "r") as f:
#             lines_all = f.readlines()                  
#         ####----------------------------------------------------
#         #### Add any cards that we want that are not in the file
#         ####----------------------------------------------------
#         for card in card_flag:
#             if card_flag[card] == False:

#                 with open(iisset_file, "w") as f:
#                     for line in lines_all:
#                         if 'ALBEDO' in line:
#                             f.write(line)
#                             f.write(card_strings[card] + ' \n')                 
#                         else:
#                             f.write(line)




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


                elif 'FAILED  2.0 PERCENT CONVERGENCE' in line:

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








    def make_datetime_column(self, df, YR):
       
        YR = int(str(YR)[-2:])

        VERBOSE_timer = False
        if VERBOSE_timer == True:
            start = time.time()
        else:
            pass


        df['YYMMDD'] = df['YYMMDD'].str.strip()
        df['HHMM']   = df['HHMM'].str.strip()


        timeHHMM = [] 
        for i,val in enumerate(df['HHMM']):
        #         print(len(val))
            if len(val) == 3:
                timehhmm_val = '0'+ val
                timeHHMM.append(timehhmm_val)
            elif len(val) == 2:
                timehhmm_val = '00'+ val
                timeHHMM.append(timehhmm_val)
            elif len(val) == 1:
                timehhmm_val = '000'+ val
                timeHHMM.append(timehhmm_val)
            elif len(val) == 4:
                timehhmm_val = val
                timeHHMM.append(timehhmm_val)
            elif len(val) == 0:
        #             timehhmm_val = val
        #             timeHHMM.append(timehhmm_val)
                pass

        df['timeHHMM'] = timeHHMM

        YYMMDD_list   = df['YYMMDD'].astype(int).astype(str)
        timeHHMM_list = df['timeHHMM'].astype(str)
        SEC_UTC_list  = df['SEC_UTC'].astype(str)

        if YR < 10:
            year    = ['200' + x[:1]  for x in YYMMDD_list]
            month   = [        x[1:3] for x in YYMMDD_list]
            day     = [        x[3:]  for x in YYMMDD_list]
            hours   = [        x[:2]  for x in timeHHMM_list]
            minutes = [        x[2:4]  for x in timeHHMM_list]
            secs    = [        x[:2] for x in SEC_UTC_list]
            millsecs= [        x[3:]  for x in SEC_UTC_list]
        else:
            year    = ['20' + x[:2]  for x in YYMMDD_list]
            month   = [        x[2:4] for x in YYMMDD_list]
            day     = [        x[4:]  for x in YYMMDD_list]
            hours   = [        x[:2]  for x in timeHHMM_list]
            minutes = [        x[2:4]  for x in timeHHMM_list]
            secs    = [        x[:2] for x in SEC_UTC_list]
            millsecs= [        x[3:]  for x in SEC_UTC_list]


        df['year']  = year
        df['month'] = month
        df['day']   = day
        df['hours']  = hours
        df['minutes'] = minutes
        df['secs']  = secs
        df['millsecs'] = millsecs

        fix_decimal = []
        for i,val in enumerate(df['secs'].astype(str)):
            if val.find('.') == 1:
                fix_decimal.append( '0'+val[:-1])
            else:
                fix_decimal.append( val)

        year        = list(map(int, df['year'].values))
        month       = list(map(int, df['month'].values))
        day         = list(map(int, df['day'].values))
        hour        = list(map(int, df['hours'].values))
        minute      = list(map(int, df['minutes'].values))
        second      = list(map(int, fix_decimal))
        millsecs = list(map(int, df['millsecs'].values))

        DATE = list(map(datetime, year,month, day, hour,minute,second,millsecs ))
        return(DATE)
    
    
    def iteration_number(self, iieout_filename):
        '''
        This function opens the iieout file, and returns the final iteration number
        '''

        with open(iieout_filename, 'r') as f:
            for line_no, line in enumerate(f):
                if 'CONVERGENCE' in line:
                    line_text = line
#                     print(line)
#         print('line_text',line_text)
#         print('num_iters',line_text)

        num_iters = float(line_text[38:42])
        self.total_iterations = int(num_iters) - 1

        if len(str(self.total_iterations)) == 1:
             self.str_iteration = ' '+str(self.total_iterations)  # add a space if the iteration number is not double digit
        else:
             self.str_iteration =     str(self.total_iterations)
                
#         print('total_iterations',self.total_iterations)
#         print('str_iteration',self.total_iterations)

        return(self.total_iterations, self.str_iteration)
    
    
    
    def organize_output_object_keys(self, data_keys, arc, iarc, num_arcs):    
        ''' 
        This function cleans the keys that are stored in the object:   
         The run parameters are stored in their own dimension under 'run_parameters'.
                     RUNOBJECT.__dict__['run_parameters']
         The remaining datasets are left in the 1st dimension
                     RUNOBJECT.__dict__.keys()
        '''
        
        global_keys = [
                    'run_settings',
                    'satellite',
                    'den_model',
                    'empirical_accels',
                    'SpecialRun_name',
                    'arc_input',
                    'options_in',
                    'verbose',
                    'DEN_DIR',
                    'SETUP_DEN_DIR',
                    'iisset_den',
                    'ACCELS',
                    'GDYN_version',
                    'G2SDIR',
                    'G2EDIR',
                    'SATELLITE_dir',
                    'SATID',
                    'YR',
                    'DATA_TYPE',
                    'grav_id',
                    'g2b_file',
                    'atgrav_file',
                    'ephem_file',
                    'gravfield_file',
                    'arc_length',
                    'path_to_model',
                      ]
        
        data_keys.append('run_parameters'+arc)
        data_keys.append('global_params')
        to_move_and_delete = []
        self.__dict__['run_parameters'+arc]    = {}
        self.__dict__['global_params']    = {}
        ####   Loop thru the keys stored in self.  If the key is NOT in the Data_keys
        ####     it will get moved and nested in a new key called run_parameters
        for ii,i in enumerate(self.__dict__.keys()):
            if i in data_keys:
                pass
            else:
                to_move_and_delete.append(i)
                if i in global_keys:
                    self.__dict__['global_params'][i] =  self.__dict__[i]
                else:
                    self.__dict__['run_parameters'+arc][i] =  self.__dict__[i]
                
        #### Once you have moved the keys to be nested under run_parameters,
        ####    delete them from the 0th dimension
#         print(iarc+1)
#         print(num_arcs)

        ##### On the last arc, delete the additional stuff
        if iarc+1 == num_arcs :
#             print('FINAL ARC-- Deleting extra keys from 0th dim')
                     #             print(self.__dict__.keys())
    
    
            ARC_FILES = self.make_list_of_arcfilenames()
            for i in ARC_FILES:
                if os.path.exists(self.path_to_model+'DENSITY/'):
                    os.chdir(self.path_to_model+'DENSITY/')
                    os.system('bzip2 -v '+ i )

                if os.path.exists(self.path_to_model+'ORBITS/'):
                    os.chdir(self.path_to_model+'ORBITS/')
                    os.system('bzip2 -v '+i+'_orb1')

                    

            for i_del in to_move_and_delete:         
                del self.__dict__[i_del]
            
        return(self)
            
        
        
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

                                       

                        
                        
                        



# %load_ext autoreload
# %autoreload 2

# import pandas as pd
# import sys
# sys.path.insert(0,'/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_dir/')
# from common_functions import Convert_cartesian_to_RSW, Convert_cartesian_to_NTW_getT, Convert_cartesian_to_NTW_returnall


# def ResidInvestigation_make_common_residsDF(obj_m1):



#     ###### GET THE PCE DATA:
#     StateVector_PCE_datafile = '/data/data_geodyn/inputs/icesat2/setups/PCE_ascii.txt'
#     SAT_ID = int(obj_m1.__dict__['global_params']['SATID'])
#     which_stat = 'CURRENT_VALUE'

#     for ii,arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):#[::2]):
#         print(arc)

#         ####--------------------- Residual  ---------------------

#         arc_first_time  = obj_m1.__dict__['Trajectory_orbfil'][arc]['data_record']['Date_UTC'].iloc[0]
#         arc_last_time   = obj_m1.__dict__['Trajectory_orbfil'][arc]['data_record']['Date_UTC'].iloc[-2]

#         arc_first_time_str     =  str(arc_first_time)#.replace( "'",' ') 
#         arc_last_time_str      =  str(arc_last_time)#.replace( "'",' ') 


#         A=[]
#         for i,val in enumerate(np.arange(-20,20)):
#             A.append(str(pd.to_datetime(arc_first_time)+pd.to_timedelta(val,'s')))
#         B=[]
#         for i,val in enumerate(np.arange(-20,20)):
#             B.append(str(pd.to_datetime(arc_last_time)+pd.to_timedelta(val,'s')))

#         ####---------------------------------------------------------
#         last_line = False
#         with open(StateVector_PCE_datafile, 'r') as f:
#             for line_no, line_text in enumerate(f):
#                 if any(times in line_text for times in A):
#                     first_line = line_no
#                 if any(times in line_text for times in B):
#                     last_line = line_no
#                     break

#             if not last_line:
#                 last_line = first_line +32220
#                 print('No matching lastline time: ',arc_last_time_str, last_line )

#         ####   IF YOU GET AN ERROR HERE stating that either first_line or last_line is 
#         ####    It is probably an issue with the date in the arc not matching up with the dates given in the PCEfile
#         print('Loading PCE data...')
#         PCE_data = pd.read_csv(StateVector_PCE_datafile, 
#                     skiprows = first_line, 
#                     nrows=last_line-first_line,           
#                     sep = '\s+',
#                     dtype=str,
#                     names = [
#                             'Date',
#                             'MJDSECs', 
#                             'RSECS', #(fractional secs)
#                             'GPS offset', # (UTC - GPS offset (secs))
#                             'X',
#                             'Y',
#                             'Z',
#                             'X_dot',
#                             'Y_dot',
#                             'Z_dot',
#                             'YYMMDDhhmmss',
#                                 ],)

#         PCE_data['Date_pd'] = pd.to_datetime(PCE_data['Date'])
#         orbfil_arc1 = obj_m1.__dict__['Trajectory_orbfil'][arc]['data_record']
#         orbfil_arc1['Date_pd'] = pd.to_datetime(orbfil_arc1 ['Date_UTC'])


#         ### C_1 is a dataframe containing all data between the two files where the dates match
#         C_1 = pd.merge(left=orbfil_arc1, left_on='Date_pd',
#              right=PCE_data, right_on='Date_pd')

#     return(C_1)


# def ResidInvestigation_get_residuals_coordsystems(C_1):

    
    
#     ### Convert the ORBIT FILE data to NTW
#     print('Converting OrbitFile data to other coordinates...')

#     data_orbfil = {}
#     data_PCE    = {}
#     resids      = {}

#     X = C_1['Satellite Inertial X coordinate']
#     Y = C_1['Satellite Inertial Y coordinate']
#     Z = C_1['Satellite Inertial Z coordinate']
#     Xdot = C_1['Satellite Inertial X velocity']
#     Ydot = C_1['Satellite Inertial Y velocity']
#     Zdot = C_1['Satellite Inertial Z velocity']
#     state_vector = np.transpose(np.array([X, Y, Z, Xdot, Ydot, Zdot]))
#     data_orbfil['Date'] = C_1['Date_pd']

#     ##### NTW Coordinate System
#     NTW_orbfil  = [Convert_cartesian_to_NTW_returnall(x) for x in state_vector]
#     data_orbfil['N'] = np.array(NTW_orbfil)[:,0]
#     data_orbfil['T'] = np.array(NTW_orbfil)[:,1]
#     data_orbfil['W'] = np.array(NTW_orbfil)[:,2]
#     ##### XYZ Coordinate System
#     data_orbfil['X'] = X
#     data_orbfil['Y'] = Y
#     data_orbfil['Z'] = Z
#     ##### R theta phi Coordinate System
#     data_orbfil['R']     = np.sqrt( np.square(X) + 
#                                     np.square(Y) +
#                                     np.square(Z) )
#     data_orbfil['theta'] = np.arctan(Y / X)
#     data_orbfil['phi']   = np.arccos(Z / (np.sqrt( np.square(X) + 
#                                     np.square(Y) +
#                                     np.square(Z) )))


#     ### Convert the PCE data to NTW
#     print('Converting PCE data to other coordinates...')
#     X = C_1['X'].astype(float)
#     Y = C_1['Y'].astype(float)
#     Z = C_1['Z'].astype(float)
#     Xdot = C_1['X_dot'].astype(float)
#     Ydot = C_1['Y_dot'].astype(float)
#     Zdot = C_1['Z_dot'].astype(float)
#     state_vector = np.transpose(np.array([X, Y, Z, Xdot, Ydot, Zdot]))
#     data_PCE['Date'] = C_1['Date_pd']

#     ##### NTW Coordinate System
#     NTW_PCE  = [Convert_cartesian_to_NTW_returnall(x) for x in state_vector]
#     data_PCE['N'] = np.array(NTW_PCE)[:,0]
#     data_PCE['T'] = np.array(NTW_PCE)[:,1]
#     data_PCE['W'] = np.array(NTW_PCE)[:,2]
#     ##### XYZ Coordinate System
#     data_PCE['X'] = X
#     data_PCE['Y'] = Y
#     data_PCE['Z'] = Z
#     ##### R theta phi Coordinate System
#     data_PCE['R']     = np.sqrt( np.square(X) + 
#                                     np.square(Y) +
#                                     np.square(Z) )
#     data_PCE['theta'] = np.arctan(Y / X)
#     data_PCE['phi']   = np.arccos(Z / (np.sqrt( np.square(X) + 
#                                     np.square(Y) +
#                                     np.square(Z) )))


#     ### RESIDUALS:
#     resids['Date'] = C_1['Date_pd']

#     ##### NTW Coordinate System
#     resids['N'] = (np.array(data_PCE['N']) - np.array(data_orbfil['N']))
#     resids['T'] = (np.array(data_PCE['T']) - np.array(data_orbfil['T']))
#     resids['W'] = (np.array(data_PCE['W']) - np.array(data_orbfil['W']))
#     ##### XYZ Coordinate System
#     resids['X'] = (np.array(data_PCE['X']) - np.array(data_orbfil['X']))
#     resids['Y'] = (np.array(data_PCE['Y']) - np.array(data_orbfil['Y']))
#     resids['Z'] = (np.array(data_PCE['Z']) - np.array(data_orbfil['Z']))
#     ##### R theta phi Coordinate System
#     resids['R']     = (np.array(data_PCE['R'])     - np.array(data_orbfil['R']))
#     resids['theta'] = (np.array(data_PCE['theta']) - np.array(data_orbfil['theta']))
#     resids['phi']   = (np.array(data_PCE['phi'])   - np.array(data_orbfil['phi']))

#     orbit_resids = {} 
#     orbit_resids['data_orbfil'] = data_orbfil
#     orbit_resids['data_PCE']    = data_PCE
#     orbit_resids['resids']      = resids



#     return(orbit_resids)

