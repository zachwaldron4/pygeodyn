"""
_summary_

_extended_summary_


TODO:
    * add Spire to the physics model path writing section

"""

#### ----------------------------------------
#### Import modules:
#### -----------------
import os
import os.path
import sys
import time
import pandas as pd
import numpy  as np
from datetime import datetime


class Util_Tools:
    
    def __init__(self):  
        pass

    def set_density_model_setup_params(self, den_model):
        
#         print('UTIL ---- set_density_model_setup_params ')
        
        if den_model == 'msis86':
            self.SETUP_DEN_DIR = 'msis'
            self.iisset_den = '86'
        elif den_model == 'msis00':
            self.SETUP_DEN_DIR = 'msis'
            self.iisset_den = '86'
        elif den_model == 'msis2':
            self.SETUP_DEN_DIR = 'msis'
            self.iisset_den = '86'
        elif den_model == 'dtm87':
            self.SETUP_DEN_DIR = 'dtm87'
            self.iisset_den = '87'
        elif den_model == 'jaachia71':
            self.SETUP_DEN_DIR = 'jaachia71'
            self.iisset_den = '71'
        #### Running a model in Kamodo uses OrbitCloud method (model_oc)
        elif den_model == 'tiegcm_oc':
            self.SETUP_DEN_DIR = 'tiegcm_oc'
            self.iisset_den = '86'
        elif den_model == 'hasdm_oc':
            self.SETUP_DEN_DIR = 'hasdm_oc'
            self.iisset_den = '86'
        elif den_model == 'ctipe_oc':
            self.SETUP_DEN_DIR = 'ctipe_oc'
            self.iisset_den = '86'

        elif den_model == 'jb2008':
            self.SETUP_DEN_DIR = 'jb2008'
            self.iisset_den = '71'
            
        elif den_model == 'dtm2020_o':   
            self.SETUP_DEN_DIR = 'dtm2020_o'
            self.iisset_den = '87' # Will run GEODYN WITH DTM87 according to IIS, but switch out for DTM2020 in DRAG.f90

        elif den_model == 'dtm2020_r':   ### use the research vers. of dtm2020
            self.SETUP_DEN_DIR = 'dtm2020_r'
            self.iisset_den = '87' # Will run GEODYN WITH DTM87 according to IIS, but switch out for DTM2020 in DRAG.f90        else:
            print('Density model string formats: [msis86, msis00, msis2, dtm87, jaachia71, tiegcm_oc, jb2008, dtm2020_o]')   

        elif den_model == 'gitm':
            self.SETUP_DEN_DIR = 'gitm'
            self.iisset_den = '86'
            
        elif den_model == 'orbit_cloud':
            self.SETUP_DEN_DIR = 'orbit_cloud'
            self.iisset_den = '00'

        #### -------------------------------------------------------    
        #### For the Physics models that are connected via Kamodo,
        ####  Write the data path to a file to be read by fortran.
        ####
        from re import search
        if search('tiegcm', self.prms['den_model']):
            if self.prms['satellite'] == 'icesat2':
                self.model_data_path = self.prms['model_data_path']
                filemodels = open(self.path_io_geodyn+"/geodyn_modelpaths.txt","w+")
                filemodels.writelines(self.model_data_path+'\n')
                filemodels.writelines('none'+'\n')
                filemodels.close()
        elif search('hasdm', self.prms['den_model']):
            if self.prms['satellite'] == 'icesat2':
                self.model_data_path = self.prms['model_data_path']
                filemodels = open(self.path_io_geodyn+"/geodyn_modelpaths.txt","w+")
                filemodels.writelines(self.model_data_path+'\n')
                filemodels.writelines('none'+'\n')
                filemodels.close()
        elif search('ctipe', self.prms['den_model']):
            if self.prms['satellite'] == 'icesat2':
                self.model_data_path = self.prms['model_data_path']
                filemodels = open(self.path_io_geodyn+"/geodyn_modelpaths.txt","w+")
                filemodels.writelines(self.model_data_path+'\n')
                filemodels.writelines('none'+'\n')
                filemodels.close()
        elif search('gitm', self.prms['den_model']):
            if self.prms['satellite'] == 'icesat2':
                self.model_data_path = self.prms['model_data_path']
                filemodels = open(self.path_io_geodyn+"/geodyn_modelpaths.txt","w+")
                filemodels.writelines(self.model_data_path+'\n')
                filemodels.writelines('none'+'\n')
                filemodels.close()                
        else:
            self.model_data_path = " "
            filemodels = open(self.path_io_geodyn+"/geodyn_modelpaths.txt","w+")
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

    def geodyn_modify_inputs(self, DRHODZ_update, density_model):
        '''
        This function overwrites a text file that will be fed into 
            the GEODYN IIE routine (mainly at and around the DRAG.f90 routine).
            
        These options serve effectively as variables for making changes to the GEODYN code after compiling.
            Options include:
                DRHODZ_update
                Choice of Density model
                Choice of CD Physics model
                Pass a value into fortran (to be used for anything... i have used it to scale the density)
                
        An additional file has been added that includes the various parameters that are inputs to the DRIA physical CD model.
        
        '''
        if DRHODZ_update== True:
            drhodz_val = '1'
        elif DRHODZ_update== False:
            drhodz_val = '0'
        else:
            sys.exit("DRHODZ option is in incorrect format")
        #
        ### The following models are run with 86 (msis86) as the input to IIS on the ATMDEN CARD    
        if density_model== 'msis86':
            model_val = '0'
        elif density_model== 'msis00':
            model_val = '1'
        elif density_model== 'msis2':
            model_val = '2'
        elif density_model== 'tiegcm_oc':
            model_val = '6'
        elif density_model== 'hasdm_oc':
            model_val = '7'
        elif density_model== 'ctipe_oc':
            model_val = '8'
        elif density_model== 'gitm':
            model_val = '9'        

        # The following models are run with 71 (jaachia71) as the input to IIS
        # on the ATMDEN CARD    
        elif density_model== 'jaachia71':
            model_val = '0'       
        elif density_model== 'jb2008':
            model_val = '1' 
        #
        # The following models are run with 87 (dtm87) as the input to IIS on
        # the ATMDEN CARD    
        elif density_model== 'dtm87':
            model_val = '0'
        elif density_model== 'dtm2020_o':
            model_val = '1'
        elif density_model== 'dtm2020_r':
            model_val = '2'
        #        
        #
        else:
            sys.exit("Density Model Option (den_model) is in an incorrect format")
        
        
        #
        #  Save the options to a file (overwrite it) to be read into the GEODYN
        #  fortran code


        file1 = open(self.path_io_geodyn+"/geodyn_options.txt","w+")
        
        file1.writelines(drhodz_val+'\n') # first value is for DrhoDz
        file1.writelines(model_val +'\n') # 2nd values is for model switching
        #
        #######  ADD AN OPTIONAL INPUT VALUE TO FORTRAN
        if self.prms['value_io_fortran']  == 'None' :
            file1.writelines('1'+'\n')
        else:
            file1.writelines(self.prms['value_io_fortran']+'\n')
        #
        #######  Choose CD Model to be used by GEODYN
        if self.prms['cd_model']  == 'BWDRAG' :
            file1.writelines('1'+'\n')
            
        elif self.prms['cd_model']  == 'DRIA' :
            file1.writelines('2' +'\n')
        else:
            file1.writelines('0' +'\n')

        
        if self.prms['scaling_factor'] == True:
            file1.writelines('1'+'\n')
        else:
            file1.writelines('0'+'\n')

        file1.close()
        
        
        ##### MAKE CD MODEL INPUT FILE
        
        if self.prms['cd_model']  == 'DRIA' :
            file_CDparams = open(self.path_io_geodyn+"/cd_params.txt","w+")
            file_CDparams.writelines(str(self.cd_model_params['MS'])    + '\n') 
            file_CDparams.writelines(str(self.cd_model_params['TW'])    + '\n')  
            file_CDparams.writelines(str(self.cd_model_params['ALPHA']) + '\n')
            file_CDparams.writelines(str(self.cd_model_params['KL'])    + '\n')
            file_CDparams.writelines(str(self.cd_model_params['FRACOX'])+ '\n')
            
            file_CDparams.close()

        else:
            pass
        

        
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
                    'prms',
                    'prms_arc',
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
                    'arc_length_h',
                    'path_to_model',
                    'arcdate_v2',
                    'file_statevector_ICs']
        
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
                    os.system('bzip2 -v '+ i +'drag_file' )

                if os.path.exists(self.path_to_model+'ORBITS/'):
                    os.chdir(self.path_to_model+'ORBITS/')
                    os.system('bzip2 -v '+i+'_orb1')

                    

            for i_del in to_move_and_delete:         
                del self.__dict__[i_del]
                
                
            ### Delete the extra stuff in to runparams##arc## and globalparams
            del self.__dict__['global_params']['prms']['request_data']
#             for i_del in to_move_and_delete:         

        return(self)
            
        
        
    def check_if_run_converged(self, iieout_filename):
        '''Check if IIE converged properly. If it did not print to the console.
        
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

                                       

                        
                        
    def set_file_paths_for_multiple_arcs(self, arc_val, iarc, unzip_and_loadpaths=False):
        '''
        Handles the Arc naming conventions
        Construct a way to read in the satellite specific filenames.
        
        :param: arc_val aslkjdkldsj definintinonasldkfjsaldkj
        :output: slkdfjlksdjf
        
        '''
        
        self.run_ID = 'Run #'+ str(iarc+1)
                
        #### Count how many arcs of this name there are
        for x_arc in self.arc_input:
            if self.arc_input.count(x_arc) == 1:
                iarc = 0
#             elif self.arc_input.count(arc_val) > 1:
#                 self.unique_arc_count+=1
#                 iarc = self.unique_arc_count
#                 print('There are multiples of this arc, this is #',iarc)
            else:
#                 print('There are multiples of this arc, this is #',iarc+1)
                pass
        self.arc_name_id = arc_val
        self.YR  = self.arc_name_id[0:4]
        doy = self.arc_name_id[5:]
#         self.arcdate_for_files = '%02d.%d%d'   % ( str(iarc+1), self.YR, doy)
#         self.arcdate_v2        = '%02d.%d.%d'   % ( str(iarc+1), self.YR, doy) #str(iarc+1)+'.'+ self.YR + '.' + doy 
        self.arcdate_for_files = '%d%03d.%02d'   % ( int(self.YR), int(doy), (iarc+1))
        self.arcdate_v2        = '%d.%03d.%02d'   % ( int(self.YR), int(doy), (iarc+1)) #str(iarc+1)+'.'+ self.YR + '.' + doy 


        ####
        #### The setup and external attitude files have same naming convention
        self.setup_file_arc    = 'iisset.'+self.arc_name_id
        # self.external_attitude = 'EXAT01.'+self.arc_name_id+'.gz'
        # self.filename_exat = 'EXAT01.'+self.arc_name_id+'.gz'
        ####
        ### Now specify what we want the output files to be named, by arc

        self.ARC = (self.prms['satellite']    + '_' + 
                    self.arcdate_for_files+ '_' + 
                    # self.prms['arc_length'] + '.' +  
                    self.prms['den_model'] + '.' +
                    self.prms['file_string'])

        
#         self.SERIES = self.den_model + '_' + self.ACCELS + self.run_specifier
        self.series = self.prms['den_model'] + '_' + self.prms['cd_model'] + self.prms['run_specifier']
        # if iarc ==0:
        #     dir_out = self.prms['path_to_output_directory'] 
        # else:
        # dir_out = self.dir_output_raw
        self.path_to_model = self.dir_output_raw
                            #('/data/data_geodyn/results/'+
                              #     self.satellite +'/'+
                               #    self.den_model+'/'+  
                                #   self.den_model+'_'+ self.ACCELS + self.run_specifier +'/')
        # file_name =   self.ARC                
        ####  save the specific file names as "private members" with the _filename convention
        self._asciixyz_filename = self.path_to_model+'/XYZ_TRAJ/'+ self.ARC
        self._orbfil_filename   = self.path_to_model+'/ORBITS/'+ self.ARC+'_orb1'
        self._iieout_filename   = self.path_to_model+'/IIEOUT/'  + self.ARC
        self._density_filename  = self.path_to_model+'/DENSITY/' + self.ARC     
        self._drag_filename     = self.path_to_model+'/DENSITY/' + self.ARC +'drag_file'    
        self._accel_filename    = self.path_to_model+'/ORBITS/' + self.ARC +'_accel_file'    
#         self._EXTATTITUDE_filename = self.EXATDIR +'/' +self.external_attitude

        
        time.sleep(1)
        
    
    
    def make_list_of_arcfilenames(self):
        '''
        Handles the arc naming conventions for the icesat2 satellite files.
        Construct a way to read in the satellite/arc specific filenames.
        '''
        
        arc_file_list = []
        
#         print('make_list_of_arcfilenames-- self.arc_input: ',self.arc_input)
            
        for i, val in enumerate(self.arc_input):

            #### Count how many arcs of this name there are
            if self.arc_input.count(val) == 1:
#                 print('Only one arc of this name', x_arc)
                i = 0
#             elif self.arc_input.count(arc_val) > 1:
#                 self.unique_arc_count+=1
#                 i = self.unique_arc_count
#                 print('There are multiples of this arc, this is #',iarc)
            else:
#                 print('filename list #',i+1)
                pass

            arc_name_id = val
            YR  = arc_name_id[0:4]
            doy = arc_name_id[5:]
            arcdate_for_files =  '%d%03d.%02d' % ( int(YR), int(doy), (i+1)) # str(i+1)+'.' +YR + doy 
            ####
            ####
            ### Now specify what we what the output arcs to be named.
#             ARC_file = (self.satellite    + '_' + 
#                         arcdate_for_files+ '_' + 
#                         self.arc_length + '.' +  
#                         self.den_model)
            ARC_file = (self.prms['satellite']    + '_' + 
                        arcdate_for_files+ '_' + 
                        # self.prms['arc_length'] + '.' +  
                        self.prms['den_model'] + '.' +
                        self.prms['file_string'])


            arc_file_list.append(ARC_file)
        
#         print('make_list_of_arcfilenames-- arc_file_list: ',arc_file_list)

        return(arc_file_list)
    


    def load_statevectorIC_file(self, epoch_startDT):
        datetype = 'datetime_string'
        date_in_file_flag= False
        import linecache

        ### Only need to use accuracy to within 1 second (ignore the microseconds in the file)

        if datetype == 'datetime_string':
            date_str = str(epoch_startDT)
        elif datetype == 'YYMMDDHHMMSS':
            date_str = datetime.strftime(epoch_startDT, '%y%m%d%H%M%S')


        with open(self.file_statevector_ICs, 'r') as f:
            for line_no, line_text in enumerate(f):
                if date_str in line_text:
                    date_in_file_flag= True
                    print('    ','xyzline',line_no,line_text)
                    break
        if date_in_file_flag == False:
            print(date_str,'not found in file. ')

            ### Find the dates that have the same hour    
            if datetype == 'datetime_string':
                date_roundhour_str = str(epoch_startDT)[:13]
            elif datetype == 'YYMMDDHHMMSS':
                date_roundhour_str = datetime.strftime(epoch_startDT, '%y%m%d%H')

            # print(date_roundhour_str)
            ### Scan through IC file and append a list of dates within the same hour
            line_no_list = []
            line_list = []
            with open(self.file_statevector_ICs, 'r') as f:
                for line_no, line_text in enumerate(f):
                    if date_roundhour_str in line_text:
                        line_no_list.append(line_no)
            
            # print(line_no_list)
            for i in np.arange(line_no_list[0]-10, line_no_list[-1]+10):
                line = linecache.getline(self.file_statevector_ICs,i)
                line_list.append(line)
            dates = []
            for i ,val in enumerate(line_list):
                if datetype == 'datetime_string':
                    dates.append(pd.to_datetime(line_list[i][:19],format='%Y-%m-%d %H:%M:%S'))
                elif datetype == 'YYMMDDHHMMSS':
                    dates.append(pd.to_datetime(line_list[i][:19],format='%y%m%d%H%M%S.%f'))

            ## NEAREST DATETIME -----------------------------------------------

                    
            def nearest(items, pivot):
                return min(items, key=lambda x: abs(x - pivot))
            date_near = nearest(dates, epoch_startDT)
            res = (np.array(dates)==date_near).argmax()
            print("Using ICs from nearest date")
            print('   date:',date_near)
            print('   difference:', abs(date_near - epoch_startDT))

        #    date = line_list[res][:19]
            X     = float(line_list[res][20:37].ljust(20))
            Y     = float(line_list[res][37:54].ljust(20))
            Z     = float(line_list[res][54:71].ljust(20))
            X_dot = float(line_list[res][71:88].ljust(20))
            Y_dot = float(line_list[res][88:105].ljust(20))
            Z_dot = float(line_list[res][105:121].ljust(20))
        else:
            print('Found date in IC file:', str(epoch_startDT))
        xyzline = pd.read_csv(self.file_statevector_ICs, 
                    skiprows = line_no, 
                    nrows=1,           
                    sep = '\s+',
                    dtype=str,
                    names = [
                        'Date',
                        'X',
                        'Y',
                        'Z',
                        'X_dot',
                        'Y_dot',
                        'Z_dot',
                            ],)
        X     =  float(xyzline['X'].values[0].ljust(20))     #'  -745933.8926940708'
        Y     =  float(xyzline['Y'].values[0].ljust(20))     #'  -4864983.834066438'
        Z     =  float(xyzline['Z'].values[0].ljust(20))     #'    4769954.60524261'
        X_dot =  float(xyzline['X_dot'].values[0].ljust(20)) #'  457.44564954037634'
        Y_dot =  float(xyzline['Y_dot'].values[0].ljust(20)) #'   5302.381564886811'
        Z_dot =  float(xyzline['Z_dot'].values[0].ljust(20)) #'    5463.55571622269'
        print(f"   [X,Y,Z]:          [{X    :15.5f}, {Y    :15.5f}, {Z    :15.5f}]")
        print(f"   [Xdot,Ydot,Zdot]: [{X_dot:15.5f}, {Y_dot:15.5f}, {Z_dot:15.5f}]")        
        print()        
        return(X, Y, Z, X_dot, Y_dot, Z_dot)

    def get_arc_values_and_dates(self, bool_elems=True):
        ''' Method that retrieves the arc specific values and dates.
            Must be called before each arc.

            Arc specific values are stored in a attribute dict that gets 
            overwritten for each arc.
        '''
        
        prms_arc = {}
    
        # Initialize the date variables that help with time keeping
        prms_arc['epoch_start'] = self.prms['epoch_start'][self.arcnumber]
        prms_arc['epoch_stop']  = self.prms['epoch_stop'][self.arcnumber]
        #
        prms_arc['epoch_startDT'] = pd.to_datetime(prms_arc['epoch_start'],\
                                                    format='%Y-%m-%d %H:%M:%S')
        prms_arc['epoch_stopDT']  = pd.to_datetime(prms_arc['epoch_stop'],\
                                                    format='%Y-%m-%d %H:%M:%S')
        prms_arc['start_ymdhms']= prms_arc['epoch_startDT'].strftime(format='%y%m%d%H%M%S')
        prms_arc['stop_ymdhms'] = prms_arc['epoch_stopDT'].strftime(format='%y%m%d%H%M%S')

        prms_arc['epoch_delta'] =  pd.to_timedelta(\
                                        prms_arc['epoch_stopDT'] \
                                        - prms_arc['epoch_startDT'],\
                                        'hours')
        prms_arc['arc_length_h'] = prms_arc['epoch_delta'].total_seconds()/3600
        
        
        X    ,Y    ,    Z,\
        X_dot,Y_dot,Z_dot = self.load_statevectorIC_file(prms_arc['epoch_startDT'])

        prms_arc['X'] = X
        prms_arc['Y'] = Y
        prms_arc['Z'] = Z
        prms_arc['X_dot'] = X_dot
        prms_arc['Y_dot'] = Y_dot
        prms_arc['Z_dot'] = Z_dot


    #     # Get the initial conditions in cartesian coords 
    #     if bool_elems:
    #         date_in_file_flag= True
    #         with open(self.file_statevector_ICs, 'r') as f:
    #             for line_no, line_text in enumerate(f):
    #                 if str(prms_arc['epoch_startDT']) in line_text:
    #                     date_in_file_flag= True
    #                     # print('    ','xyzline',line_no,line_text)
    #                     break
    #         if date_in_file_flag == False:
    #             print(str(prms_arc['epoch_startDT']),'not found in file.  Leaving ELEMS as is.')
    #             print('Check that the start date:',str(prms_arc['epoch_startDT']))
    #             print('    is within the PCE date range saved in the file')
    #             print('       ',self.file_statevector_ICs)
    # #             sys.exit()
    #         else:
    #             # print('Found date in IC file:', str(prms_arc['epoch_startDT']))
    #             xyzline = pd.read_csv(self.file_statevector_ICs, 
    #                         skiprows = line_no, 
    #                         nrows=1,           
    #                         sep = '\s+',
    #                         dtype=str,
    #                         names = self.file_statevector_headers,)
    #             prms_arc['X']     =  float(xyzline['X'].values[0].ljust(20))     #'  -745933.8926940708'
    #             prms_arc['Y']     =  float(xyzline['Y'].values[0].ljust(20))     #'  -4864983.834066438'
    #             prms_arc['Z']     =  float(xyzline['Z'].values[0].ljust(20))     #'    4769954.60524261'
    #             prms_arc['X_dot'] =  float(xyzline['X_dot'].values[0].ljust(20)) #'  457.44564954037634'
    #             prms_arc['Y_dot'] =  float(xyzline['Y_dot'].values[0].ljust(20)) #'   5302.381564886811'
    #             prms_arc['Z_dot'] =  float(xyzline['Z_dot'].values[0].ljust(20)) #'    5463.55571622269'


        ### Save as attribute
        self.prms_arc = prms_arc










    # make_dict_header():