#### ===============
#### Import modules:   
#### ===============
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
from   datetime import datetime,timedelta, timezone

import copy
import logging


class PygeodynController():
    """ PygeodynController class documentation
    
    Description: 
    ------------
       Class that controls the files structure setup, and running of GEODYN.
       

    Long Description:
    -----------------
       This Class hosts all the methods (functions) that are used to:
           - create the necessary file structure, 
           - point data inputs to proper locations, 
           - construct a temporary folder for calling geodyn input
           - run geodyn  (in tmp folder) and check for major errors, 
           - rename and organize the data output and save to directories


    Parameters
    ----------
        Inherit UtilControl_Tools : Class
            Contains helpful tools that have been made into functions
        Inherit UtilSetInputs : Class
            Helpful tools that help setup various inputs
        param1 : int
            The first parameter.
        param2 : str
            The second parameter.

    Returns
    -------
        Object
            Returns an object with methods that control running GEODYN
               
    """

    def __init__(self):  
        pass
    
    def setup_directories_and_geodyn_input(self):
        self.verboseprint('Original -- setup_directories_and_geodyn_input()')

        os.chdir('/data/geodyn_proj/pygeodyn')

        self.verboseprint('=================================================')
        self.verboseprint('                VERBOSE OPTION ON                ')
        self.verboseprint('=================================================')
        self.verboseprint('')
        self.verboseprint(self.tabtab,'Current DIR: ', os.getcwd())
        
        now = datetime.now()-timedelta(hours=7)
        current_time = now.strftime("%H:%M:%S")
        print(self.run_ID,"    Current Time =     ", current_time, " GMT-7")
        print(self.run_ID)

        ####-------------------------------------------------------------
        ####       Setup Directory Structure 
        ####-------------------------------------------------------------
                

        #### ---- DIRECTORY PATHS----
        path_run_inputs  = '/data/data_geodyn/inputs/'+self.SATELLITE_dir
#         path_run_outputs =  self.run_settings['path_to_output_directory'] 
        path_extra_dirs  = '/data/data_geodyn/extra_dirs'
       
        #### Set up path for OUTPUT (i.e. results)
                #### SERIES NAME (output directory identifier)
                ##   I decided to make my series 
                ##   identify the density models,
                ##   accelerations, run name
#         self.SERIES = self.DEN_DIR + '_' + self.ACCELS + self.directory_name_specifier
        self.SERIES = self.DEN_DIR + '_' + self.cd_model + self.directory_name_specifier
        self.OUTPUTDIR   = self.run_settings['path_to_output_directory'] + '/'+self.DEN_DIR+'/'+self.SERIES
        # make output directory 
        self.make_directory_check_exist(self.run_settings['path_to_output_directory'])
        self.make_directory_check_exist(self.run_settings['path_to_output_directory'] + '/'+self.DEN_DIR)
        

        #### Extra and Temporary directories
        TMPDIR       = '/data/data_geodyn/' +'tmp/'+self.SERIES 
        #### Set up path for tmp run folder
        self.TMPDIR_arc = TMPDIR+'/'+self.ARC
        ## Make temporary directory path
        self.make_directory_check_exist('/data/data_geodyn/') 
        self.make_directory_check_exist('/data/data_geodyn/'+'/tmp') 
#         self.make_directory_check_exist('/data/data_geodyn/'+'/tmp/'+self.SATELLITE_dir)
        self.make_directory_check_exist('/data/data_geodyn/'+'/tmp/'+self.SERIES)

        ## Input file directories (ftn 05)
        self.INPUTDIR  = path_run_inputs + '/setups'
        DIRGRAV   = path_run_inputs     + '/gravity'
        G2BDIR    = path_run_inputs     + '/g2b'
        ATGRAVDIR = path_run_inputs     + '/atgrav'
        EPHEMDIR  = path_run_inputs     + '/ephem'
        self.EXATDIR = path_run_inputs  + '/external_attitude'

        
        #---- Planetary Ephemeris
        self._ephem_filename      = EPHEMDIR  +'/'+ self.ephem_file
        #---- Atmospheric Gravity
        self._ATGRAV_filename     = ATGRAVDIR +'/'+ self.atgrav_file
        #----- Gravity Field
        self._grav_field_filename = DIRGRAV   +'/'+ self.gravfield_file
        #---- GDYN specific binary observation data 
        self._G2B_filename        = G2BDIR    +'/'+ self.g2b_file        
        #---- GDN-table - Solar flux,Ap,Kp,PolarMotion,A1-UTC,A1-UT1
        self._gdntable_filename   = '/data/data_geodyn/inputs/common/gdntable.data'
        #----- Solar Radiation file. #### I dont have one  ####
        #        SOLRAD_filename = ARCFIL+'.'+self.GRAVITY
        #----- External Attitude File
        #          done in the satellite class
        if not self.external_attitude:
            pass
        else:
            self._EXTATTITUDE_filename = self.EXATDIR +'/' +self.external_attitude
    
        #### Remove old TMPDIR version and remake it 
        os.system('rm -rf '+self.TMPDIR_arc)

        # chmod 777 gives the tmp directory read, write and overwrite priveleges.
        self.make_directory_check_exist(self.TMPDIR_arc) 
        os.system('chmod 777 '+self.TMPDIR_arc)
        
        
        ### Construct the setup file for the Arc of Choice
        #---- Input iisset file (fort.05)
        self._INPUT_filename      = self.INPUTDIR  +'/' +self.setup_file_arc +'.bz2'
        

            
            
            
            
        ###### RECOMPILE the FORTRAN CODE IN THE GEODYN MODS FOLDER IF REQUESTED
           ###   This is useful if you want to iteratively change something in fortran 
           ##### HOWEVER IT IS BEST TO INPUT VALUES FROM A FILE AND DONT TRY TO RECOMPILE
#         if self.recompile_on==True:
#             print('CHECK some paths:', self.G2EDIR)
            
#             os.chdir(self.G2EDIR)

#             #### build the recompile command
#             print(f'   Recompiling the IIE/',self.GDYN_version,'/compile_all_models     script')
                  
# #             os.system('chmod 777 '+self.TMPDIR_arc)
#             command_recompileIIE = './ compile_all_models'

#             subprocess.run(command_recompileIIE, shell = True)
#             time.sleep(5)
                
#             os.chdir('/data/geodyn_proj/pygeodyn')

            
            
            
    def make_output_directories(self):
        '''
        This function builds the output directory structure and the temporary run directory
        
        '''
        self.verboseprint('Original -- make_output_and_temprun_directories()')

        #-------------------------------------------------------------
        #  Make Directories if they do not exists
        #-------------------------------------------------------------

        ##### Make the GEODYN output directories to be saved later
        #     If the below directories do not exists, build them: 
        self.make_directory_check_exist(self.OUTPUTDIR) 
        self.make_directory_check_exist(self.OUTPUTDIR+'/ORBITS/')
#         self.make_directory_check_exist(self.OUTPUTDIR+'/RESIDS/')
#         self.make_directory_check_exist(self.OUTPUTDIR+'/PUNCH/')
        self.make_directory_check_exist(self.OUTPUTDIR+'/IIEOUT/')
#         self.make_directory_check_exist(self.OUTPUTDIR+'/TELEM/')
#         self.make_directory_check_exist(self.OUTPUTDIR+'/EMAT/')
#         self.make_directory_check_exist(self.OUTPUTDIR+'/EMAT/scans/')
#         self.make_directory_check_exist(self.OUTPUTDIR+'/IISSET/')
        self.make_directory_check_exist(self.OUTPUTDIR+'/DENSITY/')
#         self.make_directory_check_exist(self.OUTPUTDIR+'/XYZ_TRAJ/')
#         self.make_directory_check_exist(self.OUTPUTDIR+'/KEP_TRAJ/')

        A = subprocess.check_output(["gfortran", "--version"])
        gfortran_v_string = str(A).split('nCopyright')[0][2:-1]
        linux_v_string = subprocess.check_output(["uname", "-a"])
#         linux_v_string = str(A).split('nCopyright')[0][2:-1]

        
        self.log_file = self.OUTPUTDIR+'/pygeodyn_runlog_'+self.ARC+'.txt'
#         print('log_file', self.log_file)
        with open(self.log_file, 'w') as f:
            f.write('\n')
            f.write('\n')
            f.write('\n')
            f.write('       RRRRRRRRRR         UUUU          UUUU      NNNNN        NNNN             \n')
            f.write('       RRRRRRRRRRRR       UUUU          UUUU      NNNNNN       NNNN             \n')
            f.write('       RRRR     RRRR      UUUU          UUUU      NNNNNNN      NNNN             \n')
            f.write('       RRR       RRR      UUUU          UUUU      NNNN NNN     NNNN             \n')
            f.write('       RRRR     RRRR      UUUU          UUUU      NNNN  NNN    NNNN             \n')
            f.write('       RRRRRRRRRRR        UUUU          UUUU      NNNN   NNN   NNNN             \n')
            f.write('       RRRRRR             UUUU          UUUU      NNNN   NNN   NNNN             \n')
            f.write('       RRRRRRRR           UUUU          UUUU      NNNN   NNN   NNNN             \n')
            f.write('       RRRR  RRRR         UUUU          UUUU      NNNN    NNN  NNNN             \n')
            f.write('       RRRR   RRRR        UUUUU        UUUUU      NNNN     NNN NNNN             \n')
            f.write('       RRRR     RRRR      UUUUUU      UUUUUU      NNNN      NNNNNNN             \n')
            f.write('       RRRR       RRRR     UUUUUUUUUUUUUUUU       NNNN       NNNNNN             \n')
            f.write('       RRRR        RRR       UUUUUUUUUUUU         NNNN        NNNNN             \n')
            f.write('\n')
            f.write('\n')
            f.write('\n')
            f.write('       LLLL                  OOOOOOOOOOOO            GGGGGGGGGG                 \n')
            f.write('       LLLL                OOOOOOOOOOOOOOOO        GGGGGGGGGGGGGG               \n')
            f.write('       LLLL               OOOOOOOOOOOOOOOOOO      GGGGGGGGGGGGGGGG              \n')
            f.write('       LLLL               OOOOO        OOOOO      GGGGG      GGGGG              \n')
            f.write('       LLLL               OOOO          OOOO      GGGG         GG               \n')
            f.write('       LLLL               OOOO          OOOO      GGGG                          \n')
            f.write('       LLLL               OOOO          OOOO      GGGG                          \n')
            f.write('       LLLL               OOOO          OOOO      GGGG      GGGGGGGGG           \n')
            f.write('       LLLL               OOOO          OOOO      GGGG         GGGG             \n')
            f.write('       LLLL               OOOOO        OOOOO      GGGGG       GGGGG             \n')
            f.write('       LLLL               OOOOOO      OOOOOO      GGGGGGGGGGGGGGGG              \n')
            f.write('       LLLLLLLLLLLLLLL     OOOOOOOOOOOOOOOO        GGGGGGGGGGGGGG               \n')
            f.write('       LLLLLLLLLLLLLLL       OOOOOOOOOOOO            GGGGGGGGGG                 \n')
            f.write('\n')
            f.write('\n')
            f.write('\n')
            f.write(' ———————————————————————————————————————————————————————————————————————————————————')
            f.write('\n')
            f.write('\n')
            f.write('\n')
            f.write('SYSTEM INFORMATION \n')
            f.write("------------------ \n")
            f.write(f"    Time of Run        {datetime.now()} \n")
            f.write(f"    User               {self.user}     \n")
            f.write(f"    System             CCMC AWS Server  \n")
            f.write(f"    System info        {linux_v_string} \n")
            f.write(f"    python version     {sys.version.split('GCC')[0][:-2]} \n")
            f.write(f"    gcc                [{sys.version.split('GCC')[1]} \n")
            f.write(f"    gfortran (system)  {gfortran_v_string} \n")
            f.write('\n')
            f.write('\n')
            f.write('\n')                        
            f.write( "Pygeodyn Run Parameters (Input Call) \n")
            f.write( "----------------------- \n")
            f.write( "    run_params = {} \n")
            f.write(f"    run_params['arc']                      = {self.arc_input}  \n")
            f.write(f"    run_params['satellite']                = {self.satellite}  \n")
            f.write(f"    run_params['den_model']                = {self.den_model}  \n")
            f.write(f"    run_params['directory_name_specifier'] = {self.directory_name_specifier}  \n")
            f.write(f"    run_params['verbose']                  = {self.verbose}  \n")
            f.write(f"    run_params['geodyn_StepSize'] = {self.geodyn_StepSize}  \n")           
            f.write(f"    run_params['action']         = {'run'}  \n")                
            f.write(f"    #                                        \n")           
            f.write(f"    ### Load the data into an object         \n")           
            f.write(f"    Obj_Geodyn = Pygeodyn(run_params)       \n")           
            f.write(f"    Obj_Geodyn.RUN_GEODYN()                  \n")           
            f.write('\n')
            f.write('\n')
            f.write('\n')                        

            
            f.write( "GEODYN FILE INFORMATION \n")
            f.write( "----------------------- \n")
            f.write(f"  DIRECTORIES                                   \n")
            f.write(f"    Input Directory:                {self.OUTPUTDIR}/ \n")
            f.write(f"    Temporary Directory             {self.TMPDIR_arc}/ \n")
            f.write(f"    Output Directory:               {self.OUTPUTDIR}/ \n")
            f.write('\n')
            f.write(f"  INPUT FILES                                   \n")
            f.write(f"    Planetery Ephemeris file        {self._ephem_filename} \n")
            f.write(f"    Atmospheric Gravity file        {self._ATGRAV_filename} \n")
            f.write(f"    Gravity Field file              {self._grav_field_filename} \n")
            f.write(f"    G2B observation file            {self._G2B_filename} \n")
            f.write(f"    GDN table                       {self._gdntable_filename} \n")
            f.write(f"    External Attitude file          {self._EXTATTITUDE_filename} \n")
            f.write('\n')
            f.write('\n')
            f.write('\n')
            f.write( "RUN PARAMETERS          \n")
            f.write( "-------------- \n")
            f.write(f"    Satellite                       {self.satellite} \n")
            f.write(f"    Arc name                        {self.ARC} \n")
            f.write(f"    Density model                   {self.DEN_DIR} \n")
            f.write(f"    GEODYN Version                  {self.GDYN_version} \n")
            f.write('\n')
            f.write('\n')
            f.write('\n')
            f.write( "SETUP FILE INFO          \n")
            f.write( "--------------- \n")

        
        f.close()
        
        import logging
        ### Make an execution log file
        
#         self.execlog_filename = self.OUTPUTDIR+'/pygeodyn_execlog_'+self.ARC
#         if os.path.exists(self.execlog_filename):
#             os.remove(self.execlog_filename) 

        self.execlog_filename = self.log_file
            
        logging.basicConfig(filename=self.execlog_filename,
                            filemode = 'a',
                            level=logging.INFO,
                            format='%(levelname)s(%(asctime)s) --- %(module)s.%(funcName)s() \n       %(message)s \n',
                            datefmt='%Y-%m-%d %H:%M:%S')

        

        self.clean_iisset_file()
        self._INPUT_filename      = self.TMPDIR_arc  +'/'+'cleaned_setup'+'_'  + self.arcdate_for_files


        with open(self.log_file, 'a') as log_file:
            log_file.write('\n')
            log_file.write('\n')
            log_file.write(' ——————————————————————————————————————————————————————————————————————————————————— ')
            log_file.write('\n')
            log_file.write( "EXECUTION LOG / NOTES  ")
            log_file.write('\n')
            log_file.write('\n')

        
        
    def print_runparameters_to_notebook(self):
        '''
        This is the base version of this method. 
        It is usually overridden in the Satellite Class
        '''
        self.verboseprint('Original -- print_runparameters_to_notebook()')

#                     longest_line = '|'+' File:'+self._iieout_filename
#                     print('+','—'*len(longest_line))
#                     print('|',self.tab,'----------- Execution terminated in IIE before convergence -----------')
#                     print('|',)
#                     print('|', ' File:',self._iieout_filename )
#                     print('|', ' Line number:',line_no )
#                     print('',)
#                     print('',line.rstrip("\n"))
#                     print('',)
#                     print('|',self.tab,'---------------- Continue to the next arc in the list ----------------')
#                     print('+','—'*len(longest_line))
        
        longest_line = '|      '+self.run_ID+"    Output directory:  " + self.OUTPUTDIR
        
        print('+','—'*len(longest_line))
        print('|')
        print('|','---------------------- RUN PARAMETERS  ----------------------')
        print('|')
        print('| ',self.run_ID,"    IISSET Cleaned     " , 'tmp/.../cleaned_setup'+'_' + self.arcdate_for_files)
        print('| ',self.run_ID,"    Density Model:     " , self.DEN_DIR)
        print('| ',self.run_ID,"    GEODYN Version:    " , self.GDYN_version)
        print('| ',self.run_ID,"    ARC run:           " , self.ARC)
        print('| ',self.run_ID,"    Output directory:  " , self.OUTPUTDIR)
        if not self.external_attitude:
            print('| ',self.run_ID,"    EXAT File:         " ,'No external attitude file.')
        else:
            print('| ',self.run_ID,"    EXAT File:         " , self._EXTATTITUDE_filename)
        print('|')
        print('| ',self.run_ID,"    Epoch Start: "  , str(self.epoch_start_dt) )
        print('| ',self.run_ID,"    Epoch End:   "  , str(self.epoch_end_dt)   )
        print('| ',self.run_ID,"    Step Size:   "  , str(self.geodyn_StepSize)                             )

        print('+','—'*len(longest_line))

#         if os.path.exists(self._INPUT_filename):
#             self.verboseprint(self.tabtab,"FORT.5  (input) file:  ", self._INPUT_filename)
#         else:
#             print(self.run_ID,"    FORT.5  (input) file:  ", self._INPUT_filename," not found.")    

#         if os.path.exists(self._G2B_filename):
#             self.verboseprint(self.tabtab,"FORT.40 (g2b)   file:  ", self._G2B_filename)
#         else:
#             print(self.run_ID,"    FORT.40 (g2b)   file:  ", self._G2B_filename," not found.")    

            
            
                


            
            
    def prepare_tmpdir_for_geodyn_run(self):
        '''  This it the base version of this method.
             It canbe overridden in the Satellite Class to be satellite specific. 
             
             Certain satellites and run types require different data inputs on different fortran units.
        '''
        
        logger = logging.getLogger(self.execlog_filename)
        logger.info(f"ORIGINAL- Construct a tmp directory in which to run IIS and IIE")
     
        #### Navigate TO the TMPDIR
        os.chdir(self.TMPDIR_arc)
        
        ####-------------------------------------------------------------
        ####     Construct Common Setup of a GEODYN RUN
        ####         this is run in the TMPDIR_arc
        ####-------------------------------------------------------------
        self.verboseprint('-------------------------------------------------')
        self.verboseprint('       Linking files with the command line       ')
        self.verboseprint('-------------------------------------------------')
        self.verboseprint(self.tabtab,'Current DIR',os.getcwd())
        
        print(self.run_ID,"    Copying input files to temp directory")
        
        if not self.external_attitude:
            pass
        else:
            #### make copy to the External attitude file and save as EXAT01
            if not os.path.exists(self.TMPDIR_arc +'/EXAT01'):
                shutil.copyfile(self._EXTATTITUDE_filename, self.TMPDIR_arc +'/EXAT01')
                self.verboseprint(self.tabtab,'copied:   exat file  > EXAT01')
            else:
                self.verboseprint(self.tabtab,'symlink is set up: EXAT01 file')

        
        #### make symlink to the G2B file and save as ftn40
        if not os.path.exists(self.TMPDIR_arc +'/ftn40'):
            shutil.copyfile(self._G2B_filename, self.TMPDIR_arc +'/ftn40'+'.gz')
            self.verboseprint(self.tabtab,'copied:   g2b file   > ftn40'+'.gz')
        else:
            self.verboseprint(self.tabtab,'copy is set up:  g2b file')

        #### make symlink to the gravity field and save as ftn12
        if not os.path.exists(self.TMPDIR_arc +'/ftn12'):
            shutil.copyfile(self._grav_field_filename, self.TMPDIR_arc +'/ftn12'+'.gz')
            self.verboseprint(self.tabtab,'copied:   grav field > ftn12'+'.gz')
        else:
            self.verboseprint(self.tabtab,'copy is set up: grav_field file')

        #### make symlink to the ephemerides and save as ftn01
        if not os.path.exists(self.TMPDIR_arc +'/ftn01'):
            shutil.copyfile(self._ephem_filename, self.TMPDIR_arc +'/ftn01'+'.gz')
            self.verboseprint(self.tabtab,'copied:   ephem file > ftn01'+'.gz')
        else:
            self.verboseprint(self.tabtab,'copy is set up: ephem file')

        #### make symlink to the gdntable and save as ftn02
        if not os.path.exists(self.TMPDIR_arc +'/ftn02'):
            shutil.copyfile(self._gdntable_filename, self.TMPDIR_arc +'/ftn02'+'')
            self.verboseprint(self.tabtab,'copied:   gdntable   > ftn02'+'')
        else:
            self.verboseprint(self.tabtab,'copy is set up: gdntable file')


        #### make symlink to the ATGRAVFIL and save as fort.18
        if not os.path.exists(self.TMPDIR_arc +'/fort.18'):
            shutil.copyfile(self._ATGRAV_filename, self.TMPDIR_arc +'/fort.18'+'.gz')
            self.verboseprint(self.tabtab,'copied:   atgrav     > fort.18'+'.gz')
        else:
            self.verboseprint(self.tabtab,'symlink is set up: atgrav file')

#         if self.satellite =='starlette':
#             if not os.path.exists(self.TMPDIR_arc+'/ftn05.bz2'):
#                 os.system('cp '+self._INPUT_filename+' '+self.TMPDIR_arc+'/ftn05.bz2')
#                 self.verboseprint(self.tabtab,'copying          : input file')
#             else:
#                 self.verboseprint(self.tabtab,'copied           : input file')

#             if not os.path.exists(self.TMPDIR_arc+'/ftn05'):
#                 os.system('bunzip2 '+self.TMPDIR_arc+'/ftn05.bz2')
#                 self.verboseprint(self.tabtab,'file not zipped  : input file')
#             else:
#                 self.verboseprint(self.tabtab,'file not zipped  : input file')
#         else:
        if not os.path.exists(self.TMPDIR_arc+'/ftn05'):
            os.system('cp '+self._INPUT_filename+' '+self.TMPDIR_arc+'/ftn05')
            self.verboseprint(self.tabtab,'copying          : input file')
        else:
            self.verboseprint(self.tabtab,'copied           : input file')

        if not os.path.exists(self.TMPDIR_arc+'/giis.input'):
            os.system('cp  '+self.TMPDIR_arc+'/ftn05 '+self.TMPDIR_arc+'/giis.input')
            self.verboseprint(self.tabtab,'copying          : giis.input file')
        else:
            self.verboseprint(self.tabtab,'copied              : giis.input file')   

        self.verboseprint('-------------------------------------------------------------------------')
        self.verboseprint('-------------------------------------------------------------------------')

        #### GUNZIP the files:  gzip is a very fast compression option.
        os.system('gunzip -vr *.gz')
        os.system('bunzip2 -v *.bz2')

            
    def run_geodyn_in_tmpdir(self):

        ####-------------------------------------
        ####     RUN GEODYN IIS
        ####-------------------------------------
        
        logger = logging.getLogger(self.execlog_filename)
        logger.info(f" chdir to tmp dir: {self.TMPDIR_arc} ")

        ### Must change directory to run the IIS executable
        os.chdir(self.TMPDIR_arc)

        time.sleep(1)

        #### Before running GEODYN, populate the geodyn_options.txt file with the run options:
        self.geodyn_modify_inputs(self.DRHODZ_update, self.DEN_DIR)
        
        
        #### RUN THE EXECUTABLE
        print()
        print(self.run_ID,"         Running IIS" )
        command_IIS = self.G2SDIR+'/giis2002_gfortran > '+'iisout 2> '+'iiserr'
        logger.info(f" Running IIS: {command_IIS} ")

        subprocess.run(command_IIS, shell = True)
        time.sleep(1)

        #### Save the Interface files from 2s. 
        if not os.path.exists('giie.ft12'):
            os.system('cp ftn41 giie.ft12')
            os.system('cp giie.ft12 ftn12')
        else:
            self.verboseprint('ft12 is ready')
            
        if not os.path.exists('giie.ft11'):
            os.system('cp ftn11 giie.ft11')
            os.system('cp giie.ft11 ftn11')
        else:
            self.verboseprint('ft11 is ready')

        ### Check if there are any errors from IIS output
        _iiserr_filename = self.TMPDIR_arc+'/iiserr'
            # check if size of file is 0
        if os.stat(_iiserr_filename).st_size == 0:
            print(self.run_ID,"         No errors in IIS" )
            print(self.run_ID,"---------End of IIS" )
        else:
            print('ERRORS FOUND IN IIS:', _iiserr_filename)
            with open(_iiserr_filename, 'r') as read_iiserrors:
                # Read & print the entire file
                print(read_iiserrors.read())
            sys.exit()

        
        ###  Then cleanup temporary files.
        os.system('rm -f ftn* fort.*')
        
        
        print()
        print(self.run_ID,"         Running IIE" )

        now = datetime.now()-timedelta(hours=7)
        current_time = now.strftime("%H:%M:%S")
        print(self.run_ID,"         Current Time =", current_time, 'GMT-7')

        
        #=====================================================================
              
        ####-------------------------------------
        ####     RUN GEODYN IIE
        ####-------------------------------------
        
        ##Interface files in 2e must be ftn11 and ftn12
        if not os.path.exists('ftn12'):
            os.system('cp giie.ft12 ftn12')
        else:
            self.verboseprint('ftn12 is ready')

        if not os.path.exists('ftn11'):
            os.system('cp giie.ft11 ftn11')
        else:
            self.verboseprint('ftn11 is ready')

        start = time.time()
        #### RUN THE EXECUTABLE
       
        command_IIE = self.G2EDIR+'/giie2002_gfortran > '+'iieout 2> '+'iieerr'
        time.sleep(0.5)
        logger.info(f" Running IIE: {command_IIE} ")

        subprocess.run(command_IIE, shell = True)
        time.sleep(0.5)
        
        ### Check if there are any errors from IIE output
        _iieerr_filename = self.TMPDIR_arc+'/iieerr'
            # check if size of file is 0
        if os.stat(_iieerr_filename).st_size == 0:
            print(self.run_ID,"         No errors in IIE" )
            print(self.run_ID,"---------End of IIE" )
        else:
            print('ERRORS FOUND IN IIE:', _iieerr_filename)
            end = time.time()
            elapsed = end - start
            print(self.run_ID,'         Time of IIE: ',elapsed,'secs','(',str(float(elapsed)/60),' mins)')

            with open(_iieerr_filename, 'r') as read_iieerrors:
                # Read & print the entire file
                print(read_iieerrors.read())
                sys.exit('Ending program... Errors found in iieout file.')
        self.check_if_run_converged(self.TMPDIR_arc+'/iieout')

        
        
        
        end = time.time()
        elapsed = end - start
        print(self.run_ID,'         Time of IIE: ',elapsed,'secs','(',str(float(elapsed)/60),' mins)')

        now = datetime.now()
        current_time = now.strftime("%H:%M:%S")
        print(self.run_ID,"         Current Time =", current_time)


    def post_geodynrun_savefiles_and_cleanup(self):
        self.verboseprint('Original -- post_geodynrun_savefiles_and_cleanup()')

        self.verboseprint('post_geodynrun_savefiles_and_cleanup',os.getcwd())
        ####-------------------------------------
        ####     After the IIS and IIE runs:
        ####-------------------------------------

        #### ZACH-- the below bit is from frank and I'm not sure what it is meant to do...
        ##                 PERL utility to get updated punch from geodyn output
        ##                 Does not update EPOCH cards. Use GEODYN punch file (ftn07)
        ##                 To get updated EPOCH cards and Elements.
        ##                 This turn_arc_params.pl utility works on the entire setup;
        ##                 -It will not work on the arc setup only.
        ##
        #                 /users/flemoine/bin/turn_arc_params.pl giis.input iieout 0 -1
        #--------------------------


        #### Remove files that won't be needed...
        os.system('rm -f fort.11 fort.12 fort.13 fort.14')

        
        #### Loop through the IIS file and remove the thousands of delete lines...
        ##### read in all lines of the file and save them
        with open('iisout', "r") as f:
            lines_all = f.readlines()    
        ##### Re-write the file line-by-line WITHOUT the DELETE lines     
        with open('iisout', "w") as f:
            for iline, line in enumerate(lines_all):
                if 'DELETE' in line:
                    pass
                else:
                    f.write(line)      
            #### Add a note at the end of the IIS file
            f.write('\n')      
            f.write('    ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **      \n')      
            f.write('    ** NOTE ** -- PYGEODYN USER REMOVED THE DELETE LINES FROM  \n')      
            f.write('             -- THIS SAVED FILE BUT THE DELETE LINES WERE      \n')      
            f.write('             -- IN THE SETUP DECK AT THE TIME OF THE RUN.      \n')      
            f.write('    ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **      \n')      
            f.write('\n')      

        
        #### Combine the iisout, iiserr and the new iieout files...
        os.system('cat iisout iiserr iieout iieerr > '+'IIEOUT.'+self.ARC)

        #### Rename the summary file to save
        os.system('mv fort.9 sumry')

        #### Rename the Resisual file to save
        os.system('mv fort.19 Resid')

        #### make the "punch file". 
        # Punch file just contains special geodyn stuff, it is not important for us.
        os.system('cat fort.7 ftn07 > punch.gdn')

        #### Continue adding to the blob file 
        os.system('fgrep EPOCH punch.gdn > sumry1')
        os.system('cat sumry1 sumry > blob')


        ####-----------------------------------------
        ####     Rename the files that will be saved:
        ####-----------------------------------------

        ## Check which of the files exist, and report to the user that some don't exist:
        ## These are just some common options I have seen
        output_files = {'fort.71': 'emat',
                        'ftn97'  : 'telem',
#                         'ftn08'  : '/xyzout',
#                         'ftn10': '/aeiout' ,
                        'fort.8': 'ascii_xyz',
                        'fort.10': 'ascii_kep',
                        'fort.31': 'orbfil2',
                        'fort.131': 'orbfil',
                        'fort.99': 'densityfil',
                        'fort.98': 'msisin_file_ephem',
                        'fort.101': 'msisin_file_gpiflux',
#                         'fort.101':'msis_out_file' ,
                        'fort.103':'msis_SWI_file' ,
                        }
        for i,val in enumerate(output_files):
            if not os.path.exists(val):
                self.verboseprint('File is not in',self.run_ID,':',val,'--', output_files[val] )
            else:
                pass

        os.system('mv blob sumry')
#         os.system('mv fort.71 emat')
#         os.system('mv ftn97 telem')
#         os.system('mv fort.8 ascii_xyz')          # i dont want these anymore
#         os.system('mv fort.10 ascii_kep')         # i dont want these anymore
        os.system('mv fort.131 orbfil')
        os.system('mv fort.99  densityfil')
        os.system('mv fort.98 msisin_file_ephem')     
        os.system('mv fort.101 msisin_file_gpiflux')     
#         os.system('mv fort.101 msis_out_file')    # i dont want these anymore
        os.system('mv fort.103 drag_file')    # i dont want these anymore
        os.system('rm -f slvtmp* ftn* fort.*')

        print(self.run_ID,'               Finished renaming files')      
        
        ####-----------------------------------------
        ####     Save files to their directories:
        ####-----------------------------------------

        #### First remove files stored in the output directory that have this arc name
        # os.system('rm -f  '+OUTPUTDIR+'/EMAT/${ARC}'+'')

        os.system('rm -f '+self.OUTPUTDIR+'/ORBITS/'+ self.ARC+'')
        os.system('rm -f '+self.OUTPUTDIR+'/ORBITS/'+ self.ARC+'.Z')
        os.system('rm -f '+self.OUTPUTDIR+'/ORBITS/'+ self.ARC+'.gz')

        os.system('rm -f  '+self.OUTPUTDIR+'/RESIDS/'+ self.ARC+'')
        os.system('rm -f  '+self.OUTPUTDIR+'/RESIDS/'+ self.ARC+'.Z')
        os.system('rm -f  '+self.OUTPUTDIR+'/RESIDS/'+ self.ARC+'.gz')

        os.system('rm -f  '+self.OUTPUTDIR+'/sumry/'+ self.ARC+'')
        os.system('rm -f  '+self.OUTPUTDIR+'/PUNCH/'+ self.ARC+'')
        os.system('rm -f '+self.OUTPUTDIR+'/DENSITY/'+ self.ARC+'')
        os.system('rm -f '+self.OUTPUTDIR+'/XYZ_TRAJ/'+ self.ARC+'')
        os.system('rm -f '+self.OUTPUTDIR+'/KEP_TRAJ/'+ self.ARC+'')
        os.system('rm -f  '+self.OUTPUTDIR+'/IIEOUT/'+ self.ARC+'')
        os.system('rm -f  '+self.OUTPUTDIR+'/IISSET/'+ self.ARC+'')


        ### We compress any files that are not frequently used in their ascii form
        ###   I use gzip for very large files (speed needed) and bzip2 for all others              
#         os.system('bzip2 -v giis.input')
#         os.system('bzip2 -v Resid')
        os.system('bzip2 -v orbfil')
        os.system('bzip2 -v densityfil')
        os.system('bzip2 -v drag_file')
#         os.system('bzip2 -v ascii_xyz')
#         os.system('bzip2 -v ascii_kep')
#         os.system('bzip2 -v punch.gdn')

#         os.system('cp giis.input.bz2  '+self.OUTPUTDIR+'/IISSET/'+ self.ARC+'.bz2')
#         os.system('cp Resid.bz2 '      +self.OUTPUTDIR+'/RESIDS/'  +self.ARC+     '.bz2')

#         print('Check the arc name:  ',self.ARC )
#         print(' ')
#         print('Check the ORBFIL stuff:  ')
#         print(self.OUTPUTDIR,'/ORBITS/'  ,self.ARC,'_orb1.bz2' )

        os.system('cp orbfil.bz2 '     +self.OUTPUTDIR+'/ORBITS/'  +self.ARC+'_orb1.bz2')
        os.system('cp densityfil.bz2 ' +self.OUTPUTDIR+'/DENSITY/' +self.ARC+     '.bz2')
        os.system('cp drag_file.bz2 ' +self.OUTPUTDIR+'/DENSITY/' +self.ARC+     'drag_file.bz2')
#         os.system('cp ascii_xyz.bz2 '  +self.OUTPUTDIR+'/XYZ_TRAJ/'+self.ARC+     '.bz2')
#         os.system('cp ascii_kep.bz2 '  +self.OUTPUTDIR+'/KEP_TRAJ/'+self.ARC+     '.bz2')
        os.system('mv IIEOUT.'+ self.ARC+' '+self.OUTPUTDIR+'/IIEOUT/'+ self.ARC+'')
                
        os.system('cp msisin_file_ephem '+self.OUTPUTDIR+'/DENSITY/'+self.ARC+'_msisin')
        os.system('cp msisin_file_gpiflux '+self.OUTPUTDIR+'/DENSITY/'+self.ARC+'_msisin_gpiflux')
#         os.system('cp msis_out_file '+self.OUTPUTDIR+'/DENSITY/'+self.ARC+'_msisout')
#         os.system('cp msis_SWI_file '+self.OUTPUTDIR+'/DENSITY/'+self.ARC+'_msisSWI')
#         os.system('cp punch.gdn.bz2   '+self.OUTPUTDIR+'/PUNCH/'+ self.ARC+'.gdn.bz2')
#         os.system('cp sumry '+self.OUTPUTDIR+'/sumry/'+ self.ARC+'')
#         os.system('cp punch '+self.OUTPUTDIR+'/PUNCH/'+ self.ARC+'')

        print(self.run_ID,"               Finished copying files to outputdir")

        #### Go up 3 levels and delete the temporary directories:
        os.chdir('../../')
        
        print(self.tabtab,'Deleting tmp/: ',self.SERIES)
        os.system('rm -rf'+' ' +self.SERIES)
     
    
    
    
    
#     ##########################################################################################################
#     ##### MAKE THE CSV
#     def make_orbit_cloud_csv(self):

#         import sys
#         logger = logging.getLogger(self.execlog_filename)
#         logging.info(f'in make_orbit_cloud_csv()   \n       Path to DEN_CSV file:  {self.msis2_file_path}')
        
#         DEN_csv = pd.read_csv(self.msis2_file_path, 
#                             dtype=object,
#                             names = ['YYMMDD',
#                                      'HHMMSS',
#                                      'Height_kilometers',
#                                      'Lat',
#                                      'Lon',
#                                      'STLOC',
#                                      'AVGFLX',
#                                      'FLUX',
#                                          ],
#                             sep = '\s+',
#                             )


#         sat_time1 = list(DEN_csv['YYMMDD'])  #"031115" #  
#         sat_time2 = list(DEN_csv['HHMMSS'])  #"120000" #1068897600        
#         sattime   =    [x+y   for x,y   in zip(sat_time1, sat_time2)]

#         sattime   =    [datetime.strptime(x, '%y%m%d%H%M%S')   for x   in sattime ]
#         sattime   =    [datetime.timestamp(x)   for x   in sattime ]

#         DEN_csv['sattime_utctimestamp'] = sattime
#         # DEN_csv['Height_kilometers'] = DEN_csv['Height (meters)'].astype(float)*1e-3
#         DEN_csv['Lon'] = DEN_csv['Lon'].astype(float)
#         DEN_csv['Lat'] = DEN_csv['Lat'].astype(float)

#         timeHHMMSS = [] 
#         for i,val in enumerate(DEN_csv['HHMMSS'].values.astype(int)):
#             # print(len(str(val)))
#             if len(str(val)) == 1:
#                 timehhmmss_val = '00000'+ str(val)
#                 timeHHMMSS.append(timehhmmss_val)
#             elif len(str(val)) == 2:
#                 timehhmmss_val = '0000'+ str(val)
#                 timeHHMMSS.append(timehhmmss_val)
#             elif len(str(val)) == 3:
#                 timehhmmss_val = '000'+ str(val)
#                 timeHHMMSS.append(timehhmmss_val)
#             elif len(str(val)) == 4:
#                 timehhmmss_val = '00'+ str(val)
#                 timeHHMMSS.append(timehhmmss_val)
#             elif len(str(val)) == 5:
#                 timehhmmss_val = '0'+ str(val)
#                 timeHHMMSS.append(timehhmmss_val)
#             else:
#                 timeHHMMSS.append(str(val))
#         DEN_csv['timeHHMMSS'] = timeHHMMSS
#         YR = int(18)

#         YYMMDD_list = DEN_csv['YYMMDD'].astype(int).astype(str)
#         timeHHMMSS_list = DEN_csv['timeHHMMSS'].astype(str)

#         if YR < 10:
#             year    = ['200' + x[:1]  for x in YYMMDD_list]
#             month   = [        x[1:3] for x in YYMMDD_list]
#             day     = [        x[3:]  for x in YYMMDD_list]
#             hours   = [        x[:2]  for x in timeHHMMSS_list]
#             minutes = [        x[2:4] for x in timeHHMMSS_list]
#             secs    = [        x[4:]  for x in timeHHMMSS_list]
#         else:
#             year    = ['20' + x[:2]  for x in YYMMDD_list]
#             month   = [       x[2:4] for x in YYMMDD_list]
#             day     = [       x[4:]  for x in YYMMDD_list]
#             hours   = [       x[:2]  for x in timeHHMMSS_list]
#             minutes = [       x[2:4] for x in timeHHMMSS_list]
#             secs    = [       x[4:]  for x in timeHHMMSS_list]
#         #--------------------------------------------------------
#         DEN_csv['year']  = year
#         DEN_csv['month'] = month
#         DEN_csv['day']   = day
#         DEN_csv['hours']  = hours
#         DEN_csv['minutes'] = minutes
#         DEN_csv['secs']  = secs
#         #--------------------------------------------------------
#         year= list(map(int, DEN_csv['year'].values))
#         month= list(map(int, DEN_csv['month'].values))
#         day= list(map(int, DEN_csv['day'].values))
#         hour= list(map(int, DEN_csv['hours'].values))
#         minute = list(map(int, DEN_csv['minutes'].values))
#         second = list(map(int, DEN_csv['secs'].values))

#         DATE = list(map(datetime, year,month, day, hour,minute,second ))

#         DEN_csv.insert(0, 'Date', DATE)

#         del DEN_csv['timeHHMMSS']
#         del DEN_csv['year']
#         del DEN_csv['month']
#         del DEN_csv['day']
#         del DEN_csv['hours']
#         del DEN_csv['minutes']
#         del DEN_csv['secs']

#         import sys
#         sys.path.insert(0,'/data/geodyn_proj/interface_kamodo_geodyn/Kamodo/kamodo/flythrough/')
#         from SingleSatelliteFlythrough import SingleModelFlythrough
#         sys.path.insert(0,'/data/geodyn_proj/interface_kamodo_geodyn/Kamodo/kamodo/flythrough/')
#         from SatelliteFlythrough import ModelFlythrough


#         times_list = []
#         lons_list = []
#         lats_list = []
#         alts_list = []
#         rhos_list = []
#         cube_corner = []

#         count=0
        
# #         ##############################################################
# #         from multiprocessing import set_start_method
# #         set_start_method("spawn")
# #         import sys  
# #         import multiprocessing
# #         sys.path.insert(0,'/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_dir/multiprocess_makeorbit_file')
# #         from multiprocess_makeorbit_file  import multiprocess_makeorbit_file
# #         multiprocessing.cpu_count()* 2
# #         pool_size = (multiprocessing.cpu_count() + 2  ) 
# #         pool = multiprocessing.Pool(processes=pool_size)
# #         ins = [(DEN_csv, 0, self.orbitcloud_csv_file, self.model_data_path),
# #                (DEN_csv, 1, self.orbitcloud_csv_file, self.model_data_path),
# #                ]
# #         pool.starmap(multiprocess_makeorbit_file, ins)
# #         ##############################################################

        
#         #### Open the file
#         #### We will loop thru the DEN CSV and if the file already contains the the date, don't overwrite.
#         ### NOTE: The below will search thru the file to see if the date is already in there but it does not account for the fact that the data must be written in blocks of 9 points of the cube.  This is a limitation and should be addressed eventually
        
#         print('TEST1')
#         file = open(self.orbitcloud_csv_file,  'r+')
#         for it,val in enumerate(DEN_csv['Date'][:]):
#             date_index = DEN_csv['YYMMDD'][it] + DEN_csv['HHMMSS'][it]
# #             print('TEST2')
   
# #             for iline, line in enumerate(file):
# #                 print('TEST3')
# #                 print(iline, line)
# #                 print(type(line))
# #                 if date_index in line:
# #                     print('exists!', val, date_index, line)
# #                     pass
# #                 elif (date_index not in line) or (line == '\n'):
#             unix_time  = DEN_csv['sattime_utctimestamp'][it]
#             print(f"**** {date_index} -- {count} ****")

#             count+=1

#             ### Get the coordinates along the orbit:
#             lon = float(DEN_csv['Lon'][it])
#             lat = float(DEN_csv['Lat'][it])
#             alt = float(DEN_csv['Height_kilometers'][it])
#             center_coord = [lon, lat, alt]


#             ### Find the coordinates of the cube surround the orbit point:
#             delta_deg = 2    # degrees
#             delta_m = 1000.*1e-3 # meters to kilometers
#             A = [lon + delta_deg, lat+delta_deg, alt+delta_m]  # top,    front, left
#             B = [lon + delta_deg, lat-delta_deg, alt+delta_m]  # top,    back,  Left
#             C = [lon - delta_deg, lat+delta_deg, alt+delta_m]  # top,    front, right
#             D = [lon - delta_deg, lat-delta_deg, alt+delta_m]  # top,    back,  right
#             E = [lon + delta_deg, lat+delta_deg, alt-delta_m]  # bottom, front, left
#             F = [lon + delta_deg, lat-delta_deg, alt-delta_m]  # bottom, back,  left
#             G = [lon - delta_deg, lat+delta_deg, alt-delta_m]  # bottom, front, right
#             H = [lon - delta_deg, lat-delta_deg, alt-delta_m]  # bottom, back,  right


#             ### Store the cube's coordinates in the dictionary index
#             cube_corners_and_center = []
#             cube_corners_and_center.append(center_coord)
#             cube_corners_and_center.append(A)
#             cube_corners_and_center.append(B)
#             cube_corners_and_center.append(C)
#             cube_corners_and_center.append(D)
#             cube_corners_and_center.append(E)
#             cube_corners_and_center.append(F)
#             cube_corners_and_center.append(G)
#             cube_corners_and_center.append(H)

#             #### Import Coordinates to Kamodo
#             ##
#             #### Kamodo static inputs:
#             model          = 'TIEGCM'
#             file_dir       = self.model_data_path+'/'
#             logger.debug(f"Added a forward slash to path of {self.model_data_path} to input into Kamodo")
#             variable_list  = ['rho','psi_O2', 'psi_O',  'psi_He', 'T_n']
#             coord_type     = 'SPH'
#             coord_grid     = 'sph'
#             high_res       = 1.
#             verbose        = False  
#             csv_output     = '' 
#             plot_output    = ''


#             #### Extract the coordinates from each list to plug into Kamodo with vectorization
#             lons_in = [item[0] for item in cube_corners_and_center]
#             lats_in = [item[1] for item in cube_corners_and_center]
#             alts_in = [item[2] for item in cube_corners_and_center]

#             ## Gather inputs for Kamodo
#             sat_time       = unix_time*np.ones(np.size(alts_in))
#             c1             = lons_in
#             c2             = lats_in
#             c3             = alts_in
#             ## Plug vectorized coordinates into Kamodo
#             results = ModelFlythrough(model, file_dir, variable_list, sat_time, c1, c2, c3, 
#                                 coord_type, coord_grid, high_res=20., verbose=False, 
#                                 csv_output='', plot_output='')
#             corners = ['0','1','2','3','4','5','6','7','8']
#             for i,valrho in enumerate(results['rho']):
# #                 print('writing to file')
#                 file.write(f"{date_index}   {results['c1'][i]:8.4f}   {results['c2'][i]:8.4f}   {results['c3'][i]:8.4f}   {valrho:15.8e}   {corners[i]} \n")

        
#         file.close()


# ##########################################################################################################


    ##########################################################################################################
    ##### MAKE THE CSV
    def make_orbit_cloud_csv(self, kamodo_flag=True, HASDM_format_flag=False):
        """
        This function goes through the steps necessary to construct the Orbit Cloud File.
        
        The file has the following format for the columns:
            Date(YYMMDDHHMMSS)   Longitude(deg)   Latitude(deg)   Altitude(km)   Density(g/cm^3)  .............     
                ....[cont. 1st line]....  nden_O()  nden_O2()  nden_He()  nden_N2()  T_n(K)   
         
         mmr = rho_i/rho_tot
                
         
        The rows are then structured as follows:
            1-0   Center of Cube        --  [Lon,   Lat,   Alt]     (First Original Orbit Point)
            1-1   Top,    Front, Left   --  [Lon+d, Lat+d, Alt+d]   (Perturbations from 1st orig point)
            1-2   Top,    Back,  Left   --  [Lon+d, Lat-d, Alt+d]               "      "
            1-3   Top,    Front, Right  --  [Lon-d, Lat+d, Alt+d]                  ||
            1-4   Top,    Back,  Right  --  [Lon-d, Lat-d, Alt+d]                  ||
            1-5   Bottom, Front, Left   --  [Lon+d, Lat+d, Alt-d]                  ||
            1-6   Bottom, Back,  Left   --  [Lon+d, Lat-d, Alt-d]                  ||
            1-7   Bottom, Front, Right  --  [Lon-d, Lat+d, Alt-d]                  ||
            1-8   Bottom, Back,  Right  --  [Lon-d, Lat-d, Alt-d]                  \/
            2-0   Center of Cube        --  [Lon,   Lat,   Alt]     (Second Original Orbit Point)
            2-1   Top,    Front, Left   --  [Lon+d, Lat+d, Alt+d]   (Perturbations from 2nd orig point)
            2-2   Top,    Back,  Left   --  [Lon+d, Lat-d, Alt+d]               "      "
            2-3   Top,    Front, Right  --  [Lon-d, Lat+d, Alt+d]                  ||
            2-4   Top,    Back,  Right  --  [Lon-d, Lat-d, Alt+d]                  ||
            2-5   Bottom, Front, Left   --  [Lon+d, Lat+d, Alt-d]                  ||
            2-6   Bottom, Back,  Left   --  [Lon+d, Lat-d, Alt-d]                  ||
            2-7   Bottom, Front, Right  --  [Lon-d, Lat+d, Alt-d]                  ||
            2-8   Bottom, Back,  Right  --  [Lon-d, Lat-d, Alt-d]                  \/
            3-0   Center of Cube        --  [Lon,   Lat,   Alt]     (Third Original Orbit Point)
            3-1   Top,    Front, Left   --  [Lon+d, Lat+d, Alt+d]   (Perturbations from 3rd orig point)
            3-2   Top,    Back,  Left   --  [Lon+d, Lat-d, Alt+d]               "      "
            3-3   Top,    Front, Right  --  [Lon-d, Lat+d, Alt+d]                  ||
            3-4   Top,    Back,  Right  --  [Lon-d, Lat-d, Alt+d]                  ||
            3-5   Bottom, Front, Left   --  [Lon+d, Lat+d, Alt-d]                  ||
            3-6   Bottom, Back,  Left   --  [Lon+d, Lat-d, Alt-d]                  ||
            3-7   Bottom, Front, Right  --  [Lon-d, Lat+d, Alt-d]                  ||
            3-8   Bottom, Back,  Right  --  [Lon-d, Lat-d, Alt-d]                  \/
            ...
            ...
            ...
        """
        
        
        #### Timer to measure how long this function takes
        import time
        start = time.time()
        
        ##### --------------------------------------------------------------------------------------------
        #### Import the INIT_ORBIT that was constructed from the initializing run of MSIS2 through GEODYN
        import sys
        logger = logging.getLogger(self.execlog_filename)
        logging.info(f'in make_orbit_cloud_csv()   \n       Path to DEN_CSV file:  {self.msis2_file_path}')
          
        DEN_csv = pd.read_csv(self.msis2_file_path, 
                            dtype=object,
                            names = ['YYMMDD',
                                     'HHMMSS',
                                     'Height_kilometers',
                                     'Lat',
                                     'Lon',
                                         ],
                            sep = '\s+',
                            )

        #### Fix the formatting of the dates in the density file
        #####   The following is a bit archaic but it works so I haven't messed with it
        sat_time1 = list(DEN_csv['YYMMDD'])  #"031115" #  
        sat_time2 = list(DEN_csv['HHMMSS'])  #"120000" #1068897600        
        sattime   =    [x+y   for x,y   in zip(sat_time1, sat_time2)]
        sattime   =    [datetime.strptime(x, '%y%m%d%H%M%S')   for x   in sattime ]
        sattime   =    [datetime.timestamp(x)   for x   in sattime ]
        DEN_csv['sattime_utctimestamp'] = sattime
        DEN_csv['Lon'] = DEN_csv['Lon'].astype(float)
        DEN_csv['Lat'] = DEN_csv['Lat'].astype(float)
        timeHHMMSS = [] 
        for i,val in enumerate(DEN_csv['HHMMSS'].values.astype(int)):
            # print(len(str(val)))
            if len(str(val)) == 1:
                timehhmmss_val = '00000'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            elif len(str(val)) == 2:
                timehhmmss_val = '0000'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            elif len(str(val)) == 3:
                timehhmmss_val = '000'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            elif len(str(val)) == 4:
                timehhmmss_val = '00'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            elif len(str(val)) == 5:
                timehhmmss_val = '0'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            else:
                timeHHMMSS.append(str(val))
        DEN_csv['timeHHMMSS'] = timeHHMMSS
        YR = int(18)
        YYMMDD_list = DEN_csv['YYMMDD'].astype(int).astype(str)
        timeHHMMSS_list = DEN_csv['timeHHMMSS'].astype(str)
        if YR < 10:
            year    = ['200' + x[:1]  for x in YYMMDD_list]
            month   = [        x[1:3] for x in YYMMDD_list]
            day     = [        x[3:]  for x in YYMMDD_list]
            hours   = [        x[:2]  for x in timeHHMMSS_list]
            minutes = [        x[2:4] for x in timeHHMMSS_list]
            secs    = [        x[4:]  for x in timeHHMMSS_list]
        else:
            year    = ['20' + x[:2]  for x in YYMMDD_list]
            month   = [       x[2:4] for x in YYMMDD_list]
            day     = [       x[4:]  for x in YYMMDD_list]
            hours   = [       x[:2]  for x in timeHHMMSS_list]
            minutes = [       x[2:4] for x in timeHHMMSS_list]
            secs    = [       x[4:]  for x in timeHHMMSS_list]
        #--------------------------------------------------------
        DEN_csv['year']  = year
        DEN_csv['month'] = month
        DEN_csv['day']   = day
        DEN_csv['hours']  = hours
        DEN_csv['minutes'] = minutes
        DEN_csv['secs']  = secs
        #--------------------------------------------------------
        year= list(map(int, DEN_csv['year'].values))
        month= list(map(int, DEN_csv['month'].values))
        day= list(map(int, DEN_csv['day'].values))
        hour= list(map(int, DEN_csv['hours'].values))
        minute = list(map(int, DEN_csv['minutes'].values))
        second = list(map(int, DEN_csv['secs'].values))
        DATE = list(map(datetime, year,month, day, hour,minute,second ))
        DEN_csv.insert(0, 'Date', DATE)
        del DEN_csv['timeHHMMSS']
        del DEN_csv['year']
        del DEN_csv['month']
        del DEN_csv['day']
        del DEN_csv['hours']
        del DEN_csv['minutes']
        del DEN_csv['secs']
        ##### --------------------------------------------------------------------------------------------
        #### End code block that deals with the INIT_ORBIT and fixing the date formats
        
        
               
        if kamodo_flag:
            import sys
            sys.path.insert(0,'/data/geodyn_proj/interface_kamodo_geodyn/Kamodo/kamodo/flythrough/')
            from SatelliteFlythrough import ModelFlythrough

        #### Initialize empty lists for storing the values 
        date_list = []
        unixtimes_list = []
        lons_list = []
        lats_list = []
        alts_list = []
        count=0
               
        #### Set the perturbation amounts for the coordinates for making the size of the cube
        delta_deg = 2    # degrees
        delta_m = 1000.*1e-3 # meters to kilometers
        logging.info(f'LON and LAT cube size of orbit_cloud_file: {delta_deg} degrees')
        logging.info(f'Altitude size of orbit_cloud_file:  {delta_m} kilometers')

        #### Open the file
        #### We will loop thru the DEN CSV and if the file already contains the the date, don't overwrite.
         
        for it,val in enumerate(DEN_csv['Date'][:]):
            date_index = DEN_csv['YYMMDD'][it] + DEN_csv['HHMMSS'][it]
            unix_time  = DEN_csv['sattime_utctimestamp'][it]

            count+=1

            ### Get the original coordinates along the orbit:
            lon = float(DEN_csv['Lon'][it])
            lat = float(DEN_csv['Lat'][it])
            alt = float(DEN_csv['Height_kilometers'][it])
            center_coord = [lon, lat, alt]

            #### Construct variables for the perturbations
            lon_plus_delta = lon + delta_deg
            lon_mins_delta = lon - delta_deg
            lat_plus_delta = lat + delta_deg
            lat_mins_delta = lat - delta_deg

            ### WRAP THE LONS AROUND -180 to 180
            if lon_plus_delta < -180:
                lon_plus_delta = np.mod(lon_plus_delta, 180)
            elif lon_plus_delta > 180:
                lon_plus_delta = np.mod(lon_plus_delta, -180)
            else:        
                lon_plus_delta = lon_plus_delta
            if lon_mins_delta < -180:
                lon_mins_delta = np.mod(lon_mins_delta, 180)
            elif lon_mins_delta > 180:
                lon_mins_delta = np.mod(lon_mins_delta, -180)
            else:
                lon_mins_delta = lon_mins_delta
            #
            ### WRAP THE LATS AROUND -90 to 90
            if lat_plus_delta < -90:
                lat_plus_delta = np.mod(lat_plus_delta, 90)
            elif lat_plus_delta > 90:
                lat_plus_delta = np.mod(lat_plus_delta, -90)
            else:
                lat_plus_delta = lat_plus_delta
            if lat_mins_delta < -90:
                lat_mins_delta = np.mod(lat_mins_delta, 90)
            elif lat_mins_delta > 90:
                lat_mins_delta = np.mod(lat_mins_delta, -90)
            else:
                lat_mins_delta = lat_mins_delta

            ##### Build the coordinates of the cube surround the orbit point:
            #                                                  # The first point is center 0
            A = [lon_plus_delta, lat_plus_delta, alt+delta_m]  # top,    front, left   1
            B = [lon_plus_delta, lat_mins_delta, alt+delta_m]  # top,    back,  Left   2
            C = [lon_mins_delta, lat_plus_delta, alt+delta_m]  # top,    front, right  3
            D = [lon_mins_delta, lat_mins_delta, alt+delta_m]  # top,    back,  right  4
            E = [lon_plus_delta, lat_plus_delta, alt-delta_m]  # bottom, front, left   5 
            F = [lon_plus_delta, lat_mins_delta, alt-delta_m]  # bottom, back,  left   6
            G = [lon_mins_delta, lat_plus_delta, alt-delta_m]  # bottom, front, right  7
            H = [lon_mins_delta, lat_mins_delta, alt-delta_m]  # bottom, back,  right  8

            ####### The dictionary thing i did below actually turns out to not be used, but again this works so I have left it.
            ##     The next 30 lines could have been written better but alas 
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
            #
            #### Extract the coordinates from each list to plug into Kamodo with vectorization
            lons_in = [item[0] for item in cube_corners_and_center]
            lats_in = [item[1] for item in cube_corners_and_center]
            alts_in = [item[2] for item in cube_corners_and_center]
            #
            ## Gather inputs for Kamodo
            sat_time       = unix_time*np.ones(np.size(alts_in))
            dates          = [date_index]*np.size(alts_in)
            c1             = lons_in
            c2             = lats_in
            c3             = alts_in
            #
            unixtimes_list.extend(sat_time)
            date_list.extend(dates)
            lons_list.extend(c1)
            lats_list.extend(c2)
            alts_list.extend(c3)

        if kamodo_flag:
            #### Import Coordinates to Kamodo
            ##
            #### Kamodo static inputs:
            model          = 'TIEGCM'
            file_dir       = self.model_data_path+'/'
            logger.debug(f"Added a forward slash to path of {self.model_data_path} to input into Kamodo")
            variable_list  = ['rho','psi_O2', 'psi_O', 'psi_He', 'T_n']
            coord_type     = 'SPH'
            coord_grid     = 'sph'
            high_res       = 1.
            verbose        = False  
            csv_output     = '' 
            plot_output    = ''        


            print(f'|     Running data cube thru Kamodo... please hold.')
            results = ModelFlythrough(model, file_dir, variable_list, unixtimes_list, lons_list, lats_list, alts_list,
                                      coord_type, coord_grid, high_res=20., verbose=False,csv_output='', plot_output='')

            #### Zach modified ModelFlythrough() to include a top boundary extrapolation as well as retun N2
            print(results.keys())
            end = time.time()
            elapsed = end - start
            print(f'|     Kamodo Total Run Time:', elapsed    , 'seconds')
            print(f'|     Kamodo Total Run Time:', elapsed/60, 'minutes' )

            
#                     file.write(f"{datetime.strftime(datetime.fromtimestamp(results['utc_time'][ii]), '%y%m%d%H%M%S')}   {results['c1'][ii]:8.4f}   {results['c2'][ii]:8.4f}   {results['c3'][ii]:8.4f}   {valrho:15.6e}   {results['nden_O'][ii]:12.5e}   {results['nden_O2'][ii]:12.5e}   {results['nden_He'][ii]:12.5e}   {results['nden_N2'][ii]:12.5e}   {results['T_n'][ii]:8.4f} \n")

            mp_cgs = 1.6726e-24    # [g] mass of proton

            with open(self.orbitcloud_csv_file, 'r+') as file:
                for ii, valrho in enumerate(results['rho']):
                    nden_O =  (results['psi_O'][ii]  * results['rho'][ii])/(mp_cgs*16)
                    nden_O2 = (results['psi_O2'][ii] * results['rho'][ii])/(mp_cgs*32)
                    nden_He = (results['psi_He'][ii] * results['rho'][ii])/(mp_cgs*4)
                    nden_N2 = (results['psi_N2'][ii] * results['rho'][ii])/(mp_cgs*28)

                    file.write(f"{datetime.strftime(datetime.fromtimestamp(results['utc_time'][ii]), '%y%m%d%H%M%S')}  {results['c1'][ii]:9.4f}  {results['c2'][ii]:9.4f}  {results['c3'][ii]:9.4f}  {valrho:15.6e}  {nden_O:12.5e}  {nden_O2:12.5e}  {nden_He:12.5e}  {nden_N2:12.5e}   {results['T_n'][ii]:8.4e}\n")
                        ###!!!!!   NOTE THERE IS AN EXTRA SPACE B4 TEMPERATURE BECAUSE FORTRAN SUCKS
    
            end = time.time()
            elapsed = end - start
            print(f'|     Save OrbitCloud file run rime:', elapsed,       'seconds' )
            print(f'|     Save OrbitCloud file run rime:', elapsed/60,    'minutes' )

                            # file.write(f"{datetime.strftime(datetime.fromtimestamp(results['utc_time'][ii]), '%y%m%d%H%M%S')}
                            # {results['c1'][ii]:8.4f}   {results['c2'][ii]:8.4f}   {results['c3'][ii]:8.4f}   {valrho:15.8e} \n")
        else: #if kamodo_flag=False #### OPTION TO NOT RUN THRU KAMODO,  JUST MAKE THE FILE WITH COORDINATES
            
            if HASDM_format_flag:
                print('***** Constructing the File with requested format to be made input for HASDM ***** ')
                
                with open(self.orbitcloud_csv_file, 'r+') as file:
                    for ii, valval in enumerate(date_list):
                        print('whoops gotta fix this lol')
#                         IYR
#                         IYDAY
#                         IHR
#                         IMIN
#                         SEC
#                         D1950
#                         HTM
#                         XLT
#                         XLAT
#                         XLON
#                         RHO
                        
#                         file.write(f"{IYR}   {IYDAY}   {IHR}   {IMIN}  {} \n")

            
            else:#### REGULAR GEODYN requestied format without the density from Kamodo 
                with open(self.orbitcloud_csv_file, 'r+') as file:
                    for ii, valval in enumerate(date_list):
                        file.write(f"{datetime.strftime(datetime.fromtimestamp(unixtimes_list[ii]), '%y%m%d%H%M%S')}   {lons_list[ii]:8.4f}   {lats_list[ii]:8.4f}   {alts_list[ii]:8.4f}   \n")

            
            


##########################################################################################################

        
    
    def RUN_make_OrbitCloud_trajectories(self):
        '''
        This function takes a GEODYN run state and only performs the steps necessary to construct a ORBIT_CLOUD trajectory file.
        
        The steps are as follows:
            1. Do an initial run of GEODYN with MSIS2 to get an initialized orbit of the satellite for this arc.
        '''
                
        from os.path import exists
        
        ### Make an execution log file
        iarc = 0
        arc  = self.arc_input[0]
        self.arcnumber = iarc
        self.set_file_paths_for_multiple_arcs( arc , iarc)            
        self.set_density_model_setup_params('msis2' )
        self.setup_directories_and_geodyn_input()
        self.make_output_directories()
        logger = logging.getLogger(self.execlog_filename)
        logging.info('Running PYGEODYN with the Orbit Cloud Method \n         Check to see if the CSV files have been created using msis2. ')

        #### RUN 1st WITH MSIS2 IF THE FILE DOES NOT EXIST
        self.set_density_model_setup_params( 'msis2' )
        for iarc, arc in enumerate(self.arc_input):
            self.arcnumber = iarc
            self.set_file_paths_for_multiple_arcs( arc , iarc) 
            print('****** Run GEODYN once with MSIS2 to make INIT_ORBIT \n', arc)
            
            
            self.orbitcloud_csv_file =(self.OUTPUTDIR+'/OrbitCloud_Step'+
                                   str(int(self.geodyn_StepSize))+'_'+self.arcdate_for_files+'.csv')



            self.msis2_file_path = self.OUTPUTDIR+'/DENSITY/'+self.ARC+'_msisin'
            
            
#             print()
#             print('****** outputdir',self.OUTPUTDIR)
#             print('****** orbitcloud_csv_file',self.orbitcloud_csv_file)
#             print('****** msis2_file_path',self.msis2_file_path)
#             print()

            file_exists = exists(self.msis2_file_path)
            if file_exists:
#                 print('****** file_exists (msis2 density file):',self.msis2_file_path )
                logging.info('A similar MSIS2 output has been made. Check to see if its stepsize is consistent.')
                msis2_log_file =  self.OUTPUTDIR+'/pygeodyn_runlog_'+self.ARC+'.txt'

                with open(msis2_log_file, 'r') as f:
                    for line_no, line in enumerate(f):
                        if 'STEP             ' in line:
                            check_stepsizeline = line

                check_stepsize = float(check_stepsizeline[-5:])
                if self.geodyn_StepSize == check_stepsize:
                    logging.info(f'The MSIS2 run has the correct STEP size of {self.geodyn_StepSize}')

                else:
                    logging.info(f'The existing MSIS2 run has the wrong stepsize  (found STEP to be {check_stepsize}, but need {self.geodyn_StepSize}). Running MSIS2 thru GEODYN with correct step size.')
                    self.setup_directories_and_geodyn_input()
                    self.make_output_directories()
                    self.print_runparameters_to_notebook()
                    self.prepare_tmpdir_for_geodyn_run()
                    self.run_geodyn_in_tmpdir()
                    self.post_geodynrun_savefiles_and_cleanup()

            else:
                logging.info(f'No similar runs of MSIS2 exist on this arc. Running MSIS2 thru GEODYN...')
                self.setup_directories_and_geodyn_input()
                self.make_output_directories()
                self.print_runparameters_to_notebook()
                self.prepare_tmpdir_for_geodyn_run()
                self.run_geodyn_in_tmpdir()
                self.post_geodynrun_savefiles_and_cleanup()


            logging.info(f'Running GEODYN with initialized orbit + uncertainty cloud tiegcm data. ')

            ## TODO: make the tiegcm files an input option
            orbitcloud_csv_check = exists(self.orbitcloud_csv_file)
            print('****** 2.5 check- orbitcloud_csv_file Does not exist \n ****** ****** ',  self.orbitcloud_csv_file,  )

            if orbitcloud_csv_check:
                print('****** 3- construct orbit cloud file', arc)

                self.set_file_paths_for_multiple_arcs( arc , iarc)            
                SERIES = self.DEN_DIR + '_' + self.cd_model + self.directory_name_specifier
                OUTPUTDIR   = '/data/data_geodyn/results/'+self.SATELLITE_dir + '/'+self.DEN_DIR+'/'+SERIES


                logging.info(f'Orbit Cloud exists:  {self.orbitcloud_csv_file }')
                self.make_orbit_cloud_csv(kamodo_flag=False, HASDM_format_flag=True)

            else:
                ## Construct the orbit cloud CSV
                logging.info(f'Constructing orbit file:  {self.orbitcloud_csv_file }')
                ## Use the msis2 file to identify the density file that will be use to index the satellite ephemeris within kamodo
                self.set_file_paths_for_multiple_arcs( arc , iarc)            
                print('****** 3.5- construct orbit cloud', arc)
                
                
                print('****** orbitcloud_csv_file',self.orbitcloud_csv_file)

                f = open(self.orbitcloud_csv_file, "w")
                f.write("\n")
                f.close()
                from time import perf_counter
                t0=perf_counter()
                print('****** time at start:', perf_counter()-t0)
                self.set_file_paths_for_multiple_arcs( arc , iarc)            
                self.make_orbit_cloud_csv(kamodo_flag=False, HASDM_format_flag=True)
                print('****** time at start:', perf_counter()-t0)
                print('****** 4-  construct orbit  cloud', arc)



    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
        
        
    def RUN_GEODYN(self):
        '''
        This is the main run function that calls the above functions in the Pygeodyn Controller class.
                
        '''
        
                
        from os.path import exists

        if self.den_model == 'tiegcm_oc':
                ####   If we are using one of the models that require Kamodo, we will want to
                ####   do a pre-run initialization to get the orbit output along the satellite using MSISe2
                     
            
            ### Make an execution log file
            iarc =0
            arc=self.arc_input[0]
            self.arcnumber = iarc

            self.set_file_paths_for_multiple_arcs( arc , iarc)            
            self.set_density_model_setup_params('tiegcm_oc' )
            self.setup_directories_and_geodyn_input()
            self.make_output_directories()
            
            print(f'+=======================================================================')
            print(f'|     Running GEODYN with TIEGCM Orbit Cloud Method     ',)
            print(f'|                                                       ',)
            logger = logging.getLogger(self.execlog_filename)
            logging.info('Running PYGEODYN with the Orbit Cloud Method \n         Check to see if the CSV files have been created using msis2. ')

            #### RUN FIRST WITH MSIS2 IF THE FILE DOES NOT EXIST
            self.set_density_model_setup_params( 'msis2' )
            for iarc, arc in enumerate(self.arc_input):
                self.arcnumber = iarc
                self.set_file_paths_for_multiple_arcs( arc , iarc) 

                #### identify supposed location of msis2 run
                SERIES = self.DEN_DIR + '_' + self.cd_model + self.directory_name_specifier
                OUTPUTDIR   = self.run_settings['path_to_output_directory'] + '/'+self.DEN_DIR+'/'+self.SERIES
                self.orbitcloud_csv_file =(self.model_data_path+'/OrbitCloud_Step'+
                                       str(int(self.geodyn_StepSize))+'_'+self.arcdate_for_files+'.csv')
                self.msis2_file_path = OUTPUTDIR+'/DENSITY/'+self.ARC+'_msisin'

                #### Check if the MSIS2 density file exists
                file_exists = exists(self.msis2_file_path)
                if file_exists:
                    print(f'|     MSIS2 Density file already exists.               ',)
                    print(f'|          - {self.ARC} ',)
                    #                     os.system('bunzip2 -v '+self.msis2_file_path)
                    #                     self.msis2_file_path =  OUTPUTDIR+'/DENSITY/'+self.ARC
                    logging.info('A similar MSIS2 output has been made. Check to see if its stepsize is consistent.')
                    msis2_log_file =  OUTPUTDIR+'/pygeodyn_runlog_'+self.ARC+'.txt'

                    #### Check the stepsize of the density file...
                    with open(msis2_log_file, 'r') as f:
                        for line_no, line in enumerate(f):
                            if 'STEP             ' in line:
                                check_stepsizeline = line
                    check_stepsize = float(check_stepsizeline[-5:])
                    if self.geodyn_StepSize == check_stepsize:
                        logging.info(f'The MSIS2 run has the correct STEP size of {self.geodyn_StepSize}')
                    else:
                        print(f'|     The MSIS2 Density file has the wrong stepsize, running again.')

                        logging.info(f'The existing MSIS2 run has the wrong stepsize  (found STEP to be {check_stepsize}, but need {self.geodyn_StepSize}). Running MSIS2 thru GEODYN with correct step size.')
                        self.setup_directories_and_geodyn_input()
                        self.make_output_directories()
                        self.print_runparameters_to_notebook()
                        self.prepare_tmpdir_for_geodyn_run()
                        self.run_geodyn_in_tmpdir()
                        self.post_geodynrun_savefiles_and_cleanup()
                else:
                    print(f'|     Running MSIS2 through GEODYN to construct a initialized orbit')
                    logging.info(f'No similar runs of MSIS2 exist on this arc. Running MSIS2 thru GEODYN...')
                    self.setup_directories_and_geodyn_input()
                    self.make_output_directories()
                    self.print_runparameters_to_notebook()
                    self.prepare_tmpdir_for_geodyn_run()
                    self.run_geodyn_in_tmpdir()
                    self.post_geodynrun_savefiles_and_cleanup()
                
                                
                logging.info(f'Running GEODYN with initialized orbit + uncertainty cloud tiegcm data. ')

                ### Construct the orbit cloud CSV
                logging.info(f'Constructing orbit file:  {self.orbitcloud_csv_file }')

                ### Use the msis2 run to identify the density file that 
                ###    will be used to index the satellite ephemeris within kamodo
                
                self.set_file_paths_for_multiple_arcs( arc , iarc)            
                SERIES = self.DEN_DIR + '_' + self.cd_model + self.directory_name_specifier
                OUTPUTDIR   = self.run_settings['path_to_output_directory'] + '/'+self.DEN_DIR+'/'+self.SERIES
                self.orbitcloud_csv_file =(self.model_data_path+'/OrbitCloud_Step'+
                                       str(int(self.geodyn_StepSize))+'_'+self.arcdate_for_files+'.csv')

#                 #### Construct the file to be appended to here
#                 f = open(self.orbitcloud_csv_file, "w")
#                 f.write("\n")
#                 f.close()
                
#                 from time import perf_counter
#                 t0=perf_counter()
                
#                 print(f'|     Constructing the OrbitCloud file: \n|          -{self.orbitcloud_csv_file}')
#                 print(f'|          - {self.ARC} ',)
#                 print(f'|          - time 1: {perf_counter()-t0}')
#                 self.set_file_paths_for_multiple_arcs( arc , iarc)            
#                 self.make_orbit_cloud_csv()
#                 print(f'|          - time 2: {perf_counter()-t0}')


            ### Once you have the orbitcloud csv, re-run GEODYN with the TIEGCM and orbit cloud csv
            #### RUN 2nd TIME WITH TIEGCM_oc (inputting the CSV orbitcloud) 
            self.set_density_model_setup_params( 'tiegcm_oc' )
            for iarc, arc in enumerate(self.arc_input):
                self.arcnumber = iarc
                if self.satellite == 'icesat2':
                
                    print(f'|     Running GEODYN with TIEGCM orbit cloud')
                    self.set_file_paths_for_multiple_arcs( arc , iarc)            
                    self.orbitcloud_csv_file =(self.model_data_path+'/OrbitCloud_Step'+
                                   str(int(self.geodyn_StepSize))+'_'+self.arcdate_for_files+'.csv')
                    self.model_data_path = self.run_settings['model_data_path']
                    logging.info(f'writing model path to file:  {self.model_data_path } \n {self.orbitcloud_csv_file}')
                    
                       #### Write the model path and orbitcloud filename to a file for GEODYN
                    filemodels = open("/data/geodyn_proj/pygeodyn/temp_runfiles/geodyn_modelpaths.txt","w+")
                    filemodels.write(self.model_data_path+'\n')
                    filemodels.write(self.orbitcloud_csv_file+  '\n')
                    filemodels.close()
                else:
                    print(f'| ********* Not using a valid satellite ********* ')
                    logging.info(f'Not using correct sat?  {self.satellite}')

                self.set_file_paths_for_multiple_arcs( arc , iarc)      
#                 print(f'|     Running GEODYN with TIEGCM orbit cloud')
                self.setup_directories_and_geodyn_input()
                self.make_output_directories()
                self.print_runparameters_to_notebook()
                self.prepare_tmpdir_for_geodyn_run()
                self.run_geodyn_in_tmpdir()
                self.post_geodynrun_savefiles_and_cleanup()
                
        
        elif self.den_model == 'hasdm_oc':  # regular run of GEODYN not using tiegcm Orbit Cloud
             
            self.set_density_model_setup_params( 'hasdm_oc' )
            for iarc, arc in enumerate(self.arc_input):
                self.arcnumber = iarc
                if self.satellite == 'icesat2':
                    print('5 running GEODYN with HASDM textfile', arc)
                    self.set_file_paths_for_multiple_arcs( arc , iarc)            
                    self.orbitcloud_csv_file =(self.model_data_path+'/HASDM_OrbitCloud2018313.csv')
                    self.model_data_path = self.run_settings['model_data_path']

                    logging.info(f'writing model path to file:  {self.model_data_path } \n {self.orbitcloud_csv_file}')
                    
                #### Write the model path and orbitcloud filename to a file for GEODYN
                    filemodels = open("/data/geodyn_proj/pygeodyn/temp_runfiles/geodyn_modelpaths.txt","w+")
                    filemodels.write(self.model_data_path+'\n')
                    filemodels.write(self.orbitcloud_csv_file+  '\n')
                    filemodels.close()
                else:
                    logging.info(f'Not using correct sat?  {self.satellite}')

                self.set_file_paths_for_multiple_arcs( arc , iarc)      
                self.setup_directories_and_geodyn_input()
                self.make_output_directories()
                self.print_runparameters_to_notebook()
                self.prepare_tmpdir_for_geodyn_run()
                self.run_geodyn_in_tmpdir()
                self.post_geodynrun_savefiles_and_cleanup()
                
    

        
        
        
        else:  # regular run of GEODYN not using tiegcm Orbit Cloud
            
            for iarc, arc in enumerate(self.arc_input):
                self.arcnumber = iarc
                self.set_file_paths_for_multiple_arcs( arc , iarc)            
                self.setup_directories_and_geodyn_input()
                self.make_output_directories()

                self.print_runparameters_to_notebook()
                self.prepare_tmpdir_for_geodyn_run()
                self.run_geodyn_in_tmpdir()
                self.post_geodynrun_savefiles_and_cleanup()
        



        