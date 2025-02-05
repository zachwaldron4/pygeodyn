"""Module that controls the running of GEODYN.

"""


#### Imports
import numpy as np
import pandas as pd
#### Computer/CL functions
import os
import os.path
import sys
import subprocess
import shutil
import time
#### reading and converting data
from  datetime import datetime,timedelta
import logging
import gc
from gc import collect as gc_collect


class RunController():
    """Inherited class that controls the GEODYN runs.
    
    Level 3 in the Pygeodyn Infrastructure. 
    Controls the directory structure, file maneuvering, and running of GEODYN
    for the purposes of using satellite drag to conduct IT model validation. 
    
    This class hosts all the methods  that are used to:
           1. create the necessary file/directory structure  
           2. construct a temporary folder 
           3. copy input files to the tmp dir and rename them for execution
           4. point to the chosen satellite_class for run parameters
           5. runs geodyn in tmp dir and checks for major errors
           6. rename and organize the output raw data and saves to directories
    """

    def __init__(self):  
        pass
    

    # previously setup_directories_and_geodyn_input
    # setup1_path_pointers
    def ctrlStage1_setup_path_pointers(self, skip_files=False):
        """Sets up directories and GEODYN input files.
        
        Notes:    
        """
        #### Change the working dir to the pygeodyn package
        os.chdir(self.path_pygeodyn)


        #### Setup the directory structure by identifying the numerous paths
        path_inputs = self.path_data_inputs+'/sat_'+self.prms['satellite']        
        #### series is the identifier for the run (i.e. msis2_fixedCD_)
        self.series = self.prms['den_model']+'_'+self.prms['cd_model'] \
                                        +self.prms['run_specifier']
        self.dir_output_raw = self.path_data_outputs_raw + '/'   \
                                    + self.prms['satellite']+ '/'\
                                    + self.prms['den_model'] +'/'+self.series
        self.make_directory_check_exist(self.path_data_outputs_raw)       
        # make output directory if it does not exist
        # self.make_directory_check_exist(self.params['path_output_dir'])
        # self.make_directory_check_exist(self.params['path_output_dir'] 
        #                                   + '/'+self.prms['den_model'])
        ## Path to Input file directories (ftn 05)
        self.dir_input = path_inputs + '/setups'
        self.dir_exat  = path_inputs + '/external_attitude'
        dir_g2b        = path_inputs + '/g2b'

        self.dir_makeg2b = dir_g2b


        
        if skip_files==False:
            #### Add temporary fix for spire
            # if 'spire'in self.prms['satellite']:
            dir_gravfield = self.path_data_inputs +'/common_2018'+''
            dir_atmograv  = self.path_data_inputs +'/common_2018'+''
            dir_ephem     = self.path_data_inputs +'/common_2018'+''
            # else:
            #     dir_gravfield = path_inputs + '/gravity'
            #     dir_atmograv  = path_inputs + '/atgrav'
            #     dir_ephem     = path_inputs + '/ephem'

            #--Planetary Ephemeris
            self.file_ephem      = dir_ephem     +'/'+ self.filename_ephem
            #--Atmospheric Gravity
            self.file_atmograv   = dir_atmograv  +'/'+ self.filename_atmograv
            #--Gravity Field
            self.file_grav_field = dir_gravfield +'/'+ self.filename_gravfield
            #--GDYN specific binary tracking data 
            self.file_G2B        = dir_g2b        +'/'+ str(self.filename_g2b)
            #--GDN-table - Solar flux,Ap,Kp,PolarMotion,A1-UTC,A1-UT1
            self.filename_gdntable = 'gdntable.data'
            self.file_gdntable = self.path_data_inputs+'/common_2018/'\
                                +self.filename_gdntable
            #----- Solar Radiation file. #### I dont have one  ####
            #        SOLRAD_filename = ARCFIL+'.'+self.GRAVITY
        else:
            pass
    



        
               


               
                 
                
    # previously make_output_directories(self):
    #            setup2_make_tmp_and_output_dirs
    def ctrlStage2_make_tmp_and_output_dirs(self):
        '''Method to build the output directory structure and the temporary
        run directory
        '''
        self.verboseprint('Original -- make_output_and_temprun_directories()')


        #   External Attitude File handled in the prep class
        # if not self.filename_exat:
        #     pass
        # else:
        #     self.file_exat = self.dir_exat +'/' +self.filename_exat

        ####  Make Directories if they do not exists
        #-------------------------------------------------------------
        # self.make_directory_check_exist(self.path_data_outputs_raw)
        # self.make_directory_check_exist(self.path_data_outputs_raw \
        #                                 +'/'+self.prms['den_model'])
        
        #### Setup the Temporary directory
        dir_tmp = self.path_tmp+'/'+self.series 
        self.dir_tmp_arc = dir_tmp+'/'+self.ARC
        
        ## Make temporary directory path
        self.make_directory_check_exist(self.path_tmp) 
        self.make_directory_check_exist(dir_tmp)
        #### Remove old tmp dir version and remake it 
        os.system('rm -rf '+self.dir_tmp_arc)
        # chmod 777 gives the tmp dir read, write and overwrite priveleges.
        self.make_directory_check_exist(self.dir_tmp_arc) 
        os.system('chmod 777 '+self.dir_tmp_arc)

        ####  Make the GEODYN output directories. If the below directories do 
        #      not exists, make them. 
        self.make_directory_check_exist(self.dir_output_raw) 
        self.make_directory_check_exist(self.dir_output_raw+'/ORBITS/')
        self.make_directory_check_exist(self.dir_output_raw+'/IIEOUT/')
        self.make_directory_check_exist(self.dir_output_raw+'/DENSITY/')
#         self.make_directory_check_exist(self.OUTPUTDIR+'/RESIDS/')
#         self.make_directory_check_exist(self.OUTPUTDIR+'/PUNCH/')
#         self.make_directory_check_exist(self.OUTPUTDIR+'/TELEM/')
#         self.make_directory_check_exist(self.OUTPUTDIR+'/EMAT/')
#         self.make_directory_check_exist(self.OUTPUTDIR+'/EMAT/scans/')
#         self.make_directory_check_exist(self.OUTPUTDIR+'/IISSET/')
#         self.make_directory_check_exist(self.OUTPUTDIR+'/XYZ_TRAJ/')
#         self.make_directory_check_exist(self.OUTPUTDIR+'/KEP_TRAJ/')


        ### Construct the setup file for the arc of choice
        #---- Input iisset file (fort.05)
        # if self.prms['satellite']  == 'spire83':
        #     self.filename_iisset      = self.dir_input  +'/' +self.setup_file_arc +''
        # else:
        #     self.filename_iisset      = self.dir_input  +'/' +self.setup_file_arc +'.bz2'

#         if self.prms['satellite']  == 'spire83':
#             #### Initialize our variables from user input
#             (path_to_setupfiles,
#              setup_file_arc, 
#              SAT_ID, 
#              den_model_setupval) = ( self.dir_input,  
#                                     self.setup_file_arc, 
#                                     self.prms['sat_ID'], 
#                                     self.iisset_den)

#             ORIG_iisset_file = self.filename_iisset 
#             iisset_file      = 'cleaned_setup'+'_'  + self.arcdate_for_files
#             ####    COPY THE FILE SO THAT YOU DON'T OVERWRITE THE ORIGINAL
#             ####    We copy to a temporary file "cleaned_setup_file"
#             shutil.copyfile(ORIG_iisset_file, self.dir_tmp_arc +'/'+iisset_file+'')
#             #### CHANGE TO THE TMPDIR
#             os.chdir(self.dir_tmp_arc)
# #             os.system('bunzip2 -v '+ '*.bz2')
#             os.chdir('/data/geodyn_proj/pygeodyn')
#             iisset_file = self.dir_tmp_arc+'/' +'cleaned_setup'+'_'  + self.arcdate_for_files
#             print('iisset_file',iisset_file)
#         else:
#             self.clean_iisset_file()

#         self.filename_iisset      = self.dir_tmp_arc  +'/'+'cleaned_setup'+'_'  + self.arcdate_for_files



        




    def ctrlStage3_print_to_notebook(self):
        ''' Function that prints run details to the notebook output.
        
        '''
        self.verboseprint('Original -- ctrlStage3_print_to_notebook()')


        
        now = datetime.now()-timedelta(hours=7)
        current_time = now.strftime("%H:%M:%S")
        self.verboseprint('=================================================')
        self.verboseprint('                VERBOSE OPTION ON                ')
        self.verboseprint('=================================================')
        self.verboseprint('')
        self.verboseprint(self.tabtab,'Current DIR: ', os.getcwd())
        print(self.run_ID,"    Current Time =     ", current_time, " GMT-7")
        print(self.run_ID)

        
        longest_line = '|      '+self.run_ID+"    Output directory:  " + self.dir_output_raw
        
        # if self.prms['satellite']  == 'spire83':
            # print('ERROR--fix this-- ctrlStage3_print_to_notebook()')
            # if  self.prms['epoch_start'] == None :  # if no options given 
            #     pass  ## use the defaults from the setup file (parsed above)
            # else:
            #     epoch_start            = self.prms['epoch_start'][0]
            #     epoch_start_YYMMDD     = epoch_start[:6].strip() 
            #     epoch_start_HHMM       = epoch_start[7:11].strip()
            #     epoch_start_SS_SSSSSSS = epoch_start[11:21].strip()
            #     epoch_start            = epoch_start_YYMMDD+epoch_start_HHMM+epoch_start_SS_SSSSSSS
            # if  self.prms['epoch_end'] == None :
            #     pass
            # else:
            #     epoch_end            = self.prms['epoch_end'][0]
            #     epoch_end_YYMMDD     = epoch_end[:6].strip() 
            #     epoch_end_HHMM       = epoch_end[7:11].strip()
            #     epoch_end_SS_SSSSSSS = epoch_end[11:21].strip()
            #     epoch_end            = epoch_end_YYMMDD+epoch_end_HHMM+epoch_end_SS_SSSSSSS

            # epoch_start_dt = pd.to_datetime( epoch_start_YYMMDD+epoch_start_HHMM, format='%y%m%d%H%M%S')
            # epoch_end_dt   = pd.to_datetime( epoch_end_YYMMDD+epoch_end_HHMM, format='%y%m%d%H%M%S')      
            # #### RE-SAVE THE DATE in datetime format for easy printing
            # self.epoch_start_dt = epoch_start_dt
            # self.epoch_end_dt   = epoch_end_dt        
        
        line_nos1 = '|---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8'
        print('|'+"—"*len(line_nos1))
        print('|',self.run_ID,'Parameters')
        print("| "+ "—"*len(self.run_ID+'Parameters'+' '))
        print('| '+" Run Specs")
        print('| '+" --------- ")
        print('| '+"   Satellite   ",self.prms['satellite'])
        print('| '+"   Run Type    ",self.prms['run_type'])
#         print('| '+"   CD Type     ",self.prms['cd_type'])
        print('| '+"   CD Value    ",self.prms['cd_value'])
        print('| '+"   Density     ",self.prms['den_model'])
        print('|')
        print('| '+" Epoch Info ")
        print('| '+" ---------- ")
        print('| '+"   Arc         ",self.arc_name_id)
        print('| '+"   Arc length  ",str(self.prms_arc['arc_length_h']), "hours")
        print('| '+"   Epoch Start ",str(self.prms_arc['epoch_startDT']))
        print('| '+"   Epoch End   ",str(self.prms_arc['epoch_stopDT']) )
        print('| '+"   Step Size   ",str(self.prms['step']), "seconds")
        print('|')
        print('| '+" Files Info")
        print('| '+" ---------- ")
        print('| '+"   Arc Name   ",self.ARC)
        print('|')
        print('| '+"   IISSET     ",".../"+"/".join(self.file_iisset.split('/')[3:]))
        print('| '+"   EXAT       ",".../"+"/".join(self.file_exat.split('/')[3:]))
        print('| '+"   Output Raw ",".../"+"/".join(self.dir_output_raw.split('/')[3:])+'/')
        print('|')
        print('|'+"—"*len(line_nos1))

#         if os.path.exists(self._INPUT_filename):
#             self.verboseprint(self.tabtab,"FORT.5  (input) file:  ", self._INPUT_filename)
#         else:
#             print(self.run_ID,"    FORT.5  (input) file:  ", self._INPUT_filename," not found.")    

#         if os.path.exists(self._G2B_filename):
#             self.verboseprint(self.tabtab,"FORT.40 (g2b)   file:  ", self._G2B_filename)
#         else:
#             print(self.run_ID,"    FORT.40 (g2b)   file:  ", self._G2B_filename," not found.")    

            
            
                
    def ctrlStage4_populate_tmpdir_for_run(self):
        '''  This is the ICESat-2 version of this method.
             
             It is being overridden to include the external attitude
        '''
        
        # logger = logging.getLogger(self.execlog_filename)
        # logger.info(f"ICESat2 - Construct a tmp directory in which to run IIS and IIE")

        
     
        #### Navigate to the tmp dirdir_tmp_arc
        os.chdir(self.dir_tmp_arc)
        
        ####-------------------------------------------------------------
        ####     Construct Common Setup of a GEODYN RUN
        ####         this is run in the TMPDIR_arc
        ####-------------------------------------------------------------
        self.verboseprint('-------------------------------------------------')
        self.verboseprint('       Linking files with the command line       ')
        self.verboseprint('-------------------------------------------------')
        
        self.verboseprint(self.tabtab,'Current DIR',os.getcwd())

        #### make copy to the External attitude file and save as EXAT01
        if not os.path.exists(self.dir_tmp_arc +'/EXAT01'+'.gz'):
#             if np.size(self.external_attitude) >= 1:
# #                 print('dict of EXATfilename: ',np.size(self.external_attitude))
#                 shutil.copyfile(self._EXTATTITUDE_filename[1], self.TMPDIR_arc +'/EXAT01'+'.gz')
#                 shutil.copyfile(self._EXTATTITUDE_filename[2], self.TMPDIR_arc +'/EXAT02'+'.gz')
#                 shutil.copyfile(self._EXTATTITUDE_filename[3], self.TMPDIR_arc +'/EXAT03'+'.gz')
#                 shutil.copyfile(self._EXTATTITUDE_filename[4], self.TMPDIR_arc +'/EXAT04'+'.gz')
#                 shutil.copyfile(self._EXTATTITUDE_filename[5], self.TMPDIR_arc +'/EXAT05'+'.gz')
#                 print('Copied 5 EXAT files')
#             else:
            shutil.copyfile(self.file_exat, self.dir_tmp_arc +'/EXAT01'+'.gz')
            self.verboseprint(self.tabtab,'copied:   exat file  > EXAT01'+'.gz')
            self.verboseprint(self.tabtab,'copied:   '+self.file_exat+' > EXAT01'+'.gz')
        else:
            self.verboseprint(self.tabtab,'copy is set up: EXAT01 file')

        
        #### make symlink to the G2B file and save as ftn40
        if not os.path.exists(self.dir_tmp_arc +'/ftn40'+''):
            os.symlink(self.file_G2B, self.dir_tmp_arc +'/ftn40'+'')
#             shutil.copyfile(self._G2B_filename, self.TMPDIR_arc +'/ftn40'+'')
            self.verboseprint(self.tabtab,'copied:   g2b file   > ftn40'+'')
        else:
            self.verboseprint(self.tabtab,'copy:  g2b file')

        #### make symlink to the gravity field and save as ftn12
        if not os.path.exists(self.dir_tmp_arc +'/ftn12'+''):
            shutil.copyfile(self.file_grav_field, self.dir_tmp_arc +'/ftn12'+'')
#             self.verboseprint(self.tabtab,'gravfield:',self._grav_field_filename)
            self.verboseprint(self.tabtab,'copied:   grav field > ftn12'+'')
        else:
            self.verboseprint(self.tabtab,'copy is set up: grav_field file')

        #### make symlink to the ephemerides and save as ftn01
        if not os.path.exists(self.dir_tmp_arc +'/ftn01'+''):
#             os.symlink(self._ephem_filename, self.TMPDIR_arc +'/ftn01')
            shutil.copyfile(self.file_ephem, self.dir_tmp_arc +'/ftn01'+'')
            self.verboseprint(self.tabtab,'copied:   ephem file > ftn01'+'')
        else:
            self.verboseprint(self.tabtab,'copy is set up: ephem file'+'')

        #### make symlink to the gdntable and save as ftn02
        if not os.path.exists(self.dir_tmp_arc +'/ftn02'):
            shutil.copyfile(self.file_gdntable, self.dir_tmp_arc +'/ftn02')
            self.verboseprint(self.tabtab,'copied:   gdntable   > ftn02')
        else:
            self.verboseprint(self.tabtab,'copy is set up: gdntable file')


        #### make symlink to the ATGRAVFIL and save as fort.18
        if not os.path.exists(self.dir_tmp_arc +'/fort.18'+''):
#             os.symlink(self._ATGRAV_filename, self.TMPDIR_arc +'/fort.18')
            shutil.copyfile(self.file_atmograv, self.dir_tmp_arc +'/fort.18'+'')
#             shutil.copyfile(self._ATGRAV_filename, self.TMPDIR_arc +'/ftn18')
#             self.verboseprint(self.tabtab,'ATGRAV:',self._ATGRAV_filename)
            self.verboseprint(self.tabtab,'copied:  atgrav     > fort.18'+'')
        else:
            self.verboseprint(self.tabtab,'symlink is set up: atgrav file')

            
        if not os.path.exists(self.dir_tmp_arc+'/ftn05'):
            os.system('cp '+self.file_iisset+' '+self.dir_tmp_arc+'/ftn05')
            self.verboseprint(self.tabtab,'copying          : iieout file')
        else:
            self.verboseprint(self.tabtab,'copied           : iieout file')

        if not os.path.exists(self.dir_tmp_arc+'/giis.input'):
            os.system('cp  '+self.dir_tmp_arc+'/ftn05 '+self.dir_tmp_arc+'/giis.input')
            self.verboseprint(self.tabtab,'copying          : giis.input file')
        else:
            self.verboseprint(self.tabtab,'copied              : giis.input file')   

        self.verboseprint('-------------------------------------------------------------------------')
        self.verboseprint('-------------------------------------------------------------------------')


        #### GUNZIP the files:  gzip is a very fast compression option.
#         self.verboseprint(self.tabtab, "gunzipping the input data files")
        self.verboseprint(self.tabtab, "gunzipping the input data files")

        os.system('gunzip -vr *.gz')


        
#         os.system('gunzip -ftn01.gz')
#     # previously called  prepare_tmpdir_for_geodyn_run()
#     #                    setup4_populate_tmpdir_for_run()
#     def ctrlStage4_populate_tmpdir_for_run(self):
#         '''  This it the base version of this method.  It can be overridden in
#              the Satellite Class to be satellite specific. 
             
#              Certain satellites and run types require different data inputs on
#              different fortran units.
#         '''
        
#         # logger = logging.getLogger(self.execlog_filename)
#         # logger.info(f"ORIGINAL- Construct a tmp directory in which to run IIS and IIE")
     
#         #### Navigate to the TMPDIR
#         os.chdir(self.dir_tmp_arc)
        
#         ####-------------------------------------------------------------
#         ####     Construct Common Setup of a GEODYN RUN
#         ####         this is run in the TMPDIR_arc
#         ####-------------------------------------------------------------
#         self.verboseprint('-------------------------------------------------')
#         self.verboseprint('       Linking files with the command line       ')
#         self.verboseprint('-------------------------------------------------')
#         self.verboseprint(self.tabtab,'Current DIR',os.getcwd())
        
#         print(self.run_ID,"    Copying input files to temp directory")
        
#         if not self.filename_exat:
#             pass
#         else:
#             #### make copy to the External attitude file and save as EXAT01
#             if not os.path.exists(self.dir_tmp_arc +'/EXAT01'):
#                 shutil.copyfile(self.file_exat, self.dir_tmp_arc +'/EXAT01')
#                 self.verboseprint(self.tabtab,'copied:   exat file  > EXAT01')
#             else:
#                 self.verboseprint(self.tabtab,'symlink is set up: EXAT01 file')

#         #### make symlink to the G2B file and save as ftn40
#         if not os.path.exists(self.dir_tmp_arc +'/ftn40'):
#             shutil.copyfile(self.file_G2B, self.dir_tmp_arc +'/ftn40'+'.gz')
#             self.verboseprint(self.tabtab,'copied:   g2b file   > ftn40'+'.gz')
#         else:
#             self.verboseprint(self.tabtab,'copy is set up:  g2b file')

#         #### make symlink to the gravity field and save as ftn12
#         if not os.path.exists(self.dir_tmp_arc +'/ftn12'):
#             shutil.copyfile(self.file_grav_field, self.dir_tmp_arc +'/ftn12'+'.gz')
#             self.verboseprint(self.tabtab,'copied:   grav field > ftn12'+'.gz')
#         else:
#             self.verboseprint(self.tabtab,'copy is set up: grav_field file')

#         #### make symlink to the ephemerides and save as ftn01
#         if not os.path.exists(self.dir_tmp_arc +'/ftn01'):
#             shutil.copyfile(self.file_ephem, self.dir_tmp_arc +'/ftn01'+'.gz')
#             self.verboseprint(self.tabtab,'copied:   ephem file > ftn01'+'.gz')
#         else:
#             self.verboseprint(self.tabtab,'copy is set up: ephem file')

#         #### make symlink to the gdntable and save as ftn02
#         if not os.path.exists(self.dir_tmp_arc +'/ftn02'):
#             shutil.copyfile(self.file_gdntable, self.dir_tmp_arc +'/ftn02'+'')
#             self.verboseprint(self.tabtab,'copied:   gdntable   > ftn02'+'')
#         else:
#             self.verboseprint(self.tabtab,'copy is set up: gdntable file')


#         #### make symlink to the ATGRAVFIL and save as fort.18
#         if not os.path.exists(self.dir_tmp_arc +'/fort.18'):
#             shutil.copyfile(self.file_atmograv, self.dir_tmp_arc +'/fort.18'+'.gz')
#             self.verboseprint(self.tabtab,'copied:   atgrav     > fort.18'+'.gz')
#         else:
#             self.verboseprint(self.tabtab,'symlink is set up: atgrav file')

# #         if self.satellite =='starlette':
# #             if not os.path.exists(self.TMPDIR_arc+'/ftn05.bz2'):
# #                 os.system('cp '+self._INPUT_filename+' '+self.TMPDIR_arc+'/ftn05.bz2')
# #                 self.verboseprint(self.tabtab,'copying          : input file')
# #             else:
# #                 self.verboseprint(self.tabtab,'copied           : input file')

# #             if not os.path.exists(self.TMPDIR_arc+'/ftn05'):
# #                 os.system('bunzip2 '+self.TMPDIR_arc+'/ftn05.bz2')
# #                 self.verboseprint(self.tabtab,'file not zipped  : input file')
# #             else:
# #                 self.verboseprint(self.tabtab,'file not zipped  : input file')
# #         else:
#         if not os.path.exists(self.dir_tmp_arc+'/ftn05'):
#             os.system('cp '+self.filename_iisset+' '+self.dir_tmp_arc+'/ftn05')
#             self.verboseprint(self.tabtab,'copying          : input file')
#         else:
#             self.verboseprint(self.tabtab,'copied           : input file')

#         if not os.path.exists(self.dir_tmp_arc+'/giis.input'):
#             os.system('cp  '+self.dir_tmp_arc+'/ftn05 '+self.dir_tmp_arc+'/giis.input')
#             self.verboseprint(self.tabtab,'copying          : giis.input file')
#         else:
#             self.verboseprint(self.tabtab,'copied              : giis.input file')   

#         self.verboseprint('-------------------------------------------------------------------------')
#         self.verboseprint('-------------------------------------------------------------------------')

#         #### GUNZIP the files:  gzip is fast compression option
#         os.system('gunzip -vr *.gz')
#         os.system('bunzip2 -v *.bz2')


    # previously called  run_geodyn_in_tmpdir()
    #                    run1_execute_geodyn_in_tmpdir
    def ctrlStage5_execute_geodyn_in_tmpdir(self):
        """ADD DOCSTRING
        """
        ####-------------------------------------
        ####     RUN GEODYN IIS
        ####-------------------------------------        

        ### Must change directory to run the IIS executable
        os.chdir(self.dir_tmp_arc)
        time.sleep(1)

        #### Populate the geodyn_options.txt file with the run options:
        self.geodyn_modify_inputs( self.prms['DRHODZ_update'],
                                   self.prms['den_model']      )      
        
        #### Run the IIS (geodyn II-Scheduler) executable 
        print()
        print(self.run_ID,"         Running IIS" )
        command_IIS = self.dir_IIS+'/giis2002_gfortran > '+'iisout 2> '+'iiserr'
        # logger.info(f" Running IIS: {command_IIS} ")
        
        subprocess.run(command_IIS, shell = True)
        ####  Because of memory issues if running lots of arcs in a loop, we 
        ###   may need to explicitly kill these subprocesses.       
#         p = subprocess.Popen(command_IIS ,shell=True  )
#         while(True):
#             poll = p.poll()
# #             print(stdout)
#             if not (poll == None):
#                 print('poll',poll)
#                 print('p',p)

#                 p.terminate()
#                 p = subprocess.Popen(command_IIS ,shell=True )                
#                 gc.collect()
#                 break
        time.sleep(1)

        ### Save the Interface files from IIS. 
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
        #_iiserr_filename
        file_iiserr = self.dir_tmp_arc+'/iiserr'
            # check if size of file is 0 
        if os.stat(file_iiserr).st_size == 0:
            print(self.run_ID,"         No errors in IIS" )
            print(self.run_ID,"---------End of IIS" )
        else:
            print('ERRORS FOUND IN IIS:', file_iiserr)
            with open(file_iiserr, 'r') as read_iiserrors:
                # Read & print the entire file
                print(read_iiserrors.read())
            sys.exit()

        ###  Cleanup temporary files from IIS to prepare for IIE files.
        os.system('rm -f ftn* fort.*')        
        
        ####--------------------------------------------------------------
        ####     RUN GEODYN IIE
        ####--------------------------------------------------------------
        print()
        print(self.run_ID,"         Running IIE" )
        now = datetime.now()-timedelta(hours=7)
        current_time = now.strftime("%H:%M:%S")
        print(self.run_ID,"         Current Time =", current_time, 'GMT-7')
        
        ##Interface files in IIE must be ftn11 and ftn12
        if not os.path.exists('ftn12'):
            os.system('cp giie.ft12 ftn12')
        else:
            self.verboseprint('ftn12 is ready')
        if not os.path.exists('ftn11'):
            os.system('cp giie.ft11 ftn11')
        else:
            self.verboseprint('ftn11 is ready')

        #### RUN THE IIE EXECUTABLE
        start = time.time()
        print(' ------ Current DIR: ', os.getcwd())
#         os.system("touch gmon.out")
        command_IIE = self.dir_IIE+'/giie2002_gfortran > '+'iieout 2> '+'iieerr' 
        time.sleep(0.5)
        # logger.info(f" Running IIE: {command_IIE} ")
        subprocess.run(command_IIE, shell = True)
               
            
############################  IIE PROFILER  ############################  
#
#         time.sleep(0.5)
#         print()
#         print(self.run_ID,"         Running IIE with profiler" )
#         command_IIE_prof = 'gprof ' + self.G2EDIR+'/giie2002_gfortran > iierun.stats'
#         subprocess.run(command_IIE_prof, shell = True)
#
########################################################################
#         proc_IIE = subprocess.Popen(command_IIE, stdout=subprocess.PIPE, 
#                        shell=True, preexec_fn=os.setsid) 
#         time.sleep(1)
#         os.killpg(os.getpgid(proc_IIE.pid), signal.SIGTERM)
#         p = subprocess.Popen(command_IIE ,shell=True  )
#         while(True):
#             poll = p.poll()
# #             print(stdout)
#             if not (poll == None):
#                 print('poll',poll)
#                 print('p',p)

#                 p.terminate()
#                 p = subprocess.Popen(command_IIE ,shell=True )                
#                 gc.collect()
#                 break
        time.sleep(1)

               
        ### Check if there are any errors from IIE output
        #_iieerr_filename
        file_iieerr = self.dir_tmp_arc+'/iieerr'
            # check if size of file is 0
        if os.stat(file_iieerr).st_size == 0:
            print(self.run_ID,"         No errors in IIE" )
            print(self.run_ID,"---------End of IIE" )
        else:
            print('ERRORS FOUND IN IIE:', file_iieerr)
            end = time.time()
            elapsed = end - start
            print(self.run_ID,'         Time of IIE: ',elapsed,'secs','(',str(float(elapsed)/60),' mins)')

            with open(file_iieerr, 'r') as read_iieerrors:
                # Read & print the entire file
                print(read_iieerrors.read())
                sys.exit('Ending program... Errors found in iieout file.')
        
        #### Check if the IIE ran properly....
        self.check_if_run_converged(self.dir_tmp_arc+'/iieout')

        end = time.time()
        elapsed = end - start
        print(self.run_ID,'         Time of IIE: ',elapsed,'secs','(',str(float(elapsed)/60),' mins)')
        now = datetime.now()
        current_time = now.strftime("%H:%M:%S")
        print(self.run_ID,"         Current Time =", current_time)




    # previously called post_geodyn
    # avefiles_and_cleanup()
    #                   run2_save_rawoutputs_and_cleanup()
    def ctrlStage6_save_rawoutputs_and_cleanup(self):
        """docstring

         After the IIS and IIE runs:
        """

        #### Remove files that won't be needed...
            # fort.11 and fort.12 are iis->iie interface files
            # fort.13 and fort.14 are ...[unknown]
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
            # f.write('\n')      
            # f.write('    ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **      \n')      
            # f.write('    ** NOTE ** -- PYGEODYN USER REMOVED THE DELETE LINES FROM  \n')      
            # f.write('             -- THIS SAVED FILE BUT THE DELETE LINES WERE      \n')      
            # f.write('             -- IN THE SETUP DECK AT THE TIME OF THE RUN.      \n')      
            # f.write('    ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **      \n')      
            # f.write('\n')      

        
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
#                         'fort.103':'msis_SWI_file' ,
                        'fort.105':'accel_file' ,
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
        os.system('mv fort.98  msisin_file_ephem')     
        os.system('mv fort.101 msisin_file_gpiflux')     
#         os.system('mv fort.101 msis_out_file')    # i dont want these anymore
        
#         print("self.save_drag_file", self.save_drag_file)

        if self.prms['save_drag_file']:
            print("Saving fort.103 as drag_file")
            print("Saving fort.104 as SatGeometry_file")
            os.system('mv fort.103 drag_file')     
            os.system('mv fort.104 SatGeometry_file')     

        if self.prms['save_accel_file']:
            os.system('mv fort.105 accel_file')     

        

        # os.system('rm -f slvtmp* ftn* fort.*')



        print(self.run_ID,'               Finished renaming files')      
        
        ####-----------------------------------------
        ####     Save files to their directories:
        ####----------------------------------------- 
        #
        # First remove files stored in the output directory that have this arc
        # name.
        
        dir_out = self.dir_output_raw

        os.system('rm -f '+dir_out+'/ORBITS/'+ self.ARC+'')
        os.system('rm -f '+dir_out+'/ORBITS/'+ self.ARC+'.Z')
        os.system('rm -f '+dir_out+'/ORBITS/'+ self.ARC+'.gz')
        #         
        os.system('rm -f '+dir_out+'/ORBITS/'+ self.ARC+'_orb1')
        os.system('rm -f '+dir_out+'/ORBITS/'+ self.ARC+'_orb1.bz2')
        os.system('rm -f '+dir_out+'/ORBITS/'+ self.ARC+'_accel_file')
        os.system('rm -f '+dir_out+'/ORBITS/'+ self.ARC+'_accel_file.bz2')
        #
        os.system('rm -f '+dir_out+'/RESIDS/'+ self.ARC+'')
        os.system('rm -f '+dir_out+'/RESIDS/'+ self.ARC+'.Z')
        os.system('rm -f '+dir_out+'/RESIDS/'+ self.ARC+'.gz')
        #
        os.system('rm -f '+dir_out+'/sumry/'+ self.ARC+'')
        os.system('rm -f '+dir_out+'/PUNCH/'+ self.ARC+'')
        os.system('rm -f '+dir_out+'/DENSITY/'+ self.ARC+'')
        os.system('rm -f '+dir_out+'/XYZ_TRAJ/'+ self.ARC+'')
        os.system('rm -f '+dir_out+'/KEP_TRAJ/'+ self.ARC+'')
        os.system('rm -f '+dir_out+'/IIEOUT/'+ self.ARC+'')
        os.system('rm -f '+dir_out+'/IISSET/'+ self.ARC+'')

        # Any files that are not frequently used are compressedf in their ascii
        #   form. gzip is used for very large files (speed needed) and bzip2 for
        #   all others.
        #              
#         os.system('bzip2 -v giis.input')
#         os.system('bzip2 -v Resid')
        os.system('bzip2 -v orbfil')
        os.system('bzip2 -v densityfil')
        
        if self.prms['save_drag_file']:
            #os.system('bzip2 -v drag_file')
#             os.system('bzip2 -v SatGeometry_file')
            pass
            
        if self.prms['save_accel_file']:
            os.system('bzip2 -v accel_file')

        
#         os.system('cp giis.input.bz2  '+self.OUTPUTDIR+'/IISSET/'+ self.ARC+'.bz2')
#         os.system('cp Resid.bz2 '      +self.OUTPUTDIR+'/RESIDS/'  +self.ARC+     '.bz2')

#         print('Check the arc name:  ',self.ARC )
#         print(' ')
#         print('Check the ORBFIL stuff:  ')
#         print(self.OUTPUTDIR,'/ORBITS/'  ,self.ARC,'_orb1.bz2' )

        os.system('cp orbfil.bz2 '     +dir_out+'/ORBITS/'  +self.ARC+'_orb1.bz2')
        
        if self.prms['save_accel_file']:
            os.system('cp accel_file '     +dir_out+'/ORBITS/'  +self.ARC+'_accel_file')
        
        os.system('cp densityfil.bz2 ' +dir_out+'/DENSITY/' +self.ARC+     '.bz2')
        
        if self.prms['save_drag_file']:
            os.system('cp drag_file '        +dir_out+'/DENSITY/' +self.ARC+     'drag_file')
            os.system('cp SatGeometry_file ' +dir_out+'/DENSITY/' +self.ARC+     'SatGeometry_file')
#         os.system('cp ascii_xyz.bz2 '  +self.OUTPUTDIR+'/XYZ_TRAJ/'+self.ARC+     '.bz2')
#         os.system('cp ascii_kep.bz2 '  +self.OUTPUTDIR+'/KEP_TRAJ/'+self.ARC+     '.bz2')
        os.system('mv IIEOUT.'+ self.ARC+' '+dir_out+'/IIEOUT/'+ self.ARC+'')
        os.system('cp msisin_file_ephem '+dir_out+'/DENSITY/'+self.ARC+'_msisin')
        os.system('cp msisin_file_gpiflux '+dir_out+'/DENSITY/'+self.ARC+'_msisin_gpiflux')
#         os.system('cp msis_out_file '+self.OUTPUTDIR+'/DENSITY/'+self.ARC+'_msisout')
#         os.system('cp msis_SWI_file '+self.OUTPUTDIR+'/DENSITY/'+self.ARC+'_msisSWI')
#         os.system('cp punch.gdn.bz2   '+self.OUTPUTDIR+'/PUNCH/'+ self.ARC+'.gdn.bz2')
#         os.system('cp sumry '+self.OUTPUTDIR+'/sumry/'+ self.ARC+'')
#         os.system('cp punch '+self.OUTPUTDIR+'/PUNCH/'+ self.ARC+'')

        print(self.run_ID,"               Finished copying files to outputdir")

        #### Go up 3 levels and delete the temporary directories:
        os.chdir('../../')        
        # print('current dir: ', os.getcwd())

        del_tmp = '/'.join(self.dir_tmp_arc.split('/')[:-1])


        print(self.tabtab,'Deleting tmp/: ',del_tmp)
        os.system('rm -rf'+' ' +del_tmp)
     






    ###########################################################################
    ##### MAKE THE CSV
    def make_orbit_cloud_csv(self, kamodo_flag=True, HASDM_format_flag=False):
        r"""
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
        from  datetime import datetime,timedelta

        ##### --------------------------------------------------------------------------------------------
        #### Import the INIT_ORBIT that was constructed from the initializing run of MSIS2 through GEODYN
        import sys
        # logger = logging.getLogger(self.execlog_filename)
        # logging.info(f'in make_orbit_cloud_csv()   \n       Path to DEN_CSV file:  {self.msis2_file_path}')
          
        print('make_orbit_cloud_csv():  msis2_file_path', self.msis2_file_path)

        DEN_csv = pd.read_csv(self.msis2_file_path, 
                            dtype=object,
                            names = ['YYMMDD',
                                     'HHMMSS',
                                     'Height_kilometers',
                                     'Lat',
                                     'Lon',
                                         ],
                            sep = r"\s+",
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
            sys.path.insert(0,self.path_kamodo_src+'/Kamodo_vJan2023/kamodo_ccmc/flythrough/')
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
        delta_m = 2000.*1e-3 # meters to kilometers

        # logging.info(f'LON and LAT cube size of orbit_cloud_file: {delta_deg} degrees')
        # logging.info(f'Altitude size of orbit_cloud_file:  {delta_m} kilometers')

        #### Open the file
        #### We will loop thru the DEN CSV and if the file already contains the the date, don't overwrite.
        
        ### The below removes any repeated dates in the file.
        # vals  = np.arange(DEN_csv.index[0],DEN_csv.index[-1]+1)
        # df = DEN_csv.set_index('Date',drop=False ) 
        # df['i_vals'] = vals
        # index_date = df.loc[df.index.max()]['i_vals'].min()
        
        DEN_csv = DEN_csv.drop_duplicates(subset=["Date"], keep='first'\
                                ).sort_values(by='Date'\
                                             ).reset_index(drop=True)
        
        
        for it,val in enumerate(DEN_csv['Date']):
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
                lon_plus_delta = lon      #np.mod(lon_plus_delta, 180)
            elif lon_plus_delta > 180:
                lon_plus_delta = lon      #np.mod(lon_plus_delta, -180)
            else:        
                lon_plus_delta = lon_plus_delta
            if lon_mins_delta < -180:
                lon_mins_delta = lon      #np.mod(lon_mins_delta, 180)
            elif lon_mins_delta > 180:
                lon_mins_delta = lon      #np.mod(lon_mins_delta, -180)
            else:
                lon_mins_delta = lon_mins_delta
            #
            ### WRAP THE LATS AROUND -90 to 90
            if lat_plus_delta < -90: ##less than
                lat_plus_delta = lat      #np.mod(lat_plus_delta, 90)
            elif lat_plus_delta > 90:
                lat_plus_delta = lat      # np.mod(lat_plus_delta, -90)
            else:
                lat_plus_delta = lat_plus_delta
            if lat_mins_delta < -90:
                lat_mins_delta = lat      #np.mod(lat_mins_delta, 90)
            elif lat_mins_delta > 90:
                lat_mins_delta = lat      #np.mod(lat_mins_delta, -90)
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
            if self.prms['den_model'] == 'tiegcm_oc':
                model          = 'TIEGCM'
                file_dir       = self.prms['model_data_path']+'/data/'
                # logger.debug(f"Added a forward slash to path of {self.model_data_path} to input into Kamodo")
                # variable_list  = ['rho','psi_O2', 'psi_O', 'psi_He','psi_N2', 'T_n']
                variable_list  = ['rho','mmr_O2', 'mmr_O', 'mmr_He','mmr_N2', 'T_n']

                # coord_type     = 'SPH'
                # coord_grid     = 'sph'
                # high_res       = 1.
                coord_type     = 'GDZ'
                coord_grid     = 'sph'
                high_res       = 1.
                verbose        = False  
                temp_var = 'T_n'
                den_var = 'rho'
                output_type='csv' 
                output_name='' 
                plot_output='' 
                plot_coord='' 
                _print_units=False
                # plot_output    = '' 
                # plot_coord     = '' 
                # _print_units   = True


            elif self.prms['den_model'] == 'ctipe_oc':
                #### Kamodo static inputs:
                model          = 'CTIPe'
                file_dir       = self.prms['model_data_path']+'/data/'
                variable_list  = ['rho','N_O', 'N_O2', 'N_N2', 'T']
                coord_type     = 'GDZ'#'SPH'
                coord_grid     = 'sph'
                high_res       = 1.
                verbose        = False   
                output_type='csv' 
                output_name='' 
                plot_output='' 
                plot_coord='' 
                _print_units=False
                temp_var = 'T'
                den_var = 'rho'

                ### IF USING CTIPE, in the old version of KAMODO, SHIFT THE LON
                lons_list  = [x+360 if x<0 else x for x in lons_list]  
                print("***** shifting the longitude range from -180,180 to 0,360 for CTIPe *****")
            
            elif self.prms['den_model'] == 'gitm':
                #### Kamodo static inputs:
                model          = 'GITM'
                
                list1 =  [ '2018.313', '2018.314', '2018.315', '2018.316',
                           '2018.317', '2018.318', '2018.319']
                list2 =  [ '2018.320', '2018.321', '2018.322','2018.323', 
                           '2018.324', '2018.325', '2018.326', '2018.327' ]  
                if   self.arc_name_id in list1:
                    file_dir       = self.prms['model_data_path']+'/data/'
                elif self.arc_name_id in list2:
                    file_dir       = self.prms['model_data_path']+'/data2/'
                else:
                    print("arcdate not found")
                    raise ValueError(f"ERROR, '{self.arc_name_id}' arcdate not found")

                variable_list  = ['rho_n','T_n']
                coord_type     = 'GDZ'
                coord_grid     = 'sph'
                high_res       = 1.
                verbose        = False   
                output_type='csv' 
                output_name='' 
                plot_output='' 
                plot_coord='' 
                _print_units=False
                temp_var = 'T_n'
                den_var = 'rho_n'
            
            from gc import collect as gc_collect
            gc_collect()
    
            print(f'|     Running data cube thru Kamodo... please hold.')
#             results = ModelFlythrough(model, file_dir, variable_list, unixtimes_list, lons_list, lats_list, alts_list,
#                                       coord_type, coord_grid, high_res=20., verbose=False,csv_output='', plot_output='')





            results  =  ModelFlythrough(model, file_dir, variable_list, 
                                    unixtimes_list, lons_list, lats_list, alts_list, 
                                    coord_type, coord_grid, high_res, 
                                    verbose, output_type, output_name, plot_output, 
                                    plot_coord, _print_units)




            #### Zach modified ModelFlythrough() to include a top boundary extrapolation as well as retun N2 
            print(results.keys())
            end = time.time()
            elapsed = end - start
            print(f'|     Kamodo Total Run Time:', elapsed    , 'seconds')
            print(f'|     Kamodo Total Run Time:', elapsed/60, 'minutes' )

            
#                     file.write(f"{datetime.strftime(datetime.fromtimestamp(results['utc_time'][ii]), '%y%m%d%H%M%S')}   {results['c1'][ii]:8.4f}   {results['c2'][ii]:8.4f}   {results['c3'][ii]:8.4f}   {valrho:15.6e}   {results['nden_O'][ii]:12.5e}   {results['nden_O2'][ii]:12.5e}   {results['nden_He'][ii]:12.5e}   {results['nden_N2'][ii]:12.5e}   {results['T_n'][ii]:8.4f} \n")

            mp_cgs = 1.6726e-24    # [g] mass of proton

            with open(self.orbitcloud_csv_file, 'r+') as file:
                for ii, valrho in enumerate(results[den_var]):
                    
                    #### For tiegcm, we convert mmrs to number densities and put into cgs units
                    if self.prms['den_model'] == 'tiegcm_oc':
                        nden_O =  (results['mmr_O'][ii]  * results['rho'][ii])/(mp_cgs*16)
                        nden_O2 = (results['mmr_O2'][ii] * results['rho'][ii])/(mp_cgs*32)
                        nden_He = (results['mmr_He'][ii] * results['rho'][ii])/(mp_cgs*4)
                        nden_N2 = (results['mmr_N2'][ii] * results['rho'][ii])/(mp_cgs*28)
                    
                    #### For ctipe, we convert from SI to CGS 
                    ##              ctipe only has O1, O2, and N2
                    elif self.prms['den_model'] == 'ctipe_oc':
                        nden_O =  (results['N_O'][ii]  / 1000.)
                        nden_O2 = (results['N_O2'][ii] / 1000.)
                        nden_N2 = (results['N_N2'][ii] / 1000.)
                            # fill the Helium values with zeros so as to not cause fortran formatting problems in the file read...
                        nden_He = 0.
                        valrho = valrho / 1000.
                         
                    #### For GITM, we convert from SI to CGS 
                    ##              gitm output can be upgraded to have more than 
                    ##                                        rho and temp
                    elif self.prms['den_model'] == 'gitm':
                        nden_O   = 0.
                        nden_O2  = 0.
                        nden_N2  = 0.
                            # fill the Helium values with zeros so as to not cause fortran formatting problems in the file read...
                        nden_He = 0.
                        valrho = valrho / 1000.
                                   
                        
                    file.write(f"{datetime.strftime(datetime.fromtimestamp(results['utc_time'][ii]), '%y%m%d%H%M%S')}  "\
                              +f"{results['c1'][ii]:9.4f}  {results['c2'][ii]:9.4f}  {results['c3'][ii]:9.4f}  "\
                              +f"{valrho:15.6e}  {nden_O:12.5e}  {nden_O2:12.5e}  {nden_He:12.5e}  "\
                              +f"{nden_N2:12.5e}   {results[temp_var][ii]:8.4e}\n")
                        ###!!!!!   NOTE THERE IS AN EXTRA SPACE B4 TEMPERATURE BECAUSE FORTRAN SUCKS
    
            results = 0
            del results
            end = time.time()
            elapsed = end - start
            print(f'|     Save OrbitCloud file run rime:', elapsed,       'seconds' )
            print(f'|     Save OrbitCloud file run rime:', elapsed/60,    'minutes' )

                            # file.write(f"{datetime.strftime(datetime.fromtimestamp(results['utc_time'][ii]), '%y%m%d%H%M%S')}
                            # {results['c1'][ii]:8.4f}   {results['c2'][ii]:8.4f}   {results['c3'][ii]:8.4f}   {valrho:15.8e} \n")
        else: #if kamodo_flag=False #### OPTION TO NOT RUN THRU KAMODO,  JUST MAKE THE FILE WITH COORDINATES
            
            if HASDM_format_flag:
                print("    Fill orbit_cloud with values from SET-HASDM ")
                
                ### SET-HASDM interpolator!

                ##### Open the right files:
                path_sethasdm_database = \
                    "/data/SatDragModelValidation/data/inputs/atmos_models/hasdm/SET_hasdm_density_database/"
                # print('date_list')
                # print(date_list[0])
                # print(date_list)
                max_doy   = pd.to_datetime(date_list, format='%y%m%d%H%M%S').max().day_of_year
                min_doy   = pd.to_datetime(date_list, format='%y%m%d%H%M%S').min().day_of_year 
                max_year  = pd.to_datetime(date_list, format='%y%m%d%H%M%S').max().year
                min_year  = pd.to_datetime(date_list, format='%y%m%d%H%M%S').min().year
                max_alt   = np.max(alts_list)
                min_alt   = np.min(alts_list)
                years  =  np.unique([min_year , max_year])  
                alts   =  np.unique([min_alt  , max_alt])/1000  
                DOYs   =  np.unique([min_doy  , max_doy])  

                print(f"--------------------------------------")
                # print(f"orbit params: {files_to_load}")
                print(f"   years: {years}")
                print(f"   alts:  {alts}")
                print(f"   DOYs:  {DOYs}")


                ## for sake of simplicity just load all relevant files in that altitude range
                files_to_load = []
                for iyear, valyear in enumerate(years):
                    files_to_load.append( str(valyear)+"_HASDM_400-475KM.den")
                    files_to_load.append( str(valyear)+"_HASDM_500-575KM.den")


                files_to_load = list(np.unique(files_to_load))
                print(f"****Loading SET-HASDM files: {files_to_load}****")
                                
                # #### Find the indices for the desired Day of Year (DOY)
                # doy1 = np.sort(DOYs)[0]-1
                # # doy1 = np.sort(DOYs)[0]
                # doy2 = np.sort(DOYs)[-1]+1
                # flag1=True


                # with open(path_sethasdm_database+files_to_load[0], 'r') as f:
                #     for line_no, line_text in enumerate(f):
                #         ### First instance of DOY
                #         if str(doy1) in line_text[5:10] and flag1:
                #             index1 = line_no
                #             flag1=False
                #         ### Final instance of DOY
                #         if str(doy2) in line_text[5:10]:
                #             index2 = line_no
                #             break
                # print('Will load data from index', index1,'to', index2 ,'for days', doy1,'-',doy2)


                ### Prepare to load HASDM dataset with correct dateformat and columns
                # from datetime import datetime
                # custom_date_parser= lambda v,w,x,y,z: datetime.strptime(f"{v}-{w} {x}:{y}:{z}", "%Y-%j %H:%M:%S.000")
                col_names =   [ "IYR"   ,   #  = year
                                "IYDAY" ,   #  = day of year (1 through 366)
                                "IHR"   ,   #  = hour of day
                                "IMIN"  ,   #  = minutes of hour
                                "SEC"   ,   #  = seconds of minute
                                "D1950" ,   #  = days since 0 Jan 1950
                                "HTM"   ,   #  = altitude above EGM-96(km)
                                "XLT"   ,   #  = local solar time (hour)
                                "XLAT"  ,   #  = geocentric latitude  (deg)
                                "XLON"  ,   #  = geographical longitude (deg)
                                "RHO"   ,   #  = HASDM density (kg/m3)
                                ]

                df_bighasdm = {}
                for ifile, file in enumerate(files_to_load):

                    ### Load HASDM data
                    df_bighasdm[file]  =  pd.read_csv(path_sethasdm_database+file,
                                                # skiprows = index1, 
                                                # nrows=(index2-index1),           
                                                sep = r"\s+",
                                                dtype=object,
                                                names= col_names)
                    df_bighasdm[file]['Date'] =  pd.to_datetime(     df_bighasdm[file]['IYR'] \
                                                                +' '+df_bighasdm[file]['IYDAY'] \
                                                                +' '+df_bighasdm[file]['IHR'] \
                                                                +' '+df_bighasdm[file]['IMIN'] \
                                                                +' '+df_bighasdm[file]['SEC'], \
                                                 format="%Y %j %H %M %S.000")
                    df_bighasdm[file] = df_bighasdm[file].query(f"Date >= '{pd.to_datetime(date_list, format='%y%m%d%H%M%S').min()}' and Date < '{pd.to_datetime(date_list, format='%y%m%d%H%M%S').max()}'")

                    del df_bighasdm[file]['IYR']
                    del df_bighasdm[file]['IYDAY']
                    del df_bighasdm[file]['IHR']
                    del df_bighasdm[file]['IMIN']
                    del df_bighasdm[file]['SEC']

                ### Concatinate it all into one dataframe
                DF = pd.concat([ df_bighasdm[file] for file in files_to_load]  )
                DF.reset_index(drop=True)

                ### Reduce size of dataframe by dropping unused altitudes.
                # DF = DF.query(f"HTM > '{(min_alt/1000)-25}'and HTM < '{(max_alt/1000)+25}'")


                from gc import collect as gc_collect
                ### Save memory:
                del df_bighasdm
                del DF['Date']
                hasdm_matrix = DF.to_numpy(dtype=float)
                del DF
                gc_collect()



                def Call_HASDM_interpolation(hasdm_mat):
                    ''' This inputs the HASDM grid for a given timeperiod and altitude range as 
                    a numpy matrix as input and returns an interpolation function to the
                    densities within that matrix'''
                    import numpy as np
                    from scipy.interpolate import RegularGridInterpolator, interp1d

                    ### Get unique values for the coordinates
                    vec_alt = np.unique(hasdm_mat[:,1])
                    vec_tim = np.unique(hasdm_mat[:,0])
                    vec_lon = np.unique(hasdm_mat[:,4])
                    vec_lat = np.unique(hasdm_mat[:,3])

                    ### Project the densities into log space
                    hasdm_mat[:,5] = np.log(hasdm_mat[:,5])
                    ### Construct an array of longitude indices
                    ind_lon = np.arange(0,np.size(vec_lon))
                    ### Make an empty data matrix to store the densities
                    den_grid = np.zeros((np.size(vec_alt),
                                        np.size(vec_tim),
                                        np.size(vec_lon),
                                        np.size(vec_lat),
                                        ))

                    ### Loop through the coordinates to construct an 
                    ###    even smaller grid through which to interpolate.
                    N_alt = np.size(vec_alt)
                    N_tim = np.size(vec_tim)
                    N_lat = np.size(vec_lat)
                    N_lon = np.size(vec_lon)

                    for ii_alt in range(N_alt):
                        ind_alt = np.where(hasdm_mat[:,1] == vec_alt[ii_alt])[0]

                        for ii_tim in range(N_tim):
                            ind_tim = np.where(hasdm_mat[:,0] == vec_tim[ii_tim])[0]

                            for ii_lat in range(N_lat):
                                ind_alt_tim = np.intersect1d(ind_alt, ind_tim)
                                hasdm_small = hasdm_mat[ind_alt_tim,:]

                                ind_lat = np.where(hasdm_small[:,3] == vec_lat[ii_lat])[0]
                                hasdm_small = hasdm_small[ind_lat,:]

                                den_ind = interp1d(hasdm_small[:,4], hasdm_small[:,5], 
                                                    kind='linear', fill_value='extrapolate')(vec_lon)
                                den_grid[ii_alt, ii_tim, ind_lon, ii_lat] = den_ind

                    # Create a gridded interpolant
                    gridVecs = [vec_alt, vec_tim, vec_lon, vec_lat]
                    interp_func = RegularGridInterpolator(gridVecs, den_grid, 
                                                        bounds_error=False,fill_value=None)
                    return(interp_func)


                interpolate_hasdm =  Call_HASDM_interpolation(hasdm_matrix)


                ### IF USING HASDM, SHIFT THE LONGITUDE TO CORRECTION RANGE
                lons_list  = [x+360 if x<0 else x for x in lons_list]  
                print("***** shifting the longitude range from -180,180 to 0,360 for HASDM *****")



                RHO_hasdm = []
                for index, datetimeval in enumerate(pd.to_datetime(date_list, format='%y%m%d%H%M%S')):

                        #### Convert to 1950 Julian Day from 
                        #### Julian date:    12:00 January 1, 4713 BC    (proleptic Julian calendar -->  JD)
                        #### CNES JD:         0:00 January 1, 1950       (JD − 2433282.5) 

                        
                        ### Lon is 0-360, geographical
                        ### Lat is -90 to 90
                        ### Alt above EGM-96(km)
                        
                    JD = pd.Timestamp(datetimeval).to_julian_date()
                    # JD = np.trunc(DF['D1950'].mean())
                    tim_sat = (JD - 2433282.5)+1 #JD+(datetimeval.hour/24)     # 25150.3
                    lat_sat = float(lats_list[index])    
                    lon_sat = float(lons_list[index])    
                    alt_sat = float(alts_list[index] )   
                    sat_coords = np.array([alt_sat,tim_sat, lon_sat, lat_sat])
                    
                    # Convert from kg/m3 to g/cm^3
                    RHO_hasdm.append( np.exp(interpolate_hasdm(sat_coords))[0]/1000)



                with open(self.orbitcloud_csv_file, 'r+') as file:
                    
                    for ii, valval in enumerate(unixtimes_list):
#                         print('whoops gotta fix this lol')
                        
                        file.write(f"{datetime.strftime(datetime.fromtimestamp(valval), '%y%m%d%H%M%S')}  "\
                                  +f"{lons_list[ii]:9.4f}  {lats_list[ii]:9.4f}  {alts_list[ii]:9.4f}  "\
                                  +f"{RHO_hasdm[ii]:15.6e}  {0.:12.5e}  {0.:12.5e}  {0.:12.5e}  "\
                                  +f"{0.:12.5e}   {0.:8.4e}\n")                        



            else:#### REGULAR GEODYN requestied format without the density from Kamodo 
                with open(self.orbitcloud_csv_file, 'r+') as file:
                    for ii, valval in enumerate(date_list):
#                         file.write(f"{datetime.strftime(datetime.fromtimestamp(unixtimes_list[ii]), '%y%m%d%H%M%S')}   {lons_list[ii]:8.4f}   {lats_list[ii]:8.4f}   {alts_list[ii]:8.4f}   \n")
                        file.write(f"{datetime.strftime(datetime.fromtimestamp(valval), '%y%m%d%H%M%S')}  {lons_list[ii]:9.4f}  {lats_list[ii]:9.4f}  {alts_list[ii]:9.4f}  \n")                        

            
            


##########################################################################################################

        
    
    def RUN_make_OrbitCloud_trajectories(self):
        '''
        This function takes a GEODYN run state and only performs the steps necessary to construct a ORBIT_CLOUD trajectory file.
        
        The steps are as follows:
            1. Do an initial run of GEODYN with MSIS2 to get an initialized orbit of the satellite for this arc.
            2. Construct a fixed width file with the Coordinates 
        '''
                
        from os.path import exists
        
        ### Make an execution log file
        iarc = 0
        arc  = self.arc_input[0]
        self.arcnumber = iarc
        self.set_file_paths_for_multiple_arcs( arc , iarc)            
        self.set_density_model_setup_params('msis2' )
        self.ctrlStage1_setup_path_pointers()
        self.ctrlStage2_make_tmp_and_output_dirs()
        # logger = logging.getLogger(self.execlog_filename)
        # logging.info('Running PYGEODYN with the Orbit Cloud Method \n         Check to see if the CSV files have been created using msis2. ')

        #### RUN 1st WITH MSIS2 IF THE FILE DOES NOT EXIST
        self.set_density_model_setup_params( 'msis2' )
        for iarc, arc in enumerate(self.arc_input):
            self.arcnumber = iarc
            self.set_file_paths_for_multiple_arcs( arc , iarc) 
            print('****** Run GEODYN once with MSIS2 to make INIT_ORBIT \n', arc)
            
            
#             self.orbitcloud_csv_file =(self.OUTPUTDIR+'/OrbitCloud_Step'+
#                                    str(int(self.prms['step']))+'_'+self.arcdate_for_files+'.csv')

            self.orbitcloud_csv_file = ('/data/data_geodyn/atmos_models_data/OrbitCloud_Arcs/ICESat2_FixedCD_2.5/' +
                                          '/OrbitCloud_Step'+str(int(self.prms['step']))+'_'+self.arcdate_for_files+'.csv')


            self.msis2_file_path = self.dir_output_raw+'/DENSITY/'+self.ARC+'_msisin'
            
            
#             print()
#             print('****** outputdir',self.OUTPUTDIR)
#             print('****** orbitcloud_csv_file',self.orbitcloud_csv_file)
#             print('****** msis2_file_path',self.msis2_file_path)
#             print()

            file_exists = exists(self.msis2_file_path)
            if file_exists:
#                 print('****** file_exists (msis2 density file):',self.msis2_file_path )
                # logging.info('A similar MSIS2 output has been made. Check to see if its stepsize is consistent.')
                msis2_log_file =  self.dir_output_raw+'/pygeodyn_runlog_'+self.ARC+'.txt'

                with open(msis2_log_file, 'r') as f:
                    for line_no, line in enumerate(f):
                        if 'STEP             ' in line:
                            check_stepsizeline = line

                check_stepsize = float(check_stepsizeline[-5:])
                if self.prms['step'] == check_stepsize:
                    logging.info(f"The MSIS2 run has the correct STEP size of {self.prms['step']}")

                else:
                    # logging.info(f"The existing MSIS2 run has the wrong stepsize  (found STEP to be {check_stepsize}, but need {self.prms['step']}). Running MSIS2 thru GEODYN with correct step size.")
                    self.ctrlStage1_setup_path_pointers()
                    self.ctrlStage2_make_tmp_and_output_dirs()
                    self.ctrlStage3_print_to_notebook()
                    self.ctrlStage4_populate_tmpdir_for_run()
                    self.ctrlStage5_execute_geodyn_in_tmpdir()
                    self.ctrlStage6_save_rawoutputs_and_cleanup()

            else:
                # logging.info(f'No similar runs of MSIS2 exist on this arc. Running MSIS2 thru GEODYN...')
                self.ctrlStage1_setup_path_pointers()
                self.ctrlStage2_make_tmp_and_output_dirs()
                self.ctrlStage3_print_to_notebook()
                self.ctrlStage4_populate_tmpdir_for_run()
                self.ctrlStage5_execute_geodyn_in_tmpdir()
                self.ctrlStage6_save_rawoutputs_and_cleanup()


            # logging.info(f'Running GEODYN with initialized orbit + uncertainty cloud tiegcm data. ')

            ## TODO: make the tiegcm files an input option
            orbitcloud_csv_check = exists(self.orbitcloud_csv_file)
            print('****** 2.5 check- orbitcloud_csv_file Does not exist \n ****** ****** ',  self.orbitcloud_csv_file,  )

            if orbitcloud_csv_check:
                print('****** 3- construct orbit cloud file', arc)

                self.set_file_paths_for_multiple_arcs( arc , iarc)            
                series = self.prms['den_model'] + '_' + self.cd_model + self.directory_name_specifier
#                 OUTPUTDIR   = '/data/data_geodyn/results/'+self.SATELLITE_dir + '/'+self.prms['den_model']+'/'+series
                OUTPUTDIR   = self.prms['path_output_dir'] + '/'+self.prms['den_model']+'/'+self.series


#                 logging.info(f'Orbit Cloud exists:  {self.orbitcloud_csv_file }')
                self.make_orbit_cloud_csv(kamodo_flag=False, HASDM_format_flag=True)

            else:
                ## Construct the orbit cloud CSV
#                 logging.info(f'Constructing orbit file:  {self.orbitcloud_csv_file }')
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
        This is the OLD main run function that calls the above functions in the
        Pygeodyn Controller class.
                
        '''
        
        self.unique_arc_count = 0
       
    
    
        from os.path import exists
        
        ####   If we are using one of the models that require Kamodo, we will want to
        ####   do a pre-run initialization to get the orbit output along the satellite using MSISe2
        if  self.prms['den_model'] == 'tiegcm_oc'  or \
            self.prms['den_model'] == 'ctipe_oc'   or \
            self.prms['den_model'] == 'gitm'       :
                     
            
            ### Make an execution log file
            iarc = 0
            arc=self.arc_input[0]
            self.arcnumber = iarc

            self.set_file_paths_for_multiple_arcs( arc , iarc)            
            self.set_density_model_setup_params(self.prms['den_model'])
            self.ctrlStage1_setup_path_pointers()
            self.ctrlStage2_make_tmp_and_output_dirs()
            
            print(f'+==================================================')
            print(f'|     Running GEODYN with Orbit Cloud Method     ',)
            print(f'|                                                ',)
            # logger = logging.getLogger(self.execlog_filename)
            # logging.info('Running PYGEODYN with the Orbit Cloud Method \n         
            # Check to see if the CSV files have been created using msis2. ')

            #### RUN FIRST WITH MSIS2 IF THE FILE DOES NOT EXIST
            self.set_density_model_setup_params( 'msis2' )
            for iarc, arc in enumerate(self.arc_input):
                self.arcnumber = iarc
                self.set_file_paths_for_multiple_arcs( arc , iarc) 

                #### identify supposed location of msis2 run
                series = self.prms['den_model'] + '_' + self.cd_model + self.directory_name_specifier
                OUTPUTDIR   = self.prms['path_output_dir'] + '/'+self.prms['den_model']+'/'+self.series
                self.orbitcloud_csv_file =(self.prms['model_data_path']+'/OrbitCloud_Step'+
                                       str(int(self.prms['step']))+'_'+self.arcdate_for_files+'.csv')
                self.msis2_file_path = OUTPUTDIR+'/DENSITY/'+self.ARC+'_msisin'

#                 self.arcdate_for_files = str(iarc+1)+'.'+ self.YR + doy 
#                 print('msis2_file_path',self.msis2_file_path)
                
                #### Check if the MSIS2 density file exists
                file_exists = exists(self.msis2_file_path)
                if file_exists:
                    print(f'|     MSIS2 Density file already exists.               ',)
                    print(f'|          - {self.ARC}_msisin ',)
                    #                     os.system('bunzip2 -v '+self.msis2_file_path)
                    #                     self.msis2_file_path =  OUTPUTDIR+'/DENSITY/'+self.ARC
                    # logging.info('A similar MSIS2 output has been made. Check to see if its stepsize is consistent.')
                    msis2_log_file =  OUTPUTDIR+'/pygeodyn_runlog_'+self.ARC+'.txt'

                    #### Check the stepsize of the density file...
                    with open(msis2_log_file, 'r') as f:
                        for line_no, line in enumerate(f):
                            if 'STEP             ' in line:
                                check_stepsizeline = line
                    check_stepsize = float(check_stepsizeline[-5:])
                    if self.prms['step'] == check_stepsize:
                        logging.info(f"The MSIS2 run has the correct STEP size of {self.prms['step']}")
                    else:
                        print(f'|     The MSIS2 Density file has the wrong stepsize, running again.')

                        # logging.info(f"The existing MSIS2 run has the wrong stepsize  (found STEP to be {check_stepsize}, but need {self.prms['step']}). Running MSIS2 thru GEODYN with correct step size.")
                        self.ctrlStage1_setup_path_pointers()
                        self.ctrlStage2_make_tmp_and_output_dirs()
                        self.ctrlStage3_print_to_notebook()
                        self.ctrlStage4_populate_tmpdir_for_run()
                        self.ctrlStage5_execute_geodyn_in_tmpdir()
                        self.ctrlStage6_save_rawoutputs_and_cleanup()
                else:
                    print(f'|     Running MSIS2 through GEODYN to construct a initialized orbit')
                    # logging.info(f'No similar runs of MSIS2 exist on this arc. Running MSIS2 thru GEODYN...')
                    self.ctrlStage1_setup_path_pointers()
                    self.ctrlStage2_make_tmp_and_output_dirs()
                    self.ctrlStage3_print_to_notebook()
                    self.ctrlStage4_populate_tmpdir_for_run()
                    self.ctrlStage5_execute_geodyn_in_tmpdir()
                    self.ctrlStage6_save_rawoutputs_and_cleanup()
                
                                
                # logging.info(f'Running GEODYN with initialized orbit + uncertainty cloud  data. ')

                ### Construct the orbit cloud CSV
                # logging.info(f'Constructing orbit file:  {self.orbitcloud_csv_file }')

                ### Use the msis2 run to identify the density file that 
                ###    will be used to index the satellite ephemeris within kamodo
                
                self.set_file_paths_for_multiple_arcs( arc , iarc)            
                series = self.prms['den_model'] + '_' + self.cd_model + self.directory_name_specifier
                OUTPUTDIR   = self.prms['path_output_dir'] + '/'+self.prms['den_model']+'/'+self.series
                self.orbitcloud_csv_file =(self.prms['model_data_path']+'/OrbitCloud_Step'+
                                       str(int(self.prms['step']))+'_'+self.arcdate_for_files+'.csv')

                #### ONLY RERUN ORBITCLOUD if PATH NOT EXIST or if file is super small (i.e. empty)
                file_exists = exists(self.orbitcloud_csv_file)                           
                try:
                    orbit_cloud_csv_size = os.path.getsize(self.orbitcloud_csv_file)
                except:
                    orbit_cloud_csv_size = 1   

                if not file_exists or orbit_cloud_csv_size < 1000: # run if file has less than 1 kilobytes  
                    if orbit_cloud_csv_size < 1000:
                        print(f"|     File is too small or empty: {self.orbitcloud_csv_file.split('/')[-1]}" )
    
                    #### Construct the file to be appended to here
                    f = open(self.orbitcloud_csv_file, "w")
                    f.write("\n")
                    f.close()

                    from time import perf_counter
                    t0=perf_counter()

                    print(f'|     Constructing the OrbitCloud file: \n|          -{self.orbitcloud_csv_file}')
#                     print(f'|          - {self.ARC} ')
#                     print(f'|          - time 1: {perf_counter()-t0}')
                    ###########################################################
                    self.set_file_paths_for_multiple_arcs( arc , iarc)          #  ORBIT CLOUD CALL!    
#                     print(f'|          - {self.ARC} ')
                    self.make_orbit_cloud_csv()                                 # 

                    gc.collect()

                    ###########################################################
                    print(f'|          - time 2: {perf_counter()-t0}')

                else:
                    print(f'|     Already have OrbitCloud file for this arc: \n|          -{self.orbitcloud_csv_file}')
                    print(f'|          - {self.ARC} ',)

            ### Once you have the orbitcloud csv, re-run GEODYN with the TIEGCM and orbit cloud csv
            #### RUN 2nd TIME WITH TIEGCM_oc (inputting the CSV orbitcloud) 
            self.set_density_model_setup_params( self.prms['den_model'] )
            for iarc, arc in enumerate(self.arc_input):
                self.arcnumber = iarc
                if self.prms['satellite'] == 'icesat2':
                
                    print(f'|     Running GEODYN with orbit cloud')
                    self.set_file_paths_for_multiple_arcs( arc , iarc)            
                    self.orbitcloud_csv_file =(self.prms['model_data_path']+'/OrbitCloud_Step'+
                                   str(int(self.prms['step']))+'_'+self.arcdate_for_files+'.csv')
                    # self.model_data_path = self.prms['model_data_path']
                    # logging.info(f'writing model path to file:  {self.model_data_path } \n {self.orbitcloud_csv_file}')
                    
                       #### Write the model path and orbitcloud filename to a file for GEODYN
                    filemodels = open("/data/geodyn_proj/pygeodyn/temp_runfiles/geodyn_modelpaths.txt","w+")
                    filemodels.write(self.prms['model_data_path']+'\n')
                    filemodels.write(self.orbitcloud_csv_file+  '\n')
                    filemodels.close()
                else:
                    print(f'| ********* Not using a valid satellite ********* ')
                    # logging.info(f"Not using correct sat?  {self.prms['satellite']}")

                self.set_file_paths_for_multiple_arcs( arc , iarc)      
#                 print(f'|     Running GEODYN with TIEGCM orbit cloud')
                self.ctrlStage1_setup_path_pointers()
                self.ctrlStage2_make_tmp_and_output_dirs()
                self.ctrlStage3_print_to_notebook()
                self.ctrlStage4_populate_tmpdir_for_run()
                self.ctrlStage5_execute_geodyn_in_tmpdir()
                self.ctrlStage6_save_rawoutputs_and_cleanup()
                gc.collect()
        
        ### Case for the HASDM model that uses the orbit cloud
        elif self.prms['den_model'] == 'hasdm_oc':  
             
            #### setup the hasdm case
            self.set_density_model_setup_params(  'hasdm_oc'  )
            for iarc, arc in enumerate(self.arc_input):
                self.arcnumber = iarc
                if self.prms['satellite'] == 'icesat2':

                    self.set_file_paths_for_multiple_arcs( arc , iarc)  
                    # self.model_data_path = self.prms['model_data_path']

                    print('self.model_data_path:  ', self.prms['model_data_path'])
    
    
                    self.orbitcloud_csv_file = ( self.prms['model_data_path']    +
                                                 '/HASDM_OrbitCloud_'    +
                                                 self.arcdate_for_files  +
                                                 '.csv' )
        

                    # logging.info(f'writing model path to file:  {self.model_data_path } \n {self.orbitcloud_csv_file}')
                    
                #### Write the model path and orbitcloud filename to a file for GEODYN
                    filemodels = open("/data/geodyn_proj/pygeodyn/temp_runfiles/geodyn_modelpaths.txt","w+")
                    filemodels.write(self.prms['model_data_path']+'\n')
                    filemodels.write(self.orbitcloud_csv_file+  '\n')
                    filemodels.close()
                else:
                    logging.info(f"Not using correct sat?  {self.prms['satellite']}")

                self.set_file_paths_for_multiple_arcs( arc , iarc)      
                self.ctrlStage1_setup_path_pointers()
                self.ctrlStage2_make_tmp_and_output_dirs()
                self.ctrlStage3_print_to_notebook()
                self.ctrlStage4_populate_tmpdir_for_run()
                self.ctrlStage5_execute_geodyn_in_tmpdir()
                self.ctrlStage6_save_rawoutputs_and_cleanup()
                
                gc.collect()


        
        
        
        else:  # regular run of GEODYN not using tiegcm Orbit Cloud
            
            for iarc, arc in enumerate(self.arc_input):
                self.arcnumber = iarc
                self.set_file_paths_for_multiple_arcs( arc , iarc)            
                self.ctrlStage1_setup_path_pointers()
                self.ctrlStage2_make_tmp_and_output_dirs()
                self.ctrlStage3_print_to_notebook()
                self.ctrlStage4_populate_tmpdir_for_run()
                self.ctrlStage5_execute_geodyn_in_tmpdir()
                self.ctrlStage6_save_rawoutputs_and_cleanup()
                
                gc.collect()

                



    def call_control_sequence(self):
        self.ctrlStage1_setup_path_pointers()
        self.ctrlStage2_make_tmp_and_output_dirs()
        self.ctrlStage3_print_to_notebook()
        self.ctrlStage4_populate_tmpdir_for_run()
        self.ctrlStage5_execute_geodyn_in_tmpdir()
        self.ctrlStage6_save_rawoutputs_and_cleanup()
        gc.collect()

                
                
                
# if __name__ == '__main__':
#     main_memory()                
#             import psutil
#             from psutil._common import bytes2human
#             def pprint_ntuple(nt):
#                 for name in nt._fields:
#                     value = getattr(nt, name)
#                     if name != 'percent':
#                         value = bytes2human(value)
#                     print('%-10s : %7s' % (name.capitalize(), value))
#             def main_memory():
#                 print('MEMORY\n------')
#                 pprint_ntuple(psutil.virtual_memory())
# #                 print('\nSWAP\n----')
# #                 pprint_ntuple(psutil.swap_memory())

        