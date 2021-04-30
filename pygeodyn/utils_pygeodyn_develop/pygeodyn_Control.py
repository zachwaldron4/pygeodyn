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








from util_Set_Inputs            import UtilSetInputs
from util_ControlTools          import UtilControl_Tools



            
class pygeodyn_CONTROL(UtilControl_Tools, UtilSetInputs):

    def __init__(self, params):  
        # CHANGEABLE INPUTS
        self.satellite        = params['satellite']
        self.den_model         = params['den_model']
        self.empirical_accels  = params['empirical_accels']
        self.SpecialRun_name = params['SpecialRun_name']
        self.arc             = params['arc']
        self.options_in      = params['options_in']
        self.verbose         = params['verbose']
        self.run_ID          = params['run_ID']

        self.set_satellite_params( self.satellite )
        self.set_density_model_setup_params( self.den_model )
        self.set_acceleration_params( self.empirical_accels )
            
            
        self.tabtab = '       '
            
    def setup_directories_and_geodyn_input(self):
        self.verboseprint('=================================================')
        self.verboseprint('                VERBOSE OPTION ON                ')
        self.verboseprint('=================================================')
        self.verboseprint('')
        self.verboseprint(self.tabtab,'Current DIR: ', os.getcwd())
        ####-------------------------------------------------------------
        ####       Setup Directory Structure 
        ####-------------------------------------------------------------
        
        now = datetime.now()
        current_time = now.strftime("%H:%M:%S")
        print(self.run_ID,"    Current Time =     ", current_time)
        print()

        #### TO DO: For different satellites, these will change
        path_run_sat     = '/data/geodyn_proj/runs_geodyn/'+self.SATELLITE_dir
        path_run_inputs  = '/data/data_geodyn/inputs/'+self.SATELLITE_dir
        path_run_outputs  = '/data/data_geodyn/results/'+self.SATELLITE_dir
        path_geodyn_code = '/data/geodyn_proj/geodyn_code'
        path_extra_dirs  = '/data/data_geodyn/extra_dirs'

        #### Run Series (This will be a output directory identifier)
        SERIES = self.DEN_DIR + '_' + self.ACCELS + self.SpecialRun_name

        #### Point to the GEODYN executables
        self.G2SDIR      = path_geodyn_code + '/IIS/ORIG'
        self.G2EDIR      = path_geodyn_code + '/IIE/' + self.GDYN_version

        #### Extra and Temporary directories
        EMATUDIR     = path_extra_dirs          
        COMMONDIR    = path_extra_dirs                 
        COMMONHD4DIR = path_extra_dirs             
        DELDIR       = path_extra_dirs + '/deletes/'+self.SATELLITE_dir                 
        BINDIR       = path_extra_dirs + '/bin'
        TMPDIR       = path_extra_dirs +'/tmp/'+self.SATELLITE_dir+'/'+SERIES 

        #### Control Inputs for the GEODYN run
            ## Input Setup File directory
        FTN05DIR  = path_run_inputs + '/setups'
        INPUTDIR  = FTN05DIR

        ## Input other necessary directorys
        DIRGRAV   = path_run_inputs + '/gravity'
        G2BDIR    = path_run_inputs + '/g2b'
        ATGRAVDIR = path_run_inputs + '/atgrav'
        EPHEMDIR  = path_run_inputs + '/ephem'
        
        #### Set up path for OUTPUT
        self.OUTPUTDIR = path_run_outputs + '/'+self.DEN_DIR+'/'+SERIES

        
        #-------------------------------------------------------------
        #  Make Directories if they do not exists
        #-------------------------------------------------------------
        #### If the below directories do not exists, build them:    
        ## make output directory 
        self.make_directory_check_exist(path_run_outputs)
        self.make_directory_check_exist(path_run_outputs + '/'+self.DEN_DIR)
        self.make_directory_check_exist(self.OUTPUTDIR) 
        
        ## Make temporary directory path
        self.make_directory_check_exist(path_extra_dirs) 
        self.make_directory_check_exist(path_extra_dirs+'/tmp') 
        self.make_directory_check_exist(path_extra_dirs+'/tmp/'+self.SATELLITE_dir)
        self.make_directory_check_exist(path_extra_dirs+'/tmp/'+self.SATELLITE_dir+'/'+SERIES)

        
        ####-------------------------------------------------------------
        ####      Control Input Files:
        ####-------------------------------------------------------------

        ### Arc File      
        if self.satellite == 'starlette':
            self.ARC =  self.SATELLITE_dir + self.arc
        else:
            self.ARC =   self.arc
        
        
        ARCFIL   = self.ARC


        ### Gravity Field
        self.GRAVITY = self.grav_id

        ### Solar Radiation file
        SOLRAD_filename = ARCFIL+'.'+self.GRAVITY

        #### TO DO: FOR different satellites these will change
        
        
        ### GEODYN DATA FILE (GEODYN2 BINARY FILE FOR OBSERVATIONS)
        G2B_filename    = self.g2b_file

        ### ATMOSPHERIC GRAVITY
        ATGRAV_filename = self.atgrav_file
        

        ####-------------------------------------------------------------
        ####      Double check that the input files exist:
        ####-------------------------------------------------------------

        self._G2B_filename   = G2BDIR   +'/'+ G2B_filename
        
        if self.satellite == 'starlette':
            self._grav_field_filename = DIRGRAV +'/grvfld.'+self.GRAVITY
            self._INPUT_filename = INPUTDIR +'/'+ ARCFIL+'.bz2'

        elif self.satellite == 'icesat2':
            self._grav_field_filename = DIRGRAV +'/'+self.gravfield_file
            self._INPUT_filename = INPUTDIR +'/'+ ARCFIL+''
            self._EXTATTITUDE_filename = path_run_inputs + '/external_attitude/' +self.external_attitude
            
            
        self._ephem_filename      = EPHEMDIR+'/'+ self.ephem_file
        self._gdntable_filename   = '/data/data_geodyn/inputs/common/gdntable.data'
        self._ATGRAV_filename     = ATGRAVDIR +'/'+ ATGRAV_filename
        ##### TODO Add an if statement to choose correct ephemeris file.

        

        # #### Make the GEODYN output directories to be saved later

        self.make_directory_check_exist(self.OUTPUTDIR+'/orbits/')
        self.make_directory_check_exist(self.OUTPUTDIR+'/RESIDS/')
        self.make_directory_check_exist(self.OUTPUTDIR+'/PUNCH/')
        self.make_directory_check_exist(self.OUTPUTDIR+'/IIEOUT/')
        self.make_directory_check_exist(self.OUTPUTDIR+'/TELEM/')
        self.make_directory_check_exist(self.OUTPUTDIR+'/EMAT/')
        self.make_directory_check_exist(self.OUTPUTDIR+'/EMAT/scans/')
        self.make_directory_check_exist(self.OUTPUTDIR+'/IISSET/')
        self.make_directory_check_exist(self.OUTPUTDIR+'/DENSITY/')
        self.make_directory_check_exist(self.OUTPUTDIR+'/XYZ_TRAJ/')
        self.make_directory_check_exist(self.OUTPUTDIR+'/KEP_TRAJ/')
        
        self.TMPDIR_arc = TMPDIR+'/'+self.ARC
        os.system('rm -rf '+self.TMPDIR_arc)

        #### Remove old TMPRDIR version and remake it 
        os.system('rm -rf '+self.TMPDIR_arc)

        self.make_directory_check_exist(self.TMPDIR_arc) 
        os.system('chmod 777 '+self.TMPDIR_arc)

        print(self.run_ID,"    Density Model:     " ,self.DEN_DIR)
        print(self.run_ID,"    GEODYN Version:    " ,self.GDYN_version)
        print(self.run_ID,"    Estimate GenAccel: " ,self.ACCELS)
        print(self.run_ID,"    ARC run:           " ,self.ARC)
        print(self.run_ID,"    Output directory:  " ,self.OUTPUTDIR)
        print(self.run_ID,"    Call Options:      " ,self.options_in)

        if self.satellite == 'icesat2':
            print(self.run_ID,"    EXAT File:    " ,self._EXTATTITUDE_filename)

        if os.path.exists(self._INPUT_filename):
            self.verboseprint(self.tabtab,"FORT.5  (input) file:  ", self._INPUT_filename)
        else:
            print(self.run_ID,"    FORT.5  (input) file:  ", self._INPUT_filename," not found.")    

        if os.path.exists(self._G2B_filename):
            self.verboseprint(self.tabtab,"FORT.40 (g2b)   file:  ", self._G2B_filename)
        else:
            print(self.run_ID,"    FORT.40 (g2b)   file:  ", self._G2B_filename," not found.")    

    def prepare_tmpdir_for_geodyn_run(self):
        self.verboseprint(self.tabtab,'Current DIR: ',os.getcwd())
     
        #### Navigate TO the TMPDIR
        os.chdir(self.TMPDIR_arc)
        
#         #### Save the YMD and YR as files
#         YMD = self.ARC[2:8]
#         yr  = self.ARC[2:4] 

#         f = open(self.TMPDIR_arc+'/'+"YMD", "w")
#         f.write(YMD)
#         f.close()

#         f = open(self.TMPDIR_arc+'/'+"YR", "w")
#         f.write(yr)
#         f.close()

        
        ####-------------------------------------------------------------
        ####     Construct Common Setup of a GEODYN RUN
        ####-------------------------------------------------------------
        self.verboseprint('-------------------------------------------------')
        self.verboseprint('       Linking files with the command line       ')
        self.verboseprint('-------------------------------------------------')
        
        self.verboseprint(self.tabtab,'Current DIR',os.getcwd())

        if self.satellite == 'icesat2':
            #### make symlink to the G2B file and save as ftn40
            if not os.path.exists(self.TMPDIR_arc +'/EXAT01'):
                shutil.copyfile(self._EXTATTITUDE_filename, self.TMPDIR_arc +'/EXAT01')
#                 os.symlink(self._EXTATTITUDE_filename, self.TMPDIR_arc +'/EXAT01')
#                 self.verboseprint(self.tabtab,'EXAT01:',self._EXTATTITUDE_filename)
                self.verboseprint(self.tabtab,'copied:   exat file  > EXAT01')
            else:
                self.verboseprint(self.tabtab,'symlink is set up: EXAT01 file')

        
        #### make symlink to the G2B file and save as ftn40
        if not os.path.exists(self.TMPDIR_arc +'/ftn40'):
#             os.symlink(self._G2B_filename, self.TMPDIR_arc +'/ftn40')
            shutil.copyfile(self._G2B_filename, self.TMPDIR_arc +'/ftn40')
            self.verboseprint(self.tabtab,'copied:   g2b file   > ftn40')
        else:
            self.verboseprint(self.tabtab,'symlink:  g2b file')

        #### make symlink to the gravity field and save as ftn12
        if not os.path.exists(self.TMPDIR_arc +'/ftn12'):
            shutil.copyfile(self._grav_field_filename, self.TMPDIR_arc +'/ftn12')
            self.verboseprint(self.tabtab,'gravfield:',self._grav_field_filename)
            self.verboseprint(self.tabtab,'copied:   grav field > ftn12')
        else:
            self.verboseprint(self.tabtab,'symlink is set up: grav_field file')

        #### make symlink to the ephemerides and save as ftn01
        if not os.path.exists(self.TMPDIR_arc +'/ftn01'):
#             os.symlink(self._ephem_filename, self.TMPDIR_arc +'/ftn01')
            shutil.copyfile(self._ephem_filename, self.TMPDIR_arc +'/ftn01')
            self.verboseprint(self.tabtab,'copied:   ephem file > ftn01')
        else:
            self.verboseprint(self.tabtab,'symlink is set up: ephem file')

        #### make symlink to the gdntable and save as ftn02
        if not os.path.exists(self.TMPDIR_arc +'/ftn02'):
            shutil.copyfile(self._gdntable_filename, self.TMPDIR_arc +'/ftn02')
            self.verboseprint(self.tabtab,'copied:   gdntable   > ftn02')
        else:
            self.verboseprint(self.tabtab,'symlink is set up: gdntable file')


        #### make symlink to the ATGRAVFIL and save as fort.18
        if not os.path.exists(self.TMPDIR_arc +'/fort.18'):
#             os.symlink(self._ATGRAV_filename, self.TMPDIR_arc +'/fort.18')
            shutil.copyfile(self._ATGRAV_filename, self.TMPDIR_arc +'/fort.18')
            shutil.copyfile(self._ATGRAV_filename, self.TMPDIR_arc +'/ftn18')
            self.verboseprint(self.tabtab,'ATGRAV:',self._ATGRAV_filename)
            self.verboseprint(self.tabtab,'copied:   atgrav     > fort.18')
        else:
            self.verboseprint(self.tabtab,'symlink is set up: atgrav file')

        if self.satellite =='starlette':
            if not os.path.exists(self.TMPDIR_arc+'/ftn05.bz2'):
                os.system('cp '+self._INPUT_filename+' '+self.TMPDIR_arc+'/ftn05.bz2')
                self.verboseprint(self.tabtab,'copying          : iieout file')
            else:
                self.verboseprint(self.tabtab,'copied           : iieout file')

            if not os.path.exists(self.TMPDIR_arc+'/ftn05'):
                os.system('bunzip2 '+self.TMPDIR_arc+'/ftn05.bz2')
                self.verboseprint(self.tabtab,'file not zipped  : iieout file')
            else:
                self.verboseprint(self.tabtab,'file not zipped  : iieout file')
        else:
            if not os.path.exists(self.TMPDIR_arc+'/ftn05'):
                os.system('cp '+self._INPUT_filename+' '+self.TMPDIR_arc+'/ftn05')
                self.verboseprint(self.tabtab,'copying          : iieout file')
            else:
                self.verboseprint(self.tabtab,'copied           : iieout file')

        if not os.path.exists(self.TMPDIR_arc+'/giis.input'):
            os.system('cp  '+self.TMPDIR_arc+'/ftn05 '+self.TMPDIR_arc+'/giis.input')
            self.verboseprint(self.tabtab,'copying          : giis.input file')
        else:
            self.verboseprint(self.tabtab,'copied              : giis.input file')   

        
        self.verboseprint('-------------------------------------------------------------------------')
        self.verboseprint('-------------------------------------------------------------------------')

            
            
    def run_geodyn_in_tmpdir(self):
        self.verboseprint(self.tabtab,'Current DIR',os.getcwd())    
        ####-------------------------------------
        ####     RUN GEODYN IIS
        ####-------------------------------------

        ### Must change directory to run the IIS executable
        os.chdir(self.TMPDIR_arc)
        time.sleep(1)

        #### Before running GEODYN, populate the geodyn_options.txt file with the run options:
        self.geodyn_modify_inputs(self.options_in, self.DEN_DIR)
        
        
        #### RUN THE EXECUTABLE
        print()
        print(self.run_ID,"         Running IIS" )

        command_IIS = self.G2SDIR+'/giis2002_gfortran > '+'iisout 2> '+'iiserr'
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
#         print(self.run_ID,"         End of  IIS" )
        
        
        print()
        print(self.run_ID,"         Running IIE" )

        now = datetime.now()
        current_time = now.strftime("%H:%M:%S")
        print(self.run_ID,"         Current Time =", current_time)

        
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
        subprocess.run(command_IIE, shell = True)
        time.sleep(0.5)
        
        ### Check if there are any errors from IIEE output
        _iieerr_filename = self.TMPDIR_arc+'/iieerr'
            # check if size of file is 0
        if os.stat(_iieerr_filename).st_size == 0:
            print(self.run_ID,"         No errors in IIE" )
            print(self.run_ID,"---------End of IIE" )
        else:
            print('ERRORS FOUND IN IIE:', _iiserr_filename)
            with open(_iieerr_filename, 'r') as read_iieerrors:
                # Read & print the entire file
                print(read_iieerrors.read())

        
        end = time.time()
        elapsed = end - start
        print(self.run_ID,'         Time of IIE: ',elapsed,'secs','(',str(float(elapsed)/60),' mins)')

        now = datetime.now()
        current_time = now.strftime("%H:%M:%S")
        print(self.run_ID,"         Current Time =", current_time)


    def post_geodynrun_savefiles_and_cleanup(self):
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

        #### Combine the iisout, iiserr and the new iieout files...
        os.system('cat iisout iiserr iieout iieerr > '+'IIEOUT.'+self.ARC+'.'+self.GRAVITY)

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
        ## These are just some common options i have seen
        output_files = {'fort.71': 'emat',
                        'ftn97'  : 'telem',
#                         'ftn08'  : '/xyzout',
#                         'ftn10': 'a/eiout' ,
                        'fort.8': 'ascii_xyz',
                        'fort.10': 'ascii_kep',
                        'fort.30': 'orbfil',
                        'fort.31': 'orbfil2',
                        'fort.99': 'densityfil',
                        'fort.98': 'msis_in_file',
                        'fort.101':'msis_out_file' ,
                        'fort.103':'msis_SWI_file' ,
                        }
        for i,val in enumerate(output_files):
            if not os.path.exists(val):
                print('File is not in',self.run_ID,':',val,'--', output_files[val] )
            else:
                pass

        os.system('mv blob sumry')
        os.system('mv fort.71 emat')
        os.system('mv ftn97 telem')
        os.system('mv fort.8 ascii_xyz')
        os.system('mv fort.10 ascii_kep')
        os.system('mv fort.30 orbfil')
        os.system('mv fort.31 orbfil2')
        os.system('mv fort.99 densityfil')
        os.system('mv fort.98 msis_in_file')
        os.system('mv fort.101 msis_out_file')
        os.system('mv fort.103 msis_SWI_file')
        os.system('rm -f slvtmp* ftn* fort.*')

        # $EMATUDIR/ematu <<EOF 2>err >output
        # emat
        # 2
        # EOF
        # cat err output > blob
        # mv blob output.scan

        print(self.run_ID,'               Finished renaming files')      
        

        ####-----------------------------------------
        ####     Save files to their directories:
        ####-----------------------------------------

        #### First remove all files currently stored in the output directory
        os.system('rm -f  '+self.OUTPUTDIR+'/sumry/'+ self.ARC+'.'+ self.GRAVITY+'')
        os.system('rm -f  '+self.OUTPUTDIR+'/PUNCH/'+ self.ARC+'.'+ self.GRAVITY+'')
        # os.system('rm -f  '+OUTPUTDIR+'/EMAT/${ARC}.'+ GRAVITY+'')

        os.system('rm -f '+self.OUTPUTDIR+'/orbits/'+ self.ARC+'.'+ self.GRAVITY+'')
        os.system('rm -f '+self.OUTPUTDIR+'/orbits/'+ self.ARC+'.'+ self.GRAVITY+'.Z')
        os.system('rm -f '+self.OUTPUTDIR+'/orbits/'+ self.ARC+'.'+ self.GRAVITY+'.gz')

        os.system('rm -f  '+self.OUTPUTDIR+'/RESIDS/'+ self.ARC+'.'+ self.GRAVITY+'')
        os.system('rm -f  '+self.OUTPUTDIR+'/RESIDS/'+ self.ARC+'.'+ self.GRAVITY+'.Z')
        os.system('rm -f  '+self.OUTPUTDIR+'/RESIDS/'+ self.ARC+'.'+ self.GRAVITY+'.gz')

        os.system('rm -f  '+self.OUTPUTDIR+'/IIEOUT/'+ self.ARC+'.'+ self.GRAVITY+'')
        os.system('rm -f  '+self.OUTPUTDIR+'/IIEOUT/'+ self.ARC+'.'+ self.GRAVITY+'.Z')
        os.system('rm -f  '+self.OUTPUTDIR+'/IIEOUT/'+ self.ARC+'.'+ self.GRAVITY+'.bz2')

        # os.system('rm -f  '+OUTPUTDIR+'/EMAT/scans/scan.'+ ARC+'.'+GRAVITY+'')
        # os.system('rm -f  '+OUTPUTDIR+'/EMAT/scans/scan.'+ ARC+'.'+GRAVITY+'.Z')
        # os.system('rm -f  '+OUTPUTDIR+'/EMAT/scans/scan.'+ ARC+'.'+GRAVITY+'.bz2')

        os.system('rm -f '+self.OUTPUTDIR+'/DENSITY/'+ self.ARC+'.'+self.GRAVITY+'')
        os.system('rm -f '+self.OUTPUTDIR+'/DENSITY/'+ self.ARC+'.'+self.GRAVITY+'.Z')
        os.system('rm -f '+self.OUTPUTDIR+'/DENSITY/'+ self.ARC+'.'+self.GRAVITY+'.gz')

        os.system('rm -f '+self.OUTPUTDIR+'/XYZ_TRAJ/'+ self.ARC+'.'+self.GRAVITY+'')
        os.system('rm -f '+self.OUTPUTDIR+'/XYZ_TRAJ/'+ self.ARC+'.'+self.GRAVITY+'.Z')
        os.system('rm -f '+self.OUTPUTDIR+'/XYZ_TRAJ/'+ self.ARC+'.'+self.GRAVITY+'.gz')

        os.system('rm -f '+self.OUTPUTDIR+'/KEP_TRAJ/'+ self.ARC+'.'+self.GRAVITY+'')
        os.system('rm -f '+self.OUTPUTDIR+'/KEP_TRAJ/'+ self.ARC+'.'+self.GRAVITY+'.Z')
        os.system('rm -f '+self.OUTPUTDIR+'/KEP_TRAJ/'+ self.ARC+'.'+self.GRAVITY+'.gz')



        os.system('cp giis.input  '+self.OUTPUTDIR+'/IISSET/'+ self.ARC+'.'+self.GRAVITY+'')
        # cp output.scan $OUTPUTDIR/EMAT/scans/scan.${ARC}.${GRAVITY}
        # cp output.scan $OUTPUTDIR/EMAT/scans/scan.${ARC}.${GRAVITY}
        # cp emat  $OUTPUTDIR/EMAT/${ARC}.${GRAVITY}
        os.system('cp sumry '+self.OUTPUTDIR+'/sumry/'+ self.ARC+'.'+self.GRAVITY +'')
        os.system('cp Resid '+self.OUTPUTDIR+'/RESIDS/'+ self.ARC+'.'+self.GRAVITY+'')
        os.system('cp punch '+self.OUTPUTDIR+'/PUNCH/'+ self.ARC+'.'+self.GRAVITY+'')
        os.system('cp punch.gdn   '+self.OUTPUTDIR+'/PUNCH/'+ self.ARC+'.'+self.GRAVITY+'.gdn')
        os.system('mv IIEOUT.'+ self.ARC+'.'+self.GRAVITY+' '+self.OUTPUTDIR+'/IIEOUT/'+ self.ARC+'.'+self.GRAVITY+'')

        os.system('cp orbfil '+self.OUTPUTDIR+'/orbits/'+self.ARC+'.'+self.GRAVITY+'_orb1')
        os.system('cp orbfil2 '+self.OUTPUTDIR+'/orbits/'+self.ARC+'.'+self.GRAVITY+'_orb2')

        os.system('mv telem    '+self.OUTPUTDIR+'/TELEM/'+self.ARC+'.'+self.GRAVITY+'')
        os.system('cp densityfil  '+self.OUTPUTDIR+'/DENSITY/'+self.ARC+'.'+self.GRAVITY+'')
        os.system('cp msis_in_file '+self.OUTPUTDIR+'/DENSITY/'+self.ARC+'.'+self.GRAVITY+'_msisin')
        os.system('cp msis_out_file '+self.OUTPUTDIR+'/DENSITY/'+self.ARC+'.'+self.GRAVITY+'_msisout')
        os.system('cp msis_SWI_file '+self.OUTPUTDIR+'/DENSITY/'+self.ARC+'.'+self.GRAVITY+'_msisSWI')
        os.system('cp ascii_xyz '+self.OUTPUTDIR+'/XYZ_TRAJ/'+self.ARC+'.'+self.GRAVITY+'')
        os.system('cp ascii_xyz '+self.OUTPUTDIR+'/KEP_TRAJ/'+self.ARC+'.'+self.GRAVITY+'')

        print(self.run_ID,"               Finished copying files to outputdir")

        #### Go up a level and delete the temporary directory:
        os.chdir('../')
        os.system('rm -rf '+self.TMPDIR_arc)


        
    def RUN_GEODYN(self):
                
        self.setup_directories_and_geodyn_input()
        self.prepare_tmpdir_for_geodyn_run()
        self.run_geodyn_in_tmpdir()
        self.post_geodynrun_savefiles_and_cleanup()
        
        
        
        
        
        



        