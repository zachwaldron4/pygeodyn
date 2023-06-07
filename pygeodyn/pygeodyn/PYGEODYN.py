"""Top-level function for using the pygeodyn wrapper.

Todo:
    * make utiltools more specialized and specific for each case

"""

### The pygeodyn local dir must be in python path env for the imports to work
## level 0 - paths
from pygeodyn.config_paths import path_project
from pygeodyn.config_paths import path_pygeodyn
from pygeodyn.config_paths import path_io_geodyn
from pygeodyn.config_paths import path_data_inputs
from pygeodyn.config_paths import path_data_rawinputs
from pygeodyn.config_paths import path_data_outputs_raw
from pygeodyn.config_paths import path_tmp
from pygeodyn.config_paths import path_IIS_exec
from pygeodyn.config_paths import path_IIE_exec
from pygeodyn.config_paths import path_utilpce
from pygeodyn.config_paths import path_kamodo_src
## level 1
from pygeodyn.util_classtools   import Util_Tools
from pygeodyn.prep_inputs import PrepareInputs

## level 2
from pygeodyn.satellite_icesat2 import ICESat2
from pygeodyn.satellite_spire   import Spire_Lemur2_v33

import os
import yaml
from yaml.loader import SafeLoader
import gc
from datetime import datetime
import pandas as pd
import sys
from os.path import exists


# class InheritIcesat2(ICESat2):
#     def __init__(self):
#         ICESat2.__init__(self)
#         pass
    
# class Inherit_Starlette(Satellite_Starlette):
#     def __init__(self):
#         Satellite_Starlette.__init__(self)
#         pass
    
# class Inherit_Spire(Satellite_Spire):
#     def __init__(self):
#         Satellite_Spire.__init__(self)
#         pass




class SatelliteHandler(ICESat2):
    def __init__(self, whichSatellite):
        if whichSatellite == 'icesat2':
            ICESat2.__init__(self)
            # print("check sat ICESat2")

        elif    whichSatellite == 'spire083' \
             or whichSatellite == 'spire084' \
             or whichSatellite == 'spire085':
            pass
        #     Spire_Lemur2_v33.__init__(self)
        #     print("check sat Spire")
        else:
            raise ValueError(f'ERROR, {whichSatellite} is not a valid satellite option')
        
# class SatelliteHandler( Spire_Lemur2_v33):
#     def __init__(self, whichSatellite):
#         if whichSatellite == 'icesat2':
#             pass
#             # ICESat2.__init__(self)
#             # print("check sat ICESat2")

#         elif    whichSatellite == 'spire083' \
#              or whichSatellite == 'spire084' \
#              or whichSatellite == 'spire085':
#             Spire_Lemur2_v33.__init__(self)
#             print("check sat Spire")
#         else:
#             raise ValueError(f'ERROR, {whichSatellite} is not a valid satellite option')








# class SatelliteHandler(ICESat2, Spire_Lemur2_v33):
#     def __init__(self, whichSatellite):
#         if whichSatellite is ICESat2 :
#             ICESat2.__init__(self)
#             print("check sat ICESat2")

#         elif    whichSatellite is Spire_Lemur2_v33 :#
#             #  or whichSatellite is 'spire084' \
#             #  or whichSatellite is 'spire085':
#             Spire_Lemur2_v33.__init__(self)
#             print("check sat Spire")
#         else:
#             raise ValueError(f'ERROR, {whichSatellite} is not a valid satellite option')



# class Inheriter():
#     def __init__(self, whichOne):
#         if      whichOne == 'icesat2':
#             return SatelliteHandler(ICESat2)
#         elif    whichOne == 'spire083' \
#              or whichOne == 'spire084' \
#              or whichOne == 'spire085':
#             return SatelliteHandler(Spire_Lemur2_v33)
#         else:
#             raise ValueError("Error when inheriting the satellite. "\
#                              +"Check Satellite name.")




class Pygeodyn(Util_Tools,PrepareInputs, SatelliteHandler):
    
    #  Initialize a Pygeodyn Object with the YAML file containing the run
    #  settings as an input.
    def __init__(self, run_settings_input, use_file=True):  
        #
        # Init the various paths to the object
        self.path_project          = str(path_project)
        self.path_pygeodyn         = str(path_pygeodyn)
        self.path_data_inputs      = str(path_data_inputs)
        self.path_data_inputs_raw  = str(path_data_rawinputs)
        self.path_data_outputs_raw = str(path_data_outputs_raw)
        self.path_tmp              = str(path_tmp)
        self.path_io_geodyn        = str(path_io_geodyn)
        self.dir_IIS               = str(path_IIS_exec)
        self.dir_IIE               = str(path_IIE_exec)
        self.path_utilpce          = str(path_utilpce)
        self.path_kamodo_src       = str(path_kamodo_src)
        # Set environment variables to be read in by GEODYN/fortran:       
        os.environ["PATH_IO_GEODYN"] = str(self.path_io_geodyn)
        os.environ["PATH_IIELOCAL"]  = str(self.dir_IIE)
        self.user                    = os.environ["USER"]
        # Set pce converter environ vars 
        os.environ["PATH_UTIL_PCE"] = str(self.path_utilpce)


        # Load the user inputs as a yaml file or string.
        if use_file:
            with open(run_settings_input) as f:
                self.prms = yaml.load(f, Loader=SafeLoader)
        else:
            self.prms = {}
            # input as dictionary
            # remove the auxiliary items in the input dict
            for key in run_settings_input.keys():
                self.prms[key] = run_settings_input[key]['input']

        if 'initial_conditions' not in self.prms.keys():
            self.prms['initial_conditions'] = None
        if 'orbfil_step'        not in self.prms.keys():
            self.prms['orbfil_step'] = 120 # Print to orbit file every 2 mins
        
        if 'which_ICfile' not in self.prms.keys():
            self.prms['which_ICfile'] = None
        
        
    


        self.prms['initialize'] = False
        # Store some commonly used settings into their own attributes
        self.arc_input = self.prms['arc']  # datetime.strftime(pd.to_datetime(self.prms['epoch_start'][0]),'%Y.%j') 
        # print("self.arc_input" , self.arc_input)
        #### Set some more permanant default options.
        # self.prms['verbose']          = False
        self.verbose                  = False
        self.prms['value_io_fortran'] = "None"
        self.prms['DRHODZ_update']    = True
        self.prms['recompile_on']     = False
        self.prms['empirical_accels'] = False  # use empirical accelerations?
        # Request Specific Raw Output Files?
        self.prms['save_drag_file']   =  True
        self.prms['save_accel_file']  =  True
        # Formatting.    
        self.tab    = '  '
        self.tabtab = '       '
        
        ### Prepare epoch and arc date info


        ###### CD Model Input Handling
        if self.prms['cd_model'] == 'DRIA': 
            self.cd_model_params = self.prms['cd_model_params']


        ### Populate the io_geodyn user input files with control options
        ### these allow FORTRAN to read in options without recompiling.
        self.set_density_model_setup_params( self.prms['den_model'] )     


        ### Inherit the satellite class. 
        # Inheriter.__init__(self, self.prms['satellite'])
        SatelliteHandler.__init__(self, self.prms['satellite'])


        # if self.prms['satellite'] == 'icesat2':
        #     InheritIcesat2.__init__(self)
        # elif self.prms['satellite'] == 'spire':
        #     Inherit_Spire.__init__(self)


    def initialize_timeperiod_stage1(self, startdate, enddate, 
                                           overwrite_exat, overwrite_g2b):
        """
        initialize_timeperiod _summary_

        _extended_summary_

        Args:
            startdate (_type_): _description_
            enddate (_type_): _description_
        """
        import pandas as pd
        from  datetime import datetime, timedelta


        self.prms['initialize'] = True


        ### 1) Make 0UT to 0UT, 24hour epochs
        ###    construct list of consecutive epochs from startdate to enddate
        ###    each epoch must have:
        ###        - format='%Y-%m-%d %H:%M:%S' and 
        ###        - be 24 hours with start and end at 00:00:00 UT
        dt_1day = pd.Series(pd.to_timedelta(24,'h'))
        startdate_dt = pd.to_datetime(startdate, format='%Y-%m-%d')
        enddate_dt   = pd.to_datetime(enddate,   format='%Y-%m-%d')
        starts_linspace_dt = pd.date_range(start=startdate_dt,
                                           end=enddate_dt,
                                           freq=str(1)+"D")
        ends_linspace_dt = pd.date_range(\
                                start=pd.to_datetime(startdate_dt+dt_1day)[0],
                                end=  pd.to_datetime(enddate_dt+dt_1day  )[0],
                                freq=str(1)+"D")

        startdate_list_str = [datetime.strftime(idate, '%Y-%m-%d %H:%M:%S') \
                                              for idate in starts_linspace_dt]
        enddate_list_str   = [datetime.strftime(idate, '%Y-%m-%d %H:%M:%S') \
                                              for idate in ends_linspace_dt]

        print(f"----------------------------------------------------------------------------")
        print(f"Initializing the time period from "\
              f"{startdate_list_str[0]} to {enddate_list_str[-1]}")
        print("     overwriting the epoch start and stop to match")
        print(f"----------------------------------------------------------------------------")
        print()
        
        ### Update the input date options 
        ###  (overwriting other date inputs that may otherwise be set)
        self.prms['arc'] = [datetime.strftime(idate, '%Y.%j') for idate in starts_linspace_dt]
        self.arc_input = self.prms['arc'] 
        self.prms['epoch_start'] = startdate_list_str
        self.prms['epoch_stop']  = enddate_list_str
        self.verbose=True

        ### Re-initialize the satellite.....
        SatelliteHandler.__init__(self, self.prms['satellite'])

        #-------------------------------------------------------------------
        print("Step 0: Make directory structure for satellite input data")
        # for iarc, arc in enumerate(self.arc_input):  # must be a list
        #     self.arcnumber = iarc
        #     ###
        #     self.verbose=True
        #     self.set_file_paths_for_multiple_arcs( arc , iarc)
        #     self.get_arc_values_and_dates(skip_ic=True, verbose = self.verbose)


        path_inputs = self.path_data_inputs+'/sat_'+self.prms['satellite']        
        self.make_directory_check_exist(path_inputs            ,verbose=True)
        self.make_directory_check_exist(path_inputs+'/setups',verbose=True)
        self.make_directory_check_exist(path_inputs+'/external_attitude',verbose=True)
        self.make_directory_check_exist(path_inputs+'/g2b'    ,verbose=True)


        # print(self.dir_output_raw)
        # print(self.prms['satellite'])

        #-------------------------------------------------------------------
        if self.prms['satellite'] =='icesat2':
            print("Do not update EXAT files for ICESat-2")
        else:
            print("Step 1: Construct daily external Attitude files")
        for iarc, arc in enumerate(self.arc_input):  # must be a list
            self.arcnumber = iarc
            ###
            if iarc==0: 
                self.verbose=True
            else:
                self.verbose=False
            self.set_file_paths_for_multiple_arcs( arc , iarc)
            self.get_arc_values_and_dates(skip_ic=True, verbose = self.verbose)

            ## Construct external attitude files for each arc, using the 0UT 24hr epochs
            ##   the epochs will provide +/- 1 hour buffer as required by GEODYN
            
            # 1. overwrite any existing EXAT files
            #    Needs: - location of raw attitude  
            #           - input date, quaternion, and Pos Vel reference frames

        
            ## Will need to make some way to work around the maneuver days
            if self.prms['satellite'] =='icesat2':
                pass
            else:
                self.prep_exat_check(self.raw_satinput,             \
                                 bool_overwrite=overwrite_exat, \
                                 verbose=self.verbose)



        print()
        print("Step 2: Construct/Find a PCE file")
        #-------------------------------------------------------------------
        ## Construct a PCE file for the time period OR be sure that any PCE 
        #  file already contains our daterange of interest.
            # - need raw data input location
            # - raw sat ephemeris input file (POD, OD. etc just needs to 
            #   be some kind of ephemeris in Cartesian-ECI-J2000 with UTC dates)
            # - Make a readme file for the G2B files 
            # - Make PCE data based on the raw sat ephemeris
            # - Print to the user the dates that are available. 
            # - print to user the name of G2B file that was created or should be
            #   used.


        # datestr1 = pd.to_datetime(self.prms['epoch_start'][0]).strftime('%Y%m%d')
        # datestr2 = pd.to_datetime(self.prms['epoch_stop'][-1]).strftime('%Y%m%d')
        # self.raw_satinput['daterange'] = f"{datestr1}_{datestr2}"
        # print("self.raw_satinput['daterange']",self.raw_satinput['daterange'])
        # ### Include paths to the raw_satinput datafiles/directories and assorted
        # ### data format/settings.
        #                                 ## must be converted to UTC and J2000
        # self.raw_satinput['ephem_path'] = self.path_data_inputs +'/'\
        #                             +'sat_spire83/g2b/'\
        #                     +f'Spire83_RawEphem_{datestr1}_{datestr2}.txt'
        # self.raw_satinput['ephem_days'] =  [pd.to_datetime(i).day \
        #                                 for i in self.prms['epoch_start']]
        self.prep_g2b_check(self.raw_satinput,\
                            bool_overwrite=overwrite_g2b,\
                            verbose = True)


        # print()
        # print("Step 3: Update Initial Conditions")
        # #-------------------------------------------------------------------
        # ## Update Initial Conditions: (requires running of GEODYN with ICs) 
        # ##     (note, this method truncates the first day... :( )
        #     # 1. for each find the earlist date that matches the data,
        #     # 2. run geodyn with earliest matching ICs until end of day
        #     #     - step and orbstep must be <= 30s 
        #     # 3. Grab the midnight values and make these save as
        #     #    intermediary IC values for the 0UT of next day.
        #     # 4. Run again with new IC values and 0UT start/end values
        #     #    first day in the list gets truncated here (sad)
        #     # 5. Save values as the primary initial conditions file.
            
        
        # self.init_updatedICs(verbose = True)


        # print()
        # print("Step 4: Setup files for each day.")

        # # for iarc, arc in enumerate(self.arc_input):  # must be a list
        # #     self.arcnumber = iarc

        # #     print()

        # #     # Arc Specific Preparations
        # #     # --------------------------
        # #     self.set_file_paths_for_multiple_arcs( arc , iarc)
        
        # #     # Make some default setup files for referencing 
        # #     #   (this will be overwritten at run time since options may need to
        # #     #   be changed)
        # #     # self.prep_iisset_write(verbose=self.verbose)

            
    def initialize_physics_model_run(self, hasdm_bool=False):
        """
        Use the orbit cloud method and Kamodo to prepare the data inputs for
        Physical models.
        """

        # for iarc, arc in enumerate(self.arc_input):  # must be a list
        #     self.arcnumber = iarc
            
        #     # Arc Specific Preparations
        #     # --------------------------
        #     self.set_file_paths_for_multiple_arcs( arc , iarc)
            
        #     # Make or overwrite the setup file 
        #     self.prep_iisset_write()
            
        #     # Locate the external attitude file
        #     self.prep_exat_check(self.raw_satinput)
            
        #     # Locate G2B data (In the Satellite __init__() for now. )
        #     self.prep_g2b_check(self.raw_satinput)


        #     ## Run the GEODYN code 
        #     self.call_control_sequence()



        # First, Construct directories for the model of interest 
        # --------------------------------------------------------
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

        #### Check to see if the CSV files have been created using msis2.
        #### Next if MSIS2 has not yet been run, run these arcs with MSIS2
        
        model_save = self.prms['den_model']
        for iarc, arc in enumerate(self.arc_input):
            self.arcnumber = iarc
            self.prms['den_model']='msis2'
            self.set_density_model_setup_params( 'msis2' )
            self.ctrlStage1_setup_path_pointers(skip_files = True)
            self.set_file_paths_for_multiple_arcs( arc , iarc) 

            if hasdm_bool:
                # HASDM_OrbitCloud_2018292.01.csv
                self.orbitcloud_csv_file = (self.prms['model_data_path']   \
                                                + '/HASDM_OrbitCloud_'     \
                                                +  self.arcdate_for_files  \
                                                + '.csv' )
            else:
                self.orbitcloud_csv_file =(self.prms['model_data_path'] \
                                       +f"/OrbitCloud_{self.prms['satellite']}_Step"\
                                       + str(int(self.prms['step']))          \
                                       +'_'+self.arcdate_for_files+'.csv')
            #### identify location of msis2 run
            self.msis2_file_path = self._density_filename+'_msisin'  

            
            #### Check if the MSIS2 density file exists
            file_exists = exists(self.msis2_file_path)
            # print('msis2_file_path, 1: ', self.msis2_file_path)
            if file_exists:
                print(f'|     MSIS2 Density file already exists. ')
                print(f'|          - {self.ARC}_msisin           ')
                # msis2_log_file =  OUTPUTDIR+'/pygeodyn_runlog_'+self.ARC+'.txt'

                #### Check the stepsize of the density file...
                with open(self._iieout_filename, 'r') as f:
                    for line_no, line in enumerate(f):
                        if 'STEP             ' in line:
                            check_stepsizeline = line
                            break
                offset = len(' STEP             1807001           ')
                check_stepsize = float(check_stepsizeline[offset-1 : offset+6])
                if self.prms['step'] == check_stepsize:
                    pass
                else:
                    print(f'|     The MSIS2 Density file has the wrong stepsize, running again.')
                    self.prep_iisset_write()
                    self.prep_exat_check(self.raw_satinput)
                    if self.prms['run_type']  !=  "OrbitPropagation":
                        self.prep_g2b_check(self.raw_satinput)
                    self.call_control_sequence()
            else:
                print(f'|     Running MSIS2 through GEODYN to construct a initialized orbit')
                self.prep_iisset_write()
                self.prep_exat_check(self.raw_satinput)
                if self.prms['run_type']  !=  "OrbitPropagation":
                    self.prep_g2b_check(self.raw_satinput)
                self.call_control_sequence()
            
                            

            ### Construct the orbit cloud CSV
            ### Use the msis2 run to identify the density file that 
            ###    will be used to index the satellite ephemeris within kamodo
            self.prms['den_model'] = model_save
            # self.set_density_model_setup_params(self.prms['den_model'])
            # self.ctrlStage1_setup_path_pointers(skip_files = True)
            self.set_file_paths_for_multiple_arcs( arc , iarc)            
            # series = self.prms['den_model'] + '_' + self.cd_model + self.directory_name_specifier
            # OUTPUTDIR   = self.prms['path_output_dir'] + '/'+self.prms['den_model']+'/'+self.series


            #### ONLY RERUN ORBITCLOUD if PATH NOT EXIST or if file is super small (i.e. empty)
            file_exists = exists(self.orbitcloud_csv_file)                           
            try:
                orbit_cloud_csv_size = os.path.getsize(self.orbitcloud_csv_file)
            except:
                orbit_cloud_csv_size = 1   

            if not file_exists or orbit_cloud_csv_size < 1000: # run if file has less than 1 kilobytes  
                if orbit_cloud_csv_size < 1000:
                    print(f"|     File is too small or empty: {self.orbitcloud_csv_file.split('/')[-1]}" )

                if hasdm_bool==False:
                    #### Construct the file to be appended to here
                    f = open(self.orbitcloud_csv_file, "w")
                    f.write("\n")
                    f.close()

                from time import perf_counter
                t0=perf_counter()

                print(f"|     Constructing the OrbitCloud file: \n|          -{self.orbitcloud_csv_file.split('/')[-1]}")
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
                print(f"|     Already have OrbitCloud file for this arc: \n|          -{self.orbitcloud_csv_file.split('/')[-1]}")
                print(f'|          - {self.ARC} ',)
        
        self.prms['den_model'] = model_save
        self.ctrlStage1_setup_path_pointers(skip_files = True)

        # self.set_density_model_setup_params(self.prms['den_model'])

        ### Once you have the orbitcloud csv, re-run GEODYN with the TIEGCM and orbit cloud csv
        #### RUN 2nd TIME WITH TIEGCM_oc (inputting the CSV orbitcloud) 
        self.set_density_model_setup_params( self.prms['den_model'] )
        for iarc, arc in enumerate(self.arc_input):
            self.arcnumber = iarc
            if self.prms['satellite'] == 'icesat2':

                print(f'|     Running GEODYN with orbit cloud')
                self.set_file_paths_for_multiple_arcs( arc , iarc)            
                
                if hasdm_bool:
                    # HASDM_OrbitCloud_2018292.01.csv
                    self.orbitcloud_csv_file = (self.prms['model_data_path']   \
                                                    + '/HASDM_OrbitCloud_'     \
                                                    +  self.arcdate_for_files  \
                                                    + '.csv' )
                else:
                    self.orbitcloud_csv_file =(self.prms['model_data_path'] \
                                    +f"/OrbitCloud_{self.prms['satellite']}_Step"\
                                    + str(int(self.prms['step']))          \
                                    +'_'+self.arcdate_for_files+'.csv')

                # self.orbitcloud_csv_file =(self.prms['model_data_path']+'/OrbitCloud_Step'+
                                # str(int(self.prms['step']))+'_'+self.arcdate_for_files+'.csv')
                # self.model_data_path = self.prms['model_data_path']
                
                #### Write the model path and orbitcloud filename to a file for GEODYN
                filemodels = open(self.path_io_geodyn+"/geodyn_modelpaths.txt","w+")
                filemodels.write(self.prms['model_data_path']+'\n')
                filemodels.write(self.orbitcloud_csv_file+  '\n')
                filemodels.close()
            else:
                print(f'| ********* Not using a valid satellite ********* ')


            self.set_file_paths_for_multiple_arcs( arc , iarc)                  
            self.prep_iisset_write()
            self.prep_exat_check(self.raw_satinput)
            if self.prms['run_type']  !=  "OrbitPropagation":
                self.prep_g2b_check(self.raw_satinput)
            self.call_control_sequence()
            gc.collect()

#==============================================================================

    def run_arcs(self):

        """ Command to Run All features

        This will be the new location for controlling all pygeodyn features.

        The choice to run different configurations should be passed in by user
        """
        
        ####  If we are using one of the models that require Kamodo, we will 
        ####    want to do a pre-run initialization to get the orbit output 
        ####    along the satellite using MSISe2
        if  self.prms['den_model'] == 'tiegcm_oc'  or \
            self.prms['den_model'] == 'ctipe_oc'   or \
            self.prms['den_model'] == 'gitm'       :
            
            self.initialize_physics_model_run()
        
        elif  self.prms['den_model'] == 'hasdm_oc':
        
            self.initialize_physics_model_run(hasdm_bool=True )

        else:
            for iarc, arc in enumerate(self.arc_input):  # must be a list
                self.arcnumber = iarc
                
                # Arc Specific Preparations
                # --------------------------
                self.set_file_paths_for_multiple_arcs( arc , iarc)
                
                # Make or overwrite the setup file 
                self.prep_iisset_write()
                
                # Locate the external attitude file
                self.prep_exat_check(self.raw_satinput)
                
                if self.prms['run_type']  !=  "OrbitPropagation":
                    # Locate G2B data (In the Satellite __init__() for now. )
                    self.prep_g2b_check(self.raw_satinput)


                ## Run the GEODYN code 
                self.call_control_sequence()

#==============================================================================









#==============================================================================
        





    def print_input(options=True, info=True):
        """
        DOCSTRING

        _extended_summary_
        """
        
        setup = {}

        setup['den_model'] = {}
        setup['den_model']['input']   = None
        if options:
            setup['den_model']['options'] = \
                                        ['msis86','msis00','msis2',
                                        'dtm87','dtm2020_o','dtm2020_r',
                                        'jaachia71','jb2008',
                                        'tiegcm_oc',
                                        'ctipe_oc',
                                        'gitm',
                                        'hasdm_oc']
        if info:
            setup['den_model']['info'] =  """User designated density model"""


        setup['satellite']            = {}
        setup['satellite']['input']   = None
        if options: setup['satellite']['options'] = ['icesat2', 'spire83',
                                                     'starlette', 'iss' ]
        if info: setup['satellite']['info']    =  """"""

        setup['cd_model']            = {}
        setup['cd_model']['input']   = None
        if options:
            setup['cd_model']['options'] = []
        if info:
            setup['cd_model']['info']    =  """"""




        setup['run_specifier']   = {}
        setup['run_specifier']['input']   = None
        # setup['run_specifier']['options'] = []
        # setup['run_specifier']['info']    =  """"""


        setup['run_type']        = {}
        setup['run_type']['input']   = None
        # setup['run_type']['options'] = []
        # setup['run_type']['info']    =  """"""

        setup['arc']             = {}
        setup['arc']['input']   = None
        # setup['arc']['options'] = []
        # setup['arc']['info']    =  """"""


        setup['geodyn_StepSize'] = {}
        setup['geodyn_StepSize']['input']   = None
        # setup['geodyn_StepSize']['options'] = []
        # setup['geodyn_StepSize']['info']    =  """"""


        setup['arc_length']      = {}      
        setup['arc_length']['input']   = None      
        # setup['arc_length']['options'] = []      
        # setup['arc_length']['info']    =  """"""


        setup['request_data']    = {}     
        setup['request_data']['input']   = None    
        # setup['request_data']['options'] = []    
        # setup['request_data']['info']    =  """"""    


# #     orbit_propagation (i.e., orbit propagation)
#      data_reduction (i.e. adjust initial conditions using bayesian least 
#      squares and tracking data)

#      GEODYN performs orbit propagation using Cowells method of numerical
#      integration, and performs data-reduction utilizing a Bayesian
#     least-squares batch estimation process to optimally estimate parameters by
#     minimizing the residuals between tracking data and orbit propagations. 


        print("""
                ** Not necessary but this could be made to autoprint the setting 
                options for copying.

                """)
        return(setup)

            
            
            


    # def run_sequence():
        

        # self.prep_inputs()



        # self.RUN_GEODYN()



















 
            
        
        """
## README Pygeodyn is a python-based wrapper for the GEODYN Orbit Determination
Software.  This notebook attempts to explain how to use the the program in a
jupyter notebook and detail some of pygeodyn's various parts and
functionalities.

Pygeodyn is split up into a few distinct parts:

**The Pygeodyn Program**
 -  `/data/geodyn_proj/pygeodyn/pygeodyn_develop/` - `Pygeodyn.py`
       - Serving as the code's entry point, this program is inherits the
         remaining codes (structured as python Classes) in this list into a
         module (actually its a python object) to be interacted with by the
         user.  This object also reads the .yaml settings file that the user
         inputs.
    - `PYGEODYN_Control.py`
        - This class is inherited into the `Pygeodyn` object and is used by the
          user to run GEODYN.  It contains all of the functions needed to input
          geodyn specific files, setup and copy files to a temporary directory,
          run the geodyn IIS and IIE executables from fortran in the temporary
          directory, and finally setup and save requested outputs to an output
          directory. 
    - `PYGEODYN_Read.py`
        - This class is inherited into the `Pygeodyn` object and is used by the
          user to interpret and view the GEODYN output.  The requested data is
          identified in the .yaml settings file and then interpreted and saved
          to pandas DataFrames accordingly. 
    - `/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_dir/`
        - The codes in the `util_dir` help with the general running and
          functioning of Pygeodyn.
        - `common_functions.py`
        - `util_classtools.py`
        - `EditSetupFile.py`
    - `/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_preprocessing/`
        - We have added some limited capabilities in processing the G2B files
          for GEODYN.  So far this has been used with success to construct G2B
          PCE data for the ICESat2 Satellite."
        -`PYGEODYN_Preprocess.py`

  - **Inclusion of different Satellites**
      - Different satellites and their various inputs/formats are included in
        Pygeodyn in two ways:
          1. The various datasets and input files are placed in an input
             directory (`/data/data_geodyn/inputs/icesat2`)
          2. `PYGEODYN_ICESat2.py`: A pygeodyn class in constructed to overwrite
             any functions in PYGEODYN_Control and PYGEODYN_Read that may need
             to be specialized for the satellite and its tracking datatype.


    
**The GEODYN source code**  
   - `/data/geodyn_proj/geodyn_code/`
    
   - **IIS**: `/data/geodyn_proj/geodyn_code/IIS` - unchanged from original
     version
   - **IIE**: /data/geodyn_proj/geodyn_code/IIE
       - `ORIG/`
           - The MSIS routine and its references have been renamed to MSIS86.
       - `Kamodo_pygeodyn_MODS/`
          - modified version of the iie subroutines that were used to add msis2,
            msis00, the interface to Kamodo's Physics models as well as the
            various modifications necessary to conduct satellite drag research.
       - `CD_model_proj/`
           - Contains the more up-to-date modifications from the
             `Kamodo_pygeodyn_MODS/` folder and additionally added JB2008,
             DTM2020, and a physics based CD calculation through the DRIA
             routine.

**Kamodo** (CCMC's Ionosphere-Thermosphere Model API)
 - `/data/geodyn_proj/interface_kamodo_geodyn`
 - Kamodo Flythrough:
     - `/data/geodyn_proj/interface_kamodo_geodyn/Kamodo/kamodo/flythrough`
     - The code that is used to call Kamodo is nested in `PygeodynControl`, but
       it calls the `ModelFlythrough()` function in `SatelliteFlythrough.py`.
     - The Kamodo API and its model outputs are interfaced to GEODYN using an
       OrbitCloud Cube interpolation.



"""