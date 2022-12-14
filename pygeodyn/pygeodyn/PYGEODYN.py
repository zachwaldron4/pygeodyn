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
from pygeodyn.config_paths import path_data_outputs_raw
from pygeodyn.config_paths import path_tmp
from pygeodyn.config_paths import path_IIS_exec
from pygeodyn.config_paths import path_IIE_exec
## level 1
from pygeodyn.util_classtools   import Util_Tools
## level 2
from pygeodyn.satellite_icesat2 import ICESat2


# from subprocess import run as subprocess_run
import os



class InheritIcesat2(ICESat2):
    def __init__(self):
        ICESat2.__init__(self)
        pass
    
# class Inherit_Starlette(Satellite_Starlette):
#     def __init__(self):
#         Satellite_Starlette.__init__(self)
#         pass
    
# class Inherit_Spire(Satellite_Spire):
#     def __init__(self):
#         Satellite_Spire.__init__(self)
#         pass


# # class SatelliteHandler(Satellite_ICESat2,Satellite_Starlette,Satellite_Spire):
# #     def __init__(self, whichSatellite):
# #         

#         if whichSatellite == sat1:
#             Satellite_ICESat2.__init__(self)
#         elif whichSatellit == sat2:
            
# #         pass  
    





class Pygeodyn(Util_Tools, ICESat2): #Inherit_Icesat2): Inherit_Starlette 
    
    #  Initialize a Pygeodyn Object with the YAML file containing the run
    #  settings as an input.
    def __init__(self, run_settings_yaml, settings_as_file=True):  
        #### Save the various paths to the object
        self.path_project          = str(path_project)
        self.path_pygeodyn         = str(path_pygeodyn)
        self.path_data_inputs      = str(path_data_inputs)
        self.path_data_outputs_raw = str(path_data_outputs_raw)
        self.path_tmp              = str(path_tmp)
        self.path_io_geodyn        = str(path_io_geodyn)
        self.dir_IIS               = str(path_IIS_exec)
        self.dir_IIE               = str(path_IIE_exec)

        import yaml
        from yaml.loader import SafeLoader
        # Open the file and load the file
        if settings_as_file:
            with open(run_settings_yaml) as f:
                run_settings = yaml.load(f, Loader=SafeLoader)
        else:
            run_settings = yaml.load(run_settings_yaml, Loader=SafeLoader)
        self.params = run_settings
        self.user             = self.params['user']
        self.satellite        = self.params['satellite']
        self.den_model        = self.params['den_model']
        self.run_specifier    = self.params['run_specifier']
        self.verbose          = self.params['verbose']
        self.arc_input        = self.params['arc']
        self.geodyn_StepSize  = self.params['geodyn_StepSize']
        self.request_data     = self.params['request_data']      
        self.empirical_accels = self.params['empirical_accels']      
        self.ACCELS           = self.params['ACCELS']      
        self.arc_length       = self.params['arc_length']
        
        
        
        ##### SATELLITE SPECIFIC OPTIONS:
        # self.SATELLITE_dir     = self.params['satellite']
        self.satellite_id             = self.params['satellite_id']
        self.DATA_TYPE         = self.params['DATA_TYPE']
        self.DRHODZ_update     = self.params['DRHODZ_update']
        #### File Choices
        self.filename_g2b       = self.params['filename_g2b']
        self.filename_atmograv  = self.params['filename_atmograv']
        self.filename_ephem     = self.params['filename_ephem']
        self.filename_gravfield = self.params['filename_gravfield']
        self.StateVector_epochs_datafile = self.params['StateVector_epochs_datafile']
        
        
        
        self.PASS_INPUT_VALUE_TO_fortran = self.params['PASS_INPUT_VALUE_TO_fortran']
        self.recompile_on = self.params['recompile_on']


        ### User choose if certain files should be saved or not.
        self.save_drag_file  =  self.params['save_drag_file']
        self.save_accel_file =  self.params['save_accel_file']
        
        
        ###### RELEVANT CD MODEL INPUTS
        self.cd_model        = self.params['cd_model']
        if self.cd_model == 'DRIA': 
            self.cd_model_params = self.params['cd_model_params']

        self.scaling_factor        = self.params['scaling_factor']

        
        ### Populate the GEODYN user input files with control options
        self.set_density_model_setup_params( self.den_model )     

        ########## THE BLEOW SHOULD BE REMOVED/UPGRADED WITH AN IMPROVED METHOD FOR CONTROLLING NAMES OF RUNS
        #### The below interprets that no input has been given for special name
        if self.run_specifier == None:
            self.run_specifier = ''
        else:
            self.run_specifier = self.params['run_specifier']
       
 

        # Set Environment variables to be read in by GEODYN:       
        os.environ["PATH_IO_GEODYN"] = str(self.path_io_geodyn)
        os.environ["PATH_IIELOCAL"] = str(self.dir_IIE)

        
        
        
        
        
        #### Do some book-keeping:    
        self.tab = '  '
        self.tabtab = '       '
        
#         if "accels" in params.keys():
#             if params["accels"] == True:
#                 self.empirical_accels =  True  
#                 self.ACCELS = 'accelon'
#             else:
#                 self.empirical_accels =  False  
#                 self.ACCELS = 'acceloff'
#         else:
#             self.empirical_accels =  False  
#             self.ACCELS = 'acceloff'

            
            
        if self.satellite == 'icesat2':
            InheritIcesat2.__init__(self)

            
        # elif self.satellite == 'starlette':
        #     Inherit_Starlette.__init__(self)


            
            
        
#         ########## ACTUALLY IM NOT SO SURE WAHT THIS IS DOING TBH
#         ##### The following cleans up the run settings so that they are added to the final object that is returned to the user. 
#         append_names = []
#         for i,val in enumerate(self.run_settings):
#             append_names.append(val)
        
#         for i,val in enumerate(self.run_settings):
#             if val in append_names: 
# #                         ['run_params', 
# #                         'model_data_path',
# #                         'request_data',
# #                         'cards_to_remove',
# #                         'epoch_start',
# #                         'epoch_end', 
# #                         'file_string',
# #                         'cd_adjustment_boolean',
# #                         'total_hours_in_run',
# #                         'hours_between_cd_adj',
# #                        'path_to_output_directory',
# #                         'GEODYN_iie_MOD_version',
# #                         'arc',
# #                         'accels',
# #                         'empirical_accels',
# #                         'ACCELS',
# #                         'arc_length',   
# #                        ]:
#                 pass
#             else:
#                 if self.run_settings[val] == self.__dict__[val]:
#                     pass
#                 else:
#                     self.__dict__[val] = self.run_settings[val]
            
            
            
            
            
            
            
            
            
            
            
            
###### THE BELOW ARE ATTEMPTS TO BETTER IMPORT VARIOUS SATELLITES
            
            
#         super(self, self).__init__()
#         if self.satellite == 'icesat2':
#             print('icesat2')
#             class Inherit_Icesat2(Satellite_ICESat2):
#                 def __init__(self):
#                     print('inherit Satellite_ICESat2 ')
#                     Satellite_ICESat2.__init__(self)
            
#             Inherit_Icesat2()

            
#         elif self.satellite == 'starlette':
#             print('starlette')
            
#             class Inherit_Starlette(Satellite_Starlette):
#                 def __init__(self):
# #                     super(Satellite_Starlette, self).__init__()
#                     print('inherit Satellite_Starlette ')
#                     pass

#             Inherit_Starlette()
# #         super(self, self).__init__()
            
#         print(self.__dict__)
        
        
        """
## README
Pygeodyn is a python-based wrapper for the GEODYN Orbit Determination Software.  This notebook attempts to explain how to use the the program in a jupyter notebook and detail some of pygeodyn's various parts and functionalities.

Pygeodyn is split up into a few distinct parts:

**The Pygeodyn Program**
 -  `/data/geodyn_proj/pygeodyn/pygeodyn_develop/`
    - `Pygeodyn.py`
       - Serving as the code's entry point, this program is inherits the remaining codes (structured as python Classes) in this list into a module (actually its a python object) to be interacted with by the user.  This object also reads the .yaml settings file that the user inputs.
    - `PYGEODYN_Control.py`
        - This class is inherited into the `Pygeodyn` object and is used by the user to run GEODYN.  It contains all of the functions needed to input geodyn specific files, setup and copy files to a temporary directory, run the geodyn IIS and IIE executables from fortran in the temporary directory, and finally setup and save requested outputs to an output directory. 
    - `PYGEODYN_Read.py`
        - This class is inherited into the `Pygeodyn` object and is used by the user to interpret and view the GEODYN output.  The requested data is identified in the .yaml settings file and then interpreted and saved to pandas DataFrames accordingly. 
    - `/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_dir/`
        - The codes in the `util_dir` help with the general running and functioning of Pygeodyn.
        - `common_functions.py`
        - `util_classtools.py`
        - `EditSetupFile.py`
    - `/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_preprocessing/`
        - We have added some limited capabilities in processing the G2B files for GEODYN.  So far this has been used with success to construct G2B PCE data for the ICESat2 Satellite."
        -`PYGEODYN_Preprocess.py`

  - **Inclusion of different Satellites**
      - Different satellites and their various inputs/formats are included in Pygeodyn in two ways:
          1. The various datasets and input files are placed in an input directory (`/data/data_geodyn/inputs/icesat2`)
          2. `PYGEODYN_ICESat2.py`: A pygeodyn class in constructed to overwrite any functions in PYGEODYN_Control and PYGEODYN_Read that may need to be specialized for the satellite and its tracking datatype.


    
**The GEODYN source code**  
   - `/data/geodyn_proj/geodyn_code/`
    
   - **IIS**: `/data/geodyn_proj/geodyn_code/IIS`
     - unchanged from original version
   - **IIE**: /data/geodyn_proj/geodyn_code/IIE
       - `ORIG/`
           - The MSIS routine and its references have been renamed to MSIS86.
       - `Kamodo_pygeodyn_MODS/`
          - modified version of the iie subroutines that were used to add msis2, msis00, the interface to Kamodo's Physics models as well as the various modifications necessary to conduct satellite drag research.
       - `CD_model_proj/`
           - Contains the more up-to-date modifications from the `Kamodo_pygeodyn_MODS/` folder and additionally added JB2008, DTM2020, and a physics based CD calculation through the DRIA routine.

**Kamodo** (CCMC's Ionosphere-Thermosphere Model API)
 - `/data/geodyn_proj/interface_kamodo_geodyn`
 - Kamodo Flythrough:
     - `/data/geodyn_proj/interface_kamodo_geodyn/Kamodo/kamodo/flythrough`
     - The code that is used to call Kamodo is nested in `PygeodynControl`, but it calls the `ModelFlythrough()` function in `SatelliteFlythrough.py`.
     - The Kamodo API and its model outputs are interfaced to GEODYN using an OrbitCloud Cube interpolation.



"""