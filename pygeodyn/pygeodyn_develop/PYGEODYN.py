import sys
sys.path.insert(0, '/data/geodyn_proj/pygeodyn/pygeodyn_develop/')
sys.path.insert(0,'/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_preprocessing/')

from PYGEODYN_Starlette import Satellite_Starlette
from PYGEODYN_ICESat2   import  Satellite_ICESat2
from util_classtools import Util_Tools


class Inherit_Icesat2(Satellite_ICESat2):
    def __init__(self):
        Satellite_ICESat2.__init__(self)
        pass
        
class Inherit_Starlette(Satellite_Starlette):
    def __init__(self):
        Satellite_Starlette.__init__(self)
        pass

    
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
    
    
class Pygeodyn(Util_Tools, Inherit_Icesat2): #Inherit_Icesat2): Inherit_Starlette 
    
    ####  Initialize a Pygeodyn Object with the YAML file containing the run settings as an input.
    def __init__(self, run_settings_yaml):  
        
        
        import yaml
        from yaml.loader import SafeLoader
        # Open the file and load the file
        with open(run_settings_yaml) as f:
            run_settings = yaml.load(f, Loader=SafeLoader)
        self.run_settings = run_settings
        
        
        self.user              = self.run_settings['user']
        self.satellite         = self.run_settings['satellite']
        self.den_model         = self.run_settings['den_model']
        self.directory_name_specifier   = self.run_settings['directory_name_specifier']
        self.verbose           = self.run_settings['verbose']
        self.arc_input         = self.run_settings['arc']
        self.geodyn_StepSize   = self.run_settings['geodyn_StepSize']
        self.request_data      = self.run_settings['request_data']      
        self.empirical_accels  = self.run_settings['empirical_accels']      
        self.ACCELS            = self.run_settings['ACCELS']      
        self.arc_length        = self.run_settings['arc_length']
        
        
        
        ##### SATELLITE SPECIFIC OPTIONS:
        self.SATELLITE_dir     = self.run_settings['satellite']
        self.SATID             = self.run_settings['SATID']
        self.DATA_TYPE         = self.run_settings['DATA_TYPE']
        self.DRHODZ_update     = self.run_settings['DRHODZ_update']
        #### File Choices
        self.g2b_file          = self.run_settings['g2b_file']
        self.atgrav_file       = self.run_settings['atgrav_file']
        self.ephem_file        = self.run_settings['ephem_file']
        self.gravfield_file    = self.run_settings['gravfield_file']
        self.StateVector_epochs_datafile = self.run_settings['StateVector_epochs_datafile']
        
        
        
        self.PASS_INPUT_VALUE_TO_fortran = self.run_settings['PASS_INPUT_VALUE_TO_fortran']
        self.recompile_on = self.run_settings['recompile_on']

        
        
        ###### RELEVANT CD MODEL INPUTS
        self.cd_model        = self.run_settings['cd_model']
        if self.cd_model == 'DRIA': 
            self.cd_model_params = self.run_settings['cd_model_params']

        
        
        ### Populate the GEODYN user input files with control options
        self.set_density_model_setup_params( self.den_model )     

        ########## THE BLEOW SHOULD BE REMOVED/UPGRADED WITH AN IMPROVED METHOD FOR CONTROLLING NAMES OF RUNS
        #### The below interprets that no input has been given for special name
        if self.directory_name_specifier == None:
            self.directory_name_specifier = ''
        else:
            self.directory_name_specifier = self.run_settings['directory_name_specifier']
       
 
        #------ Point to the GEODYN executables
        self.GDYN_version =  self.run_settings['GEODYN_iie_MOD_version']  
        self.G2SDIR       =  '/data/geodyn_proj/geodyn_code' + '/IIS/ORIG'
        self.G2EDIR       =  '/data/geodyn_proj/geodyn_code' + '/IIE/' + self.GDYN_version


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
            Inherit_Icesat2.__init__(self)

            
        elif self.satellite == 'starlette':
            Inherit_Starlette.__init__(self)


            
            
        
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
        
        
        