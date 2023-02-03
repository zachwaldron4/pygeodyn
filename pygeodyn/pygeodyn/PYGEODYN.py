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
from pygeodyn.config_paths import path_utilpce
## level 1
from pygeodyn.util_classtools   import Util_Tools
from pygeodyn.prep_inputs import PrepareInputs

## level 2
from pygeodyn.satellite_icesat2 import ICESat2
from pygeodyn.satellite_spire   import Spire

import os
import yaml
from yaml.loader import SafeLoader
import gc



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


class SatelliteHandler(ICESat2, Spire):
    def __init__(self, whichSatellite):
        if whichSatellite == 'icesat2':
            ICESat2.__init__(self)
            # print("check sat ICESat2")
        elif whichSatellite == 'spire83':
            Spire.__init__(self)
            # print("check sat spire83")
        else:
            print('ERROR, not a valid satellite option')
            pass    





class Pygeodyn(Util_Tools,PrepareInputs, SatelliteHandler):
    
    #  Initialize a Pygeodyn Object with the YAML file containing the run
    #  settings as an input.
    def __init__(self, run_settings_input, use_file=True):  
        #
        # Init the various paths to the object
        self.path_project          = str(path_project)
        self.path_pygeodyn         = str(path_pygeodyn)
        self.path_data_inputs      = str(path_data_inputs)
        self.path_data_outputs_raw = str(path_data_outputs_raw)
        self.path_tmp              = str(path_tmp)
        self.path_io_geodyn        = str(path_io_geodyn)
        self.dir_IIS               = str(path_IIS_exec)
        self.dir_IIE               = str(path_IIE_exec)
        self.path_utilpce          = str(path_utilpce)
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

        if 'initial_conditions' not in  self.prms.keys():
            self.prms['initial_conditions'] = None
            # print('Setting IC to None')
        
        # else:
            # self.prms['initial_conditions'] = run_settings_input['initial_conditions']['input']
        # Store some commonly used settings into their own attributes
        self.arc_input        = self.prms['arc']
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
        SatelliteHandler.__init__(self, self.prms['satellite'])


        # if self.prms['satellite'] == 'icesat2':
        #     InheritIcesat2.__init__(self)
        # elif self.prms['satellite'] == 'spire':
        #     Inherit_Spire.__init__(self)




#==============================================================================

    def run_arcs(self):

        """ Command to Run All features

        This will be the new location for controlling all pygeodyn features.

        The choice to run different configurations should be passed in by user
        """

        for iarc, arc in enumerate(self.arc_input):  # must be a list
            self.arcnumber = iarc
            
            # Arc Specific Preparations
            # --------------------------
            self.set_file_paths_for_multiple_arcs( arc , iarc)
            #
            # Make or overwrite the setup file 
            self.prep_iisset_write()
            # Locate the external attitude file
            self.prep_exat_check()
            #
            # Locate G2B data (In the Satellite __init__() for now. )
            self.prep_g2b_check()


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