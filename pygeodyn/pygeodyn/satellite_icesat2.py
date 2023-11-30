"""SHORT EXPLAIN THIS MODULE

_extended_summary_
"""

import pandas as pd
import numpy  as np
import os
import os.path
import shutil
import sys
from gc import collect as gc_collect




#### Import the Pygeodyn Modules
from pygeodyn.control import RunController
from pygeodyn.read    import ReadRawOutput
from pygeodyn.util_dir.time_systems     import mjds_to_ymdhms



class InheritControlStages(RunController):
    """Class that enable satellite to inherit classes"""
    def __init__(self):
        RunController.__init__(self)
class InheritReadRaw(ReadRawOutput):
    """Class that enable satellite to inherit classes"""
    def __init__(self):
        ReadRawOutput.__init__(self)



# class Satellite_ICESat2(PygeodynController, PygeodynReader ):
class ICESat2(RunController, ReadRawOutput):
    """Class with satellite specific config for running Pygeodyn with ICESat2.
    
    In the Pygeodyn Infrastructure it inherits the RunController and
    ReadRawOutput.  Class with satellite specific confiuguration for running Pygeodyn with
    ICESat2. The setup here is originally for PCE trajectory
    analysis.
    """

    def __init__(self):
        # Initialize the ICESat2 class to contain the methods from RunController
        InheritControlStages.__init__(self)
        InheritReadRaw.__init__(self)


        if self.prms['satellite'] != 'icesat2':
            raise ValueError(f'Error in satellite inheritance and initialization.' \
                             +'Wrong satellite methods may have been inherited.')
        
        print('Using the ICESat-2 Class')


        # Call the control path pointer to establish attributed paths
        self.ctrlStage1_setup_path_pointers(skip_files = True)

        #------------------------------
        # Universal ICESat2 Properties
        #------------------------------
        self.prms['sat_ID']                  = '1807001'   # cospar-id number
        self.prms['sat_area_cross_sec']      = 9.530       # estimate (m^2), overwritten by geodyn force model
        self.prms['sat_mass']                = 1514.0      # estimate (kg)
        self.prms['sat_geometry_panel_num']  = 14 
        #
        self.prms['coord_ref_system']        = 'j2000'     # j2000 is default
        self.prms['orbit_elements_form']     = 'cartesian' # (meters), XYZ
        self.prms['bool_exatfiles']          = True        # use EXTATT file
        self.prms['number_of_satellites']    = 1



        self.raw_satinput = {} 
        datestr1 = pd.to_datetime(self.prms['epoch_start'][0]).strftime('%Y%m%d')    
        datestr2 = pd.to_datetime(self.prms['epoch_start'][-1]).strftime('%Y%m%d') 
        self.raw_satinput['daterange'] = f"{datestr1}_{datestr2}"
        self.raw_satinput['att_path']   = "done"
        if self.prms['initialize']:
            self.raw_satinput['ephem_path'] = self.path_data_inputs +'/'\
                                        +f'sat_icesat2/g2b/'  \
                                        +f'ICESat2_RawEphem_{datestr1}_{datestr2}.txt'
        else:
            self.raw_satinput['ephem_path'] = self.path_data_inputs +'/'\
                                    +f'sat_icesat2/g2b/'  \
                                    +f'ICESat2_RawEphem_20181108_20181124.txt'

        self.raw_satinput['ephem_path_dir'] = self.path_data_inputs_raw \
                                                +'/data_ICESat2/traj_files_rvg/'


        #------------------------------------------------------------------
        # Use the Global Files for ICESat-2 Precise Science Orbit from 2018
        #------------------------------
        if self.prms['global_options']=='pso_2018':

            #### Atmospheric Gravity file name
            self.filename_atmograv  = 'ATGRAV.glo-3HR_20160101-PRESENT'\
                                                +'_9999_AOD1B_0006.0090'
            #### Planetary Ephemeris file name
            self.filename_ephem     = 'ephem1430.data_2025'
            #### Gravity field file name
            self.filename_gravfield = 'eigen-6c.gfc_20080101_do_200_fix.grv'
            ### Global Option Cards for PSO            
            self.file_globalcards = self.path_data_inputs +'/common_2018'\
                                        +'/icesat2_pso_global_cards.txt'

        else:
            print("Run Settings Error: User input bad option as global_options.")
            print("    bad input:           ",self.prms['global_options'])
            print("    available run types: ", '***ADD***')
            sys.exit(0)

        #---------------------------------------------------------------------
        ### Run-type Options
        list_run_types = ["DataReduction_PCE",
                          "OrbitPropagation"] 
        # Fill in the appropriate settings based on the run_type.
        if self.prms['run_type'] == "DataReduction_PCE":
            self.tracking_data_type = 'PCE'
            # self.prms['cards_to_remove'] = [ 'ACCEL9','ORBTVU', 'RESID', 'CON9PA']
            #### G2B file name
            # self.filename_g2b                 = 'g2b_pce_fullset_nomaneuver'  
            # self.filename_g2b                 = 'g2b_pce_fullset_nomaneuver'  
            if self.prms['initialize']:
                self.filename_g2b = f"pce_icesat2_pso_{self.raw_satinput['daterange']}" 
            else:
#                 self.filename_g2b = f"pce_icesat2_pso_20181108_20181124save" 
                
                if self.prms['which_g2bfile'] is not None:
                    self.filename_g2b = self.prms['which_g2bfile']

    #                 self.filename_g2b = f"pce_icesat2_pso_2018_10" 


            

            #### PCE Ascii textfile name
            if self.prms['which_ICfile'] is not None:
                self.file_statevector_ICs = self.prms['which_ICfile']
            else:
                self.file_statevector_ICs = self.dir_input+'/'\
                        +f"ICESat2_initialconditions_"\
                        +f"{self.raw_satinput['daterange']}_v1.txt"
            
            # self.file_statevector_ICs  = self.dir_input+'/PCE_ascii.txt'
            self.file_statevector_headers   = [
                                'Date',
                                'MJDSECs', 
                                'RSECS',      #(fractional secs)
                                'GPS offset', # (UTC - GPS offset (secs))
                                'X',
                                'Y',
                                'Z',
                                'X_dot',
                                'Y_dot',
                                'Z_dot',
                                'YYMMDDhhmmss']

        #
        elif self.prms['run_type']  == "OrbitPropagation":
                self.filename_g2b = False
                # self.file_statevector_ICs  = self.dir_input\
                #                                     +'/PCE_ascii.txt'
                #### PCE Ascii textfile name
                if self.prms['which_ICfile'] is not None:
                    self.file_statevector_ICs = self.prms['which_ICfile']
                else:
                    self.file_statevector_ICs = self.dir_input+'/'\
                            +f"ICESat2_initialconditions_"\
                            +f"{self.raw_satinput['daterange']}_v1.txt"

                self.file_statevector_headers   = [
                                'Date',
                                'MJDSECs', 
                                'RSECS',      #(fractional secs)
                                'GPS offset', # (UTC - GPS offset (secs))
                                'X',
                                'Y',
                                'Z',
                                'X_dot',
                                'Y_dot',
                                'Z_dot',
                                'YYMMDDhhmmss']

        else:
            print("Run Settings Error: User input bad option as run_type.")
            print("    bad input:           ",self.run_type)
            print("    available run types: ",list_run_types)
            sys.exit(0)


    #===========================================================================       

    def sat_geometry_panel_model(self):
        '''
            Calls the information used to construct the satellite panel model.

        The PanelModel takes the following format:
            PanelModel['Parameter (unit)']= [panel1, panel2, panel3, ...etc]

            If any panels are moveable they must be last in the list.


        '''
        # from tabulate import tabulate
        # fmt = 'pretty'
        # PanelModel = {}
        # sp = 10
        # param_headers1 = ['Panel #',
        #                 "1".center(sp,' '),
        #                 "2".center(sp,' '),
        #                 "3".center(sp,' '),
        #                 "4".center(sp,' '),
        #                 "5".center(sp,' '),
        #                 "6".center(sp,' '),
        #                 "7".center(sp,' ')]
        # table=[]
        # table.append( ["Normal Vector X"]+PanelModel['Normal Vector X'][:7])
        # table.append( ["Normal Vector Y"]+PanelModel['Normal Vector Y'][:7])
        # table.append( ["Normal Vector Z"]+PanelModel['Normal Vector Z'][:7])
        # table.append( [" "] + [" "]*len(param_headers1))
        # table.append( ["Area (m^2)"]+PanelModel["Area (m^2)"][:7])
        # table.append( ["Moves=1|Fixed=0"]+PanelModel["Moves=1|Fixed=0"][:7])
        # table.append( ["Specular"]+PanelModel["Specular"][:7])
        # table.append( ["Diffuse"]+PanelModel["Diffuse"][:7])
        # table.append( ["Emissivity"]+PanelModel["Emissivity"][:7])
        # table.append( ["RadiationFreq|Both=0"]+PanelModel["RadiationFreq|Both=0"][:7])
        # table.append( [" "] + [" "]*len(param_headers1))
        # table.append( ["Temperature A"]+PanelModel["Temperature A"][:7])
        # table.append( ["Temperature C"]+PanelModel["Temperature C"][:7])
        # table.append( ["Temperature rate D"]+PanelModel["Temperature rate D"][:7])
        # table.append( ["Temperature rate F"]+PanelModel["Temperature rate F"][:7])
        # table.append( ["Temperature rotate X"]+PanelModel["Temperature rotate X"][:7])
        # print(tabulate(table, headers=param_headers1, tablefmt=fmt))
        # param_headers2 = ['Panel #',
        #                 "8".center(sp,' '),
        #                 "9".center(sp,' '),
        #                 "10".center(sp,' '),
        #                 "11".center(sp,' '),
        #                 "12".center(sp,' '),
        #                 "13".center(sp,' '),
        #                 "14".center(sp,' ')]
        # table=[]
        # ind = 7
        # table.append( ["Normal Vector X"]+PanelModel['Normal Vector X'][ind:])
        # table.append( ["Normal Vector Y"]+PanelModel['Normal Vector Y'][ind:])
        # table.append( ["Normal Vector Z"]+PanelModel['Normal Vector Z'][ind:])
        # table.append( [" "] + [" "]*len(param_headers1))
        # table.append( ["Area (m^2)"]+PanelModel["Area (m^2)"][ind:])
        # table.append( ["Moves=1|Fixed=0"]+PanelModel["Moves=1|Fixed=0"][ind:])
        # table.append( ["Specular"]+PanelModel["Specular"][ind:])
        # table.append( ["Diffuse"]+PanelModel["Diffuse"][ind:])
        # table.append( ["Emissivity"]+PanelModel["Emissivity"][ind:])
        # table.append( ["RadiationFreq|Both=0"]+PanelModel["RadiationFreq|Both=0"][ind:])
        # table.append( [" "] + [" "]*len(param_headers1))
        # table.append( ["Temperature A"]+PanelModel["Temperature A"][ind:])
        # table.append( ["Temperature C"]+PanelModel["Temperature C"][ind:])
        # table.append( ["Temperature rate D"]+PanelModel["Temperature rate D"][ind:])
        # table.append( ["Temperature rate F"]+PanelModel["Temperature rate F"][ind:])
        # table.append( ["Temperature rotate X"]+PanelModel["Temperature rotate X"][ind:])
        # print(tabulate(table, headers=param_headers2, tablefmt=fmt))

        panelmodel_lookuptable='''
        +----------------------+------------+------------+------------+------------+------------+------------+------------+
        |       Panel #        |     1      |     2      |     3      |     4      |     5      |     6      |     7      |
        +----------------------+------------+------------+------------+------------+------------+------------+------------+
        |   Normal Vector X    |    1.0     |    0.0     |    0.0     |    0.0     |    0.0     |    -1.0    |    0.0     |
        |   Normal Vector Y    |    0.0     |    1.0     |    -1.0    |    0.0     |    0.0     |    0.0     |    1.0     |
        |   Normal Vector Z    |    0.0     |    0.0     |    0.0     |    1.0     |    -1.0    |    0.0     |    0.0     |
        |                      |            |            |            |            |            |            |            |
        |      Area (m^2)      |  3.414187  |  4.172895  |  4.172895  |  2.47935   |  2.47935   |   1.369    |   1.157    |
        |   Moves=1|Fixed=0    |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |
        |       Specular       |    0.0     |    0.0     |    0.5     |    0.4     |    0.0     |    0.0     |    0.0     |
        |       Diffuse        |    1.0     |    1.0     |    0.5     |    0.6     |    1.0     |    1.0     |    1.0     |
        |      Emissivity      |    0.83    |    0.83    |    0.78    |    0.6     |    0.83    |    0.83    |    0.83    |
        | RadiationFreq|Both=0 |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |
        |                      |            |            |            |            |            |            |            |
        |    Temperature A     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |
        |    Temperature C     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |
        |  Temperature rate D  |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |
        |  Temperature rate F  |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |
        | Temperature rotate X |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |
        +----------------------+------------+------------+------------+------------+------------+------------+------------+
        +----------------------+------------+------------+------------+------------+------------+------------+------------+
        |       Panel #        |     8      |     9      |     10     |     11     |     12     |     13     |     14     |
        +----------------------+------------+------------+------------+------------+------------+------------+------------+
        |   Normal Vector X    |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |
        |   Normal Vector Y    |    0.5     |    -0.5    |    -1.0    |    -0.5    |    0.5     |    0.0     |    0.0     |
        |   Normal Vector Z    |  0.866025  |  0.866025  |    0.0     | -0.866025  | -0.866025  |    -1.0    |    1.0     |
        |                      |            |            |            |            |            |            |            |
        |      Area (m^2)      |   1.157    |   1.157    |   1.157    |   1.157    |   1.157    |   19.214   |   19.214   |
        |   Moves=1|Fixed=0    |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    1.0     |    1.0     |
        |       Specular       |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.08    |   0.014    |
        |       Diffuse        |    1.0     |    1.0     |    1.0     |    1.0     |    1.0     |    0.02    |   0.056    |
        |      Emissivity      |    0.83    |    0.83    |    0.83    |    0.83    |    0.83    |    0.78    |    0.75    |
        | RadiationFreq|Both=0 |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |
        |                      |            |            |            |            |            |            |            |
        |    Temperature A     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |
        |    Temperature C     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |
        |  Temperature rate D  |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |
        |  Temperature rate F  |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |
        | Temperature rotate X |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |
        +----------------------+------------+------------+------------+------------+------------+------------+------------+
        '''

        PanelModel = {\
            'Area (m^2)'          : [3.414187, 4.172895, 4.172895,2.47935,
                                     2.47935, 1.369, 1.157, 1.157, 1.157, 
                                     1.157, 1.157, 1.157, 19.214, 19.214],
            'Moves=1 Fixed=0'     : [0.0,0.0,0.0,0.0,0.0,0.0,0.0,
                                     0.0,0.0,0.0,0.0,0.0,1.0,1.0],
            'Diffuse'            : [1.0, 1.0, 0.5, 0.6, 1.0, 1.0, 1.0, 
                                     1.0, 1.0, 1.0, 1.0, 1.0, 0.02, 0.056],
            'Emissivity'             : [0.83, 0.83, 0.78, 0.6 , 0.83, 0.83, 0.83,
                                     0.83, 0.83, 0.83, 0.83, 0.83, 0.78, 0.75],
            'Specular'         : [0.0, 0.0, 0.5, 0.4, 0.0,  0.0, 0.0, 
                                     0.0, 0.0, 0.0, 0.0, 0.0, 0.08, 0.014],
            'RadiationFreq Both=0': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                                     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
            'Normal Vector X'     : [1.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0,
                                     0.0, 0.0, 0.0, 0.0, 0.0,  0.0, 0.0],
            'Normal Vector Y'     : [0.0, 1.0, -1.0, 0.0, 0.0, 0.0, 1.0,
                                     0.5, -0.5, -1.0, -0.5, 0.5, 0.0, 0.0],
            'Normal Vector Z'     : [0.0,0.0,0.0,1.0,-1.0,0.0,0.0,0.866025,
                                     0.866025,0.0,-0.866025,-0.866025,-1.0,1.0],
            #                                     
            'Temperature A'       : [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                                     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
            'Temperature C'       : [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                                     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
            'Temperature rate D'  : [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                                     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
            'Temperature rate F'  : [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                                     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
            'Temperature rotate X': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                                     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]}


        return(PanelModel)
        # #### Read in lines from PANEL CARDS:
        # nrm_sbf = {} # plate normal vectors in spacecraft body-fixed coordinates
        # A       = {} # plate areas (m^2)
        # bwspec  = {} #
        # bwdiff  = {} #

        # emissivity={}
        # temp_A={}
        # temp_C={}
        # temp_decay_D={}
        # temp_decay_F={}
        # temp_rot_X={}
        # motion_ind = {}
        # freq_ind = {}
        # sat_id = {}


        # Au = 196.96655  # gold atomic mass
        # #SiO2 = 60.0843
        # SiO2 = 20.03    # silicon dioxide atomic mass
        # Al = 26.981538  # aluminum atomic mass
        # plt_M   = {} # plate materials

        # for iline, line in enumerate(panels_str):
        #     print(line)
        #     card        = 'PANEL'
        #     panel_num   = int(line[11-1 : 12])
        #     param_type  = line[13-1 : 14]
        #     #
        #     motion_ind[panel_num]  = line[9 -1]  # fixed = '0',   moves = '1'
        #     if motion_ind[panel_num]== ' ': motion_ind[panel_num]= '0'
        #     freq_ind[panel_num]    = line[10-1]  # both=0, short=1, long=2
        #     sat_id[panel_num]      = line[18-1 : 24]
        #     #
        #     ### NORMAL VECTORS
        #     if param_type   == str( 1 ).rjust(2,' '):
        #         nrm_sbf[panel_num] = {}
        #         nrm_sbf[panel_num][0] = line[25-1 : 44]
        #         nrm_sbf[panel_num][1] = line[45-1 : 59]
        #         nrm_sbf[panel_num][2] = line[60-1 : 72]
            
        #     #### AREA (m**2)
        #     elif param_type == str( 2 ).rjust(2,' '):
        #         A[panel_num] = line[25-1 : 44]
            
        #     #### Specular  reflectivity  
        #     elif param_type == str( 3 ).rjust(2,' '):
        #         bwspec[panel_num] = line[25-1 : 44]
            
        #     #### Diffuse  reflectivity  
        #     elif param_type == str( 4 ).rjust(2,' '):
        #         bwdiff[panel_num] = line[25-1 : 44]
                
        #     #### Emissivity  
        #     elif param_type == str( 5 ).rjust(2,' '):
        #         emissivity[panel_num] = line[25-1 : 44]

        #     #### 6   Temperature A (cold  equilibrium  temperature) (Kelvin)
        #     elif param_type == str( 6 ).rjust(2,' '):
        #         temp_A[panel_num] = line[25-1 : 44]
        #     #### 7   Temperature C (delta  temperature  between  hot and cold  
        #     #        equilibrium  temperature) (K)
        #     elif param_type == str( 7 ).rjust(2,' '):
        #         temp_C[panel_num] = line[25-1 : 44]
        #     #### 8   Temperature decay  time D  (exponential  decay  time  
        #     #         for  panel cooling) (Sec)
        #     elif param_type == str( 8 ).rjust(2,' '):
        #         temp_decay_D[panel_num] = line[25-1 : 44]
        #     #### 9   Temperature decay  time F  (exponential  decay  time  
        #     #         for  panel heating) (Sec)
        #     elif param_type == str( 9 ).rjust(2,' '):
        #         temp_decay_F[panel_num] = line[25-1 : 44]
        #     #### 10  Temperature/satellite  rotation X (divisor  for cos(theta)
        #     #          term inheating  equation)
        #     elif param_type == str( 10).rjust(2,' '):
        #         temp_rot_X[panel_num] = line[25-1 : 44]
                
        
        
        
#     for iparam,paramval in enumerate([1,2,3,4,5,6,7,8,9,10]):   # ]):
#         param = str(paramval).rjust(2,' ')






    def sat_process_raw_ephemeris(self, verbose=False):



        from datetime import datetime,timedelta
        # from pygeodyn.satellite_spire import read_SpireLeoOrbPOD_sp3c
        import time
        ## Load coordinate transformation functions
        # from pygeodyn.util_dir.time_systems import time_gps_to_utc,\
        #                                         get_leapseconds, jday           


        raw_ephem = self.raw_satinput['ephem_path']
        ephem_path_dir = self.raw_satinput['ephem_path_dir']
        #
        dt_2days = pd.Series(pd.to_timedelta(48,'h'))
        startdate = pd.to_datetime(self.prms['epoch_start'][0])
        enddate   = pd.to_datetime(self.prms['epoch_start'][-1])
        startdate_dt = pd.to_datetime(startdate, format='%Y-%m-%d')
        enddate_dt   = pd.to_datetime(enddate,   format='%Y-%m-%d')
        starts_linspace_dt = pd.date_range(start=startdate_dt ,
                                            end=enddate_dt   ,
                                            freq=str(1)+"D")

        if verbose: print(f"{self.tabtab} - ICESAT-2")
        if verbose: print(f"{self.tabtab} - processing raw satellite ephemerides from files.")
        if verbose: print(f"{self.tabtab} - for dates: {starts_linspace_dt}")


        ### PLACE THE RAW FILE PROCESSING CODE HERE
        ### ---------------------------------------
        # path_g2b = path_preprocessing
        rvg_params = {}
        rvg_params["record_length"] = 29        # words (2 Bytes per word)
        rvg_params["overlap"]       = 5.40027   # hours



        # ##### Add a check to see if the arcs have maneuver indicators (A or B)
        # arc_timesfile = '/data/SatDragModelValidation/data/inputs/raw_inputdata/data_ICESat2/arc_times.txt'
        
        # ### Find the right range of dates in this file.
        # arcs = pd.read_csv(arc_timesfile, 
        #             sep = ',',
        # #             dtype=object,
        #             names = [
        #                 'arc'         ,
        #                 'epoch_start' ,
        #                 'epoch_stop'  ,
        #                 'orbit_start' ,
        #                 'orbit_stop'  ,
        #                     ],)

        # ### Convert to list and remove whitespaces in each string
        # arcs_withmaneuvs = []
        # for iarc in self.prms['arc']:
        #     index = arcs['arc'].str.contains(iarc)
        #     arcs_withmaneuvs.append(arcs['arc'][index].values[0].strip())

        # # Update the arcs to reflect maneuvers
        # self.prms['arc'] = arcs_withmaneuvs



        # print("sat_process_raw_ephemeris")
        # print('ephem_path_dir' ,ephem_path_dir)
        # print("self.prms['arc']" ,self.prms['arc'])
        # print('rvg_params' ,rvg_params)


        RVG_FINAL = get_timechopped_rvgdata_1(ephem_path_dir, \
                                            self.prms['arc'],\
                                            rvg_params)
        ### ---------------------------------------



        start = time.time()

        ### Date must be in UTC
        ### Ephemerides must be in ECI-J2000

        if verbose: print(f"{self.tabtab}   saving satellite ephemeris to single file.")
        f = open(raw_ephem, "w")
        f.write("\n")
        f.close()




        ### Write to file
        with open(raw_ephem, 'r+') as file:

            #### Manually write the header units
            header_units =\
                    f"{'UTC'.rjust(19-1,' ') }"\
                    +f"  {'(m)'.rjust(15,' ')}"\
                    +f"  {'(m)'.rjust(15,' ')}"\
                    +f"  {'(m)'.rjust(15,' ')}"\
                    +f"  {'(m/s)'.rjust(15,' ')}"\
                    +f"  {'(m/s)'.rjust(15,' ')}"\
                    +f"  {'(m/s)'.rjust(15,' ')}"\

            #### Manually write the header field names
            header_names =\
                    f"{'Date'.rjust(19-1,' ') }"\
                    +f"  {'X'.rjust(15,' ')}"\
                    +f"  {'Y'.rjust(15,' ')}"\
                    +f"  {'Z'.rjust(15,' ')}"\
                    +f"  {'X_dot'.rjust(15,' ')}"\
                    +f"  {'Y_dot'.rjust(15,' ')}"\
                    +f"  {'Z_dot'.rjust(15,' ')}"\

            #### Manually write the detailed header description
            header_meta = \
            f'''### "Raw" Satellite Ephemeris
### -----------------------
###     Satellite: ICESat2_({self.prms['sat_ID']})
###     Epoch: +start____ {RVG_FINAL['Date_utc'].values[0]} 
###            +stop_____ {RVG_FINAL['Date_utc'].values[-1]}
###     Last modified: {datetime.now()-timedelta(hours=7)}
###
### Source
### -------
###     {self.raw_satinput['ephem_path_dir']}
###     ICESat-2 PSO, RVG binary files
###
### Contents
### --------
###     Date:  (YYYY-MM-DD hh:mm:ss) (UTC, converted from mjdsec-gps time)
###     Ephem:  Position and velocity (X, Y, Z, X_dot, Y_dot, Z_dot)
###             coordinate: ECI-J2000
###             unit: m
###
###
#{header_units}
#{header_names}
### %eoh
'''
            print(np.shape(RVG_FINAL['X_ECI_m'].values))

            file.write(header_meta)  
            for indx,valdate in enumerate(RVG_FINAL['Date_utc'].values):
            #### Manually write each row of the data.
                row =   f"{pd.to_datetime(RVG_FINAL['Date_utc'].values[indx]).strftime(format='%Y-%m-%d %H:%M:%S')}"\
                    +f"  {RVG_FINAL['X_ECI_m'].values[indx]:15.5f}"\
                    +f"  {RVG_FINAL['Y_ECI_m'].values[indx]:15.5f}"\
                    +f"  {RVG_FINAL['Z_ECI_m'].values[indx]:15.5f}"\
                    +f"  {RVG_FINAL['XDOT_ECI_m_s'].values[indx]:15.5f}"\
                    +f"  {RVG_FINAL['YDOT_ECI_m_s'].values[indx]:15.5f}"\
                    +f"  {RVG_FINAL['ZDOT_ECI_m_s'].values[indx]:15.5f}"\
                    +f"\n"
                file.write(row)
        #
        end = time.time()
        elapsed = end - start
        #
        print()
        # print(f'       indxes: 0 -',indx,'') 
        print(f'       Processed file in : ',np.round(elapsed,5),'secs', f"({np.round(elapsed,5)/60} minutes)") 



















#### ==========================================================================
####    AUXILIARY FUNCTIONS
#### ==========================================================================



import os





def get_timechopped_rvgdata_1(path_binary, arc_files, rvg_params):

    total_files=np.size(arc_files)
    tabtab = '     '

    print(tabtab,'Running through the pre-processing procedure...')
    print(tabtab,'=======================================================')
    print(tabtab,'STEP 1: Convert RVG binary files to pandas DataFrame...')
    print(tabtab,'=======================================================')
    
    time_estimate_onAWS = 0.0005 # ~ num of secs per record ( 56.11/ 109434)
    rate = (total_files*time_estimate_onAWS*109000)/60
    print(tabtab, f"Loading and processing {total_files} files will take approx. {rate:.2f} minutes.")
    print(tabtab,tabtab, 'Not including unzipping/zipping times')

    print()
#         print(tabtab,'First we unzip the files... check the console for progess.')
    os.chdir(path_binary)


    # Initialize a Dataframe 
    df1 = pd.DataFrame()

    count=0
    for i, file in enumerate(arc_files):

        filenum  =i+1 
        print(tabtab, '--- File %i / %i'% (filenum, total_files))

#         print(tabtab, '----- Unzipping file...', file)
        print(tabtab, '----- Loading ', file  )

#             os.system('gunzip -vr '+file+'.gz')
#             os.system('bunzip2 -v '+file+'.bz2')


        __rvg_filename =  path_binary + f"orbit.1807001.{file}"


        # Read in the binary data into a dict w/ pd.dataframes
        rvg_data = RVGfiles_read_rvg_binary_2(rvg_params, __rvg_filename)

        # Convert MJDSecs to pandas datetime
        rvg_data = RVG_Files_add_datetime_column_3(rvg_data)

        # Chop off the ends of the files to eliminate overlap
        # rvg_data_chopped = RVGfiles_chop_the_ends_4(rvg_params, rvg_data)

        if count == 0:
            df1 = rvg_data
            count += 1
        else:
            # to append df2 at the end of df1 dataframe
            df1 = pd.concat([df1, rvg_data])

        df1 = df1.drop_duplicates(subset=["Date_utc"], keep='first'\
        ).sort_values(by='Date_utc'\
                        ).reset_index(drop=True)

        del rvg_data
        gc_collect()

#         print(tabtab,'Zipping file...', file)
#             os.system('gzip -v '+file)

        print()

    RVG_FINAL = df1
    return(RVG_FINAL)

def RVGfiles_read_rvg_binary_2(rvg_params, __rvg_filename):
    '''
    This function converts the RVG trajectory data to a python friendly format.
    Output is a dict that contains the header, data, and sentinal records for a file.

    #------------INFO------------------------
    #
    1. These files are in what is called the **RVG format**. The RVG files are 
                    pretty simple to unpack (lol not)
    2. Each **record has 29 words**
    3. Each **word is a 64 bit floating point number**
    4. The first record is a *header record* with information about the file.

        ```
        #|   Header Record Format:
        #|   ---------------------
        #|   
        #|   WORD   | Type | Description
        #|   ----     ----   -----------
        #|   1         DP     Coord. Sys. Flag
        #|                        0 = TOD
        #|                        1 = TOR
        #|                        2 = J2000
        #|   2         DP     Traj start date MJDSEC GPS 
        #|   3         DP     Traj start frac sec 
        #|   4         DP     Traj start date (YYMMDDHHMMSS) UTC 
        #|   5         DP     Traj stop date MJDSEC GPS 
        #|   6         DP     Traj stop frac sec 
        #|   7         DP     Traj stop date (YYMMDDHHMMSS) UTC 
        #|   8         DP     Traj interval sec 
        #|   9         DP     GEODYN 2s version no. 
        #|   10        DP     GEODYN 2s run date 
        #|   11        DP     GEODYN 2s run time 
        #|   12        DP     GEODYN 2e version no.w 
        #|   13        DP     GEODYN 2e run date 
        #|   14        DP     GEODYN 2e run time 
        #|   15        DP     Speed of light 
        #|   16        DP     GM for Earth 
        #|   17        DP     Semi-major axis of Earth ref. ellipsoid 
        #|   18        DP     Equatorial Flattening of Earth ref. ellipsoid 
        #|   19        DP     Gravitational Potential Checksum 
        #|   20        DP     Maximum Degree of Gravitational Expansion 
        #|   21        DP     Maximum Order Of Gravitational Expansion 
        #|   22-29     DP       spares
        ```
    5.  The last record is a *sentinal record* to tell you that you have reached the end of the file. 
        ```
        #|   Sentinel Record Format:
        #|   ---------------------
        #|   
        #|   WORD | Type | Description
        #|   ----   ----   -----------
        #|   1       DP     999.0
        #|   2       DP     Satellite ID 
        #|   3       DP     GEODYN IIS Versions
        #|   4       DP     GEODYN IIE Versions 
        #|   5-29    DP     0.0 
        ```
      - The first word of that record has the value 999.0.  
             when you encounter a record whose first word has the value 999.0,  you have reached the end of the file.

    6. All the records in the file except the first and last records, are data records.
    ```
    #|   Data Record Format:
    #|   ---------------------
    #|   
    #|   WORD   | Type | Description
    #|   ----     ----   -----------
    #|   1         DP     MJDSEC (secs)  % time is in GPS 
    #|   2         DP     RSEC (fractional secs) 
    #|   3         DP     UTC - GPS offset (secs) 
    #|   4         DP     spare_4 
    #|   5         DP     X Inertial sat. S.Vec (m) 
    #|   6         DP     Y Inertial sat. S.Vec (m) 
    #|   7         DP     Z Inertial sat. S.Vec (m) 
    #|   8         DP     X_dot Inertial sat. S.Vec (m/sec) 
    #|   9         DP     Y_dot Inertial sat. S.Vec (m/sec) 
    #|   10        DP     Z_dot Inertial sat. S.Vec (m/sec) 
    #|   11        DP     Satellite latitude (degrees) 
    #|   12        DP     Satellite longitude (degrees) 
    #|   13        DP     Satellite height (m) 
    #|   14        DP     X-component ECF position (m) 
    #|   15        DP     Y-component ECF position (m) 
    #|   16        DP     Z-component ECF position (m) 
    #|   17        DP     X_dot-component ECF velocity (m/sec) 
    #|   18        DP     Y_dot-component ECF velocity (m/sec) 
    #|   19        DP     Z_dot-component ECF velocity (m/sec) 
    #|   20        DP     X component of polar motion (milliarcsec) 
    #|   21        DP     Y component of polar motion (milliarcsec) 
    #|   22        DP     beta angle (degrees) 
    #|   23        DP     yaw angle (degrees) 
    #|   24        DP     orbit angle (degrees) 
    #|   25        DP     Quaternion QI for J2000 to ITRF (ECF) 
    #|   26        DP     Quaternion 02 for J2000 to ITRF (ECF) 
    #|   27        DP     Quaternion 03 for J2000 to ITRF (ECF) 
    #|   28        DP     Quaternion 04 for J2000 to ITRF (ECF) 
    #|   29        DP     Greenwich HR angle 
    ```

    '''

    from scipy.io import FortranFile
    tabtab = '     '

    header_titles = [ 'coordinate_system',
                      'Traj_start_date_MJDSEC_GPS' ,
                      'Traj_start_frac_sec' ,
                      'Traj_start_date_YYMMDDHHMMSS_UTC' ,
                      'Traj_stop_date_MJDSEC_GPS' ,
                      'Traj_stop_frac_sec' ,
                      'Traj_stop_date_YYMMDDHHMMSS_UTC' ,
                      'Traj_interval_sec' ,
                      'GEODYN_2s_version_no' ,
                      'GEODYN_2s_run_date' ,
                      'GEODYN_2s_run_time' ,
                      'GEODYN_2e_version_no' ,
                      'GEODYN_2e_run_date',
                      'GEODYN_2e_run_time',
                      'Speed_of_light' ,
                      'GM_for_Earth' ,
                      'Semimajor_axis_of_Earth_ref_ellipsoid' ,
                      'Equatorial_Flattening_of_Earth_ref_ellipsoid' ,
                      'Gravitational_Potential_Checksum' ,
                      'Maximum_Degree_of_Gravitational_Expansion' ,
                      'Maximum_Order_Of_Gravitational_Expansion' ,
                      'spare_22' ,
                      'spare_23',
                      'spare_24',
                      'spare_25',
                      'spare_26',
                      'spare_27',
                      'spare_28',
                      'spare_29',
                      ]

    data_titles = [ 'MJDSEC_GPS' ,
                    'RSEC',
                    'GPS_offset_secs_utc' ,
                    'spare_4' ,
                    'X_ECI_m' ,
                    'Y_ECI_m' ,
                    'Z_ECI_m' ,
                    'XDOT_ECI_m_s' ,
                    'YDOT_ECI_m_s' ,
                    'ZDOT_ECI_m_s' ,
                    'latitude',
                    'longitude',
                    'height_m',
                    'X_ECF_m' ,
                    'Y_ECF_m' ,
                    'Z_ECF_m' ,
                    'XDOT_ECF_m_s' ,
                    'YDOT_ECF_m_s' ,
                    'ZDOT_ECF_m_s' ,
                    'X_polarmotion_milliarcsec',
                    'Y_polarmotion_milliarcsec',
                    'beta_angle',
                    'yaw_angle',
                    'orbit_angle',
                    'Quat_QI_J2000_to_ITRF_ECF',
                    'Quat_Q2_J2000_to_ITRF_ECF',
                    'Quat_Q3_J2000_to_ITRF_ECF',
                    'Quat_Q4_J2000_to_ITRF_ECF',
                    'Greenwich_HR_angle',
                    ]

    sentinel_titles = ['delimeter', 
                      'Satellite_ID',
                      'G_IIS_vers',
                      'G_IIE_vers',
                      ]


    record_len = rvg_params["record_length"]

    #### determine the approximate number of records...
    # Open file
    with open(__rvg_filename,'rb') as f:
        b=f.read()      # read in binary file as bytes
    np_data = np.frombuffer(b)  # create a numpy array
    est_num_records = int((np_data.size/29) - 29*2)

#         print(tabtab, '----- Loading ',__rvg_filename  )
    # print(tabtab, '----- The file has ~%i records. Will take ~%i seconds' % (\
    #                                      est_num_records,  time_estimate_onAWS*est_num_records ) )

    #### Save the data as a dictionary with keys to hold 
    #          1   header   record, 
    #         many  data    records
    #          1   sentinal record

    rvg_data = {}
    rvg_data['header']    = {}
    rvg_data['sentinel']  = {}
    rvg_data['data'] = pd.DataFrame(dict(zip(data_titles,np.ones(record_len)*np.nan)\
                                        ), index=np.arange(0,est_num_records) )

    f = FortranFile(__rvg_filename, 'r')

    end_data_val = -999.0
    end_datarecord = False
    counter = 0

    ####   Loop through the binary file and save out each full record. 
    #      when we encounter the -999.0 delimeter at the start of the sentnial,
    #      we have reached the end of the header record.
    #
    #      The data is saved into a DataFrame for "simplicity"

    while end_datarecord == False:
        a = f.read_record(float)  # read the record with the required datatype
        if end_data_val in a:
            ####  If the the first index has -999.0 we are at the sentinel record 
            #     which denotes the end of the data section.
            print(tabtab, '----- End of file')

            rvg_data['sentinel'] = dict(zip(sentinel_titles, a))    
            end_datarecord = True
            counter += 1
            f.close()  # be sure to close the file
            break  
        else:
            if counter == 0:
                #### If the counter is 0 we are on the header record.
                #    this is simply because it is the first record. bottabing bottaboom
                rvg_data['header'] = dict(zip(header_titles, a))    
            else:
                #### Everything in the file that isn't header or senitinel is data
                rvg_data['data'].loc[counter-1] = dict(zip(data_titles,a) ) 
            counter += 1
    # remove the extra NANs that were used to initialize the dataframe
    rvg_data['data'] = rvg_data['data'].dropna(axis=0 ,how='all')

    # To construct the G2b data, we will run the PCE_converter.f function
    #    this function needs:
    #        MJDS(I),FSEC(I),X(I,1),X(I,2),X(I,3)
    ### We can get rid of (almost) everything else


#         del rvg_data['data']['RSEC_fractional_secs']
    del rvg_data['data']['spare_4']
#         del rvg_data['data']['XDOT_statevector_m_s']
#         del rvg_data['data']['YDOT_statevector_m_s']
#         del rvg_data['data']['ZDOT_statevector_m_s']
    del rvg_data['data']['latitude']
    del rvg_data['data']['longitude']
    del rvg_data['data']['height_m']
    del rvg_data['data']['X_ECF_m']
    del rvg_data['data']['Y_ECF_m']
    del rvg_data['data']['Z_ECF_m']
    del rvg_data['data']['XDOT_ECF_m_s']
    del rvg_data['data']['YDOT_ECF_m_s']
    del rvg_data['data']['ZDOT_ECF_m_s']
    del rvg_data['data']['X_polarmotion_milliarcsec']
    del rvg_data['data']['Y_polarmotion_milliarcsec']
    del rvg_data['data']['beta_angle']
    del rvg_data['data']['yaw_angle']
    del rvg_data['data']['orbit_angle']
    del rvg_data['data']['Quat_QI_J2000_to_ITRF_ECF']
    del rvg_data['data']['Quat_Q2_J2000_to_ITRF_ECF']
    del rvg_data['data']['Quat_Q3_J2000_to_ITRF_ECF']
    del rvg_data['data']['Quat_Q4_J2000_to_ITRF_ECF']
    del rvg_data['data']['Greenwich_HR_angle']

    gc_collect()
    return(rvg_data['data'])


def RVG_Files_add_datetime_column_3(rvg_data):
    '''
    This function includes an additional function to convert
    the MJDSecs to datetime string.

    '''
    #convert the MJDS to a usable string.
    yymmdd_str = [mjds_to_ymdhms(x) for x in rvg_data['MJDSEC_GPS']]

    # convert string of dates to datetime for plotting
    dates_without_offset = [pd.to_datetime( x, format='%y%m%d-%H%M%S') for x in yymmdd_str]

    offset = pd.Series(pd.to_timedelta(rvg_data['GPS_offset_secs_utc'],'s'))

    dates = pd.Series(dates_without_offset) + offset


    rvg_data.insert(0, 'Date_utc', dates)
#     rvg_data['data']['yymmdd_str'] = yymmdd_str

    return(rvg_data)


# def RVGfiles_chop_the_ends_4(rvg_params, rvg_data):
#     '''
#     Chop the ends off the file.
#     '''
#     def RVGfiles_timeoverlap_GetChoppingTime(rvg_params, rvg_data):
#         '''
#         This function retrieves the times in datetime at which the chop will happen
#         '''
#     #         (_, _, tot_overlap) = time_overlap(file1, file2)
#         tot_overlap = rvg_params['overlap']


#         file1_start = rvg_data['data']['Date_utc'].iloc[0] 
#         file1_end = rvg_data['data']['Date_utc'].iloc[-1] 

#         file1_new_start = file1_start + pd.Timedelta(tot_overlap/2, unit='hours')
#         file1_new_end = file1_end - pd.Timedelta(tot_overlap/2, unit='hours')

#         return(file1_new_start, file1_new_end)

# #         print(tabtab,'Chopping the overlap times off of the datasets.')
#     (file1_new_start, 
#     file1_new_end) = RVGfiles_timeoverlap_GetChoppingTime(rvg_params, rvg_data )


#     df1 = rvg_data['data']

#     ##### Chop the FRONT off of the FIRST file
#     # select all the values greater than our new start and grab the last one 
#     val1_front = df1.Date_utc[df1.Date_utc < file1_new_start].iloc[-1]
#     indx1_front = df1.Date_utc[df1.Date_utc==val1_front].index.unique()[0]

#     ##### Chop the END off of the FIRST file
#     # select all the values less than our new start and grab the first one 
#     val1_end = df1.Date_utc[df1.Date_utc > file1_new_end].iloc[1]
#     indx1_end = df1.Date_utc[df1.Date_utc==val1_end].index.unique()[0]


#     df1_new = df1[:indx1_end][indx1_front+1:] # add one index so there is no overlap in time

# #         rvg_data_chopped = df1_new
#     return(df1_new)













