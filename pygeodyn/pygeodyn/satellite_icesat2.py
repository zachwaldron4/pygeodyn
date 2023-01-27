"""SHORT EXPLAIN THIS MODULE

_extended_summary_
"""

import pandas as pd
import numpy  as np
import os
import os.path
import shutil
import sys

#### Import the Pygeodyn Modules
from pygeodyn.control import RunController
from pygeodyn.read    import ReadRawOutput


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



        print('CHECK, ICESAT2 class')


        # Call the control path pointer to establish attributed paths
        self.ctrlStage1_setup_path_pointers(skip_files = True)

        #------------------------------
        # Universal ICESat2 Properties
        #------------------------------
        self.prms['sat_ID']                  = '1807001' # cospar-id number
        self.prms['sat_area_cross_sec']      = 9.530     # estimate (m^2)
        self.prms['sat_mass']                = 1514.0    # estimate (kg)
        self.prms['sat_geometry_panel_num']  = 14 
        #
        self.prms['coord_ref_system']     = 'j2000'      # j2000 is default
        self.prms['orbit_elements_form']  = 'cartesian'  # (meters), XYZ
        self.prms['bool_exatfiles']       = True         # use EXTATT file
        self.prms['number_of_satellites'] = 1

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
            self.filename_g2b                 = 'g2b_pce_fullset_nomaneuver'  
            #### PCE Ascii textfile name
            self.file_statevector_ICs  = self.dir_input+'/PCE_ascii.txt'
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
                self.file_statevector_ICs  = self.dir_input\
                                                    +'/PCE_ascii.txt'
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






