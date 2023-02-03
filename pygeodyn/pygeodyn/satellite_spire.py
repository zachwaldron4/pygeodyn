"""SHORT EXPLAIN THIS MODULE

    _extended_summary_
"""

import pandas as pd
import numpy  as np
import os.path
import sys
import datetime
import gc
from datetime import datetime

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



class Spire(RunController, ReadRawOutput):
    """Class with config for running Pygeodyn with Spire cubesats.
    
    In the Pygeodyn Infrastructure it inherits the RunController and
    ReadRawOutput.  Class with satellite specific confiuguration for running 
    Pygeodyn with Spire.
  
    """


    def __init__(self):
        # Initialize the class to contain all methods
        InheritControlStages.__init__(self)
        InheritReadRaw.__init__(self)

        # print('Running Pygeodyn with SPIRE')


        # Call the control path pointer to establish attributed paths
        self.ctrlStage1_setup_path_pointers(skip_files = True)

        #------------------------------
        # Universal ICESat2 Properties
        #------------------------------
        self.prms['sat_ID']                  = '1804607'  # 83 # cospar-id #
        self.prms['sat_area_cross_sec']      = 999        # estimate (m^2)
        self.prms['sat_mass']                = 4.933      # estimate (kg)
        self.prms['sat_geometry_panel_num']  = 8 
        #
        self.prms['coord_ref_system']     = 'j2000'      # j2000 is default
        self.prms['orbit_elements_form']  = 'cartesian'  # (meters), XYZ
        self.prms['bool_exatfiles']       =  True         # use EXTATT file
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
        list_run_types = ["DataReduction_PCE" , "OrbitPropagation"] 
        # Fill in the appropriate settings based on the run_type.
        if self.prms['run_type'] == "DataReduction_PCE":
            self.tracking_data_type   = 'PCE'
            #### G2B file name
            self.filename_g2b         = 'g2b_pce_leoOrb_nov2018' 
            #### PCE Ascii textfile name
            self.file_statevector_ICs = self.dir_input+'/'\
                                    +'Spire83_initialconditions_Nov2018_v1.txt'
                                    # +'Spire83_initialconditions_Nov2018_v2.txt'
            ### Identify all the header names in the file
            self.file_statevector_headers   = ['Date',
                                                'X',
                                                'Y',
                                                'Z',
                                                'X_dot',
                                                'Y_dot',
                                                'Z_dot']
        #
        elif self.prms['run_type']  == "OrbitPropagation":
                self.filename_g2b = 'None'
                self.file_statevector_ICs   = self.dir_input+'/Spire83_initialconditions_Nov2018_v2.txt'
                print("self.file_statevector_ICs",self.file_statevector_ICs)
                #'/data/SatDragModelValidation/data/inputs/sat_spire83/setups'\
#                        + '/statevector_ICs.txt'
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


            Spire Sat info from Eric:
                Au = 196.96655  # gold atomic mass
                #SiO2 = 60.0843
                SiO2 = 20.03    # silicon dioxide atomic mass
                Al = 26.981538  # aluminum atomic mass

                Panel 1 - BODY: Front (FRO Ant.) panel +X
                          Material = Al
                Panel 2 - BODY: Port -Y
                          Material = Al
                Panel 3 - SOLAR PANELS: "Front" panels 
                            only half covered w/ solar panels, so split into
                            halves (1/2 SiO2 and 1/2 Al) if using SRP model)
                          Material = SiO2
                Panel 4 - BODY: Aft panel -X (mostly covered in solar panels 
                              as of rev. 3.3 & prior to rev. 3.4)
                          Material = SiO2
                Panel 5 - BODY: Star +Y (mostly covered in solar panels)
                          Material = SiO2
                Panel 6 - SOLAR PANELS: "Back/Aft" panels 
                              mostly covered w/ solar panels)
                          Material = SiO2
                Panel 7 - BODY: Zenith (POD ant.) -Z
                          Material  = Al
                Panel 8 - BODY: Nadir +Z
                          Material  = Al
        '''
        
        PanelModel = {\
            'Area (m^2)'       : [ 0.1053314*0.3388226,
                                  (0.105186*0.3388226)+(3*0.005334*0.074549),
                                  2*0.150*0.3222,
                                  0.1053314*0.3388226,
                                  (0.105186*0.3388226)+(3*0.005334*0.074549),
                                  2*0.150*0.3222,
                                  0.105186*0.1053314,
                                  0.105186*0.1053314,
                                ],
            'Moves=1 Fixed=0' : [0.,0.,0.,0.,0.,0.,0.,0.],
            'Diffuse'        : [0.4,
                                0.4,
                                0.3,
                                0.3,
                                0.3,
                                0.3,
                                0.4,
                                0.4,
                                ],
            'Emissivity'     : [0.83,  # these may need to be changed
                                0.83,
                                0.83,
                                0.83,
                                0.83,
                                0.83,
                                0.83,
                                0.83,
                                ],
            'Specular'       : [0.2,
                                0.2,
                                0.05,
                                0.05,
                                0.05,
                                0.05,
                                0.2,
                                0.2,
                                ],
            'RadiationFreq Both=0':[0.,0.,0.,0.,0.,0.,0.,0.],
            'Normal Vector X'    : [ 1.,
                                     0.,
                                     0.70710678,
                                    -1.,
                                     0.,
                                    -0.70710678,
                                     0.,
                                     0.,
                                    ],
            'Normal Vector Y'    : [ 0.,
                                    -1.,
                                    -0.70710678,
                                     0.,
                                     1.,
                                     0.70710678,
                                     0.,
                                     0.,
                                    ],
            'Normal Vector Z'    : [ 0.,
                                     0.,
                                     0.,
                                     0.,
                                     0.,
                                     0.,
                                    -1.,
                                     1.,
                                    ],
            #                                     
            'Temperature A'       : [0.,0.,0.,0.,0.,0.,0.,0.],
            'Temperature C'       : [0.,0.,0.,0.,0.,0.,0.,0.],
            'Temperature rate D'  : [0.,0.,0.,0.,0.,0.,0.,0.],
            'Temperature rate F'  : [0.,0.,0.,0.,0.,0.,0.,0.],
            'Temperature rotate X': [0.,0.,0.,0.,0.,0.,0.,0.],
            }


        return(PanelModel)













#### ==========================================================================
####    AUXILIARY FUNCTIONS
#### ==========================================================================


# class SpireAuxiliary():
    """These auxiliary functions are for spire specific functionality.
    
    They are more useful as a reference to accomplish specific tasks.
    """

    # def __init__(self):
    #     pass


def load_attitude_spire(filename, start_date, stop_date):
    """Load attitude data from the Spire satellite constellation
    
        
    Additional Info (from attitude file header):
        %spire version 1.3  revision   1 2019 07 05 00:00 Spire     Spire Processing Center
        +satellite       0  083                
        +data_____ tim sca
        +reference gps sbf
        +first____ 2018  9 23  0 12  0.0300000 
        +last_____ 2018 12  9 23 43 35.9080000 
        *comment:  tim: GPS time: yr, mon, day, hr, min, sec
        *comment:  Defintion of last character of 'tim' field:
        *comment:      '0': Valid observed data used
        *comment:      '1': Epoch is missing. Previous valid data used
        *comment:      '2': Epoch is missing. Default quaterion used
        *comment:  sca: Quaternion to convert spacecraft reference frame to orbit level frame:
        *comment:         Z nadir = -r/|r|         (where r is s/c position vector)
        *comment:         Y = -(r x v) / | r x v | (where v is s/c velocity vector)
        *comment:         X = Y x Z                (where 'x' is vector cross product)
        *comment:       A vector, u, in the spacecraft frame, has coordinates u' = quq* in the local
        *comment:       level frame
        *comment:       All quaternions are given in order:  qx, qy, qz, qw
        *comment:  pvi:  Position and velocity in ECI coordinates (km)
        *comment:        X, Y, Z, Xdot, Ydot, Zdot
    
    Parameters
    ----------
        filename   : string
            Full global path to the filename
        startEpoch  = "2018-11-08 23:00:00"  
        
        stopEpoch   = "2018-11-10 01:00:00" 
    Returns
    -------
        SpireDF : Pandas DataFrame
            Dataframe that contains the Spire attitude information
    """


    dict_data = {}
    dict_data['tim (gps)'] = []
    dict_data['q (sbf)'] = []
    dict_data['pos (eci)'] = []
    dict_data['vel (eci)'] = []

    ### Loop through the file
    with open(filename, 'r') as f:
        for l_no, line in enumerate(f): 
            ###  Identify the file headers
    #         if line[0]=="%spire" or line[0]=="*" or line[0]=="+":
    #             section_HeaderEnd = l_no
    #         ###  Identify the End Of File
    #         elif line[0:4]=="%eof":
    #             section_EOF = l_no
    #         noSkip_flag=False
            if line[0:4]=="tim ":
                yr    = int(line[4:8]   )
                mon   = int(line[9:11]  )
                day   = int(line[12:14] )
                hr    = int(line[15:17] )
                minute= int(line[18:20] )
                sec   = int(line[21:23] )
                msec  = int(line[24:30] )
                date = datetime(yr,mon, day, hr,minute,sec, msec )
                if  pd.to_datetime(date) in dict_data['tim (gps)']:
                    #print('Found a copy, SKIP', date)
                    noSkip_flag=False
                    continue # to next line of for loop
                else:
                    noSkip_flag = True
                    dict_data['tim (gps)'].append( pd.to_datetime(date))

            elif line[0:4]=="sca " and noSkip_flag:
                qx = float(line[4:17] )
                qy = float(line[18:31])
                qz = float(line[32:45])
                qw = float(line[46:59])
                dict_data['q (sbf)'].append(np.array([qx, qy, qz, qw]))

            elif line[0:4]=="pvi "and noSkip_flag:
                x    = float(line[4:17] )
                y    = float(line[18:31])
                z    = float(line[32:45])
                xdot = float(line[46:59])
                ydot = float(line[60:73])
                zdot = float(line[74:87])
                dict_data['pos (eci)'].append(np.array([x, y, z]))
                dict_data['vel (eci)'].append(np.array([xdot,ydot,zdot]))

    #### Make a Dataframe for easier utility                   
    SpireDF = pd.DataFrame.from_dict(dict_data)

    #### Mask the data according to the time period of interest
    startDT = pd.to_datetime(start_date, format='%Y-%m-%d %H:%M:%S')
    stopDT  = pd.to_datetime(stop_date,  format='%Y-%m-%d %H:%M:%S')
    mask_month = SpireDF['tim (gps)'].dt.month==  startDT.month
    mask_days = np.logical_and(SpireDF['tim (gps)'].dt.day  >=  startDT.day,
                                SpireDF['tim (gps)'].dt.day  <=  stopDT.day)
    mask       = mask_month & mask_days
    SpireDF =  SpireDF[:][mask].reset_index(drop=True)
    
    gc.collect()
    return(SpireDF)









def aux__make_file_statevector_ICs():

    # import pandas as pd

    # file_statevector_ICs = '/data/SatDragModelValidation/data/inputs/sat_spire83/setups'\
    #                     + '/statevector_ICs.txt'

    # prms_arc = {}
    # prms_arc['epoch_start'] = "2018-11-09 00:00:00"
    # prms_arc['epoch_stop']  = "2018-11-10 00:00:00"
    # prms_arc['epoch_startDT'] = pd.to_datetime(prms_arc['epoch_start'],\
    #                                             format='%Y-%m-%d %H:%M:%S')
    # prms_arc['epoch_stopDT']  = pd.to_datetime(prms_arc['epoch_stop'],\
    #                                             format='%Y-%m-%d %H:%M:%S')

    import pandas as pd
    import numpy as np
    from datetime import datetime
    from pygeodyn.util_dir.time_systems import time_gps_to_utc

    def load_attitude_spire(filename):
        Spire_dict = {}
        Spire_dict['info']  = """fill info here"""


        dict_data = {}
        dict_data['tim (gps)'] = []
        dict_data['q (sbf)'] = []
    #     dict_data['pos (eci)'] = []
    #     dict_data['vel (eci)'] = []
        dict_data['X'] = []
        dict_data['Y'] = []
        dict_data['Z'] = []
        dict_data['X_dot'] = []
        dict_data['Y_dot'] = []
        dict_data['Z_dot'] = []

        ### Loop through the file
        with open(filename, 'r') as f:
            for l_no, line in enumerate(f): 
                if line[0:4]=="tim ":
                    yr    = int(line[4:8]   )
                    mon   = int(line[9:11]  )
                    day   = int(line[12:14] )
                    hr    = int(line[15:17] )
                    minute= int(line[18:20] )
                    sec   = int(line[21:23] )
                    msec  = int(line[24:30] )
                    date = datetime(yr,mon, day, hr,minute,sec, msec )
                    if  pd.to_datetime(date) in dict_data['tim (gps)']:
                        #print('Found a copy, SKIP', date)
                        noSkip_flag=False
                        continue # to next line of for loop
                    else:
                        noSkip_flag = True
                        dict_data['tim (gps)'].append( pd.to_datetime(date))

                elif line[0:4]=="sca " and noSkip_flag:
                    qx = float(line[4:17] )
                    qy = float(line[18:31])
                    qz = float(line[32:45])
                    qw = float(line[46:59])
                    dict_data['q (sbf)'].append(np.array([qx, qy, qz, qw]))

                elif line[0:4]=="pvi "and noSkip_flag:
                    x    = float(line[4:17] )
                    y    = float(line[18:31])
                    z    = float(line[32:45])
                    xdot = float(line[46:59])
                    ydot = float(line[60:73])
                    zdot = float(line[74:87])
    #                 dict_data['pos (eci)'].append(np.array([x, y, z]))
    #                 dict_data['vel (eci)'].append(np.array([xdot,ydot,zdot]))
                    dict_data['X'].append(x)
                    dict_data['Y'].append(y)
                    dict_data['Z'].append(z)
                    dict_data['X_dot'].append(xdot)
                    dict_data['Y_dot'].append(ydot)
                    dict_data['Z_dot'].append(zdot)

        Spire_dict['units'] = {'tim (gps)': "pd.datetime" ,
                            'q (sbf)':  "" ,
                            'X':  "m" ,
                            'Y':  "m" ,
                            'Z':  "m" ,
                            'X_dot':  "m/s" ,
                            'Y_dot':  "m/s" ,
                            'Z_dot':  "m/s" ,
                            }
        Spire_dict['data'] = dict_data


        #### Make a Dataframe for easier utility                   
    #     SpireDF = pd.DataFrame.from_dict(dict_data)

        return(Spire_dict)

    dir_spire='/data/SatDragModelValidation/data/inputs/sat_spire83/data_Spire/'
    file_spire = dir_spire+'attitude/20180923_20181209_TaskOrder3Data/'\
                        +'leoAtt_2018-09-23T00-12-00Z.9999999.083.log'
    statevector_ICs = load_attitude_spire(file_spire)
    del statevector_ICs['data']['q (sbf)']
    del statevector_ICs['units']['q (sbf)']


    ### Convert from GPS time to UTC time
    tim_utc = [datetime.strftime(time_gps_to_utc(tim, leap_sec=37), '%y%m%d%H%M%S.%f' )
                                    for tim in statevector_ICs['data']['tim (gps)'] ]


    statevector_ICs['data']['Date'] =   tim_utc 

    ### Update the units to reflect the above changes:
    del statevector_ICs['data']['tim (gps)']
    del statevector_ICs['units']['tim (gps)']
    statevector_ICs['units']['Date'] = 'UTC'

    ### Update the metadata.  This will become the file header.
    statevector_ICs['info']= 'fill info here'



    from  datetime import datetime, timedelta

    ### Write to an ascii text file. 
    file_save = '/data/SatDragModelValidation/data/inputs/sat_spire83/setups'\
                    + '/statevector_ICs.txt'
    # 
    with open(file_save, 'r+') as file:
        #### Manually write the header units
        header_units =\
                    f"{'UTC'.rjust(len(str(statevector_ICs['data']['Date'][1]))-1,' ') }"\
                +f"  {'(m)'.rjust(15,' ')}"\
                +f"  {'(m)'.rjust(15,' ')}"\
                +f"  {'(m)'.rjust(15,' ')}"\
                +f"  {'(m/s)'.rjust(15,' ')}"\
                +f"  {'(m/s)'.rjust(15,' ')}"\
                +f"  {'(m/s)'.rjust(15,' ')}"\

        #### Manually write the header field names
        header_names =\
                    f"{'Date'.rjust(len(str(statevector_ICs['data']['Date'][1]))-1,' ') }"\
                +f"  {'X'.rjust(15,' ')}"\
                +f"  {'Y'.rjust(15,' ')}"\
                +f"  {'Z'.rjust(15,' ')}"\
                +f"  {'X_dot'.rjust(15,' ')}"\
                +f"  {'Y_dot'.rjust(15,' ')}"\
                +f"  {'Z_dot'.rjust(15,' ')}"\



        #### Manually write the detailed header description
        header_meta = \
    f'''### Initial conditions file
    ### -----------------------
    ###     Satellite: Spire_083 (1804607)
    ###     Epoch: +start____ 2018  9 23  0 12  0.0300000 
    ###            +stop_____ 2018 12  9 23 43 35.9080000 
    ###     Last modified: {datetime.now()-timedelta(hours=7)}
    ###
    ### Source
    ### -------
    ###     leoAtt_2018-09-23T00-12-00Z.9999999.083.log
    ###     (long quaternion file)
    ###     %spire version 1.3  revision   1 2019 07 05 00:00 Spire     Spire Processing Center
    ###
    ### Contents
    ### --------
    ###     Date: (YYYY-MM-DD hh:mm:ss.ssssss) (UTC, converted from gps time)
    ###     pvi: Position and velocity (X, Y, Z, X_dot, Y_dot, Z_dot)
    ###          coordinate: ECI
    ###          unit: m
    ###
    #{header_units}
    #{header_names}
    ### %eoh
    '''
        file.write(header_meta)

        #### Manually write each row of the data.
        for ii, val in enumerate(statevector_ICs['data']['Date']):
            row =   f"{statevector_ICs['data']['Date'][ii]}"\
                +f"  {statevector_ICs['data']['X'][ii]*1000:15.5f}"\
                +f"  {statevector_ICs['data']['Y'][ii]*1000:15.5f}"\
                +f"  {statevector_ICs['data']['Z'][ii]*1000:15.5f}"\
                +f"  {statevector_ICs['data']['X_dot'][ii]*1000:15.5f}"\
                +f"  {statevector_ICs['data']['Y_dot'][ii]*1000:15.5f}"\
                +f"  {statevector_ICs['data']['Z_dot'][ii]*1000:15.5f}"\
                + f"\n"
            file.write(row)
    return



def read_SpireLeoOrbPOD_sp3c(filelist_sat):
    """
    Reads the Spire Level 1B Precise Orbit Determination Solution (leoOrb)


    File Content
    -------
        This file contains the estimated position, velocity 
        and receiver clock error of a given Spire satellite 
        after processing of the POD observation file by Spire’s
        precise orbit determination software. 
        Estimates are typically given in the ECEF (IGS08) frame.

    File Naming Convention
    ----------------------
        Each leoOrb file has the following naming convention:
            spire_nav_L1B_leoOrb_{VERSION}_{DATA_TIME}_{FM}.sp3
                VERSION: Product version (i.e. v6.02) 
                DATA_TIME: Start time of file (i.e 2020-01-01T00-00-00) 
                FM: Spire satellite id (i.e. FM103)

    The data is given in standard sp3c format
    (https://files.igs.org/pub/data/format/sp3c.txt).  Under each time epoch,
    there are two lines beginning with 'P' and 'V'.  The first three values in
    the 'P' line contain the XYZ position coordinates with units of kilometers
    in a given orbital frame.  The fourth value in the 'P' line refers to the
    estimated receiver clock error from true GPS time given in microseconds.
    The XYZ velocity coordinates are given in the 'V' line in units of
    decimeters/sec. The final value of the 'V' line refers to the estimated
    receiver clock error rate of change in units of 10-4 microseconds/sec, which
    is normally not estimated from the precise orbit determination software and
    thus set to 999999.999999.

                        # The clock values are in microseconds and are precise
                        to 1 picosecond.  # Bad or absent positional values are
                        to be set to 0.000000.  # Bad or absent clock values are
                        to be set to _999999.999999.

    """

    import os
    import numpy as np
    from datetime import datetime
    from pandas import isnull as pd_isnull
    from pandas import Timestamp as pd_Timestamp
    from pandas import to_datetime as pd_to_datetime
    import gc



    print("read_SpireLeoOrbPOD_sp3c(): Reading ephemeris data from  ", len(filelist_sat), "files.")
    ### initialize large empty arrays (save through loops)
    ### initialize empty arrays (in loops)
    ### initialize the data storage    
    
    init_val = 4000*len(filelist_sat)
    #
    tot__date_gps       = np.ones(init_val)*np.nan
    tot__x_km           = np.ones(init_val)*np.nan
    tot__y_km           = np.ones(init_val)*np.nan
    tot__z_km           = np.ones(init_val)*np.nan
    tot__clock_microsec = np.ones(init_val)*np.nan
    tot__xdot_dms       = np.ones(init_val)*np.nan
    tot__ydot_dms       = np.ones(init_val)*np.nan
    tot__zdot_dms       = np.ones(init_val)*np.nan

    ## initialize a "global" looping index
    tleng = 0

    for ifile,file_pod in enumerate(filelist_sat):
        with open(file_pod, 'r') as f:

            ## 1) Read the 1st line to determine the number of epochs in file
            ## SP3 Line 1
            line = f.readline()

        # Gregorian date and time of day of the first epoch
            start_year       = line[4-1  :7]   # I4 
            start_month      = line[9-1  :10]  # I2
            start_day        = line[12-1 :13]  # I2
            start_hour       = line[15-1 :16]  # I2
            start_minute     = line[18-1 :19]  # I2
            start_second     = line[21-1 :31]  # F11.8
            coord_sys        = line[47-1 :51]  # A5

            if line[0:2]=='#c':
                num_epochs       = int(line[33-1 :39])  # I7

            elif line[0:2]=='#a':
                num_epochs = 10000
            else:
                print('Weird file type:', line )
                print(file_pod )

            ## Find the "global" indecies for this file
            leng1 = num_epochs
            leng2 = leng1 + tleng

            ## SP3 Line Two
            line = f.readline()
            gps_week         = line[4-1  : 7 ]  # I4     
            gps_week_seconds = line[9-1  : 23]  # F15.8   (seconds  of GPS Week elapsed at start of orbit)
            epoch_interval   = line[25-1 : 38]  # F14.8  
            start_MJD        = line[40-1 : 44]  # I5      (where 44244 represents GPS zero time -- January 6, 1980)
            fracday          = line[46-1 : 60]  # F15.13 

            ## SP3 Line 13
            for skip in [1,2,3,4,5,6,7,8,9,10,11]: 
                line = f.readline()
            file_type   = line[ 4-1 : 5 ] # A2   ("G " for GPS only files)
            time_system = line[10-1 : 12] # A3


            ## Check that things are as we expect.
            if time_system != 'GPS' and time_system !='ccc':
                print('Weird Time system:',time_system)
                print(start_year,start_month,start_day,start_hour,start_minute,start_second)
                print()
                print(file_pod)
            if coord_sys != 'IGS08':
                print('Weird coord_system:',coord_sys)
                print(start_year,start_month,start_day,start_hour,start_minute,start_second)
                print()
                print(file_pod)


        ### initialize the inner loop data storage
        loop__date_gps       = np.ones(num_epochs)*np.nan #np.zeros(int(num_epochs))
        loop__x_km           = np.ones(num_epochs)*np.nan #np.zeros(int(num_epochs))
        loop__y_km           = np.ones(num_epochs)*np.nan #np.zeros(int(num_epochs))
        loop__z_km           = np.ones(num_epochs)*np.nan #np.zeros(int(num_epochs))
        loop__clock_microsec = np.ones(num_epochs)*np.nan #np.zeros(int(num_epochs))
        loop__xdot_dms       = np.ones(num_epochs)*np.nan #np.zeros(int(num_epochs))
        loop__ydot_dms       = np.ones(num_epochs)*np.nan #np.zeros(int(num_epochs))
        loop__zdot_dms       = np.ones(num_epochs)*np.nan #np.zeros(int(num_epochs))

        with open(file_pod, 'r') as f:

            i = 0

            ## the i counter will equal num_epochs upon reaching EOF
            for  line in f: 

            #### Begin reading data        

                #### The Epoch Header Record
                if line[0]=='*':
                    epoch_year   = int(line[4-1 :7 ])   # I4
                    epoch_month  = int(line[9-1 :10])   # I2
                    epoch_day    = int(line[12-1:13])   # I2
                    epoch_hour   = int(line[15-1:16])   # I2
                    epoch_minute = int(line[18-1:19])   # I2
                    epoch_second = int(line[21-1:22])   ### just grab the second as int
                    date = datetime(epoch_year,epoch_month,  epoch_day,
                                    epoch_hour,epoch_minute, epoch_second )
                    ### save as a unixtime float to use np.array index cacheing
                    loop__date_gps[i]       = pd_Timestamp(date).value/(10**9)                    

                #### The Position and Clock Record
                elif line[0]=='P':
                    x_km = line[5-1 : 18]   #   F14.6 
                    y_km = line[19-1: 32]   #   F14.6
                    z_km = line[33-1: 46]   #   F14.6
                    clock_microsec = line[47-1 : 60]  #  F14.6
                    #
                    loop__x_km[i]           = x_km
                    loop__y_km[i]           = y_km
                    loop__z_km[i]           = z_km
                    loop__clock_microsec[i] = clock_microsec

                #### Velocity and Clock Rate-of-Change Record
                elif line[0]=='V':
                    xdot_dms = line[5-1 : 18]       # F14.6 (_dms refers to the unit, decimeters/second )
                    ydot_dms = line[19-1: 32]       # F14.6
                    zdot_dms = line[33-1: 46]       # F14.6
                    loop__xdot_dms[i]       = xdot_dms
                    loop__ydot_dms[i]       = ydot_dms
                    loop__zdot_dms[i]       = zdot_dms

                    ### only advance the index after the velocity
                    i+=1 


                elif line[0:3] == "EOF":
                    break

                elif line[0] in ['#','+','%','/']:
                    continue

                else:
                    import sys
                    print('unknown character starting line...')
                    print(line)
                    sys.exit(0)

            else:
                pass

        ### End of file reached,
        ###    add inner loop indicies to the global index
        tot__date_gps[tleng:leng2]       = loop__date_gps
        tot__x_km[tleng:leng2]           = loop__x_km
        tot__y_km[tleng:leng2]           = loop__y_km
        tot__z_km[tleng:leng2]           = loop__z_km
        tot__clock_microsec[tleng:leng2] = loop__clock_microsec
        tot__xdot_dms[tleng:leng2]       = loop__xdot_dms
        tot__ydot_dms[tleng:leng2]       = loop__ydot_dms
        tot__zdot_dms[tleng:leng2]       = loop__zdot_dms

        tleng = tleng + leng1

        ## delete all big arrays 
        del loop__date_gps
        del loop__x_km
        del loop__y_km
        del loop__z_km
        del loop__clock_microsec
        del loop__xdot_dms
        del loop__ydot_dms
        del loop__zdot_dms
        #
        gc.collect()

    ## After all is done, remove the placeholder nans
    tot__date_gps       = tot__date_gps[~np.isnan(tot__date_gps)]
    tot__x_km           = tot__x_km[    ~np.isnan(tot__x_km)]
    tot__y_km           = tot__y_km[    ~np.isnan(tot__y_km)]
    tot__z_km           = tot__z_km[    ~np.isnan(tot__z_km)]
    tot__clock_microsec = tot__clock_microsec[~np.isnan(tot__clock_microsec)]
    tot__xdot_dms       = tot__xdot_dms[~np.isnan(tot__xdot_dms)]
    tot__ydot_dms       = tot__ydot_dms[~np.isnan(tot__ydot_dms)]
    tot__zdot_dms       = tot__zdot_dms[~np.isnan(tot__zdot_dms)]


    leoOrb_dict = {}
    leoOrb_dict['date_gps']       = tot__date_gps
    leoOrb_dict['x_km']           = tot__x_km
    leoOrb_dict['y_km']           = tot__y_km
    leoOrb_dict['z_km']           = tot__z_km
    leoOrb_dict['clock_microsec'] = tot__clock_microsec
    leoOrb_dict['xdot_dms']       = tot__xdot_dms
    leoOrb_dict['ydot_dms']       = tot__ydot_dms
    leoOrb_dict['zdot_dms']       = tot__zdot_dms
    
    ## delete all big arrays 
    del tot__date_gps
    del tot__x_km
    del tot__y_km
    del tot__z_km
    del tot__clock_microsec
    del tot__xdot_dms
    del tot__ydot_dms
    del tot__zdot_dms
    del tleng, leng1, leng2
    gc.collect()
    
    ### convert date back to datetime from unixtime float
    leoOrb_dict['date_gps'] =  [pd_to_datetime(
            datetime.strftime(datetime.fromtimestamp(ts), '%y%m%d%H%M%S.%f'),
                format ='%y%m%d%H%M%S.%f' )
                                 for ts in leoOrb_dict['date_gps'] ]
    
    print("read_SpireLeoOrbPOD_sp3c(): Done combining data.")


    
    return(leoOrb_dict)