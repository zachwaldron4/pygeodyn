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



class GRACEFOC(RunController, ReadRawOutput):
    """Class with satellite specific config for running Pygeodyn with GRACE-FO-C.
    
    In the Pygeodyn Infrastructure it inherits the RunController and 
    ReadRawOutput.  Class with satellite specific confiuguration for running 
    Pygeodyn with GRACE-FO-C. The setup here is originally for PCE trajectory
    analysis.
    """

    def __init__(self):
        # Initialize the GRACEFOC class to contain the methods from RunController
        InheritControlStages.__init__(self)
        InheritReadRaw.__init__(self)


        if self.prms['satellite'] != 'gracefoc':
            raise ValueError(f'Error in satellite inheritance and initialization.' \
                             +'Wrong satellite methods may have been inherited.')
        
        print('Using the GRACE-FO-C Class')


        # Call the control path pointer to establish attributed paths
        self.ctrlStage1_setup_path_pointers(skip_files = True)

        #------------------------------
        # Universal GRACEFOC Properties
        #------------------------------
        self.prms['sat_ID']                  = '2012003'   # cospar-id number
        self.prms['sat_area_cross_sec']      = 0.9551567       # estimate (m^2), overwritten by geodyn panel model
        self.prms['sat_mass']                = 600.983         # estimate (kg)
        self.prms['sat_geometry_panel_num']  = 12 
        #
        # self.prms['coord_ref_system']        = 'j2000'     # j2000 is default
        self.prms['coord_ref_system']        = 'true_of_reference'     # j2000 is default
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
                                        +f'sat_gracefoc/g2b/'  \
                                        +f'GRACEFOC_RawEphem_{datestr1}_{datestr2}.txt'
        else:
            self.raw_satinput['ephem_path'] = self.path_data_inputs +'/'\
                                    +f'sat_gracefoc/g2b/'  \
                                    +f'GRACEFOC_RawEphem_20181108_20181124.txt'

        self.raw_satinput['ephem_path_dir'] = self.path_data_inputs_raw \
                                                +'/data_GRACEFO/traj_raw/'


        #------------------------------------------------------------------
        # Use the Global Files from ICESat-2 Precise Science Orbit from 2018
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

        elif self.prms['global_options']=='**INIT**':
            print(f"{self.tab}* Global Option not needed during Initializing Stage")
            self.filename_atmograv  = '*****'
            self.filename_ephem     = '*****'
            self.filename_gravfield = '*****'
            self.file_globalcards   = '*****'


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
            
            #### G2B file name
            # self.filename_g2b                 = 'g2b_pce_fullset_nomaneuver'  
            if self.prms['initialize']:
                self.filename_g2b = f"pce_gracefoc_pso_{self.raw_satinput['daterange']}" 
            else:
#                 self.filename_g2b = f"pce_gracefoc_pso_20181108_20181124save" 
                
                if self.prms['which_g2bfile'] is not None:
                    self.filename_g2b = self.prms['which_g2bfile']

    #                 self.filename_g2b = f"pce_gracefoc_pso_2018_10" 


            

            #### PCE Ascii textfile name
            if self.prms['which_ICfile'] is not None:
                self.file_statevector_ICs = self.prms['which_ICfile']
            else:
                self.file_statevector_ICs = self.dir_input+'/'\
                        +f"GRACEFOC_initialconditions_"\
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

        #
        elif self.prms['run_type']  == "OrbitPropagation":
                self.filename_g2b = False
                #### PCE Ascii textfile name
                if self.prms['which_ICfile'] is not None:
                    self.file_statevector_ICs = self.prms['which_ICfile']
                else:
                    self.file_statevector_ICs = self.dir_input+'/'\
                            +f"GRACEFOC_initialconditions_"\
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

        panelmodel_lookuptable='''
+----------------------+------------+------------+------------+------------+------------+------------+------------+
|       Panel #        |     1      |     2      |     3      |     4      |     5      |     6      |     7      |
+----------------------+------------+------------+------------+------------+------------+------------+------------+
|   Normal Vector X    |    1.0     |    -1.0    |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |
|   Normal Vector Y    |    0.0     |    0.0     |  0.766044  | -0.766044  | -0.766044  |  0.766044  |    0.0     |
|   Normal Vector Z    |    0.0     |    0.0     | -0.642787  |  0.642787  | -0.642787  |  0.642787  |    1.0     |
|                      |            |            |            |            |            |            |            |
|      Area (m^2)      | 0.9551567  | 0.9551567  | 3.1554792  | 0.2282913  | 3.1554792  | 0.2282913  |  6.071112  |
|   Moves=1|Fixed=0    |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |
|       Specular       |    0.4     |    0.4     |    0.05    |    0.4     |    0.05    |    0.4     |    0.68    |
|       Diffuse        |    0.26    |    0.26    |    0.3     |    0.26    |    0.3     |    0.26    |    0.2     |
|      Emissivity      |    0.62    |    0.62    |    0.81    |    0.62    |    0.81    |    0.62    |    0.75    |
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
|   Normal Vector X    |    0.0     |    1.0     |    -1.0    |    0.0     |    0.0     |            |            |
|   Normal Vector Y    |    0.0     |    0.0     |    0.0     |    1.0     |    -1.0    |            |            |
|   Normal Vector Z    |    -1.0    |    0.0     |    0.0     |    0.0     |    0.0     |            |            |
|                      |            |            |            |            |            |            |            |
|      Area (m^2)      |  2.167362  | 0.0461901  | 0.0461901  | 0.0461901  | 0.0461901  |            |            |
|   Moves=1|Fixed=0    |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |            |            |
|       Specular       |    0.05    |    0.4     |    0.4     |    0.4     |    0.4     |            |            |
|       Diffuse        |    0.3     |    0.26    |    0.26    |    0.26    |    0.26    |            |            |
|      Emissivity      |    0.81    |    0.62    |    0.62    |    0.62    |    0.62    |            |            |
| RadiationFreq|Both=0 |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |            |            |
|                      |            |            |            |            |            |            |            |
|    Temperature A     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |            |            |
|    Temperature C     |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |            |            |
|  Temperature rate D  |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |            |            |
|  Temperature rate F  |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |            |            |
| Temperature rotate X |    0.0     |    0.0     |    0.0     |    0.0     |    0.0     |            |            |
+----------------------+------------+------------+------------+------------+------------+------------+------------+        '''

        PanelModel = {\
            'Area (m^2)': [0.9551567, 0.9551567, 3.1554792, 0.2282913, 
                           3.1554792, 0.2282913, 6.071112,  2.167362, 
                           0.0461901, 0.0461901, 0.0461901, 0.0461901],
            'Moves=1 Fixed=0': [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
            'Diffuse': [0.26,0.26,0.3,0.26,0.3,0.26,0.2,0.3,0.26,0.26,0.26,0.26],
            'Emissivity': [0.62,0.62,0.81,0.62,0.81,0.62,0.75,0.81,0.62,0.62,0.62,0.62],
            'Specular': [0.4, 0.4, 0.05, 0.4, 0.05, 0.4, 0.68, 0.05, 0.4, 0.4, 0.4, 0.4],
            'RadiationFreq Both=0': [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
            'Normal Vector X': [1.0,-1.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0,-1.0,0.0,0.0],
            'Normal Vector Y': [0.0,0.0,0.766044,-0.766044,-0.766044,0.766044,
                                0.0,0.0,0.0,0.0,1.0,-1.0],
            'Normal Vector Z': [0.0,0.0,-0.642787,0.642787,-0.642787,0.642787,
                                1.0,-1.0,0.0,0.0,0.0,0.0],
            'Temperature A': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
            'Temperature C': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
            'Temperature rate D': [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
            'Temperature rate F': [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
            'Temperature rotate X': [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]}




        return(PanelModel)







    def sat_process_raw_ephemeris(self, verbose=False):


        from datetime import datetime,timedelta
        import time


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

        if verbose: print(f"{self.tabtab} - GRACEFOC")
        if verbose: print(f"{self.tabtab} - processing raw satellite ephemerides from files.")
        if verbose: print(f"{self.tabtab} - for dates: {starts_linspace_dt}")


        ### PLACE THE RAW FILE PROCESSING CODE HERE
        ### --------------------------------------------------------------------
        ### Date must be in UTC
        ### Ephemerides must be in ECI-J2000

        ORBFIL_FINAL = get_timechopped_trajdata_1(ephem_path_dir, \
                                        self.prms['arc'],\
                                        )
        ### --------------------------------------------------------------------

        start = time.time()

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
###     Satellite: GRACE-FO-C_({self.prms['sat_ID']})
###     Epoch: +start____ {ORBFIL_FINAL['Date_UTC'].values[0]} 
###            +stop_____ {ORBFIL_FINAL['Date_UTC'].values[-1]}
###     Last modified: {datetime.now()-timedelta(hours=7)}
###
### Source
### -------
###     {self.raw_satinput['ephem_path_dir']}
###     GRACE-FO-C PSO, Orbit Trajectory binary files
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
            print(np.shape(ORBFIL_FINAL['X j2000'].values))

            file.write(header_meta)  
            for indx,valdate in enumerate(ORBFIL_FINAL['Date_UTC'].values):
            #### Manually write each row of the data.
                row =   f"{pd.to_datetime(ORBFIL_FINAL['Date_UTC'].values[indx]).strftime(format='%Y-%m-%d %H:%M:%S')}"\
                    +f"  {ORBFIL_FINAL['X j2000'].values[indx]:15.5f}"\
                    +f"  {ORBFIL_FINAL['Y j2000'].values[indx]:15.5f}"\
                    +f"  {ORBFIL_FINAL['Z j2000'].values[indx]:15.5f}"\
                    +f"  {ORBFIL_FINAL['X_dot j2000'].values[indx]:15.5f}"\
                    +f"  {ORBFIL_FINAL['Y_dot j2000'].values[indx]:15.5f}"\
                    +f"  {ORBFIL_FINAL['Z_dot j2000'].values[indx]:15.5f}"\
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


def read_binary_ORBFIL(orb_fil):

    from pygeodyn.util_dir.time_systems import time_mjdsecs_tdt_to_utc
    from pygeodyn.util_dir.time_systems import mjds_to_ymdhms
    from scipy.io import FortranFile

    # orb_fil = file
    f = FortranFile(orb_fil, 'r')

    #### -----------------------------------------------------
    #### ------------------- HEADER RECORD -------------------
    #### -----------------------------------------------------
    ### Read the first record, this is the header buffer
    a = f.read_record(float)  # read the record with the required datatype


    #### Glean important variables
    NA     = int(a[2-1]) # Number of alphanumeric buffers to follow the header
    NC     = int(a[3-1]) # Number of card images in the GEODYN II input control deck
    NSATS  = int(a[7-1])  # Number of satellites on this file:  
    NWDSAT = int(a[8-1])  # Actual number of words per satellite per time point (NWDSAT <= 39).
    NWDATA = int(a[9-1])   #NSATS*NWDSAT
    NTIMBF = int(a[10-1]) # Number of time points per Data Buffer

    header= {}
    header['Number of alphanumeric data buffers to follow (NA)']             = a[2-1]
    header['Number of card images in the GEODYN II input control deck (NC)'] = a[3-1]
    header['Arc Number.']                                                    = a[4-1]
    header['Global Iteration Number']                                        = a[5-1]
    header['Inner Iteration Number']                                         = a[6-1]
    header['Number of satellites on this file']                              = a[7-1]  # upper limit of 50
    header['Actual number of words per satellite per time point']            = a[8-1]
    header['Number of words of data per time point (NWDATA=NSATS*NWDSAT)']   = a[9-1]
    header['Number of time points per Data Buffer (NTIMBF)']                 = a[10-1]
    header['Trajectory Start Date & Time in form YYMMDDHHMMSS .0D0 UTC']     = a[11-1]
    header['Fractional seconds of Start Time. UTC']                          = a[12-1]
    header['Trajectory Stop Date & Time in form YYMMDDHHMMSS .0D0 UTC']      = a[13-1]
    header['Fractional seconds of Stop Time. UTC']                           = a[14-1]
    header['Trajectory Start Date & Time in MJDS']                           = a[15-1] # (MJDS=(JD -2430000.5 D0 )*86400+ ISEC) ET
    header['Fractional seconds of Start Time']                               = a[16-1]
    header['Trajectory Stop Date & Time in MJDS']                            = a[17-1] # (MJDS=(JD -2430000.5 D0 )*86400+ ISEC) ET
    header['Fractional seconds of Stop Time. ET']                            = a[18-1]
    header['Nominal interval between trajectory times in seconds. ET ']      = a[19-1]
    header['Nominal number of trajectory times.']                            = a[20-1]
    header['Output S/C ephem ref sys(0 = TOD, 1 = TOR, 2 =  J2000)']         = a[22-1]
    # 
    header['Speed of Light.']                                     = a[101-1]
    header['GM for Earth.']                                       = a[102-1]
    header['Semi -major axis of Earth reference ellipsoid.']      = a[103-1]
    header['Equatorial Flattening of Earth reference ellipsoid.'] = a[104-1]
    header['Gravitational Potential Checksum.']                   = a[105-1]
    header['Maximum Degree of Gravitational Expansion.']          = a[106-1]
    header['Maximum Order of Gravitational Expansion.']           = a[107-1]   ### SKIP from 108 -200
    #
    #### PRESENCE ON FILE INDICATORS
    ## right ascension of Greenwich 
    header['Presence of right ascension of Greenwich for each time point in each Buffer'] = a[201-1]   
    ## Inertial State Vector
    header['Presence per Sat. of inertial X coordinate for each time point']    = a[202-1]   
    header['Presence per Sat. of inertial Y coordinate for each time point']    = a[203-1]   
    header['Presence per Sat. of inertial Z coordinate for each time point']    = a[204-1]   
    header['Presence per Sat. of inertial Xdot coordinate for each time point'] = a[205-1]   
    header['Presence per Sat. of inertial Ydot coordinate for each time point'] = a[206-1]   
    header['Presence per Sat. of inertial Zdot coordinate for each time point'] = a[207-1] 
    # 
    header['Presence per Sat. of geodetic latitude for each time point'] = a[208-1]   
    header['Presence per Sat. of east longitude for each time point']    = a[209-1]   
    # 
    header['Presence per Sat. of ECF X coordinate for each time point']  = a[210-1]   
    header['Presence per Sat. of ECF Y coordinate for each time point']  = a[211-1]   
    header['Presence per Sat. of ECF Z coordinate for each time point']  = a[212-1]   
    header['Presence per Sat. of ECF Xdot for each time point']         = a[213-1]   
    header['Presence per Sat. of ECF Ydot for each time point']         = a[214-1]   
    header['Presence per Sat. of ECF Zdot for each time point']         = a[215-1]   
    header['Presence per Sat. of polar motion X for each time point']   = a[216-1]   
    header['Presence per Sat. of polar motion Y for each time point']   = a[217-1]   
    header['Presence per Sat. of beta prime angle for each time point'] = a[218-1]   
    header['Presence per Sat. of yaw angle for each time point']        = a[219-1]   
    header['Presence per Sat. of orbit angle for each time point']      = a[220-1]   



    # break
    ##### Satellite ID ’s for all Satellites on File.
    ###       Trajectory data is ordered based upon order of these Satellite ID ’s.'
    for i in range(int(NSATS)):
        ii = i + 1
        index_sats = 300 + (ii)
        header['Satellite '+str(ii)+' ID'] = a[index_sats-1]  


    #### ----------------------------------------------------
    #### --------------- ALPHANUMERIC RECORDS ---------------
    #### ----------------------------------------------------
    #### We don't care about the Alphanumeric buffers so skip over them.
    for i in range(int(NA)):
        a = f.read_record(float)


    #### -----------------------------------------------------
    #### -------------- DATA + SENTINEL RECORDS --------------
    #### -----------------------------------------------------
    ### Read the Data records in a while loop.  
    ### When we hit the end_data_val, we have reached the
    ###    sentinel record and we can exit the while loop 
    ###    to read in the sentinel buffer.


    end_data_val           = 9000000000
    end_datarecord         = False
    data_dict_times        = {}
    data_dict_RA_greenwich = {}
    data_dict_sat_packets  = {}

    count_while = 0

    data_dict_sat_packets['MJDSEC ET']                       =[]
    data_dict_sat_packets['X j2000'] =[]
    data_dict_sat_packets['Y j2000'] =[]
    data_dict_sat_packets['Z j2000'] =[]
    data_dict_sat_packets['X_dot j2000']   =[]
    data_dict_sat_packets['Y_dot j2000']   =[]
    data_dict_sat_packets['Z_dot j2000']   =[]
    data_dict_sat_packets['Geodetic Latitude']     =[]
    data_dict_sat_packets['East Longitude']        =[]
    data_dict_sat_packets['Height']                =[]
    #         data_dict_sat_packets['Satellite ECF X coordinate']      =[]
    #         data_dict_sat_packets['Satellite ECF Y coordinate']      =[]
    #         data_dict_sat_packets['Satellite ECF Z coordinate']      =[]
    #         data_dict_sat_packets['Satellite ECF X velocity']        =[]
    #         data_dict_sat_packets['Satellite ECF Y velocity']        =[]
    #         data_dict_sat_packets['Satellite ECF Z velocity']        =[]
    #         data_dict_sat_packets['Polar Motion X']                  =[]
    #         data_dict_sat_packets['Polar Motion Y']                  =[]
    #         data_dict_sat_packets['Beta prime angle']                =[]
    #         data_dict_sat_packets['Yaw angle']                       =[]
    #         data_dict_sat_packets['Orbit Angle']                     =[]
    #         data_dict_sat_packets['Q(1)']                            =[]
    #         data_dict_sat_packets['Q(2)']                            =[]
    #         data_dict_sat_packets['Q(3)']                            =[]
    #         data_dict_sat_packets['Q(4)']                            =[]

    while end_datarecord == False:
        ### Read in each data buffer
        a = f.read_record(float)

        if not end_data_val in a:
            count_while+=1
            NTB    = int(a[5-1])  # Number of trajectory times in this Data Buffer (NTB <= NTIMBF ).
            MJDSBF = a[4-1]

            #### Trajectory Times in elapsed ET seconds from MJDSBF
            counter = 0
            for itime in np.arange( (6)   ,   ((NTB+5)  +1)  ):
                index_times = int(itime)
                data_dict_times[counter] = str(MJDSBF + a[index_times-1] )
                counter+=1

    #             if counter <= 100:
    #                 print(MJDSBF + a[index_times-1])


            #### Right Ascension of Greenwich Values (radians) for each time in Buffer.
            counter = 0
            for i in np.arange((NTIMBF+6) ,((NTIMBF+5 + NTB)+1)):
                counter+=1
                index = int(i)
                data_dict_RA_greenwich['Right Ascension of Greenwich Values '+ str(counter)] = a[index-1] 


            ##### Satellite Data Packets
            #####    first satellite 
            #####    first time point 
            counter = 0        
            first_sat_first_time = ((NSATS +1)* NTIMBF +6) + (NSATS -1)* NWDSAT #2* NTIMBF +6
            last_sat_last_time   = ((NSATS +1)* NTIMBF +5) + NSATS*NWDSAT*NTB #(((NSATS+1)* NTIMBF+5)+(NSATS*NWDSAT))

            for i in np.arange(first_sat_first_time, last_sat_last_time  , 24):
                index = int(i)

                data_dict_sat_packets['MJDSEC ET'].append(data_dict_times[counter])
                data_dict_sat_packets['X j2000'].append(a[(index +1) - 2])
                data_dict_sat_packets['Y j2000'].append(a[(index +2) - 2])
                data_dict_sat_packets['Z j2000'].append(a[(index +3) - 2])
                data_dict_sat_packets['X_dot j2000'].append(a[(index +4) - 2])
                data_dict_sat_packets['Y_dot j2000'].append(a[(index +5) - 2])
                data_dict_sat_packets['Z_dot j2000'].append(a[(index +6) - 2])
                data_dict_sat_packets['Geodetic Latitude'].append(a[(index +7) - 2])
                data_dict_sat_packets['East Longitude'].append(a[(index +8) - 2])
                data_dict_sat_packets['Height'].append(a[(index +9) - 2])
    #                     data_dict_sat_packets['Satellite ECF X coordinate'].append(a[(index +10) - 2])
    #                     data_dict_sat_packets['Satellite ECF Y coordinate'].append(a[(index +11) - 2])
    #                     data_dict_sat_packets['Satellite ECF Z coordinate'].append(a[(index +12) - 2])
    #                     data_dict_sat_packets['Satellite ECF X velocity'].append(a[(index +13) - 2])
    #                     data_dict_sat_packets['Satellite ECF Y velocity'].append(a[(index +14) - 2])
    #                     data_dict_sat_packets['Satellite ECF Z velocity'].append(a[(index +15) - 2])
    #                     data_dict_sat_packets['Polar Motion X'].append(a[(index +16) - 2])
    #                     data_dict_sat_packets['Polar Motion Y'].append(a[(index +17) - 2])
    #                     data_dict_sat_packets['Beta prime angle'].append(a[(index +18) - 2])
    #                     data_dict_sat_packets['Yaw angle'].append(a[(index +19) - 2])
    #                     data_dict_sat_packets['Orbit Angle'].append(a[(index +20) - 2])
    #                     data_dict_sat_packets['Q(1)'].append(a[(index +21) - 2])
    #                     data_dict_sat_packets['Q(2)'].append(a[(index +22) - 2])
    #                     data_dict_sat_packets['Q(3)'].append(a[(index +23) - 2])
    #                     data_dict_sat_packets['Q(4)'].append(a[(index +24) - 2])
                counter+=1


    #         print('counter',counter)    

        else:
            ####  If the the first index has +9000000000 we are at the sentinel record 
            #     which denotes the end of the data section.
    #                 print('----- End of file')
    #                 print('sentinel buffer indicator                       ',a[1-1])
    #                 print('Count of the number of Data Buffers. GEODYN     ',a[2-1])
    #                 print('GEODYN II Interface File creation date and time.',a[3-1])
    #                 print('GEODYN II -S version used.                      ',a[4-1])
    #                 print('GEODYN II -E version used.                      ',a[5-1])
    #                 print('spare                                           ',a[6-1])
    #                 print('spare                                           ',a[7-1])
            end_datarecord = True
            f.close()  #### be sure to close the file


    data_record_df = pd.DataFrame.from_dict(data_dict_sat_packets, orient='columns')

    #### Save as a dictionary
    orbfil_dict = {}
    orbfil_dict['header'] = header
    orbfil_dict['data_record'] = data_record_df

    ##### Convert from Terrestrial time to UTC:
    MJDS_UTC = [time_mjdsecs_tdt_to_utc(float(x), 37) \
                for x in orbfil_dict['data_record']['MJDSEC ET'] ]

    ##### Calculate the Gregorian Calendar date:
    yymmdd_str = [mjds_to_ymdhms(x) for x in MJDS_UTC]

    ##### Compute date as Datetime object:
    dates_dt_UTC = [pd.to_datetime( x, format='%y%m%d-%H%M%S') for x in yymmdd_str]


    orbfil_dict['data_record']["Date_UTC"] = dates_dt_UTC
    orbfil_dict['data_record']["MJDS_UTC"] = MJDS_UTC

    return(orbfil_dict)

def get_timechopped_trajdata_1(path_binary, arc_files):
  
    from   os.path import exists

    total_files=np.size(arc_files)
    tabtab = '     '

    print(tabtab,'Running through the pre-processing procedure...')
    print(tabtab,'=======================================================')
    print(tabtab,'STEP 1: Convert TRAJ binary files to pandas DataFrame...')
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

        file = pd.to_datetime(file, format = '%Y.%j').strftime('%Y%m%d')
        __traj_filename =  path_binary + f"geodyn.traj.{file}"
        filenum  =i+1 
        print(tabtab, '--- File %i / %i'% (filenum, total_files))

        if not exists(__traj_filename):
            print(f"File was not found.  Skipping iteration.")
            print(f"File: {file}")
            continue
        
        else:

            print(tabtab, '----- Loading ', file  )




            # Read in the binary data into a dict w/ pd.dataframes
            traj_data = read_binary_ORBFIL(__traj_filename)
            
            traj_data = pd.DataFrame.from_dict(traj_data['data_record']) 
                
            if count == 0:
                df1 = traj_data
                count += 1
            else:
                # to append df2 at the end of df1 dataframe
                df1 = pd.concat([df1, traj_data])


            df1 = df1.drop_duplicates(subset=["Date_UTC"], keep='first'\
            ).sort_values(by='Date_UTC'\
                            ).reset_index(drop=True)

            del traj_data
            gc_collect()

    #         print(tabtab,'Zipping file...', file)
    #             os.system('gzip -v '+file)

            print()

    TRAJ_FINAL = df1
    return(TRAJ_FINAL)









# panels_str = ['PANEL  0 0 1 1  02012003 1.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 1 2  02012003 0.9551567000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 1 3  02012003 0.4000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 1 4  02012003 0.2600000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 1 5  02012003 0.6200000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 1 6  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 1 7  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 1 8  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 1 9  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 110  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 2 1  02012003-1.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 2 2  02012003 0.9551567000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 2 3  02012003 0.4000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 2 4  02012003 0.2600000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 2 5  02012003 0.6200000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 2 6  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 2 7  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 2 8  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 2 9  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 210  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 3 1  02012003 0.0000000000000E+00 0.76604400E+00-0.642787E+00 0.0E+00',
# 'PANEL  0 0 3 2  02012003 3.1554792000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 3 3  02012003 0.0500000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 3 4  02012003 0.3000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 3 5  02012003 0.8100000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 3 6  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 3 7  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 3 8  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 3 9  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 310  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 4 1  02012003 0.0000000000000E+00-0.76604400E+00 0.642787E+00 0.0E+00',
# 'PANEL  0 0 4 2  02012003 0.2282913000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 4 3  02012003 0.4000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 4 4  02012003 0.2600000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 4 5  02012003 0.6200000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 4 6  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 4 7  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 4 8  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 4 9  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 410  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 5 1  02012003 0.0000000000000E+00-0.76604400E+00-0.642787E+00 0.0E+00',
# 'PANEL  0 0 5 2  02012003 3.1554792000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 5 3  02012003 0.0500000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 5 4  02012003 0.3000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 5 5  02012003 0.8100000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 5 6  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 5 7  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 5 8  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 5 9  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 510  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 6 1  02012003 0.0000000000000E+00+0.76604400E+00+0.642787E+00 0.0E+00',
# 'PANEL  0 0 6 2  02012003 0.2282913000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 6 3  02012003 0.4000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 6 4  02012003 0.2600000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 6 5  02012003 0.6200000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 6 6  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 6 7  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 6 8  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 6 9  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 610  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 7 1  02012003 0.0000000000000E+00 0.00000000E+00 1.000000E+00 0.0E+00',
# 'PANEL  0 0 7 2  02012003 6.0711120000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 7 3  02012003 0.6800000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 7 4  02012003 0.2000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 7 5  02012003 0.7500000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 7 6  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 7 7  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 7 8  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 7 9  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 710  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 8 1  02012003 0.0000000000000E+00 0.00000000E+00-1.000000E+00 0.0E+00',
# 'PANEL  0 0 8 2  02012003 2.1673620000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 8 3  02012003 0.0500000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 8 4  02012003 0.3000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 8 5  02012003 0.8100000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 8 6  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 8 7  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 8 8  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 8 9  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 810  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 9 1  02012003 1.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 9 2  02012003 0.0461901000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 9 3  02012003 0.4000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 9 4  02012003 0.2600000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 9 5  02012003 0.6200000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 9 6  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 9 7  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 9 8  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 9 9  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 0 910  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 010 1  02012003-1.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 010 2  02012003 0.0461901000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 010 3  02012003 0.4000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 010 4  02012003 0.2600000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 010 5  02012003 0.6200000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 010 6  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 010 7  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 010 8  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 010 9  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 01010  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 011 1  02012003 0.0000000000000E+00 1.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 011 2  02012003 0.0461901000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 011 3  02012003 0.4000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 011 4  02012003 0.2600000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 011 5  02012003 0.6200000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 011 6  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 011 7  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 011 8  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 011 9  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 01110  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 012 1  02012003 0.0000000000000E+00-1.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 012 2  02012003 0.0461901000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 012 3  02012003 0.4000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 012 4  02012003 0.2600000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 012 5  02012003 0.6200000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 012 6  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 012 7  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 012 8  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 012 9  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',
# 'PANEL  0 01210  02012003 0.0000000000000E+00 0.00000000E+00 0.000000E+00 0.0E+00',]

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


# num_of_panels = panel_num

# for iparam,paramval in enumerate([1,2,3,4,5,6,7,8,9,10]):   # ]):
#     param = str(paramval).rjust(2,' ')

    
    
# PanelModel = {\
#     'Area (m^2)'          : [],
#     'Moves=1 Fixed=0'     : [],
#     'Diffuse'             : [],
#     'Emissivity'          : [],
#     'Specular'            : [],
#     'RadiationFreq Both=0': [],
#     'Normal Vector X'     : [],
#     'Normal Vector Y'     : [],
#     'Normal Vector Z'     : [],
#     #                                     
#     'Temperature A'       : [],
#     'Temperature C'       : [],
#     'Temperature rate D'  : [],
#     'Temperature rate F'  : [],
#     'Temperature rotate X': [],
#     }


# for i in np.arange(1,num_of_panels+1):
#     PanelModel['Area (m^2)'].append(          float(A[i]))
#     PanelModel['Moves=1 Fixed=0'].append(     float(motion_ind[i]))
#     PanelModel['Diffuse'].append(             float(bwdiff[i]))
#     PanelModel['Emissivity'].append(          float(emissivity[i]))
#     PanelModel['Specular'].append(            float(bwspec[i]))
#     PanelModel['RadiationFreq Both=0'].append(float(freq_ind[i]))
#     PanelModel['Normal Vector X'].append(     float(nrm_sbf[i][0]))
#     PanelModel['Normal Vector Y'].append(     float(nrm_sbf[i][1]))
#     PanelModel['Normal Vector Z'].append(     float(nrm_sbf[i][2]))
#     PanelModel['Temperature A'].append(       float(temp_A[i]))
#     PanelModel['Temperature C'].append(       float(temp_C[i]))
#     PanelModel['Temperature rate D'].append(  float(temp_decay_D[i]))
#     PanelModel['Temperature rate F'].append(  float(temp_decay_F[i]))
#     PanelModel['Temperature rotate X'].append(float(temp_rot_X[i]))
# from tabulate import tabulate
# fmt = 'pretty'
# # PanelModel = {}
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
# table.append( ["Moves=1|Fixed=0"]+PanelModel["Moves=1 Fixed=0"][:7])
# table.append( ["Specular"]+PanelModel["Specular"][:7])
# table.append( ["Diffuse"]+PanelModel["Diffuse"][:7])
# table.append( ["Emissivity"]+PanelModel["Emissivity"][:7])
# table.append( ["RadiationFreq|Both=0"]+PanelModel["RadiationFreq Both=0"][:7])
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
# table.append( ["Moves=1|Fixed=0"]+PanelModel["Moves=1 Fixed=0"][ind:])
# table.append( ["Specular"]+PanelModel["Specular"][ind:])
# table.append( ["Diffuse"]+PanelModel["Diffuse"][ind:])
# table.append( ["Emissivity"]+PanelModel["Emissivity"][ind:])
# table.append( ["RadiationFreq|Both=0"]+PanelModel["RadiationFreq Both=0"][ind:])
# table.append( [" "] + [" "]*len(param_headers1))
# table.append( ["Temperature A"]+PanelModel["Temperature A"][ind:])
# table.append( ["Temperature C"]+PanelModel["Temperature C"][ind:])
# table.append( ["Temperature rate D"]+PanelModel["Temperature rate D"][ind:])
# table.append( ["Temperature rate F"]+PanelModel["Temperature rate F"][ind:])
# table.append( ["Temperature rotate X"]+PanelModel["Temperature rotate X"][ind:])
# print(tabulate(table, headers=param_headers2, tablefmt=fmt))
