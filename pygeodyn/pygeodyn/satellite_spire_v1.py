"""Module for Runnning the Spire Cubesats Lemur2v3.3.45 in GEODYN


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
# from pygeodyn.prep_inputs    import PrepareInputs


class InheritControlStages(RunController):
    """Class that enable satellite to inherit classes"""
    def __init__(self):
        RunController.__init__(self)
class InheritReadRaw(ReadRawOutput):
    """Class that enable satellite to inherit classes"""
    def __init__(self):
        ReadRawOutput.__init__(self)

list_run_types = ["DataReduction_PCE" , "OrbitPropagation"] 




class Spire_Lemur2_v33(RunController, ReadRawOutput):
    """Class with config for running Pygeodyn with Spire cubesats.
    
    In the Pygeodyn Infrastructure it inherits the RunController and
    ReadRawOutput.  Class with satellite specific configuration for running 
    Pygeodyn with Spire.
       
        # file_sat_details = self.path_data_inputs_raw+'/' \
        #                   + f"attitude/{taskorder}/"     \
        #                   + f"Spire_Satellite_List_for_NASA_BPA.csv"
        # sat_details = pd.read_csv(file_sat_details, sep = ',',dtype=str)

        import string
        alph2num=dict(zip(string.ascii_uppercase, range(1,27)))
        cospar_ID[2:4]+cospar_ID[5:8]+f"{alph2num[cospar_ID[-1]]:02d}"

    """
 


    def __init__(self):       
        ### Initialize the class to contain all methods
        InheritControlStages.__init__(self)
        InheritReadRaw.__init__(self)
        ### Call the control path pointer to establish attributed paths
        self.ctrlStage1_setup_path_pointers(skip_files = True)


        ### Time period selector (based on task order)
        taskorder = "20180923_20181209_TaskOrder3Data"
        
        raw_attitudepath = self.path_data_inputs_raw+'/data_Spire/'  \
                            + f"attitude/{taskorder}/"     \


        self.raw_satinput = {}
        print(self.prms['satellite'])

        ### Determine which Spire satellite is being called:
        if self.prms['satellite']  == 'spire083':
            satnum                 = 83
            self.prms['sat_ID']    = '1804607'   # cospar-id #
            self.raw_satinput['att_path'] = raw_attitudepath \
                    + f"leoAtt_2018-09-23T00-12-00Z.9999999.{satnum:03d}.log"

        elif self.prms['satellite']  == 'spire084':
            satnum                 = 84
            self.prms['sat_ID']    = '1804606'   # cospar-id #
            self.raw_satinput['att_path'] =  raw_attitudepath \
                    +  f"leoAtt_2018-09-23T00-06-13Z.9999999.{satnum:03d}.log"

        elif self.prms['satellite']  == 'spire085':
            satnum                 = 85
            self.prms['sat_ID']    = '1804605'   # cospar-id #
            self.raw_satinput['att_path'] =  raw_attitudepath \
                    +  f"leoAtt_2018-09-23T00-16-01Z.9999999.{satnum:03d}.log"
        else:
            print('Check satellite name in Spire_lemur2_3.3 class')

        self.prms['satnum'] = satnum

        ###----------------------------------------
        ### Universal Properties for Spire cubesats Lemur-2v3.3.45
        ###----------------------------------------
        self.prms['sat_area_cross_sec']      = 4.5          # estimate (m^2), intentionally too large
        self.prms['sat_mass']                = 4.933        # estimate (kg)
        self.prms['sat_geometry_panel_num']  = 8 
        #
        self.prms['coord_ref_system']     = 'j2000'      # j2000 is default for pygeodyn
        self.prms['orbit_elements_form']  = 'cartesian'  # (meters), XYZ
        self.prms['bool_exatfiles']       =  True        # use EXTAT file
        self.prms['number_of_satellites'] =  1
        
        
        # if self.prms['initialize']:
        dt_2days = pd.Series(pd.to_timedelta(48,'h'))
        # datestr1 = (pd.to_datetime(self.prms['epoch_start'][0])-dt_2days).dt.strftime('%Y%m%d')[0]    
        # datestr2 = (pd.to_datetime(self.prms['epoch_start'][-1])+dt_2days).dt.strftime('%Y%m%d')[0] 
        datestr1 = pd.to_datetime(self.prms['epoch_start'][0]).strftime('%Y%m%d')    
        datestr2 = pd.to_datetime(self.prms['epoch_start'][-1]).strftime('%Y%m%d') 
        self.raw_satinput['daterange'] = f"{datestr1}_{datestr2}"
        
        ### Include paths to the raw_satinput datafiles/directories and assorted
        ### data format/settings.
                                        ## must be converted to UTC and J2000
        if self.prms['initialize']:
            self.raw_satinput['ephem_path'] = self.path_data_inputs +'/'\
                        +f'sat_spire{satnum:03d}/g2b/'  \
                        +f'Spire{satnum:03d}_RawEphem_{datestr1}_{datestr2}.txt'
        else:
            self.raw_satinput['ephem_path'] = self.path_data_inputs +'/'\
                        +f"sat_spire{satnum:03d}/g2b/"\
                        +f"Spire{satnum:03d}_RawEphem__20181101_20181130.txt"
                        # +f"Spire{satnum:03d}_RawEphem__20181101_20181130.txt"
        
        # self.raw_satinput['ephem_days'] =  [pd.to_datetime(i).day \
        #                         for i in self.prms['epoch_start']]        
        ephem_days =  [pd.to_datetime(i).day \
                            for i in self.prms['epoch_start']]        
        self.raw_satinput['ephem_days'] = \
                        np.arange(ephem_days[0]-2, ephem_days[-1]+3 )


        #                  Parameter              Raw Data       GEODYN Req
        #                  -------------------   ------------    -------------
        self.raw_satinput['att_quat___refsys'] = 'SBF_to_RSW'    # SBF_to_J2000
        self.raw_satinput['att_posvel_refsys'] = 'eci_teme'      # j2000
        self.raw_satinput['att_date']          = 'date_gps'      # date_tdt
        self.raw_satinput['att_interval']      = 10 #seconds     
        ### Ephemeris (OD or POD)

        #                  Parameter              Raw Data       GEODYN Req
        #                  -------------------   ------------    -------------
        self.raw_satinput['ephem_posvel_refsys'] = 'ecef_igs08'   # j2000
        self.raw_satinput['ephem_date']          = 'date_gps'     # date_utc
        #
        # Directory that holds the raw data:
        self.raw_satinput['ephem_path_dir']  =  self.path_data_inputs_raw \
                                             +'/data_Spire/arcs'


        ### maybe some sort of list written to a file that saves the correct version of
         ### the various inputs files would be the right idea.....


        ### ------------------
        ###  Run-type Options
        ### ------------------
        #
        ###  Fill in the appropriate settings based on the run_type.
        #----------------------------------------------------------------------
        if self.prms['run_type'] == "DataReduction_PCE":
            self.tracking_data_type   = 'PCE'
            #### G2B file name
            if self.prms['initialize']:
                self.filename_g2b         = f"pce_spire{satnum:03d}_leoOrb_{self.raw_satinput['daterange']}" 
            else:
                self.filename_g2b = f"pce_spire{satnum:03d}_leoOrb_20181101_20181130" 
                # self.filename_g2b = f"pce_icesat2_pso_20181108_20181124" 
            # self.filename_g2b         = f"pce_spire{satnum:03d}_leoOrb_20181101_20181130"
            # self.filename_g2b         = f"pce_spire{satnum:03d}_leoOrb_20181107_20181111"
            #### PCE Ascii textfile name
            if self.prms['which_ICfile'] is not None:
                self.file_statevector_ICs = self.prms['which_ICfile']
            else:
                self.file_statevector_ICs = self.dir_input+'/'\
                        +f"Spire{satnum:03d}_initialconditions_"\
                        +f"{self.raw_satinput['daterange']}_v1.txt"

            ### Identify all the header names in the file
            self.file_statevector_headers   = ['Date',
                                                'X',
                                                'Y',
                                                'Z',
                                                'X_dot',
                                                'Y_dot',
                                                'Z_dot']
        #----------------------------------------------------------------------
        elif self.prms['run_type']  == "OrbitPropagation":
                self.filename_g2b = 'None'
                self.file_statevector_ICs   = self.dir_input\
                            +f'/Spire{satnum:03d}_initialconditions_Nov2018_v2.txt'
                # print("self.file_statevector_ICs",self.file_statevector_ICs)
                #'/data/SatDragModelValidation/data/inputs/sat_spire83/setups'\
#                        + '/statevector_ICs.txt'
        #----------------------------------------------------------------------
        else:
            print("Run Settings Error: User input bad option as run_type.")
            print("    bad input:           ",self.run_type)
            print("    available run types: ",list_run_types)
            sys.exit(0)

        ###-------------------------------------------------
        ### Use the Global Files from ICESat-2 PSO from 2018
        ###------------------------------------------------
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
            ### ToDo: - Make these global options the Basic/barebones options for 
            ###         the setup file 
            print("Run Settings Error: User input bad option as global_options.")
            print("    bad input:           ",self.prms['global_options'])
            print("    available run types: ", '***ADD***')
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





    def sat_process_raw_ephemeris(self, verbose=False):

        from astropy import coordinates as coord
        from astropy import units as u
        from astropy.time import Time

        from datetime import datetime,timedelta
        from pygeodyn.pygeodyn.satellite_spire_v1 import read_SpireLeoOrbPOD_sp3c
        import time
        ## Load coordinate transformation functions
        # from pygeodyn.util_dir.coordinate_systems import iau06_Call_ecef2eci
        from pygeodyn.util_dir.time_systems import time_gps_to_utc,\
                                                get_leapseconds, jday           

        spire_sat_num = int(self.prms['satnum'])
        # days = self.raw_satinput['ephem_days']

        raw_ephem = self.raw_satinput['ephem_path']
        ephem_path_dir = self.raw_satinput['ephem_path_dir']

        dt_2days = pd.Series(pd.to_timedelta(48,'h'))

        startdate = pd.to_datetime(self.prms['epoch_start'][0])
        enddate   = pd.to_datetime(self.prms['epoch_start'][-1])
        startdate_dt = pd.to_datetime(startdate, format='%Y-%m-%d')
        enddate_dt   = pd.to_datetime(enddate,   format='%Y-%m-%d')
        starts_linspace_dt = pd.date_range(start=startdate_dt ,
                                             end=enddate_dt   ,
                                            freq=str(1)+"D")
        # starts_linspace_dt
        if verbose: print(f"{self.tabtab} -SPIRE")

        if verbose: print(f"{self.tabtab} - processing raw satellite ephemerides from files.")
        #### Find files throught dir tree containing Satellite's LeoOrb POD
        files_with_sat = []

        if verbose: print(f"{self.tabtab} - for dates: {starts_linspace_dt}")

        ## Loop through the daily directories
        for iday,day in enumerate(starts_linspace_dt):
            dir_set   = ephem_path_dir + f"/{day.strftime('%Y-%m-%d')}/"

            ## Loop through the arc directories for that day,
            ## Identify the leoOrb..._{sat#}.sp3 (Spire "POD") files
            for root, dirs, files in os.walk(dir_set, topdown=False):
                for name in files:
                    if 'leoOrb'in name and f'{spire_sat_num:03d}.sp3' in name:
                        files_with_sat.append(os.path.join(root, name))

        if len(files_with_sat) == 0:
            print('**Error in sat_process_raw_ephemeris()')
            print('        did not find files')
            print('        dir_set : ', dir_set)
            print('        root    : ', root)
            print('        dirs    : ', dirs)
            print('        files   : ', files)
            
            sys.exit(0)


        del root, dirs, files, name
        del dt_2days,startdate,enddate,startdate_dt,enddate_dt
        del starts_linspace_dt, iday, day
        


        #### Load raw data from all found files
        leoOrb_ecef = read_SpireLeoOrbPOD_sp3c(files_with_sat)
        del files_with_sat
        gc.collect()



        ### Get the leapseconds for time period of interest
        dAT = get_leapseconds(leoOrb_ecef['date_gps'][0].year,
                              leoOrb_ecef['date_gps'][0].month,
                              leoOrb_ecef['date_gps'][0].day)
        
        ## Convert to UTC
        leoOrb_ecef['date_utc'] = [time_gps_to_utc(time, dAT) \
                                    for time in leoOrb_ecef['date_gps']]
        del leoOrb_ecef['date_gps']
        del leoOrb_ecef['clock_microsec']
        gc.collect()


        ### Convert all ephemerides from ECEF-igs08 to ECI-j2000
        start = time.time()
        conv_dms2kms = 1/10000.
        #
        len_ecef = np.shape(leoOrb_ecef['date_utc'])[0]    
        
        if verbose: print(f"{self.tabtab} - Converting from ECEF to J2000 using IAU2010 conventions, and ")
        if verbose: print(f"{self.tabtab}   saving satellite ephemeris to single file.")

        f = open(raw_ephem, "w")
        f.write("\n")
        f.close()
        

        leoOrb_eci={}

        ecef = coord.ITRS(x  =leoOrb_ecef['x_km']*1000*u.m,
                        y  =leoOrb_ecef['y_km']*1000*u.m,
                        z  =leoOrb_ecef['z_km']*1000*u.m,
                        v_x=leoOrb_ecef['xdot_dms']*conv_dms2kms*1000*u.m/u.s,
                        v_y=leoOrb_ecef['ydot_dms']*conv_dms2kms*1000*u.m/u.s,
                        v_z=leoOrb_ecef['zdot_dms']*conv_dms2kms*1000*u.m/u.s, 
                        representation_type='cartesian', 
                        differential_type='cartesian', 
                        obstime=Time(leoOrb_ecef['date_utc']))
        j2000 = ecef.transform_to(coord.GCRS(obstime=Time(leoOrb_ecef['date_utc'])))
        leoOrb_eci['X_m_eci']    = j2000.cartesian.x.value
        leoOrb_eci['Y_m_eci']    = j2000.cartesian.y.value
        leoOrb_eci['Z_m_eci']    = j2000.cartesian.z.value
        leoOrb_eci['Xdot_m_eci'] = j2000.cartesian.differentials['s'].d_x.value*1000
        leoOrb_eci['Ydot_m_eci'] = j2000.cartesian.differentials['s'].d_y.value*1000
        leoOrb_eci['Zdot_m_eci'] = j2000.cartesian.differentials['s'].d_z.value*1000



        ### Write to file
        with open(raw_ephem, 'r+') as file:

            #### Manually write the header units
            header_units =\
                    f"{'UTC'.rjust(len(str(leoOrb_ecef['date_utc'][1]))-1,' ') }"\
                    +f"  {'(m)'.rjust(15,' ')}"\
                    +f"  {'(m)'.rjust(15,' ')}"\
                    +f"  {'(m)'.rjust(15,' ')}"\
                    +f"  {'(m/s)'.rjust(15,' ')}"\
                    +f"  {'(m/s)'.rjust(15,' ')}"\
                    +f"  {'(m/s)'.rjust(15,' ')}"\

            #### Manually write the header field names
            header_names =\
                    f"{'Date'.rjust(len(str(leoOrb_ecef['date_utc'][1]))-1,' ') }"\
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
###     Satellite: Spire_{self.prms['satnum']:03d} ({self.prms['sat_ID']})
###     Epoch: +start____ {leoOrb_ecef['date_utc'][0]} 
###            +stop_____ {leoOrb_ecef['date_utc'][-1]}
###     Last modified: {datetime.now()-timedelta(hours=7)}
###
### Source
### -------
###     {self.raw_satinput['ephem_path_dir']}
###     (leoOrb... .sp3 files for indiv. arcs)
###
### Contents
### --------
###     Date:  (YYYY-MM-DD hh:mm:ss) (UTC, converted from gps time)
###     Ephem:  Position and velocity (X, Y, Z, X_dot, Y_dot, Z_dot)
###             coordinate: ECI-J2000
###              (converted from ECEF-IGS08 using IAU-2010 p+n conventions)
###              unit: m
###
#{header_units}
#{header_names}
### %eoh
'''
            print(np.shape(leoOrb_eci['X_m_eci']))
        # leoOrb_eci['X_m_eci']    = j2000.cartesian.x.value
        # leoOrb_eci['Y_m_eci']    = j2000.cartesian.y.value
        # leoOrb_eci['Z_m_eci']    = j2000.cartesian.z.value
        # leoOrb_eci['Xdot_m_eci'] = j2000.cartesian.differentials['s'].d_x.value
        # leoOrb_eci['Ydot_m_eci'] = j2000.cartesian.differentials['s'].d_y.value
        # leoOrb_eci['Zdot_m_eci'] = j2000.cartesian.differentials['s'].d_z.value

            file.write(header_meta)  
            for indx,valdate in enumerate(leoOrb_ecef['date_utc']):
                ## position vector earth fixed  ( km )
                # recef = [leoOrb_ecef['x_km'][indx],  
                #         leoOrb_ecef['y_km'][indx], 
                #         leoOrb_ecef['z_km'][indx]]
                # ## velocity vector earth fixed  (km/s)
                # vecef = [leoOrb_ecef['xdot_dms'][indx]*conv_dms2kms, 
                #         leoOrb_ecef['ydot_dms'][indx]*conv_dms2kms,
                #         leoOrb_ecef['zdot_dms'][indx]*conv_dms2kms]
                # ### convert the data to correct coordinate ref frame
                # (reci, veci, _)= iau06_Call_ecef2eci(\
                #                             recef, vecef, None,
                #                             leoOrb_ecef['date_utc'][indx].year  , 
                #                             leoOrb_ecef['date_utc'][indx].month , 
                #                             leoOrb_ecef['date_utc'][indx].day   , 
                #                             leoOrb_ecef['date_utc'][indx].hour  , 
                #                             leoOrb_ecef['date_utc'][indx].minute, 
                #                             leoOrb_ecef['date_utc'][indx].second, 
                #                             calc_accel = False)
            #### Manually write each row of the data.
                row =   f"{leoOrb_ecef['date_utc'][indx]}"\
                    +f"  {leoOrb_eci['X_m_eci'][indx]:15.5f}"\
                    +f"  {leoOrb_eci['Y_m_eci'][indx]:15.5f}"\
                    +f"  {leoOrb_eci['Z_m_eci'][indx]:15.5f}"\
                    +f"  {leoOrb_eci['Xdot_m_eci'][indx]:15.5f}"\
                    +f"  {leoOrb_eci['Ydot_m_eci'][indx]:15.5f}"\
                    +f"  {leoOrb_eci['Zdot_m_eci'][indx]:15.5f}"\
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
    dict_data["date_gps"] = []
    dict_data['q_SBF_to_RSW'] = []
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
                if  pd.to_datetime(date) in dict_data['date_gps']:
                    #print('Found a copy, SKIP', date)
                    noSkip_flag=False
                    continue # to next line of for loop
                else:
                    noSkip_flag = True
                    dict_data['date_gps'].append( pd.to_datetime(date))

            elif line[0:4]=="sca " and noSkip_flag:
                qx = float(line[4:17] )
                qy = float(line[18:31])
                qz = float(line[32:45])
                qw = float(line[46:59])
                dict_data['q_SBF_to_RSW'].append(np.array([qx, qy, qz, qw]))

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

    del dict_data

    # print(SpireDF.head())


    #### Mask the data according to the time period of interest
    startDT = pd.to_datetime(start_date, format='%Y-%m-%d %H:%M:%S')
    stopDT  = pd.to_datetime(stop_date,  format='%Y-%m-%d %H:%M:%S') \
                                                        + pd.to_timedelta(1,'d')

    # mask_month = SpireDF['date_gps'].dt.month==  startDT.month
    # mask_days = np.logical_and(SpireDF['date_gps'].dt.day  >=  startDT.day,
    #                            SpireDF['date_gps'].dt.day  <=  stopDT.day)
    # mask       = mask_month & mask_days
    # SpireDF =  SpireDF[:][mask].reset_index(drop=True)
    
    query = SpireDF.query(f"{startDT.year}{startDT.month:02d}{startDT.day:02d}"\
                         +f" < date_gps < "\
                         +f"{stopDT.year}{stopDT.month:02d}{stopDT.day:02d}")

    SpireDF =  query.reset_index(drop=True)


    gc.collect()
    return(SpireDF)









# def aux__make_file_statevector_ICs():

#     # import pandas as pd

#     # file_statevector_ICs = '/data/SatDragModelValidation/data/inputs/sat_spire83/setups'\
#     #                     + '/statevector_ICs.txt'

#     # prms_arc = {}
#     # prms_arc['epoch_start'] = "2018-11-09 00:00:00"
#     # prms_arc['epoch_stop']  = "2018-11-10 00:00:00"
#     # prms_arc['epoch_startDT'] = pd.to_datetime(prms_arc['epoch_start'],\
#     #                                             format='%Y-%m-%d %H:%M:%S')
#     # prms_arc['epoch_stopDT']  = pd.to_datetime(prms_arc['epoch_stop'],\
#     #                                             format='%Y-%m-%d %H:%M:%S')

#     import pandas as pd
#     import numpy as np
#     from datetime import datetime
#     from pygeodyn.util_dir.time_systems import time_gps_to_utc

#     def load_attitude_spire(filename):
#         Spire_dict = {}
#         Spire_dict['info']  = """fill info here"""


#         dict_data = {}
#         dict_data['date_gps'] = []
#         dict_data['q_SBF_to_RSW'] = []
#     #     dict_data['pos (eci)'] = []
#     #     dict_data['vel (eci)'] = []
#         dict_data['X'] = []
#         dict_data['Y'] = []
#         dict_data['Z'] = []
#         dict_data['X_dot'] = []
#         dict_data['Y_dot'] = []
#         dict_data['Z_dot'] = []

#         ### Loop through the file
#         with open(filename, 'r') as f:
#             for l_no, line in enumerate(f): 
#                 if line[0:4]=="tim ":
#                     yr    = int(line[4:8]   )
#                     mon   = int(line[9:11]  )
#                     day   = int(line[12:14] )
#                     hr    = int(line[15:17] )
#                     minute= int(line[18:20] )
#                     sec   = int(line[21:23] )
#                     msec  = int(line[24:30] )
#                     date = datetime(yr,mon, day, hr,minute,sec, msec )
#                     if  pd.to_datetime(date) in dict_data['date_gps']:
#                         #print('Found a copy, SKIP', date)
#                         noSkip_flag=False
#                         continue # to next line of for loop
#                     else:
#                         noSkip_flag = True
#                         dict_data['date_gps'].append( pd.to_datetime(date))

#                 elif line[0:4]=="sca " and noSkip_flag:
#                     qx = float(line[4:17] )
#                     qy = float(line[18:31])
#                     qz = float(line[32:45])
#                     qw = float(line[46:59])
#                     dict_data['q_SBF_to_RSW'].append(np.array([qx, qy, qz, qw]))

#                 elif line[0:4]=="pvi "and noSkip_flag:
#                     x    = float(line[4:17] )
#                     y    = float(line[18:31])
#                     z    = float(line[32:45])
#                     xdot = float(line[46:59])
#                     ydot = float(line[60:73])
#                     zdot = float(line[74:87])
#     #                 dict_data['pos (eci)'].append(np.array([x, y, z]))
#     #                 dict_data['vel (eci)'].append(np.array([xdot,ydot,zdot]))
#                     dict_data['X'].append(x)
#                     dict_data['Y'].append(y)
#                     dict_data['Z'].append(z)
#                     dict_data['X_dot'].append(xdot)
#                     dict_data['Y_dot'].append(ydot)
#                     dict_data['Z_dot'].append(zdot)

#         Spire_dict['units'] = {'date_gps': "pd.datetime" ,
#                             'q_SBF_to_RSW':  "" ,
#                             'X':  "m" ,
#                             'Y':  "m" ,
#                             'Z':  "m" ,
#                             'X_dot':  "m/s" ,
#                             'Y_dot':  "m/s" ,
#                             'Z_dot':  "m/s" ,
#                             }
#         Spire_dict['data'] = dict_data


#         #### Make a Dataframe for easier utility                   
#     #     SpireDF = pd.DataFrame.from_dict(dict_data)

#         return(Spire_dict)

#     dir_spire='/data/SatDragModelValidation/data/inputs/sat_spire83/data_Spire/'
#     file_spire = dir_spire+'attitude/20180923_20181209_TaskOrder3Data/'\
#                         +'leoAtt_2018-09-23T00-12-00Z.9999999.083.log'
#     statevector_ICs = load_attitude_spire(file_spire)
#     del statevector_ICs['data']['q_SBF_to_RSW']
#     del statevector_ICs['units']['q_SBF_to_RSW']


#     ### Convert from GPS time to UTC time
#     tim_utc = [datetime.strftime(time_gps_to_utc(tim, leap_sec=37), '%y%m%d%H%M%S.%f' )
#                                     for tim in statevector_ICs['data']['date_gps'] ]


#     statevector_ICs['data']['Date'] =   tim_utc 

#     ### Update the units to reflect the above changes:
#     del statevector_ICs['data']['date_gps']
#     del statevector_ICs['units']['date_gps']
#     statevector_ICs['units']['Date'] = 'UTC'

#     ### Update the metadata.  This will become the file header.
#     statevector_ICs['info']= 'fill info here'



#     from  datetime import datetime, timedelta

#     ### Write to an ascii text file. 
#     file_save = '/data/SatDragModelValidation/data/inputs/sat_spire83/setups'\
#                     + '/statevector_ICs.txt'
#     # 
#     with open(file_save, 'r+') as file:
#         #### Manually write the header units
#         header_units =\
#                     f"{'UTC'.rjust(len(str(statevector_ICs['data']['Date'][1]))-1,' ') }"\
#                 +f"  {'(m)'.rjust(15,' ')}"\
#                 +f"  {'(m)'.rjust(15,' ')}"\
#                 +f"  {'(m)'.rjust(15,' ')}"\
#                 +f"  {'(m/s)'.rjust(15,' ')}"\
#                 +f"  {'(m/s)'.rjust(15,' ')}"\
#                 +f"  {'(m/s)'.rjust(15,' ')}"\

#         #### Manually write the header field names
#         header_names =\
#                     f"{'Date'.rjust(len(str(statevector_ICs['data']['Date'][1]))-1,' ') }"\
#                 +f"  {'X'.rjust(15,' ')}"\
#                 +f"  {'Y'.rjust(15,' ')}"\
#                 +f"  {'Z'.rjust(15,' ')}"\
#                 +f"  {'X_dot'.rjust(15,' ')}"\
#                 +f"  {'Y_dot'.rjust(15,' ')}"\
#                 +f"  {'Z_dot'.rjust(15,' ')}"\



#         #### Manually write the detailed header description
#         header_meta = \
#     f'''### Initial conditions file
#     ### -----------------------
#     ###     Satellite: Spire_083 (1804607)
#     ###     Epoch: +start____ 2018  9 23  0 12  0.0300000 
#     ###            +stop_____ 2018 12  9 23 43 35.9080000 
#     ###     Last modified: {datetime.now()-timedelta(hours=7)}
#     ###
#     ### Source
#     ### -------
#     ###     leoAtt_2018-09-23T00-12-00Z.9999999.083.log
#     ###     (long quaternion file)
#     ###     %spire version 1.3  revision   1 2019 07 05 00:00 Spire     Spire Processing Center
#     ###
#     ### Contents
#     ### --------
#     ###     Date: (YYYY-MM-DD hh:mm:ss.ssssss) (UTC, converted from gps time)
#     ###     pvi: Position and velocity (X, Y, Z, X_dot, Y_dot, Z_dot)
#     ###          coordinate: ECI
#     ###          unit: m
#     ###
#     #{header_units}
#     #{header_names}
#     ### %eoh
#     '''
#         file.write(header_meta)

#         #### Manually write each row of the data.
#         for ii, val in enumerate(statevector_ICs['data']['Date']):
#             row =   f"{statevector_ICs['data']['Date'][ii]}"\
#                 +f"  {statevector_ICs['data']['X'][ii]*1000:15.5f}"\
#                 +f"  {statevector_ICs['data']['Y'][ii]*1000:15.5f}"\
#                 +f"  {statevector_ICs['data']['Z'][ii]*1000:15.5f}"\
#                 +f"  {statevector_ICs['data']['X_dot'][ii]*1000:15.5f}"\
#                 +f"  {statevector_ICs['data']['Y_dot'][ii]*1000:15.5f}"\
#                 +f"  {statevector_ICs['data']['Z_dot'][ii]*1000:15.5f}"\
#                 + f"\n"
#             file.write(row)
#     return



def read_SpireLeoOrbPOD_sp3c(filelist_sat):
    """
    Reads the Spire Level 1B Precise Orbit Determination Solution (leoOrb)


    File Content
    -------
        This file contains the estimated position, velocity 
        and receiver clock error of a given Spire satellite 
        after processing of the POD observation file by Spires
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



    print("        - read_SpireLeoOrbPOD_sp3c(): Reading ephemeris data from  ", len(filelist_sat), "files.")
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
    
    print("        - read_SpireLeoOrbPOD_sp3c(): Done combining data.")


    
    return(leoOrb_dict)