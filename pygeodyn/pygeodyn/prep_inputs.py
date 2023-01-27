import sys
# from   pygeodyn.satellite_icesat2 import ICESat2
import pandas  as pd
import numpy   as np
from   os.path import exists
import gc
import os
#
from pygeodyn.util_dir.time_systems     import mjds_to_ymdhms
from pygeodyn.util_dir.time_systems     import time_gps_to_tdt
#
from pygeodyn.util_dir.quaternions      import quat_trans_SBFtoRSW_to_SBFtoECI
from pygeodyn.satellite_spire           import load_attitude_spire
from pygeodyn.util_dir.quaternions      import call_slerp_SpireAtt
#
from pygeodyn.util_dir.attitude_binary  import write_EXAT_binary



# class InheritIcesat2(ICESat2):
#     def __init__(self):
#         ICESat2.__init__(self)
#         pass

class PrepareInputs():
    """ Check and/or write the Input Files that are needed to run GEODYN.

        Initialize w the satellite class to inherit its attributes.    
    """

    def __init__(self):  
        pass


    def make_write_exat(self):

        from pygeodyn.util_dir.time_systems       import get_leapseconds
        from pygeodyn.util_dir.time_systems       import time_gps_to_utc
        from pygeodyn.util_dir.coordinate_systems import call_teme2eci


        path_attitude     = "/data/SatDragModelValidation/data/inputs/"\
                           +"sat_spire83/data_Spire/attitude/"\
                           +"20180923_20181209_TaskOrder3Data/"
        filename          = "leoAtt_2018-09-23T00-12-00Z.9999999.083.log"
        file__AttitudeLog = path_attitude+filename
        interval    = 10  # seconds
        
        one_hour = pd.to_timedelta(1,'h')
        startEpoch  = self.prms_arc['epoch_startDT']  - one_hour  
        stopEpoch   = self.prms_arc['epoch_stopDT']   + one_hour
 
        #### 1. Load the attitude data that corresponds to the entire timeperiod
        SpireDF = load_attitude_spire(file__AttitudeLog,
                                        startEpoch,
                                        stopEpoch)


        #### 2. Convert from GPS time to TDT time
        SpireDF['tim (tdt)'] = [time_gps_to_tdt(tim, leap_sec=37) 
                                        for tim in SpireDF['tim (gps)'] 
                                                ]
        ### 2.5) Prerequisite transformation for pos and vel
        ###      from eci-teme-ofepoch to eci-j2000 for 
        # x, y, z = map(list,  \
        #     zip(*[[xyz[0],xyz[1],xyz[2]]\
        #           for xyz in SpireDF['pos (eci)'] ]))

        # xdot,ydot,zdot = map(list,\
        #              zip(*[[xyz_dot[0],xyz_dot[1],xyz_dot[2]] \
        #               for xyz_dot in SpireDF['vel (eci)'] ]))

        r_j2000_list = []
        v_j2000_list = []
        for i,val in enumerate(SpireDF['tim (gps)']):
            
            #### First convert to UTC
            ####   get the leapseconds for time of interest
            dat = get_leapseconds(SpireDF['tim (gps)'][0].year,\
                                SpireDF['tim (gps)'][0].month,\
                                SpireDF['tim (gps)'][0].day)
            date_utc = time_gps_to_utc(SpireDF['tim (gps)'][i], dat)
            #
            year   = date_utc.year
            mon    = date_utc.month
            day    = date_utc.day
            hr     = date_utc.hour
            minute = date_utc.minute
            sec    = date_utc.second

            # position vector earth fixed  ( km )
            r_teme = list(SpireDF['pos (eci)'][i]) 
            v_teme = list(SpireDF['vel (eci)'][i]) 

            r_j2000, v_j2000 = call_teme2eci(r_teme, v_teme, None,\
                                            year, mon, day,        \
                                            hr, minute, sec,       \
                                            calc_accel=False)
            
            r_j2000_list.append(r_j2000.transpose().squeeze())
            v_j2000_list.append(v_j2000.transpose().squeeze())

        #### 3. Perform a coordinate transformation from SBF-->RSW to SBF-->ECI(j2000)
        q_SBFtoECI = [quat_trans_SBFtoRSW_to_SBFtoECI(r_j2000_list[i], 
                                                      v_j2000_list[i], 
                                                    SpireDF['q (sbf)'].iloc[i]) 
                                                for i,val in enumerate(SpireDF['tim (gps)'])]
        # q_SBFtoECI = [quat_trans_SBFtoRSW_to_SBFtoECI(SpireDF['pos (eci)'].iloc[i], 
        #                                             SpireDF['vel (eci)'].iloc[i], 
        #                                             SpireDF['q (sbf)'].iloc[i]) 
        #                                         for i,val in enumerate(SpireDF['tim (gps)'])]
        # Fix the negatives such that no scalar component is negative
        SpireDF['q (SBFtoECI)'] = [-1*x if x[3]<0
                                        else x
                                        for x in q_SBFtoECI]
        

        
        #### 4. Interpolate Quaternions to linearly spaced time series
        exatt_quats = call_slerp_SpireAtt(SpireDF, 
                                        startEpoch, 
                                        stopEpoch, 
                                        interval )
        ##### Free up some memory:
        del SpireDF
        del q_SBFtoECI
        del r_j2000_list, v_j2000_list

        gc.collect()


        #### 5. Write the External Attitude Binary File:
        #       Initialize the satellite specific settings 
        #       for the external attitude file conditions
        exat_param={}
        
        sat = self.prms['satellite']
        if sat =='spire83':
            exat_param[sat]={}
            exat_param[sat]['SATID']    = self.prms['sat_ID'] 
            exat_param[sat]['num_sat']  = float(self.prms['number_of_satellites'])
            exat_param[sat]['version']  = 1.
            
            ### Time info
            exat_param[sat]['startEpoch']  = float(pd.to_datetime(
                        startEpoch,format='%Y-%m-%d %H:%M:%S'
                        ).strftime(format='%y%m%d%H%M%S'))
            exat_param[sat]['stopEpoch']   = float(pd.to_datetime(
                                                    stopEpoch,\
                                                    format='%Y-%m-%d %H:%M:%S'
                                                    ).strftime(\
                                                    format='%y%m%d%H%M%S'))
            exat_param[sat]['interval']    = float(interval) 
            exat_param[sat]['startFrac_S'] = float(pd.to_datetime(
                        startEpoch, format='%Y-%m-%d %H:%M:%S').microsecond)
            exat_param[sat]['stopFrac_S']  = float(pd.to_datetime(
                        stopEpoch, format='%Y-%m-%d %H:%M:%S').microsecond)
            ### Panel info:
                # QQQ is the total # of separate movable panels   
                #      + antenna quaternion sets for this satellite 
                #      (excludes SBF to J2000 quaternion set which is 
                #      mandatory for each satellite in the file).
                # PPP is the number of movable panels represented for 
                #      this satellite.
                # AAA is the number of moveable antenna represented for 
                #      this satellite.  One quaternion set may represent 
                #      the attitude for up to two movable panels and 
                #      one movable antenna. 
            QQQ                          = int(   0   )
            PPP                          = int(   0   )
            AAA                          = int(   0   )
            exat_param[sat]['qqqpppaaa'] = float(f'{QQQ:03}{PPP:03}{AAA:03}')
            exat_param[sat]['panel_num'] = float(  0  )

            
        write_EXAT_binary(self.file_exat, 
                        exat_param[sat], 
                        exatt_quats['q (SBFtoECI)'],
                        exatt_quats['tim (tdt)'],
                        writetxt=True)
        
        return


    def prep_exat_check(self):
        """Check for External Attitude File and write if doesn't exist 
        """

        ### External Attitude File must have name as follows
        self.filename_exat = 'EXAT01.'+self.arc_name_id+'.gz'
        ## full path to file
        self.file_exat = self.dir_exat +'/' +self.filename_exat



        ### Check if exists:
        ###     Edit-1/25/23, write exat every run
        # if not exists(self.file_exat):
        self.filename_exat = 'EXAT01.'+self.arc_name_id+''
        ## full path to file
        self.file_exat = self.dir_exat +'/' +self.filename_exat

        print('Making an external attitude file.')
        self.make_write_exat()
    
        os.system(f"gzip -vr {self.file_exat}")

        self.filename_exat = 'EXAT01.'+self.arc_name_id+'.gz'
        ## full path to file
        self.file_exat = self.dir_exat +'/' +self.filename_exat
        #### If doesn't exist, we need to write them....


   

    '''
    --------------------------------------------------------------
    PLACE HOLDER --- CHECK/WRITE G2B File
    --------------------------------------------------------------
    '''



    def prep_iisset_write(self):
        """
        Calls the assorted functions that write the setup file. 
        """

        ### Setup File Location:
        self.file_iisset = self.dir_input+'/'+self.setup_file_arc
        #
        ## Call the functions that populate the setup file options
        self.get_arc_values_and_dates()
        lines_global      = self.set_iisset_GlobalOptions()
        arc_options_cards = self.set_iisset_ArcOptions()

        # Write the above options to the file
        with open(self.file_iisset, "w") as f:
            ### Write Global Option set
            for iline, line in enumerate(lines_global):
                f.write(line)
            ### Write Arc Option Set
            for key in arc_options_cards.keys():
                f.write(arc_options_cards[key]) 
        return 



    def set_iisset_GlobalOptions(self):

        ## Read in the massive block containing the Global Set Option Cards
        with open(self.file_globalcards, "r") as f:
            lines_global = f.readlines()                  
        # Initialize the date variables that help with time keeping
        epoch_start = self.prms['epoch_start'][self.arcnumber]
        epoch_stop   = self.prms['epoch_stop'][self.arcnumber]
        epoch_startDT = pd.to_datetime(epoch_start,format='%Y-%m-%d %H:%M:%S')
        epoch_stopDT  = pd.to_datetime(epoch_stop ,format='%Y-%m-%d %H:%M:%S')
        #
        dt_2days = pd.Series(pd.to_timedelta(48,'h'))
        dt_1days = pd.Series(pd.to_timedelta(24,'h'))
        epoch_start_minus2day = (epoch_startDT- dt_2days).dt.strftime('%y%m%d%H%M%S.0000000').values[0]
        epoch_end_plus1day    = (epoch_stopDT + dt_1days).dt.strftime('%y%m%d%H%M%S.000').values[0]


        ## Add the mandatory global title card to the front of the list
        lines_global = ['### \n',
                        '###   '+self.arc_name_id+'  \n',
                        '### \n']                          + lines_global
        
        ## Specify Density Model Series 
        global_atmden = 'ATMDEN  '+ self.iisset_den
        ## Use atmospheric gravity from file over specified timeperiod
        global_atgrav = 'ATGRAV9090              '\
                            +epoch_start_minus2day+''+epoch_end_plus1day[:-1]   

        for iline, line in enumerate(lines_global):
            if 'ATMDEN' in line:
               lines_global[iline] = global_atmden+'\n'
            if 'ATGRAV' in line:
               lines_global[iline] = global_atgrav+'\n'
        return lines_global
        





    def set_iisset_ArcOptions(self):
        """ Arc Set contains information defining the arc. (Can have multiple
            arc sets in single input file)
            
            Overview: 
                - Mandatory Arc Description 
                    - Can say anything, must be on three (or more) title cards 
                - Mandatory cards (REFSYS,SATPAR,EPOCH,ELEMS,ELEMS)
                    - describes the reference coordinate system and time and
                      spacecraft parameters in this arc.
                - Arc Option cards
                    - may be specified to make use of GEODYN's individual arc 
                      capabilities 
                - Data Selection/Deletion Option Subgroup
                    - may be used to edit input observations. 
                    - Data selection/deletion subgroup should not be present in
                      orbit generation runs.  
                    - In data reduction runs, the use of DATA and SELECT cards 
                      is mandatory (see Vol3 Doc individual cards)
                - Mandatory Arc Set Termination card (ENDARC)
        """

        # Some hardcoded options that we don't anticipate being changed.
        #
        # refsys options
        max_arciters_global1           = 19         #  7-8       I2
        min_arciters_global1           = 3          #  9         I1
        max_arciters_global_2nd_to_end = 3          #  10        I1
        # satpar options
        SC_param_A_m       = 0      # Use S/C area and mass from this card
        SC_param_DragSRP   = 0      # No S/C param file used
        TwelveParam        = 0      # not used
        AttitudeControl    = 0      # cannonball
        LocalGravitySwitch = 0      # Gravity is considered significant
        Ext_ThermalAccel   = 0      # No external thermal acceleration
        MaxDeg_GravCoeffs  = 0      # default-- Max. of Model
        # solrad coeff of refl =1
        
        # orbfil
        orbfil_unit = 131
        orbfil_step = 120
        # sigma Observation standard deviation = 1
        # sigma Observation editing sigma      = 1

        # ---------------------------------------------------------------------
        # Arc Set Option Cards
        #  --------------------------------------------------------------------        
        arc_options_cards = {}        
        epoch_start  = self.prms_arc['epoch_start']
        epoch_stop   = self.prms_arc['epoch_stop']
        start_ymdhms = self.prms_arc['start_ymdhms']
        stop_ymdhms  = self.prms_arc['stop_ymdhms'] 

        ### Arc title Cards
        titl1 =f"\n### Arc Options: {self.arc_name_id}               \n"
        titl2 =f"###              {self.prms['satellite']}         \n"
        titl3 =f"###        {epoch_start}   {epoch_stop} \n"        

        arc_options_cards['title'] = ''.join([titl1, titl2, titl3])



        ###
        ###      REFSYS ------ Specifies the coordinate system of integration  
        ###                    and the number of arc iterations 
        ###
        refsys={}
        refsys['1_6']=str( 'REFSYS' ).rjust(6,' ') #A6
        refsys['7_8']=str( int(max_arciters_global1)    ).rjust(2,' ') #I2
        refsys['9']=str( int(min_arciters_global1)      ).rjust(1,' ') #I1
        refsys['10']=str( int(max_arciters_global_2nd_to_end)).rjust(1,' ') #I1
        refsys['13_20'] = '         '
        #
        if self.prms['coord_ref_system'] == 'j2000':
            refsys['11']    = str(1).rjust(1,' ') #I1
            refsys['21_26'] = f' '.rjust(6,' ')  #I6
            refsys['27_30'] = f' '.rjust(4,' ')  #I4
            refsys['31_40'] = f' '.rjust(10,' ') #D10.8
        elif self.prms['coord_ref_system'] == 'true_of_reference':
            refsys['11']    = str(0).rjust(1,' ') #I1
            refsys['21_26'] =\
                         f'{int(start_ymdhms[:6])  }'.rjust(6,' ')     #I6
            refsys['27_30'] =\
                         f'{int(start_ymdhms[6:10]):04}'.rjust(4,' ')  #I4
            refsys['31_40'] =\
                         f'{int(start_ymdhms[10:]) :010.7f}'.rjust(10,' ')#D10.8
        #
        refsys_line =  refsys['1_6']   + refsys['7_8']   + refsys['9']     \
                     + refsys['10']    + refsys['11']    + refsys['13_20'] \
                     + refsys['21_26'] + refsys['27_30'] + refsys['31_40']
        arc_options_cards['refsys'] =  refsys_line+'\n'        

        ###
        ###  SATPAR ----Specifies spacecraft id number (Satellite ID), 
        ###                cross sectional area and mass. Also specifies maximum 
        ###                degree of gravitational coefficients to be considered
        ###                significant for this spacecraft.
        satpar={}            
        #### Put each input option into the correct format  
        satpar['1_6']   = f"{'SATPAR' }".rjust(6,' ') #A6
        satpar['7']     = f'{SC_param_A_m }'.rjust(1,' ') #I1
        satpar['8']     = f'{SC_param_DragSRP }'.rjust(1,' ') #I1
        satpar['9']     = f'{TwelveParam }'.rjust(1,' ') #I1
        satpar['10_11'] = f'{AttitudeControl }'.rjust(2,' ') #I2
        if self.prms['bool_exatfiles']: # use external attitude file
            satpar['12']    = f'{9}'.rjust(1,' ') #I1
        satpar['13']    = f'{LocalGravitySwitch }'.rjust(1,' ') #I1
        satpar['14']    = f'{Ext_ThermalAccel   }'.rjust(1,' ') #I1
        satpar['15_17'] = f'{MaxDeg_GravCoeffs  }'.rjust(3,' ') #I3
        satpar['18_24'] = f"{self.prms['sat_ID']}".rjust(7,' ') #I7
        satpar['25_44'] = f"{self.prms['sat_area_cross_sec'] :20.8f}".rjust(20,' ')#D20.8
        satpar['45_59'] = f"{self.prms['sat_mass'] :15.3f}".rjust(15,' ')#D15.3

        satpar_line = satpar['1_6']  +satpar['7'] + satpar['8']+satpar['9']    \
                    + satpar['10_11']+satpar['12']+ satpar['13']+ satpar['14'] \
                    + satpar['15_17'] + satpar['18_24']  + satpar['25_44']     \
                    + satpar['45_59']
        arc_options_cards['satpar'] =  satpar_line +'\n'   

        ###
        ### EPOCH - Specifies epoch time for satellite elements ,optional start
        ###         time for orbit integration ,and mandatory stop time for the
        ###         run. The stop time is used by GEODYN to determine ephemeris
        ###         and flux data requirements. 
        epoch={}
        epoch['1_6']   = f"{'EPOCH'}".ljust(6 ,' ') #A6
        epoch['7_20']  = ' '.rjust(14,' ')
        epoch['21_26'] = f'{int(start_ymdhms[:6])        }'.rjust(6 ,' ') #I6
        epoch['27_30'] = f'{int(start_ymdhms[6:10])   :04}'.rjust(4 ,' ') #I4
        epoch['31_40'] = f'{int(start_ymdhms[10:]):010.7f}'.rjust(10,' ') #D10.8
        epoch['41_46'] = f'{int(start_ymdhms[:6])        }'.rjust(6 ,' ') #I6
        epoch['47_50'] = f'{int(start_ymdhms[6:10])   :04}'.rjust(4 ,' ') #I4
        epoch['51_60'] = f'{int(start_ymdhms[10:]):010.7f}'.rjust(10,' ') #D10.8
        epoch['61_66'] = f'{int(stop_ymdhms[:6])         }'.rjust(6 ,' ') #I6
        epoch['67_70'] = f'{int(stop_ymdhms[6:10])    :04}'.rjust(4 ,' ') #I4
        epoch['71_80'] = f'{int(stop_ymdhms[10:]) :010.7f}'.rjust(10,' ') #D10.8
        #
        epoch_line =   epoch['1_6'] + epoch['7_20'] + epoch['21_26'] \
                     + epoch['27_30'] + epoch['31_40'] + epoch['41_46'] \
                     + epoch['47_50'] + epoch['51_60'] + epoch['61_66'] \
                     + epoch['67_70']   + epoch['71_80'] 
        arc_options_cards['epoch'] =  epoch_line+'\n' 


        ### ELEMS1 - Introduce the 1st three components of the orbital elements
        ###          for the S/C.
        if self.prms['orbit_elements_form'] == 'cartesian':
            elems1 = {}
            elems2 = {}
             

            #### Put each input option into the correct format  
            elems1['1_6']  = f"{'ELEMS1'}".ljust(6 ,' ') #A6
            elems1['7']  = f'{ 1 }'.rjust(1 ,' ') #I1           # use cartesian
            if self.prms['coord_ref_system']  =='j2000':
                elems1['8']  = f'{ 1 }'.rjust(1 ,' ') #I1       # use j2000
            elems1['11_14']  = f'{300:04}'.rjust(4 ,' ')#I4     # earth is body
            #
            elems1['21_40']  = f"{self.prms_arc['X']  :18.10f}".ljust(20,' ') #D20.14
            elems1['41_60']  = f"{self.prms_arc['Y']  :18.10f}".ljust(20,' ') #D20.14
            elems1['61_80']  = f"{self.prms_arc['Z']  :18.10f}".ljust(20,' ') #D20.14
            #
            elems1_line = elems1['1_6'] + elems1['7'] + elems1['8'] +' '*2 \
                        + elems1['11_14']                           +' '*6 \
                        + elems1['21_40'] + elems1['41_60'] + elems1['61_80']

            elems2['1_6'] = f"{'ELEMS2'}".ljust(6 ,' ') #A6
            elems2['21_40'] = f"{self.prms_arc['X_dot']  :18.12f}".ljust(20,' ') #D20.14
            elems2['41_60'] = f"{self.prms_arc['Y_dot']  :18.12f}".ljust(20,' ') #D20.14
            elems2['61_80'] = f"{self.prms_arc['Z_dot']  :18.12f}".ljust(20,' ') #D20.14

            elems2_line = elems2['1_6']   + "              "\
                        + elems2['21_40'] + elems2['41_60'] + elems2['61_80'] 
        else:
            print('**ERROR**: ',PrepareInputs.make_setup_file.__qualname__)
            print('    Cannot yet use non-cartesian (keplerian) elements. ')
            print('    Must implement a non cartesian card writing method.')
            sys.exit(0)

        arc_options_cards['elems1'] =  elems1_line  +'\n'       
        arc_options_cards['elems2'] =  elems2_line  +'\n'       


        ### ORBFIL ----------------------------------------------------------
        orbfil = []
        orbfil.append(f"{'ORBFIL'}".ljust(6 ,' '))  
        # coordinate system
        if self.prms['coord_ref_system']  == 'j2000':
            orbfil.append(f"{2}".rjust(1,' ') )     # use j2000
        # number of satellites
        if self.prms['number_of_satellites']  == 1:
            orbfil.append(f"{0}".rjust(1,' ') )     # use 1 satellite
        else:
            orbfil.append(f"{1}".rjust(1,' ') )     # use multiple satellites
            print('using multiple satellites')
            sys.exit()
        # specify fortran unit num
        orbfil.append(f"{orbfil_unit}".ljust(3 ,' ')) #I3
        # technically there is a CGMASS vs SBF conversion option here
        # advance to column 18 with 6 spaces
        orbfil.append("".ljust(6 ,' '))               # 6 spaces
        # satellite ID
        orbfil.append(f"{self.prms['sat_ID']}".rjust(7,' ')) #I7
        # start date for traj file (YYMMDDHHMMSS.SS)
        orbfil.append(f"{start_ymdhms}.00".rjust(20,' ')) #D20.3  , col25-44
        # stop date for traj file (YYMMDDHHMMSS.SS)
        orbfil.append(f"{stop_ymdhms}.00".rjust(16,' '))  #D15.3  , col45-59, added extra space
        # Step size for traj file (seconds)
        orbfil.append(f"{orbfil_step}".rjust(11,' '))     #13.3   , col60-72
        arc_options_cards['orbfil'] = ''.join(orbfil)  +'\n'    
        orbfil=0   

        ### STEP ----------------------------------------------------------
        arc_options_cards['step']  = 'STEP             '\
                                    +f"{self.prms['sat_ID']}".rjust(7,' ')\
                                    +'           ' \
                                    +str(int(self.prms['step']))+'.'\
                                    +'\n' 
        
        arc_options_cards['solrad'] ='SOLRAD           '\
                                    +f"{self.prms['sat_ID']}".rjust(7,' ')\
                                    +'  1.  '\
                                    +'\n' 
        CD_VALUE = str(self.prms['cd_value'])
        arc_options_cards['drag'] ='DRAG             '\
                                    +f"{self.prms['sat_ID']}".rjust(7,' ')\
                                    +''+CD_VALUE+'0000000E+00'\
                                    +'\n' 
                





        ##### DRAG CARDS:
        # card_drag_strings['CONDRG']  =  'CONDRG  1        '+SAT_ID+'     '                   \
        #                                         + str(epoch_start[:-5])+str(epoch_end[:-5])  \
        #                                         + '         0.50000  28800.'
        #                                      #     '         0.50000  28800.'
        # card_drag_strings[i_cd] =  'DRAG             '+ SAT_ID+' '            \
        #                                                       + CD_VALUE+'0000000D+00'\
        #                                                       + '     '+drag_dates[i_cd][:10] \
        #                                                       + ' 0.00    0.100D+02'
        #####  IF we are using CD adjustement drag options, then set the drag cards according to the run settings
        #####  If we are NOT using CD adjustment, remove DRAG times from the file
        ####   INPUT THE DRAG OPTIONS  for time dependent drag
#         card_drag_strings={}
#         card_drag_strings['CONDRG']  =  'CONDRG  1        '+SAT_ID+'     '                   \
#                                                 + str(epoch_start[:-5])+str(epoch_end[:-5])  \
#                                                 + '         0.50000  28800.'
#                                              #     '         0.50000  28800.'

        
#         CD_VALUE = str(self.prms['cd_value'])
#         print('   Using a CD value of ', CD_VALUE)

#         if self.prms['cd_adjustment_boolean'] == True:  ### Allow CD to ADJUST, i.e. multiple DRAG cards with times
           
#             hours_between_cd_adj = self.prms['hours_between_cd_adj']
#             if self.prms['arc_length_h']==hours_between_cd_adj:   # The 24 hour case
#                 num_of_cd_adj = (self.prms['arc_length_h']/self.prms['hours_between_cd_adj'])
#             else:    
#                 num_of_cd_adj = (self.prms['arc_length_h']/self.prms['hours_between_cd_adj']) #- 1
#             add_hours_dt = pd.Series(pd.to_timedelta(hours_between_cd_adj,'h'))
            
#             drag_dates = []
#             for i_cd in np.arange(0, num_of_cd_adj):
#                 factor = i_cd+1
#                 drag_dates.append( (epoch_start_dt+add_hours_dt*factor).dt.strftime('%y%m%d%H%M%S').values[0])
                
#             for i_cd in np.arange(0, num_of_cd_adj):
#                 i_cd = int(i_cd)
#                 print('     drag_date ', i_cd ,' ',  pd.to_datetime( drag_dates[i_cd], format='%y%m%d%H%M%S'))

#                 card_drag_strings[i_cd] =  'DRAG             '+ SAT_ID+' '            \
#                                                               + CD_VALUE+'0000000D+00'\
#                                                               + '     '+drag_dates[i_cd][:10] \
#                                                               + ' 0.00    0.100D+02'
# #                 card_drag_strings[i_cd] =  'DRAG             '+SAT_ID+' 2.2000000000000D+00'+drag_dates[i_cd][:10]+' 0.00    0.100D+02'

#         else:
#             cards_to_remove.append('CONDRG')

            
#             #### --------------------------------------------------------------------
#         ####   INPUT THE DRAG OPTIONS  for time dependent drag
# #         card_drag_strings={}
# #         card_drag_strings['CONDRG']  =  'CONDRG  1        '+SAT_ID+'     '+str(epoch_start[:-5])+str(epoch_end[:-5])+'         0.50000  28800.'

#         #### for adding time dependent drag estimations.  We need to do a few things:
#         ###       Find the drag card that is already in the file:
#         ###       Add CONDRAG before all drag cards
#         ###       Add DRAG cards with TIME periods after the first drag card
#         if self.prms['cd_adjustment_boolean'] == True:
#             with open(iisset_file, "r") as f:
#                 lines_all = f.readlines()                
#             with open(iisset_file, "w") as f:
#                 for line in lines_all:
#                     if 'DRAG   0 0       '+SAT_ID+' 2.3000000000000E+00' in line:  #this finds the DRAG line.  
#                         f.write(card_drag_strings['CONDRG'] + ' \n')
#                         f.write('DRAG             '+SAT_ID+' '+CD_VALUE+'0000000E+00'+ ' \n')
#                         for i_cd in np.arange(0, num_of_cd_adj):
#                             i_cd = int(i_cd)
#                             f.write(card_drag_strings[i_cd] + ' \n')                 
#                     else:
#                         f.write(line)
                        
#         elif self.prms['cd_adjustment_boolean'] == False: ### DONT allow CD to ADJUST, i.e. only 1 DRAG card, no time dep.
#             print('   Running without DRAG time dependence')
#             with open(iisset_file, "r") as f:
#                 lines_all = f.readlines()                
#             with open(iisset_file, "w") as f:
#                 for line in lines_all:
#                     if 'DRAG   0 0       '+SAT_ID+' 2.3000000000000E+00' in line:  #this finds the DRAG line.  
#                         f.write('DRAG             '+SAT_ID+' '+CD_VALUE+'0000000E+00'+ ' \n')
#                     else:
#                         f.write(line)








        
        # --------------------------------------------------------------------
        ### PANEL - Requests application and/or adjustment of parameters
        #             associated with a flat panel for use in nonconservative 
        #             force modeling.
        #   NOTE  [1] : Movable panels must be grouped after the non-movable 
        #               panels in ascending order.
        line_block = []#""
        PM = self.sat_geometry_panel_model()

        list_panel_nums = np.arange(1,self.prms['sat_geometry_panel_num'] +1)
        for ipanel, panel in enumerate(list_panel_nums):
            panel_num = str(panel).rjust(2,' ')

            for paramval in [1,2,3,4,5,6,7,8,9,10]:  
                param = str(paramval).rjust(2,' ')


                ### NORMAL VECTORS
                if paramval == 1:  
                    col25_44 = '{:.13E}'.format(PM['Normal Vector X'][ipanel]).rjust(20,' ')
                    col45_59 =  '{:.8E}'.format(PM['Normal Vector Y'][ipanel]).rjust(15,' ') 
                    col60_72 =  '{:.6E}'.format(PM['Normal Vector Z'][ipanel]).rjust(13,' ') 
                #
                ### AREA (m**2)
                elif paramval == 2:  
                    col25_44 = '{:.13E}'.format(PM["Area (m^2)"][ipanel]).rjust(20,' ') 
                    col45_59 =  '{:.8E}'.format(0).rjust(15,' ')  
                    col60_72 =  '{:.6E}'.format(0).rjust(13,' ')  
                #
                ### Specular  reflectivity  
                elif paramval == 3:  
                    col25_44 = '{:.13E}'.format(PM["Specular"][ipanel]).rjust(20,' ') 
                    col45_59 =  '{:.8E}'.format(0).rjust(15,' ')  
                    col60_72 =  '{:.6E}'.format(0).rjust(13,' ')  
                ### Diffuse  reflectivity  
                elif paramval == 4:  
                    col25_44 = '{:.13E}'.format(PM["Diffuse"][ipanel]).rjust(20,' ') 
                    col45_59 =  '{:.8E}'.format(0).rjust(15,' ')  
                    col60_72 =  '{:.6E}'.format(0).rjust(13,' ')  
                ### Emissivity  
                elif paramval == 5:  
                    col25_44 = '{:.13E}'.format(PM["Emissivity"][ipanel]).rjust(20,' ') 
                    col45_59 =  '{:.8E}'.format(0).rjust(15,' ')  
                    col60_72 =  '{:.6E}'.format(0).rjust(13,' ')  
                #### 
                # 6   Temperature A (cold  equilibrium  temperature) (Kelvin) 
                # 7   Temperature C (delta  temperature  between  hot and cold
                #      equilibrium  temperature) (K) 
                # 8   Temperature decay  time D  (exponential  decay  time  for 
                #      panel cooling) (Sec) 
                # 9   Temperature decay  time F  (exponential  decay  time  for
                #      panel heating) (Sec) 
                # 10  Temperature/satellite  rotation X
                #       (divisor  for cos(theta) term inheating  equation)
                elif paramval == 6:
                    col25_44 = '{:.13E}'.format(PM["Temperature A"][ipanel]).rjust(20,' ') 
                    col45_59 =  '{:.8E}'.format(0).rjust(15,' ')  
                    col60_72 =  '{:.6E}'.format(0).rjust(13,' ')  
                elif paramval  == 7:
                    col25_44 = '{:.13E}'.format(PM["Temperature C"][ipanel]).rjust(20,' ') 
                    col45_59 =  '{:.8E}'.format(0).rjust(15,' ')  
                    col60_72 =  '{:.6E}'.format(0).rjust(13,' ')  
                elif paramval  == 8:
                    col25_44 = '{:.13E}'.format(PM["Temperature rate D"][ipanel]).rjust(20,' ') 
                    col45_59 =  '{:.8E}'.format(0).rjust(15,' ')  
                    col60_72 =  '{:.6E}'.format(0).rjust(13,' ')  
                elif paramval  == 9:
                    col25_44 = '{:.13E}'.format(PM["Temperature rate F"][ipanel]).rjust(20,' ') 
                    col45_59 =  '{:.8E}'.format(0).rjust(15,' ') 
                    col60_72 =  '{:.6E}'.format(0).rjust(13,' ')  
                elif paramval  == 10:
                    col25_44 = '{:.13E}'.format(PM["Temperature rotate X"][ipanel]).rjust(20,' ') 
                    col45_59 =  '{:.8E}'.format(0).rjust(15,' ')  
                    col60_72 =  '{:.6E}'.format(0).rjust(13,' ')  
                else:  
                    col25_44 = '{:.13E}'.format(0).rjust(20,' ') 
                    col45_59 =  '{:.8E}'.format(0).rjust(15,' ')  
                    col60_72 =  '{:.6E}'.format(0).rjust(13,' ')  

            ### Each panel in the model will have 10 cards attributed to it
            #     index_card                      =  [1,6]                #I6
            #     index_motionindicator           =  9                    #I1
            #     index_frequencytype             =  10                   #I1
            #     index_panelnumber               =  [11, 12]             #I2
            #     index_parameter                 =  [13, 14]             #I2
            #     index_satid                     =  [18, 24]             #I7
            #     index_apriori_param_or_Xnormvec =  [25,44]   #D20.8 
            #     index_apriori_sigma_or_Ynormvec =  [45,59]   #D15.3 
            #     index_apriori_Znormvec          =  [60,72]   #D13.1 
                line = 'PANEL' + '   ' \
                        +str(int(PM["Moves=1 Fixed=0"][ipanel])).rjust(1,' ')\
                        +str(int(PM["RadiationFreq Both=0"][ipanel])).rjust(1,' ')\
                        +panel_num+param + '   ' + self.prms['sat_ID']\
                        +col25_44 + col45_59 + col60_72+ (' '*8)+'\n'
                # print(line,sep='')

                # line_block+=line
                line_block.append(line)
        # arc_options_cards['panel'] = line_block
        arc_options_cards['panel'] = ''.join(line_block)


        ### Data Block
        ### -----------
        if self.prms['run_type'] == "DataReduction_PCE":
            datablock = []
            dates_datablk = f'{int(start_ymdhms[:6])        }'.rjust(6 ,' ')\
                          + f'{int(start_ymdhms[6:10])   :04}'.rjust(4 ,' ')\
                          + f'{int(start_ymdhms[10:]) :010.7f}'.rjust(10,' ')\
                          + f'{int(stop_ymdhms[:6])        }'.rjust(6 ,' ')\
                          + f'{int(stop_ymdhms[6:10])   :04}'.rjust(4 ,' ')\
                          + f'{int(stop_ymdhms[10:]) :010.7f}'.rjust(10,' ')
            datablock.append('DATA \n')
            datablock.append('PREPRO \n')
            datablock.append('SIGMA'+' '*11+'1'+' '*15+'1.0'+' '*17+'1.0 \n')
            datablock.append('SIGMA'+' '*11+'2'+' '*15+'1.0'+' '*17+'1.0 \n')
            datablock.append('SIGMA'+' '*11+'3'+' '*15+'1.0'+' '*17+'1.0 \n')
            datablock.append('SELECT         01                       '+dates_datablk+'\n')
            datablock.append('SELECT         02                       '+dates_datablk+'\n')
            datablock.append('SELECT         03                       '+dates_datablk+'\n')
            arc_options_cards['datablock'] = ''.join(datablock)     
            datablock       =0
            dates_datablk   =0


        arc_options_cards['endarc'] = 'ENDARC'+'\n'

        return(arc_options_cards)


        