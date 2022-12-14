#### ----------------------------------------
#### Import modules:
#### -----------------
import numpy as np
import pandas as pd
    #### Computer function
import os
import os.path
import subprocess
import shutil
import time
    #### modules for reading and converting data
import linecache
from datetime import datetime,timedelta
import copy
#### ----------------------------------------
#### ----------------------------------------

# import sys
# sys.path.insert(0,'/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_dir/')

### Import the Classes from the Tools
# from util_classtools import Util_Tools
# from common_functions   import MJDS_to_YYMMDDHHMMSS, Convert_ET_TDT_to_UTC
from pygeodyn.util_dir.time_systems import mjds_to_ymdhms
from pygeodyn.util_dir.time_systems import time_tdt_to_utc

# from common_functions   import Convert_cartesian_to_RSW_returnall, Convert_cartesian_to_NTW_returnall
from pygeodyn.util_dir.coordinate_systems import Convert_cartesian_to_RSW_returnall, Convert_cartesian_to_NTW_returnall


class PygeodynReader:
    # UtilTools and all of its methods are now inherited in our main functions
       
    #### read in the input and store as members
    def __init__(self):  
#         print('4 ---- check ---- init PygeodynReader class')
        pass
    
#         self.request_data = params['request_data'] 

#         self.satellite         = params['satellite']
#         self.den_model         = params['den_model']
#         self.SpecialRun_name   = params['SpecialRun_name']
#         self.verbose           = params['verbose']
#         self.arc_input         = params['arc']
#         self.set_density_model_setup_params( self.den_model )

#         #### The below interprets that no input has been given for special name
#         if self.SpecialRun_name == None:
#             self.SpecialRun_name = ''
#         else:
#             self.SpecialRun_name = params['SpecialRun_name']



#######         self.empirical_accels = params['empirical_accels']
#######         self.options_in       = params['options_in']
#######         self.run_ID           = params['run_ID']        
#######         self.set_acceleration_params( self.empirical_accels )


    
    
    
    def read_binary_ORBFIL(self):
        
        from scipy.io import FortranFile
#         import numpy as np
#         import pandas as pd
#         from collections import namedtuple
#         import time
        # Fortran calls
#         import subprocess
#         import os


        orb_fil= self._orbfil_filename

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
        data_dict_sat_packets['Satellite Inertial X coordinate'] =[]
        data_dict_sat_packets['Satellite Inertial Y coordinate'] =[]
        data_dict_sat_packets['Satellite Inertial Z coordinate'] =[]
        data_dict_sat_packets['Satellite Inertial X velocity']   =[]
        data_dict_sat_packets['Satellite Inertial Y velocity']   =[]
        data_dict_sat_packets['Satellite Inertial Z velocity']   =[]
        data_dict_sat_packets['Satellite Geodetic Latitude']     =[]
        data_dict_sat_packets['Satellite East Longitude']        =[]
        data_dict_sat_packets['Satellite Height']                =[]
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
                    data_dict_sat_packets['Satellite Inertial X coordinate'].append(a[(index +1) - 2])
                    data_dict_sat_packets['Satellite Inertial Y coordinate'].append(a[(index +2) - 2])
                    data_dict_sat_packets['Satellite Inertial Z coordinate'].append(a[(index +3) - 2])
                    data_dict_sat_packets['Satellite Inertial X velocity'].append(a[(index +4) - 2])
                    data_dict_sat_packets['Satellite Inertial Y velocity'].append(a[(index +5) - 2])
                    data_dict_sat_packets['Satellite Inertial Z velocity'].append(a[(index +6) - 2])
                    data_dict_sat_packets['Satellite Geodetic Latitude'].append(a[(index +7) - 2])
                    data_dict_sat_packets['Satellite East Longitude'].append(a[(index +8) - 2])
                    data_dict_sat_packets['Satellite Height'].append(a[(index +9) - 2])
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
        MJDS_UTC = [time_tdt_to_utc(float(x), 37) for x in orbfil_dict['data_record']['MJDSEC ET'] ]

        ##### Calculate the Gregorian Calendar date:
        yymmdd_str = [mjds_to_ymdhms(x) for x in MJDS_UTC]

        ##### Compute date as Datetime object:
        dates_dt_UTC = [pd.to_datetime( x, format='%y%m%d-%H%M%S') for x in yymmdd_str]
        
        
        orbfil_dict['data_record']["Date_UTC"] = dates_dt_UTC
        orbfil_dict['data_record']["MJDS_UTC"] = MJDS_UTC

        return(orbfil_dict)
    
    
    



                        
    def read_ascixyz(self):

        '''
        NOTE-- This function is not as good as the ORBFIL function.  For some reason it also doesn't read the whole dataset but I never fixed it because the orbfil is technically better according to the GEODYN people.  This is because the Orbit file gives a more complete look at the orrbit and multiple satellites in a run (if multiple)
        
        This method reads the ascii XYZ trajectory data.

        When GEODYN is run, the ascii XYZ card (ORBTVU) SHOULD only be giving the FINAL iteration 
        of the GEODYN RUN.

        In a GPS run, there are many satellites being tracked. 
        This method returns a dataframe with time series of the trajectory 
        data with the satellite as one of the columns.
        '''
        ####
        #### determine where the first data header is (want to skip the file header)
        with open(self._asciixyz_filename, 'r') as f:
            for line_no, line in enumerate(f):
                if 'SPACECRAFT EPHEMERIS FOR SATELLITE' in line:
                    first_header_line = line_no
        ####
        #### read the total number of lines
        line_total = len(open(self._asciixyz_filename).readlines())
        ####
        #### initialize some lists for iteratively storing
        list_YYMMDD   = []
        list_HHMM     = []
        list_SECONDS  = []
        list_X        = []
        list_Y        = []
        list_Z        = []
        list_XDOT     = []
        list_YDOT     = []
        list_ZDOT     = []
        list_LAT      = []
        list_LONG     = []
        list_HEIGHT   = []
        list_sat_id   = []
        ####
        #### loops through all lines in the file from the first 
        #### data header to the last file line.
        for i,val in enumerate(np.arange(first_header_line+1,line_total+1)):
            line = linecache.getline(self._asciixyz_filename,val)
            ####
            #### if the line contains the below text, the satellite is changing
            #### save the new sat ID.
            if 'SPACECRAFT EPHEMERIS FOR SATELLITE' in line:
                sat_id = line[46:54]
            ####
            #### the following keywords denote a header in the file and these lines should be skipped
            if "SPACECRAFT" in line:
                continue
            elif "YYMMDD" in line:
                continue
            elif "GREENWICH" in line:
                continue
            elif "SUNLIGHT" in line:
                continue

            ####
            #### If the line is NOT a header, save the data out.
            #### Note that at this point in the loop we are saving the 
            #### data for the above Sat ID.  When we get to a line 
            #### with the 'SPACECRAFT EPHEMERIS FOR SATELLITE' text,
            #### the satellite ID will change and the data we save 
            #### out will be for that new satellite.
            else:
                try:
                    int(line[1:8])
                    YYMMDD  = line[1:8]
                    HHMM    = line[8:13]
                    SECONDS = line[13:23] 
                    X       = line[23:36]
                    Y       = line[36:49]
                    Z       = line[49:62]
                    XDOT    = line[62:74]
                    YDOT    = line[74:86]
                    ZDOT    = line[86:98]
                    LAT     = line[98:109]
                    LONG    = line[109:120]
                    HEIGHT  = line[120:132]
                    ####
                    #### store in a list to catch 'em all
                    list_YYMMDD.append(YYMMDD)
                    list_HHMM.append(HHMM)
                    list_SECONDS.append(SECONDS)
                    list_X.append(X)
                    list_Y.append(Y)
                    list_Z.append(Z)
                    list_XDOT.append(XDOT)
                    list_YDOT.append(YDOT)
                    list_ZDOT.append(ZDOT)
                    list_LAT.append(LAT)
                    list_LONG.append(LONG)
                    list_HEIGHT.append(HEIGHT)
                    list_sat_id.append(sat_id)
                except:
                    pass
        ####
        #### Save as a dataframe
        traj_xyz_df = pd.DataFrame(data={ 'YYMMDD':list_YYMMDD,
                                        'HHMM'    :list_HHMM,
                                        'SEC_UTC' :list_SECONDS,
                                        'SAT_ID'  :list_sat_id,
                                        'X'       :list_X,
                                        'Y'       :list_Y,
                                        'Z'       :list_Z,
                                        'XDOT'    :list_XDOT,
                                        'YDOT'    :list_YDOT,
                                        'ZDOT'    :list_ZDOT,
                                        'LAT'     :list_LAT,
                                        'LONG'    :list_LONG,
                                        'HEIGHT'  :list_HEIGHT})
        ####
        #### Construct a datetime column using the YYMMDD and HHMM formats
        date = self.make_datetime_column( traj_xyz_df, self.YR) 
                                    #### TODO: consider how to remove this YR necessity
#         traj_xyz_df['Date'] = date
        traj_xyz_df.insert(0, 'Date', date)

        
        ####
        #### Convert the datatypes to FLOAT
        traj_xyz_df['X']  = traj_xyz_df['X'].astype(float)
        traj_xyz_df['Y']  = traj_xyz_df['Y'].astype(float)
        traj_xyz_df['Z']  = traj_xyz_df['Z'].astype(float)
        traj_xyz_df['XDOT']  = traj_xyz_df['XDOT'].astype(float)
        traj_xyz_df['YDOT']  = traj_xyz_df['YDOT'].astype(float)
        traj_xyz_df['ZDOT']  = traj_xyz_df['ZDOT'].astype(float)
        traj_xyz_df['LAT']  = traj_xyz_df['LAT'].astype(float)
        traj_xyz_df['LONG']  = traj_xyz_df['LONG'].astype(float)
        traj_xyz_df['HEIGHT']  = traj_xyz_df['HEIGHT'].astype(float)
        traj_xyz_df['SAT_ID']  = traj_xyz_df['SAT_ID'].astype(int)

        del traj_xyz_df['YYMMDD']
        del traj_xyz_df['HHMM']
        del traj_xyz_df['SEC_UTC']
        del traj_xyz_df['timeHHMM']
        del traj_xyz_df['year']
        del traj_xyz_df['month']
        del traj_xyz_df['day']
        del traj_xyz_df['hours']
        del traj_xyz_df['minutes']
        del traj_xyz_df['secs']
        del traj_xyz_df['millsecs']

        
        return(traj_xyz_df)

    def read_adjustedparams_iieout(self):
        start = time.time()
        '''
        The below code grabs the estimated adjusted parameters for the MAIN satellite for
        the each iteration.
        The data is stored in a dictionary and includes the associated statistics:
              APRIORI  VALUE
              PREVIOUS VALUE              
              CURRENT  VALUE              
              TOTAL DELTA
              CURRENT DELTA
              APRIORI SIGMA
              CURRENT SIGMA
        The dictionary is multi-dimensional, with the first dimension being iteration 
        number and second dimension being satellite ID, the third dim contains the above data.

                ADJUSTEDPARAMS[iteration][SatID][param][stat_value]

        #################################################################################

        The way that this function works is it basically loops through the MASSIVE IIEOUT
        file and finds the line numbers for certain keywords (0XPOS, 0YPOS, ... etc.).
        We then grab the data next to each of the keywords (the format is fixed).

        If we are looking at GPS tracked data, we must identify the satellite ID that will be next to each set of keywords

        EXAMPLE: the below block of code will be indexed such that the line number for 
                 each 0XPOS, 0YPOS, ... etc. is known.

                    1ARC  1 PARAMETER ADJUSTMENT SUMMARY FOR INNER ITERATION  6 OF GLOBAL ITERATION 1    
                    0 PARAMETER NAME       APRIORI  VALUE
                                           PREVIOUS VALUE         TOTAL   DELTA     APRIORI SIGMA
                                           CURRENT  VALUE         CURRENT DELTA     CURRENT SIGMA
                    0XPOS     5041144  -21439971.75446300
                                       -21439971.75446300       0.00000000000      0.10000000E-12
                                       -21439971.75446300       0.00000000000      0.10000000E-12

                    0YPOS     5041144  -13434973.98958000
                                       -13434973.98958000       0.00000000000      0.10000000E-12
                                       -13434973.98958000       0.00000000000      0.10000000E-12

                    0ZPOS     5041144   8698413.759462100
                                        8698413.759462100       0.00000000000      0.10000000E-12
                                        8698413.759462100       0.00000000000      0.10000000E-12

                    0XVEL     5041144   2056.571120089500
                                        2056.571120089500       0.00000000000      0.10000000E-12
                                        2056.571120089500       0.00000000000      0.10000000E-12

                    0YVEL     5041144  -1459.475867380500
                                       -1459.475867380500       0.00000000000      0.10000000E-12
                                       -1459.475867380500       0.00000000000      0.10000000E-12

                    0ZVEL     5041144   2903.476794151500
                                        2903.476794151500       0.00000000000      0.10000000E-12
                                        2903.476794151500       0.00000000000      0.10000000E-12

        '''
        
        #         
        #------------------- SECTION 1 ----------------------------------└──
        #--|
        #--|   Prepare to loop over the adjusted parameters
        #--|        ├── find the line numbers of adjusted parameters
        #--|        ├── determine number of iterations
        #--|        ├── Prepare the dictionary to be saved to
        #--|        ├── make a list of dates for the Time Dependent params (CD and GA)
        #--|--------------------------------------------------------------

        #### find the line numbers of all the adjustment parameters in the iieout file
        #### this simplifies/speds up the task of searching thru the IIEOUT file becuase it is VERY large        
        text_param_lists = ['0XPOS',
                            '0YPOS',
                            '0ZPOS',
                            '0XVEL',
                            '0YVEL',
                            '0ZVEL',]
        ####
        #### Identify the lines where there are adjusted parameters and
        #### Make a list of the satellites that have changed parameters in the file
        ####    ├── Only add satID's to list if they are unique in the loop
        sat_list = []
        lines_params = [] 
        with open(self._iieout_filename, 'r') as f:
            for line_no, line_text in enumerate(f):
                if '0XPOS' in line_text :
                    lines_params.append(line_no)
                    if int(linecache.getline(self._iieout_filename,line_no+1)[10:18]) not in sat_list:
                        sat_list.append(int(linecache.getline(self._iieout_filename,line_no+1)[10:18]))
                    else:
                        pass
#         ####                
#         #### determine number of iterations in the run (when run converges)           
#         with open(self._iieout_filename, 'r') as f:
#             for line_no, line in enumerate(f):
#                 if 'CONVERGENCE' in line:
#                     line_text = line
#         num_iters = float(line_text[39:42])-1
        (self.total_iterations, self.str_iteration) = self.iteration_number(self._iieout_filename)

        #### Build the dictionary to be index based on iteration number   
        #### and initialize each iteration number to also be a dictionary
#         text_GA_list = ["0GA 9P 11",
#                         "0GA 9P 12",
#                         "0GA 9P 21",
#                         "0GA 9P 22",
#                             ]    
#         print(self.total_iterations)
        SatMain_AdjustedParams = {}
#         for i_iter,iterval in enumerate(np.arange(1, self.total_iterations+1)):
        for i_iter,iterval in enumerate([self.total_iterations]):
#             print(iterval)
            SatMain_AdjustedParams[iterval] = {}
            for isat, satval in enumerate(sat_list):
                if self.empirical_accels == True:
#                     pass
#                     for iga, ga_val in enumerate(text_GA_list):
#                         print(ga_val)
                    SatMain_AdjustedParams[iterval][satval] = {}
                    SatMain_AdjustedParams[iterval][satval]['0CD'] = {}
                    SatMain_AdjustedParams[iterval][satval]["0GA 9P 11"] = {}
                    SatMain_AdjustedParams[iterval][satval]["0GA 9P 12"] = {}
                    SatMain_AdjustedParams[iterval][satval]["0GA 9P 21"] = {}
                    SatMain_AdjustedParams[iterval][satval]["0GA 9P 22"] = {}
                else:
                    SatMain_AdjustedParams[iterval][satval] = {}
                    SatMain_AdjustedParams[iterval][satval]['0CD'] = {}

        #### 
        #### Make a list of the dates for the time dependent drag option
        #### First, determine how many time dependent outputs there are:
        line_no_1 = [] 
        line_no_2 = [] 
        with open(self._iieout_filename, 'r') as f:
            for line_no, line_text in enumerate(f):
                if ' ARC #    1   PARAMETERS' in line_text :
                    line_no_1.append(line_no)
                elif '        GLOBAL PARAMETER SUMMARY' in line_text:
                    line_no_2.append(line_no)
        ####            
        parameter_summary_section_range = np.arange(line_no_1[0], line_no_2[0]+1)
        
        ####
        #### Count how many Time dep Cd's were processed
        timedep_Cd_count = []
        for i,val in enumerate(parameter_summary_section_range):
                line = linecache.getline(self._iieout_filename,val)            
                if 'CD' in line:
                    check_sat = int(line[24:32])
                    if check_sat == int(self.SATID):    
                        if 'T' in line:
                            #### save the list of T##s and strip of whitespaces
                            timedep_Cd_count.append(line[18:24].strip()) 
#         ####
#         #### Count how many GA's were processed
#         timedep_GA_count = []
#         for i,val in enumerate(parameter_summary_section_range):
#                 line = linecache.getline(self._iieout_filename,val)            
#                 if 'CD' in line:
#                     check_sat = int(line[24:32])
#                     if check_sat == int(self.SATID):    
#                         if 'T' in line:
#                             #### save the list of T##s and strip of whitespaces
#                             timedep_Cd_count.append(line[18:24].strip()) 

        #### Loop through the IIS cards to find the
        #### date inputs for the time dependent Cd option 
        #### First isolate the input card section:
        line_no_1 = [] 
        line_no_2 = [] 
        with open(self._iieout_filename, 'r') as f:
            for line_no, line_text in enumerate(f):
#                 if 'GEODYN IIE VERSION' in line_text :
#                     line_no_1.append(line_no)
#                 if 'OBSERVATION RESIDUALS FOR ARC' in line_text:
#                     line_no_2.append(line_no)
                if 'GEODYN-IIS VERSION' in line_text :
                    line_no_1.append(line_no)
                if 'GEODYN IIE VERSION' in line_text:
                    line_no_2.append(line_no)
                    break
#         print('line_no_1',line_no_1)
#         print('line_no_2',line_no_2)
        #### Make a list of the dates as determined by the DRAG input cards:            
        card_inputs_range = np.arange(line_no_1[0], line_no_2[0]-100)  # puts section above other places where 'DRAG' would appear
#         print(card_inputs_range)
        timedep_Cd_dates = []
        for i,val in enumerate(card_inputs_range):
                line = linecache.getline(self._iieout_filename,val)            
                if 'DRAG             '+self.SATID in line:    #"DRAG             1807001"
                    check_sat = int(line[18:26])
#                     print(check_sat)
                    if check_sat == int(self.SATID):
#                         print(line[45:56])
                        timedep_Cd_dates.append(line[45:56].strip())
                
        date_timedep_cds = pd.to_datetime(timedep_Cd_dates[1:], format='%y%m%d%H%M')  #YYMMDDHHMMSS
#         print(date_timedep_cds)
        #         
        #------------------- SECTION 2 ----------------------------------
        #--|
        #--|   - Loop thru the saved linenumbers and save out the data 
        #--|        ├── save all stats for each param taking advantage of the fixed width format
        #--|        ├── do again for each time dependent Cd
        #--|        ├── The line placement of CD is inconsistent and needs to be read in as every other...
        #--|        ├── if accelerations are on, do again for each GA 
        #--|        ├──  
        #--|        └──
        #--|--------------------------------------------------------------

        #### Search through the full file for the key words in the above list (text_param_lists)
        #### save the line numbers where those keywords occur
        lines = []
        for text_param_adjusts in text_param_lists: 
            with open(self._iieout_filename, 'r') as f:
                for line_no, line_text in enumerate(f):
                    if text_param_adjusts in line_text:
                        lines.append(line_no) 

            #### Loop thru the lines saved above and grab the data occording to its name 
            #### and location in the file
            #Sat_main_lines = [] 
            i=0
            for il,val_lines in enumerate(lines):
                check_sat = int(linecache.getline(self._iieout_filename,val_lines+1)[10:18])
                try:
                    check_iter = int((linecache.getline(self._iieout_filename,lines_params[i]-3))[57:60])
                except:
                    check_iter = int((linecache.getline(self._iieout_filename,lines_params[i]-27))[57:60])
                #print('Iter: ', check_iter)
                
                if check_iter != self.total_iterations:
#                     print(check_iter, 'is not the final iteration, move to next')
                    pass
                 
                elif check_iter == self.total_iterations:
#                     print(check_iter, 'is the final iteration')

                    data_1stline = linecache.getline(self._iieout_filename,val_lines+1) #
                    data_2ndtline = linecache.getline(self._iieout_filename,val_lines+2) #
                    data_3rdline = linecache.getline(self._iieout_filename,val_lines+3) #

                    apriorival  = float(data_1stline[19:38])
                    prevval     = float(data_2ndtline[19:38])
                    currentval  = float(data_3rdline[19:38])
                    totalDelta  = float(data_2ndtline[42:62])
                    currentDelta =  float(data_3rdline[42:62])
                    AprioriSigma = float(data_2ndtline[63:78])
                    CurrentSigma =  float(data_3rdline[63:78])

#                     print('check_iter         ',check_iter)
#                     print('check_sat          ',check_sat)
#                     print('text_param_adjusts ',text_param_adjusts)
                    SatMain_AdjustedParams[check_iter][check_sat][text_param_adjusts] = {'APRIORI_VALUE': apriorival,
#                                                          'PREVIOUS_VALUE': prevval,
                                                         'CURRENT_VALUE': currentval,
#                                                           'TOTAL_DELTA':totalDelta,
#                                                          'CURRENT_DELTA': currentDelta,
#                                                          'APRIORI_SIGMA': AprioriSigma,
#                                                          'CURRENT_SIGMA': CurrentSigma 
                                                                                        }
                    i+=1
                    #### this makes it so that you can properly index the iterations
                    i = np.mod(i,np.shape(lines_params)[0])  
        
        #####
        #### Create a list of the number TimeDep drag coeffiecient headers
        text_cd_list = ["0CD   T%02d" %i for i in np.arange(1,np.size(timedep_Cd_count)+1 )]
        lines = []
        for itt,text_param_adjusts in enumerate(text_cd_list): 
            #print(itt, text_param_adjusts)
            with open(self._iieout_filename, 'r') as f:
                for line_no, line_text in enumerate(f):
                    if text_param_adjusts in line_text:
                        lines.append(line_no) 

            #### Loop thru the lines saved above and grab the data occording to its name 
            #### and location in the file
            for il,val_lines in enumerate(lines):
                check_sat = int(linecache.getline(self._iieout_filename,val_lines+1)[10:18])
                #check_iter = int((linecache.getline(iieout_file,val_lines+1-28))[57:60])

                #### Read the lines backwars until you hit a header
                find_last_header_range = np.arange(val_lines, val_lines-1000, -1)
                for iiline, iivaline in enumerate(find_last_header_range):
                    line_find_header = linecache.getline(self._iieout_filename,iivaline)
                    if 'PARAMETER ADJUSTMENT SUMMARY' in line_find_header:
                        check_iter =int(line_find_header[57:60])
                        break
                        
                if check_iter != self.total_iterations:
#                     print(check_iter, 'is not the final iteration, move to next')
                    pass
                elif check_iter == self.total_iterations:
#                     print(check_iter, 'is the final iteration')
                
                    data_1stline = linecache.getline(self._iieout_filename,val_lines+1) #
                    data_2ndtline = linecache.getline(self._iieout_filename,val_lines+2) #
                    data_3rdline = linecache.getline(self._iieout_filename,val_lines+3) #

                    apriorival = float(data_1stline[19:41])
                    prevval = float(data_2ndtline[19:41])
                    currentval  = float(data_3rdline[19:41])
                    totalDelta = float(data_2ndtline[42:62])
                    currentDelta =  float(data_3rdline[42:62])
                    AprioriSigma = float(data_2ndtline[63:78])
                    CurrentSigma =  float(data_3rdline[63:78])

                    #print('Sat:  ', check_sat)
                    #print('Iter: ', check_iter)
                    #print('Val:  ', currentval)
                    #print('Time: ', date_timedep_cds[itt])
                    #print(date_timedep_cds[itt])
                    SatMain_AdjustedParams[check_iter][check_sat]['0CD'][date_timedep_cds[itt]] = {'APRIORI_VALUE': apriorival,
#                                                          'PREVIOUS_VALUE': prevval,
                                                         'CURRENT_VALUE': currentval,
#                                                           'TOTAL_DELTA':totalDelta,
#                                                          'CURRENT_DELTA': currentDelta,
#                                                          'APRIORI_SIGMA': AprioriSigma,
#                                                          'CURRENT_SIGMA': CurrentSigma 
                                                                                        }
        if self.empirical_accels == True:

            # 1st Value (1 or 2) Indicates direction of 9 parameter general acceleration
                # 1 - Along Track ((R x V) x R)
                # 2 - Cross Track (R x V)
                # 3 - Radial (R)

            #  2nd Value (1 or 2) Indicates type of 9 parameter general acceleration parameter
                # 1 - Cosine coefficient (A)
                # 2 - Sine coefficient (B)
                # 3 - Constant (C)            #### Extract the ACCEL9 Dates from the Cards section
            accel_9_dates = []
            for i,val in enumerate(card_inputs_range):
                    line = linecache.getline(self._iieout_filename,val)            
                    if 'ACCEL9    99' in line:
                        check_sat = int(line[18:26])
                        if check_sat == int(self.SATID):
                            accel_9_dates.append(line[45:56].strip())
            for i,val in enumerate(accel_9_dates):
                accel_9_dates[i] = val.replace(" ", "0")
            ##### Convert to datetimes
            date_GAs = pd.to_datetime(accel_9_dates, format='%y%m%d%H%M%S')
            DATE_GA= []
            for i,val in enumerate(date_GAs):
                DATE_GA.append(val)
                DATE_GA.append(val)
                DATE_GA.append(val)
                DATE_GA.append(val)

            
            text_GA_list = ["0GA 9P 11","0GA 9P 12","0GA 9P 21","0GA 9P 22"]*np.size(date_GAs)
            
#             print(np.size(text_GA_list))
#             print(np.size(DATE_GA))
            date_count = 0

            lines = []
            for itt,text_param_adjusts in enumerate(["0GA 9P 11","0GA 9P 12","0GA 9P 21","0GA 9P 22"]): 
                date_count = 0
                lines = []

                with open(self._iieout_filename, 'r') as f:
                    for line_no, line_text in enumerate(f):
                        if text_param_adjusts in line_text:
                            lines.append(line_no) 

                #### Loop thru the lines saved above and grab the data occording to its name 
                #### and location in the file
                date_count = 0
#                 print('num_lines', np.size(lines))

                for il,val_lines in enumerate(lines):
                    check_sat = int(linecache.getline(self._iieout_filename,val_lines+1)[10:18])

                    #### Read the lines backwars until you hit a header
                    find_last_header_range = np.arange(val_lines, val_lines-1000, -1)
                    for iiline, iivaline in enumerate(find_last_header_range):
                        line_find_header = linecache.getline(self._iieout_filename,iivaline)
                        if 'PARAMETER ADJUSTMENT SUMMARY' in line_find_header:
                            check_iter =int(line_find_header[57:60])
                            break

                    if check_iter != self.total_iterations:
                        pass
                    elif check_iter == self.total_iterations:

                        data_1stline = linecache.getline(self._iieout_filename,val_lines+1) #
                        data_2ndtline = linecache.getline(self._iieout_filename,val_lines+2) #
                        data_3rdline = linecache.getline(self._iieout_filename,val_lines+3) #

                        apriorival = float(data_1stline[19:41])
                        prevval = float(data_2ndtline[19:41])
                        currentval  = float(data_3rdline[19:41])
                        totalDelta = float(data_2ndtline[42:62])
                        currentDelta =  float(data_3rdline[42:62])
                        AprioriSigma = float(data_2ndtline[63:78])
                        CurrentSigma =  float(data_3rdline[63:78])

#                         print(' ')
#                         print('Sat:       ', check_sat)
#                         print('Iter:      ', check_iter)
#                         print('itt:       ', itt)
# #                         print('Time:      ', date_GAs[date_count])
#                         print('date_count ', date_count)
#                         print('Param:     ', text_param_adjusts)
#                         print(SatMain_AdjustedParams[check_iter][check_sat].keys())
                        
    
    
                        SatMain_AdjustedParams[check_iter][check_sat][text_param_adjusts][date_GAs[date_count]] = {'APRIORI_VALUE': apriorival,
#                                                          'PREVIOUS_VALUE': prevval,
                                                         'CURRENT_VALUE': currentval,
#                                                           'TOTAL_DELTA':totalDelta,
#                                                          'CURRENT_DELTA': currentDelta,
#                                                          'APRIORI_SIGMA': AprioriSigma,
#                                                          'CURRENT_SIGMA': CurrentSigma 
                                                                                        }
                        date_count += 1


    

        end = time.time()
        elapsed = end - start
#         print('Adjusted_params: ',elapsed)
        return(SatMain_AdjustedParams)
        
    def read_density_file(self):
        '''
             Read the density file.  
             The density file has a different date format than the other outputs
                       so it is dealt with here in the method.

        '''
        start = time.time()
        
        ##### Unzip the density file:
     
        
        #### The density file is a very simple format
        #### with this it can be loaded using pd.csv very easily
        DEN_csv = pd.read_csv(self._density_filename, 
                            skiprows = 1, 
                            names = ['Elapsed Secs',
                                    'YYMMDD',
                                    'HHMMSS',
                                    'Lat',
                                    'Lon',
                                    'Height (meters)',
                                    'rho (kg/m**3)',
                                    'drhodz (kg/m**3/m)',
#                                      
                                    'flux_daily',
                                    'flux_avg',
                                    'Kp',
#                                     'X',
#                                     'Y',
#                                     'Z',
#                                     'XDOT',
#                                     'YDOT',
#                                     'ZDOT',
                                  ],
                            sep = '\s+',
                            )

        #### The below takes the double precision format from Fortran (D) and puts an 
        #### E in the string to be interpeted as a float by python
        DEN_df = pd.DataFrame(DEN_csv)
        fix_D_decimal_to_E = []
        fix_D_decimal_to_E2 = []

        #### TODO: There is definitely a faster and better way to do this...
        for i,val in enumerate(DEN_df['rho (kg/m**3)']):
            val2 = DEN_df['drhodz (kg/m**3/m)'][i]

            list_val = list(val)
            list_val2 = list(val2)

            indx = list(val).index('D')
            indx2 = list(val2).index('D')

            list_val[indx] = 'E'
            list_val2[indx2] = 'E'

            list_val = "".join(list_val)
            list_val2 = "".join(list_val2)
            
            #### If you get an error here, it is likely:
            ####
            ###
            
            val_float = np.float(list_val)
            val_float2 = np.float(list_val2)

            fix_D_decimal_to_E.append(val_float)
            fix_D_decimal_to_E2.append(val_float2)

        DEN_df['rho (kg/m**3)'] = fix_D_decimal_to_E
        DEN_df['drhodz (kg/m**3/m)'] = fix_D_decimal_to_E2

        ####--------------------------------------------------------
        #### Now we must correct the formatting of the HoursMinutesSeconds column
        #### The lack of zeros was really messing things up, so I forced them in there...
        #### It works so I hope everyone is okay with how dumb it is.
        timeHHMMSS = [] 
        for i,val in enumerate(DEN_df['HHMMSS'].values.astype(int)):
            # print(len(str(val)))
            if len(str(val)) == 1:
                timehhmmss_val = '00000'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            elif len(str(val)) == 2:
                timehhmmss_val = '0000'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            elif len(str(val)) == 3:
                timehhmmss_val = '000'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            elif len(str(val)) == 4:
                timehhmmss_val = '00'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            elif len(str(val)) == 5:
                timehhmmss_val = '0'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            else:
                timeHHMMSS.append(str(val))
        DEN_df['timeHHMMSS'] = timeHHMMSS
        #--------------------------------------------------------
        #### Here we split up the years, months, and days.
        #### we add a '20' in front of the year
        # TODO:  need to find a workaround on this dumb YR variable being passed in.
        
        YR = int(str(self.YR)[-2:])
        
        YYMMDD_list = DEN_df['YYMMDD'].astype(int).astype(str)
        timeHHMMSS_list = DEN_df['timeHHMMSS'].astype(str)

        if YR < 10:
            year    = ['200' + x[:1]  for x in YYMMDD_list]
            month   = [        x[1:3] for x in YYMMDD_list]
            day     = [        x[3:]  for x in YYMMDD_list]
            hours   = [        x[:2]  for x in timeHHMMSS_list]
            minutes = [        x[2:4] for x in timeHHMMSS_list]
            secs    = [        x[4:]  for x in timeHHMMSS_list]
        else:
            year    = ['20' + x[:2]  for x in YYMMDD_list]
            month   = [       x[2:4] for x in YYMMDD_list]
            day     = [       x[4:]  for x in YYMMDD_list]
            hours   = [       x[:2]  for x in timeHHMMSS_list]
            minutes = [       x[2:4] for x in timeHHMMSS_list]
            secs    = [       x[4:]  for x in timeHHMMSS_list]
        #--------------------------------------------------------
        DEN_df['year']  = year
        DEN_df['month'] = month
        DEN_df['day']   = day
        DEN_df['hours']  = hours
        DEN_df['minutes'] = minutes
        DEN_df['secs']  = secs
        #--------------------------------------------------------
        year= list(map(int, DEN_df['year'].values))
        month= list(map(int, DEN_df['month'].values))
        day= list(map(int, DEN_df['day'].values))
        hour= list(map(int, DEN_df['hours'].values))
        minute = list(map(int, DEN_df['minutes'].values))
        second = list(map(int, DEN_df['secs'].values))

        DATE = list(map(datetime, year,month, day, hour,minute,second ))
        
        #self.DEN_df['Date']  = DATE
        DEN_df.insert(0, 'Date', DATE)
        
        #### DELETE the superfluous columns in the dataframe now.       
        del DEN_df['year']
        del DEN_df['month']
        del DEN_df['day']
        del DEN_df['hours']
        del DEN_df['minutes']
        del DEN_df['secs']
        del DEN_df['timeHHMMSS']
        del DEN_df['Elapsed Secs']
        del DEN_df['YYMMDD']
        del DEN_df['HHMMSS']
        
        #------------------------------------------------------------------------------------------
        #### DELETE THE duplicated dataset
        #------------------------------------------------------------------------------------------

#         ### Find the index for the correct date  
#         vals  = np.arange(DEN_df.index[0],DEN_df.index[-1]+1)
#         df = DEN_df.set_index('Date',drop=False ) 
#         df['i_vals'] = vals
#         index_date = df.loc[df.index.max()]['i_vals'].min() 
#         for name in DEN_df.columns:
#             DEN_df[name] = DEN_df[name][:index_date]
        
#         #### Drop the NaNs
#         DEN_df = DEN_df.dropna()
        
#         epoch_end = self.run_settings['epoch_start'][self.arcnumber]
#         epoch_end_YYMMDD     = epoch_end[:6].strip() 
#         epoch_end_HHMM       = epoch_end[7:11].strip()
#         epoch_end_dt = pd.to_datetime( epoch_end_YYMMDD+epoch_end_HHMM, format='%y%m%d%H%M%S')
        def nearest(items, pivot):
            return min(items, key=lambda x: abs(x - pivot))
        epoch_start = self.run_settings['epoch_start'][self.arcnumber]
        epoch_start_YYMMDD     = epoch_start[:6].strip() 
        epoch_start_HHMM       = epoch_start[7:11].strip()
        epoch_start_dt = pd.to_datetime( epoch_start_YYMMDD+epoch_start_HHMM, format='%y%m%d%H%M%S')
        
        vals  = np.arange(DEN_df.index[0],DEN_df.index[-1]+1)
        df = DEN_df.set_index('Date',drop=False ) 
        df['i_vals'] = vals

#         date_nearend = nearest(pd.to_datetime(df['i_vals'].index), epoch_end_dt)
        date_nearstart = nearest(pd.to_datetime(df['i_vals'].index), epoch_start_dt)
#         df['i_vals'][date_nearend]
        
        index_date = df['i_vals'][date_nearstart].min() #df['i_vals'][date_nearend].min()#df.loc[df.index.max()]['i_vals'].min() 
        for name in DEN_df.columns:
            DEN_df[name] = DEN_df[name][index_date:]

        DEN_df = DEN_df.dropna()

        #------------------------------------------------------------------------------------------

        #--------------------------------------------------------
        end = time.time()
        elapsed = end - start
#         print('Density file end: ',elapsed)
        return(DEN_df)






    def read_drag_file(self):
        '''
             Read the drag file.  
             The drag file has a different date format than the other outputs
                       so it is dealt with here in the method.

        '''
        start = time.time()
        
        ##### Unzip the density file:
     
        
        #### The density file is a very simple format
        #### with this it can be loaded using pd.csv very easily
        Drag_csv = pd.read_csv(self._drag_filename, 
#                             skiprows = 1, 
                            names = ['YYMMDD',
                                     'HHMMSS',
                                    #                                      
                                     'CD',
                                     'TOTAREA',
                                     'VELREL',
                                     'SpeedRatio',
                                    #                                      
                                    'DXDD_X',
                                    'DXDD_Y',
                                    'DXDD_Z',
                                  ],
                            sep = '\s+',
                            )

        drag_df = pd.DataFrame(Drag_csv)
        #### The below takes the double precision format from Fortran (D) and puts an 
        #### E in the string to be interpeted as a float by python
        fix_D_decimal_to_E1 = []
        fix_D_decimal_to_E2 = []
        fix_D_decimal_to_E3 = []
        fix_D_decimal_to_E4 = []
        fix_D_decimal_to_E5 = []
        fix_D_decimal_to_E6 = []
        fix_D_decimal_to_E7 = []

        #### TODO: There is definitely a faster and better way to do this...
        for i,val1 in enumerate(drag_df['CD']):
            val2 = drag_df['VELREL'][i]
            val3 = drag_df['DXDD_X'][i]
            val4 = drag_df['DXDD_Y'][i]
            val5 = drag_df['DXDD_Z'][i]
            val6 = drag_df['TOTAREA'][i]
            val7 = drag_df['SpeedRatio'][i]

            list_val1 = list(val1)
            list_val2 = list(val2)
            list_val3 = list(val3)
            list_val4 = list(val4)
            list_val5 = list(val5)
            list_val6 = list(val6)
            list_val7 = list(val7)

            indx1 = list(val1).index('D')
            indx2 = list(val2).index('D')
            indx3 = list(val3).index('D')
            indx4 = list(val4).index('D')
            indx5 = list(val5).index('D')
            indx6 = list(val6).index('D')
            indx7 = list(val7).index('D')

            list_val1[indx1] = 'E'
            list_val2[indx2] = 'E'
            list_val3[indx3] = 'E'
            list_val4[indx4] = 'E'
            list_val5[indx5] = 'E'
            list_val6[indx6] = 'E'
            list_val7[indx7] = 'E'

            list_val1 = "".join(list_val1)
            list_val2 = "".join(list_val2)
            list_val3 = "".join(list_val3)
            list_val4 = "".join(list_val4)
            list_val5 = "".join(list_val5)
            list_val6 = "".join(list_val6)
            list_val7 = "".join(list_val7)
            
            #### If you get an error here, it is likely:
            ####  due to some values not being floats?
            ###
            val_float1 = np.float(list_val1)
            val_float2 = np.float(list_val2)
            val_float3 = np.float(list_val3)
            val_float4 = np.float(list_val4)
            val_float5 = np.float(list_val5)
            val_float6 = np.float(list_val6)
            val_float7 = np.float(list_val7)

            fix_D_decimal_to_E1.append(val_float1)
            fix_D_decimal_to_E2.append(val_float2)
            fix_D_decimal_to_E3.append(val_float3)
            fix_D_decimal_to_E4.append(val_float4)
            fix_D_decimal_to_E5.append(val_float5)
            fix_D_decimal_to_E6.append(val_float6)
            fix_D_decimal_to_E7.append(val_float7)

        drag_df['CD']         = fix_D_decimal_to_E1
        drag_df['VELREL']     = fix_D_decimal_to_E2
        drag_df['DXDD_X']     = fix_D_decimal_to_E3
        drag_df['DXDD_Y']     = fix_D_decimal_to_E4
        drag_df['DXDD_Z']     = fix_D_decimal_to_E5
        drag_df['TOTAREA']    = fix_D_decimal_to_E6
        drag_df['SpeedRatio'] = fix_D_decimal_to_E7


        ####--------------------------------------------------------
        #### Now we must correct the formatting of the HoursMinutesSeconds column
        #### The lack of zeros was really messing things up, so I forced them in there...
        #### It works so I hope everyone is okay with how dumb it is.
        timeHHMMSS = [] 
        for i,val in enumerate(drag_df['HHMMSS'].values.astype(int)):
            # print(len(str(val)))
            if len(str(val)) == 1:
                timehhmmss_val = '00000'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            elif len(str(val)) == 2:
                timehhmmss_val = '0000'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            elif len(str(val)) == 3:
                timehhmmss_val = '000'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            elif len(str(val)) == 4:
                timehhmmss_val = '00'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            elif len(str(val)) == 5:
                timehhmmss_val = '0'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            else:
                timeHHMMSS.append(str(val))
        drag_df['timeHHMMSS'] = timeHHMMSS
        #--------------------------------------------------------
        #### Here we split up the years, months, and days.
        #### we add a '20' in front of the year
        # TODO:  need to find a workaround on this dumb YR variable being passed in.
        
        YR = int(str(self.YR)[-2:])
        
        YYMMDD_list = drag_df['YYMMDD'].astype(int).astype(str)
        timeHHMMSS_list = drag_df['timeHHMMSS'].astype(str)

        if YR < 10:
            year    = ['200' + x[:1]  for x in YYMMDD_list]
            month   = [        x[1:3] for x in YYMMDD_list]
            day     = [        x[3:]  for x in YYMMDD_list]
            hours   = [        x[:2]  for x in timeHHMMSS_list]
            minutes = [        x[2:4] for x in timeHHMMSS_list]
            secs    = [        x[4:]  for x in timeHHMMSS_list]
        else:
            year    = ['20' + x[:2]  for x in YYMMDD_list]
            month   = [       x[2:4] for x in YYMMDD_list]
            day     = [       x[4:]  for x in YYMMDD_list]
            hours   = [       x[:2]  for x in timeHHMMSS_list]
            minutes = [       x[2:4] for x in timeHHMMSS_list]
            secs    = [       x[4:]  for x in timeHHMMSS_list]
        #--------------------------------------------------------
        drag_df['year']  = year
        drag_df['month'] = month
        drag_df['day']   = day
        drag_df['hours']  = hours
        drag_df['minutes'] = minutes
        drag_df['secs']  = secs
        #--------------------------------------------------------
        year= list(map(int, drag_df['year'].values))
        month= list(map(int, drag_df['month'].values))
        day= list(map(int, drag_df['day'].values))
        hour= list(map(int, drag_df['hours'].values))
        minute = list(map(int, drag_df['minutes'].values))
        second = list(map(int, drag_df['secs'].values))

        DATE = list(map(datetime, year,month, day, hour,minute,second ))
        
        #self.DEN_df['Date']  = DATE
        drag_df.insert(0, 'Date', DATE)
        
        #### DELETE the superfluous columns in the dataframe now.       
        del drag_df['year']
        del drag_df['month']
        del drag_df['day']
        del drag_df['hours']
        del drag_df['minutes']
        del drag_df['secs']
        del drag_df['timeHHMMSS']
        del drag_df['YYMMDD']
        del drag_df['HHMMSS']
        
        ### DELETE the duplicated dataset
        #### Find the index for the correct date and 
#         vals  = np.arange(drag_df.index[0],drag_df.index[-1]+1)
#         df = drag_df.set_index('Date',drop=False ) 
#         df['i_vals'] = vals
#         index_date = df.loc[df.index.max()]['i_vals'].min() 
#         for name in drag_df.columns:
#             drag_df[name] = drag_df[name][:index_date]
        
#         #### Drop the NaNs
#         drag_df = drag_df.dropna()
        def nearest(items, pivot):
            return min(items, key=lambda x: abs(x - pivot))
        epoch_start = self.run_settings['epoch_start'][self.arcnumber]
        epoch_start_YYMMDD     = epoch_start[:6].strip() 
        epoch_start_HHMM       = epoch_start[7:11].strip()
        epoch_start_dt = pd.to_datetime( epoch_start_YYMMDD+epoch_start_HHMM, format='%y%m%d%H%M%S')
        vals  = np.arange(drag_df.index[0],drag_df.index[-1]+1)
        df = drag_df.set_index('Date',drop=False ) 
        df['i_vals'] = vals
        date_nearstart = nearest(pd.to_datetime(df['i_vals'].index), epoch_start_dt)
        
        index_date = df['i_vals'][date_nearstart].min() #df['i_vals'][date_nearend].min()#df.loc[df.index.max()]['i_vals'].min() 
        for name in drag_df.columns:
            drag_df[name] = drag_df[name][index_date:]

        drag_df = drag_df.dropna()
        #--------------------------------------------------------
        end = time.time()
        elapsed = end - start
#         print('Density file end: ',elapsed)
        return(drag_df)




    def read_accel_file(self):
        '''
             Read the acceleration file that is written from the F.f90 routine.  
        '''
        
        
        start = time.time()
        
        ##### Unzip the density file:
     
        
        #### The density file is a very simple format
        #### with this it can be loaded using pd.csv very easily
        Accel_csv = pd.read_csv(self._accel_filename, 
#                             skiprows = 1, 
                            names = ['YYMMDD',
                                    'HHMMSS',
                                    'drag_acc_mag',
                                    'solrad_acc_mag',
                                    'albedo_acc_mag',
                                    'tidal_acc_mag',
                                    'genrel_acc_mag',
                                    'fburn_acc_mag',
                                  ],
                            sep = '\s+',
                            )

        #### The below takes the double precision format from Fortran (D) and puts an 
        #### E in the string to be interpeted as a float by python
        accel_df = pd.DataFrame(Accel_csv)
        fix_D_decimal_to_E1 = []
        fix_D_decimal_to_E2 = []
        fix_D_decimal_to_E3 = []
        fix_D_decimal_to_E4 = []
        fix_D_decimal_to_E5 = []
        fix_D_decimal_to_E6 = []

        #### TODO: There is definitely a faster and better way to do this...
        for i,val1 in enumerate(accel_df['drag_acc_mag']):
            val2 = accel_df['solrad_acc_mag'][i]
            val3 = accel_df['albedo_acc_mag'][i]
            val4 = accel_df['tidal_acc_mag'][i]
            val5 = accel_df['genrel_acc_mag'][i]
            val6 = accel_df['fburn_acc_mag'][i]

            list_val1 = list(val1)
            list_val2 = list(val2)
            list_val3 = list(val3)
            list_val4 = list(val4)
            list_val5 = list(val5)
            list_val6 = list(val6)

            indx1 = list(val1).index('D')
            indx2 = list(val2).index('D')
            indx3 = list(val3).index('D')
            indx4 = list(val4).index('D')
            indx5 = list(val5).index('D')
            indx6 = list(val6).index('D')

            list_val1[indx1] = 'E'
            list_val2[indx2] = 'E'
            list_val3[indx3] = 'E'
            list_val4[indx4] = 'E'
            list_val5[indx5] = 'E'
            list_val6[indx6] = 'E'

            list_val1 = "".join(list_val1)
            list_val2 = "".join(list_val2)
            list_val3 = "".join(list_val3)
            list_val4 = "".join(list_val4)
            list_val5 = "".join(list_val5)
            list_val6 = "".join(list_val6)
            
            #### If you get an error here, it is likely:
            ####  due to some values not being floats?
            ###
            val_float1 = np.float(list_val1)
            val_float2 = np.float(list_val2)
            val_float3 = np.float(list_val3)
            val_float4 = np.float(list_val4)
            val_float5 = np.float(list_val5)
            val_float6 = np.float(list_val6)

            fix_D_decimal_to_E1.append(val_float1)
            fix_D_decimal_to_E2.append(val_float2)
            fix_D_decimal_to_E3.append(val_float3)
            fix_D_decimal_to_E4.append(val_float4)
            fix_D_decimal_to_E5.append(val_float5)
            fix_D_decimal_to_E6.append(val_float6)

        accel_df['drag_acc_mag']      = fix_D_decimal_to_E1
        accel_df['solrad_acc_mag']       = fix_D_decimal_to_E2
        accel_df['albedo_acc_mag']  = fix_D_decimal_to_E3
        accel_df['tidal_acc_mag']  = fix_D_decimal_to_E4
        accel_df['genrel_acc_mag']  = fix_D_decimal_to_E5
        accel_df['fburn_acc_mag']  = fix_D_decimal_to_E6

        ####--------------------------------------------------------
        #### Now we must correct the formatting of the HoursMinutesSeconds column
        #### The lack of zeros was really messing things up, so I forced them in there...
        #### It works so I hope everyone is okay with how dumb it is.
        timeHHMMSS = [] 
        for i,val in enumerate(accel_df['HHMMSS'].values.astype(int)):
            # print(len(str(val)))
            if len(str(val)) == 1:
                timehhmmss_val = '00000'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            elif len(str(val)) == 2:
                timehhmmss_val = '0000'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            elif len(str(val)) == 3:
                timehhmmss_val = '000'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            elif len(str(val)) == 4:
                timehhmmss_val = '00'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            elif len(str(val)) == 5:
                timehhmmss_val = '0'+ str(val)
                timeHHMMSS.append(timehhmmss_val)
            else:
                timeHHMMSS.append(str(val))
        accel_df['timeHHMMSS'] = timeHHMMSS
        #--------------------------------------------------------
        #### Here we split up the years, months, and days.
        #### we add a '20' in front of the year
        # TODO:  need to find a workaround on this dumb YR variable being passed in.
        
        YR = int(str(self.YR)[-2:])
        
        YYMMDD_list = accel_df['YYMMDD'].astype(int).astype(str)
        timeHHMMSS_list = accel_df['timeHHMMSS'].astype(str)

        if YR < 10:
            year    = ['200' + x[:1]  for x in YYMMDD_list]
            month   = [        x[1:3] for x in YYMMDD_list]
            day     = [        x[3:]  for x in YYMMDD_list]
            hours   = [        x[:2]  for x in timeHHMMSS_list]
            minutes = [        x[2:4] for x in timeHHMMSS_list]
            secs    = [        x[4:]  for x in timeHHMMSS_list]
        else:
            year    = ['20' + x[:2]  for x in YYMMDD_list]
            month   = [       x[2:4] for x in YYMMDD_list]
            day     = [       x[4:]  for x in YYMMDD_list]
            hours   = [       x[:2]  for x in timeHHMMSS_list]
            minutes = [       x[2:4] for x in timeHHMMSS_list]
            secs    = [       x[4:]  for x in timeHHMMSS_list]
        #--------------------------------------------------------
        accel_df['year']  = year
        accel_df['month'] = month
        accel_df['day']   = day
        accel_df['hours']  = hours
        accel_df['minutes'] = minutes
        accel_df['secs']  = secs
        #--------------------------------------------------------
        year= list(map(int, accel_df['year'].values))
        month= list(map(int, accel_df['month'].values))
        day= list(map(int, accel_df['day'].values))
        hour= list(map(int, accel_df['hours'].values))
        minute = list(map(int, accel_df['minutes'].values))
        second = list(map(int, accel_df['secs'].values))

        DATE = list(map(datetime, year,month, day, hour,minute,second ))
        
        accel_df.insert(0, 'Date', DATE)
        
        #### DELETE the superfluous columns in the dataframe now.       
        del accel_df['year']
        del accel_df['month']
        del accel_df['day']
        del accel_df['hours']
        del accel_df['minutes']
        del accel_df['secs']
        del accel_df['timeHHMMSS']
#         del accel_df['Elapsed Secs']
        del accel_df['YYMMDD']
        del accel_df['HHMMSS']
        
        ### DELETE THE duplicated dataset
        #### Find the index for the correct date and 
#         vals  = np.arange(accel_df.index[0],accel_df.index[-1]+1)
#         df = accel_df.set_index('Date',drop=False ) 
#         df['i_vals'] = vals
#         index_date = df.loc[df.index.max()]['i_vals'].min() 
#         for name in accel_df.columns:
#             accel_df[name] = accel_df[name][:index_date]
        
#         #### Drop the NaNs
#         accel_df = accel_df.dropna()
        def nearest(items, pivot):
            return min(items, key=lambda x: abs(x - pivot))
        epoch_start = self.run_settings['epoch_start'][self.arcnumber]
        epoch_start_YYMMDD     = epoch_start[:6].strip() 
        epoch_start_HHMM       = epoch_start[7:11].strip()
        epoch_start_dt = pd.to_datetime( epoch_start_YYMMDD+epoch_start_HHMM, format='%y%m%d%H%M%S')
        vals  = np.arange(accel_df.index[0],accel_df.index[-1]+1)
        df = accel_df.set_index('Date',drop=False ) 
        df['i_vals'] = vals
        date_nearstart = nearest(pd.to_datetime(df['i_vals'].index), epoch_start_dt)
        
        index_date = df['i_vals'][date_nearstart].min() #df['i_vals'][date_nearend].min()#df.loc[df.index.max()]['i_vals'].min() 
        for name in accel_df.columns:
            accel_df[name] = accel_df[name][index_date:]
        accel_df = accel_df.dropna()

        #--------------------------------------------------------
        end = time.time()
        elapsed = end - start
#         print('Density file end: ',elapsed)
        return(accel_df)


    def read_observed_resids(self):
        '''
        Now find all the instances of the OBSERVATION RESIDUALS 
        header at the last iteration.  

        We will want to store these into a dictionary and save out the following:
            - configuration type
            - contributing satellites
            - all residual data

        We first grab the start line and end line of the whole observation residual section
        next we loop through each of line in this range and save data if the block number is a     float.
        There are weird headers that have to be accounted for.

        '''

        start = time.time()

#         #### How many iterations are in this run?
        (self.total_iterations, self.str_iteration) = self.iteration_number(self._iieout_filename)
        
#         print(len(iteration) )
       
        
        
        #### I've run into an issue where some arcs have way more 
        ####       iterations that seems correct (such as 11). 
        ####       In these cases, it seems like the final iteration
        ####       listed in IIEOUT is the 9th  (last single digit??)
        ####
        #### TODO:  WILL NEED TO FIX THIS AT SOME POINT if there even is anything to fix??
        ####
#         if int(iteration) >= 10:
#             iteration = '9'

        #### the observation residuals section is contained between the following two headers
        #### therefore, we use these as bookends for the section of the IIEOUT file to loop thru
        
#         if len(self.total_iterations) == 1:
#             self.str_iteration = ' '+self.total_iterations  # add a space if the iteration number is not double digit
#         else:
#             self.str_iteration =     self.total_iterations

#         resids_iters = {}
    
#         for i_iter in [' 1', self.str_iteration]:
        text_obs_resid = 'OBSERVATION RESIDUALS FOR ARC  1 FOR INNER ITERATION '+ ( self.str_iteration)
        end_of_section = 'RESIDUAL SUMMARY BY STATION AND TYPE FOR ARC  1 INNER ITERATION '+ ( self.str_iteration)
        lines_list_1 = [] 
        lines_list_2 = []

        #### The below grabs the line numbers of the section headers 
        #### The Observation Residuals end at the first instance of the Summary by Station
        with open(self._iieout_filename, 'r') as f:
            for line_no, line in enumerate(f):
                if text_obs_resid in line:
                    lines_list_1.append(line_no)
                elif end_of_section in line:
                    lines_list_2.append(line_no)
#         print('self.str_iteration', self.str_iteration)
#         print('ll1',lines_list_1)
#         print('ll2',lines_list_2)

        #### If there are is not a list of residuals the snippet under try: 
        ####     will kick an error.  This is added to allows the code to 
        ####     continue going without error.  
        #### This issue was tied to the above issue with there being too many iterations.
        try:
            residual_range  = np.arange(lines_list_1[0], lines_list_2[0]+1)
        except:
            residual_range  = np.arange(lines_list_1[0], lines_list_2+1)

        #### Initialize some lists to save out the data
        list_config_type  = []
        list_SAT_main     = []
        list_note         = []
        list_track_1        = []
        list_track_2        = []
        list_YYMMDD       = []
        list_HHMM         = []
        list_SEC_UTC      = []
        list_Observation  = []
        list_Residual     = []
        list_RatiotoSigma = []
        list_Elev1        = []
        list_Elev2        = []
        list_OBS_No       = []
        list_Block        = []

        #### Loop through the residual section and save out data.
        #### There are some header quandries that must be dealt with
        ####      and the method for doing so is in the if, elif, else statements below
        #### This quandry is that the data that follows different kinds of headers 
        ####     is in different columns of the fixed width format file
        for i,val in enumerate(residual_range):
            line = linecache.getline(self._iieout_filename,val)
            #### HEADER TYPE 1
            if 'STATION-SATELLITE CONFIGURATION' in line:
                # print('HEADER Type 1')
                config_type = line[35:44]
                SAT_main = line[54:62]
                #### The location of the columns changes between SLR and GPS... 
                if self.DATA_TYPE == 'GPS':
                    track_1 = line[72:80]
                    track_2 = line[90:98]
                    note = np.nan
                elif self.DATA_TYPE == 'SLR':
                    track_1 = line[44:53]
                    track_2 = np.nan
                    note = np.nan
                elif self.DATA_TYPE == 'PCE':
                    track_1 = line[72:80]
                    track_2 = line[90:98]
                    note = np.nan

            #### HEADER TYPE 2
            ####         within HEADER TYPE 2 the GPS data has further another header type
            elif 'STATION-SAT CONFIG.' in line:
                if self.DATA_TYPE == 'GPS':
                    if 'DSS1WRNG' in line:
                        config_type = line[46:56]
                        SAT_main = line[65:73]
                        note = np.nan
                        track_1 = line[83:91]
                        track_2 = line[100:109]
                    else:
                        config_type = line[46:56]
                        SAT_main = np.nan
                        note = line[55:63]
                        track_1 = line[65:74]
                        track_2 = np.nan
                elif self.DATA_TYPE == 'SLR':
                    config_type = line[46:55]
                    SAT_main = line[65:73]
                    note = np.nan
                    track_1 = line[55:64]
                    track_2 = np.nan      
                elif self.DATA_TYPE == 'PCE':
                    config_type = line[46:55]
                    SAT_main = line[65:73]
                    note = np.nan
                    track_1 = line[55:64]
                    track_2 = np.nan      
            ####  If the block number is an integer 
            ####        (which it will be if the line contains data) 
            ####         then save the data out
            try:
                BLOCK_no = int(line[117:125])
                YYMMDD       = line[1:8]
                HHMM         = line[8:13]
                SEC_UTC      = line[13:23]
                Observation  = line[26:42]
                Residual     = line[42:57]
                RatiotoSigma = line[57:70]
                Elev1        = line[71:84]
                Elev2        = line[85:96]
                OBS_No       = line[106:117]
                Block        = line[117:125]

                list_config_type.append(config_type)
                list_SAT_main.append(SAT_main)
                list_note.append(note)
                list_track_1.append(track_1)
                list_track_2.append(track_2)
                list_YYMMDD.append(YYMMDD)
                list_HHMM.append(HHMM)
                list_SEC_UTC.append(SEC_UTC)
                list_Observation.append(Observation)
                list_Residual.append(Residual)
                list_RatiotoSigma.append(RatiotoSigma)
                list_Elev1.append(Elev1)
                list_Elev2.append(Elev2)
                list_OBS_No.append(OBS_No)
                list_Block.append(Block)
            except:
        #         print('Not a data block', line[117:125]) 
                pass

        ####  Save all the above data to a dictionary then convert to a dataframe
        resids_dict= {'StatSatConfig' : list_config_type,
                      'Sat_main'      : list_SAT_main   ,
                      'track_1'         : list_track_1      ,
                      'track_2'         : list_track_2      ,
                      'Note'          : list_note       ,
                      'YYMMDD'        : list_YYMMDD      ,
                      'HHMM'          : list_HHMM        ,
                      'SEC_UTC'       : list_SEC_UTC      ,
                      'Observation'   : list_Observation  ,
                      'Residual'      : list_Residual     ,
                      'RatiotoSigma'  : list_RatiotoSigma ,
                      'Elev1'         : list_Elev1       ,
                      'Elev2'         : list_Elev2       ,
                      'OBS_No'        : list_OBS_No      ,
                      'Block'         : list_Block       ,
                     } 
        resids_df = pd.DataFrame.from_dict(resids_dict)
        linecache.clearcache()
        #
        # ----------------------------------------------------------------------------------
        #
        #### Fix the date column:
        dates = self.make_datetime_column(resids_df, self.YR)

        resids_df.insert(0, 'Date', dates)

        #### The ratio-to-sigma columns has some weird strings in it
        ####        ValueError: could not convert string to float: ' -16.0620*'
        ####        remove them
        fix_string = []
        for i,val in enumerate(resids_df['RatiotoSigma']):
            try:
                float(val)
                fix_string.append(val)
            except:
                # print(i, val)
                fix_string.append(val[:-1])

        resids_df['RatiotoSigma'] = fix_string
        ####   
        #### Some of the elevations are empty.  Replace the empty strings with nans
        ####
        elev_fix = []
        for i,val in enumerate(resids_df['Elev1']):
            try:
                float(val)
                elev_fix.append(float(val))
            except:
                elev_fix.append(np.nan)
        resids_df['Elev1'] = elev_fix
        elev_fix = []
        for i,val in enumerate(resids_df['Elev2']):
            try:
                float(val)
                elev_fix.append(float(val))
            except:
                elev_fix.append(np.nan)
        resids_df['Elev2'] = elev_fix

                        # def test_apply(x):
                        #     try:
                        #         return float(x)
                        #     except ValueError:
                        #         return None

                        # cleanDF = test['Value'].apply(test_apply).dropna()        
        def test_apply(x):
            try:
                return float(x)
            except:
                return np.nan
#         cleanDF = test['Value'].apply(test_apply).dropna()



        resids_df['Observation']  = resids_df['Observation'].apply(test_apply)  #.astype(float)
        resids_df['Residual']     = resids_df['Residual'].apply(test_apply)     #.astype(float)
        resids_df['RatiotoSigma'] = resids_df['RatiotoSigma'].apply(test_apply) #.astype(float)

        #### Delete the superfluous columns
        del resids_df['year']
        del resids_df['month']
        del resids_df['day']
        del resids_df['hours']
        del resids_df['minutes']
        del resids_df['secs']
        del resids_df['millsecs']
        del resids_df['timeHHMM']
        del resids_df['YYMMDD']
        del resids_df['HHMM']
        del resids_df['SEC_UTC']
        del resids_df['Block']
        del resids_df['OBS_No']

        end = time.time()
        elapsed = end - start
#         print('Observed residuals: ',elapsed)
#         resids_iters[i_iter] = resids_df
    
#         return(resids_iters)
        return(resids_df)

        
    def read_resid_measurement_summaries(self):
        '''
        This function reads in the residuals summary from the massive IIEOUT file.

        For residuals, there are specific station-satellite configurations.  
        It is prudent to read in each configuration and save which satellites make it up.  

        '''
        start = time.time()

#         # How many iterations are in this run?
#         iteration = int(self.iteration_number(self._iieout_filename))

        # Initialize the resid measurement summary storage
        resid_meas_summry = pd.DataFrame(data={'Binary'   :[], 
                              'NUMBER'    :[],
                              'MEAN' :[],
                              'RMS'       :[], 
                              'No.-WTD'       :[],
                              'WTD-MEAN'       :[],
                              'WTD-RMS'    :[], 
                              'TYPE1'    :[],
                              'TYPE2'    :[],
                                        })

        (self.total_iterations, self.str_iteration) = self.iteration_number(self._iieout_filename)
        
        for i_iter, iter_val in enumerate(np.arange(0, self.total_iterations)+1):
            #
            # ----------------------------------------------------------------------------------
            #
            #### We first find how many observations there are. 
            #### This is stored in the Residual Summary by Measurement
            #### We use the observation number to find where to STOP reading in data. 
            #### We find this by finding the Summary by Measurment Header
            text_smry_meas = ('RESIDUAL SUMMARY BY MEASUREMENT TYPE FOR ARC  1 INNER ITERATION '+
                                   self.str_iteration +
                               ' OF GLOBAL ITERATION 1')
            ####
            #### Loop through the iieout file and find where the above header exists 
            #### (it should only show up once)
            #### detect the observation types
            ####
            with open(self._iieout_filename, 'r') as f:
                for line_no, line_text in enumerate(f):
                    if text_smry_meas in line_text:
                        # Save the exact number where this section (based on the header) appears:
                        text_smry_meas_line_no = line_no
            #
            #### Save out the Observation residual types:
            #### I read in the whole section and stop reading in when the first value of the line is no longer 0
            #### use the countline to get the number of lines in this section
            count_lines = 1
            line = linecache.getline(self._iieout_filename,text_smry_meas_line_no+count_lines)
            if int(line[0]) == 1: 
                is_integer = True
            else:
                print('Started out as wrong value in SUMMRY of MEASURMENTS')
                is_integer = False

            while is_integer == True:
                try:
                    int(line[0])
            #         print('Its an INTEGER')
                    line = linecache.getline(self._iieout_filename,text_smry_meas_line_no+count_lines)
                    count_lines += 1
                    is_integer = True
        #             print(line)
                except:
                    is_integer = False
                    count_lines -= 3

            resid_meas_summry_iter = pd.read_csv(self._iieout_filename, 
                                       skiprows = text_smry_meas_line_no + 2  , 
                                       nrows =  count_lines-2,
                                       sep = '\s+',
                                       names =['Binary' ,
                                                'NUMBER' ,
                                                'MEAN' ,
                                                'RMS' ,
                                                'No.-WTD' ,
                                                'WTD-MEAN' ,
                                                'WTD-RMS' ,
                                                'TYPE1' ,
                                                'TYPE2' ,
                                                ] ,
                                                )

            # Fix some formatting issues with the way the TYPES were read in
            # concatenate the PCE and X (Y, Z) to make one column
            type_fixed = []
            for i,val in resid_meas_summry_iter.iterrows():
                try:
                    float(val['TYPE2'])
                    type_fixed.append(val['TYPE1'])
                    pass
                except:
                    type_fixed.append(val['TYPE1'] + val['TYPE2'])
            resid_meas_summry_iter['TYPE'] = type_fixed          


            resid_meas_summry_iter['Iter'] = iter_val

            resid_meas_summry = pd.concat([ resid_meas_summry, resid_meas_summry_iter])

        del resid_meas_summry['TYPE1']
        del resid_meas_summry['TYPE2']
        del resid_meas_summry['Binary']

        end = time.time()
        elapsed = end - start
#         print('resid meas summary: ',elapsed)

#         final_iter  = float(self.total_iterations)
#         index_iter  = resid_meas_summry['Iter'] == final_iter


#         #### Index the component RMS values
#         index_PCEX = resid_meas_summry[index_iter]['TYPE'] =='PCEX'
#         index_PCEY = resid_meas_summry[index_iter]['TYPE'] =='PCEY'
#         index_PCEZ = resid_meas_summry[index_iter]['TYPE'] =='PCEZ'
#         #
#         rms_PCEX = float(resid_meas_summry[index_iter][index_PCEX]['RMS'])
#         rms_PCEY = float(resid_meas_summry[index_iter][index_PCEY]['RMS'])
#         rms_PCEZ = float(resid_meas_summry[index_iter][index_PCEZ]['RMS'])
#         w_rms_PCEX = float(resid_meas_summry[index_iter][index_PCEX]['WTD-RMS'])
#         w_rms_PCEY = float(resid_meas_summry[index_iter][index_PCEY]['WTD-RMS'])
#         w_rms_PCEZ = float(resid_meas_summry[index_iter][index_PCEZ]['WTD-RMS'])
    
#         #### Total number of observational Residuals
#         N = np.sum(np.array(resid_meas_summry[index_iter]['NUMBER']))
#         RMS_total = np.sqrt( ( np.square(rms_PCEX) + np.square(rms_PCEY) + np.square(rms_PCEZ) )/3  )
#         w_RMS_total = np.sqrt( ( np.square(w_rms_PCEX) + np.square(w_rms_PCEY) + np.square(w_rms_PCEZ) )/3  )


#         resid_meas_summry['RMS_total'] = RMS_total
#         resid_meas_summry['WTD-RMS_total'] = w_RMS_total
        
        return(resid_meas_summry)
        
        
        
    def read_RunSummary_iieout(self):
        start = time.time()

        model = self.den_model  
        sat_name = self.SATELLITE_dir       
        data_type = self.DATA_TYPE
#         if 'Verbose_Stats' in self.params:
#             Verbose_Stats = self.params['Verbose_Stats'] 
#         else:
#             Verbose_Stats = False
        
        if self.verbose: 
            Verbose_Stats = True
        else: 
            Verbose_Stats = False

        dict_stats = {}
        dict_stats['density_model'] = model
        dict_stats['sat_name']      = sat_name
        dict_stats['data_type']     = data_type


        StatsSection_start = 'CONVERGENCE WITHIN  2.0 PERCENT '
        StatsSection_end   = 'THE FOLLOWING ARE GEOCENTRIC LATITUDE AND GEOCENTRIC LONGITUDE'
        line_no_1 = []
        line_no_2 = []

        #### 
        #### FIND THE STATS SECTION
        with open(self._iieout_filename, 'r') as f:
            for line_no, line_text in enumerate(f):
                if StatsSection_start in line_text :
                    line_no_1.append(line_no)
                elif StatsSection_end in line_text :
                    line_no_2.append(line_no)


        Build_stats_lines = []
        StatsSection_range = np.arange(line_no_1[0], line_no_2[0]+1)

        headers_list_col1 = ["X POS",
                        "Y POS",
                        "Z POS",
                        "X VEL",
                        "Y VEL",
                        "Z VEL",
                        "RMS POS",]


        headers_list_col2 = ["S.M.A.",
                        "ECCEN",
                        "INCLIN",
                        "NODE",
                        "PERG   =",
                        "MEAN",
                        "RMS VEL",]

        for iheader, valheader in enumerate(headers_list_col1):
            for i,val in enumerate(StatsSection_range):
                line = linecache.getline(self._iieout_filename,val)
                if valheader in line:
                    dict_stats[valheader] = float(line[10:31].strip())

        for iheader, valheader in enumerate(headers_list_col2):
            for i,val in enumerate(StatsSection_range):
                line = linecache.getline(self._iieout_filename,val)
                if valheader in line:
                    dict_stats[valheader] = float(line[50:74].strip())


        StatsSection_start = 'THE FOLLOWING ARE GEOCENTRIC LATITUDE AND GEOCENTRIC LONGITUDE'
        StatsSection_end   = 'CORRELATION COEFFICIENTS FOR ADJUSTED PARAMETERSAFTER'
        line_no_1 = []
        line_no_2 = []

        # 
        #    FIND THE STATS SECTION

        with open(self._iieout_filename, 'r') as f:
            for line_no, line_text in enumerate(f):
                if StatsSection_start in line_text :
                    line_no_1.append(line_no)
                elif StatsSection_end in line_text :
                    line_no_2.append(line_no)


        Build_stats_lines = []
        StatsSection_range = np.arange(line_no_1[0], line_no_2[0]+1)
        headers_list_col1 = ["APOGEE",
                             "PERIGEE",
                             "PERIOD",
                             "DRAG",]


        headers_list_col2 = ["ASC NODE RATE",
                        "ARG PERG RATE",
                        "PERIOD   RATE",
                        "S-M AXIS RATE",]

        for iheader, valheader in enumerate(headers_list_col1):
            for i,val in enumerate(StatsSection_range):
                line = linecache.getline(self._iieout_filename,val)
                if valheader in line:
                    dict_stats[valheader] = float(line[12:30].strip())

        for iheader, valheader in enumerate(headers_list_col2):
            for i,val in enumerate(StatsSection_range):
                line = linecache.getline(self._iieout_filename,val)
                if valheader in line:
                    dict_stats[valheader] = float(line[58:70].strip())

        StatsSection_start = 'CONVERGENCE WITHIN  2.0 PERCENT '
        StatsSection_end   = 'CORRELATION COEFFICIENTS FOR ADJUSTED PARAMETERSAFTER'
        line_no_1 = []
        line_no_2 = []
        with open(self._iieout_filename, 'r') as f:
            for line_no, line_text in enumerate(f):
                if StatsSection_start in line_text :
                    line_no_1.append(line_no)
                elif StatsSection_end in line_text :
                    line_no_2.append(line_no)
    #     Build_stats_lines = []
        StatsSection_range = np.arange(line_no_1[0], line_no_2[0]+1)            

        for i,val in enumerate(StatsSection_range):
            line = linecache.getline(self._iieout_filename,val)
            if 'START' and 'END' in line:
            #  START 1181107 2100  0.0000EPOCH 1181107 2100  0.0000  END 1181110  300  0.0000
            # 123456789012345  
                dict_stats['START_epoch'] = float(line[7:14].strip())
                dict_stats['END_epoch']   = float(line[59:67].strip())
            if 'INTEGRATION STEP SIZE' in line:
                dict_stats['INTEGRATION_STEP_secs'] = float(line[41:50].strip())     
            if 'SAT. ID' in line:
                dict_stats['SAT. ID'] = int(line[11:19].strip())
            if 'AREA(M**2)' in line:
                dict_stats['AREA(M**2)'] = float(line[34:45].strip())
            if 'MASS(KG)' in line:
                dict_stats['MASS(KG)'] = float(line[59:69].strip())     
            if 'ORBIT INTEGRATION STEPS' in line:
                dict_stats['ORBIT INTEGRATION STEPS'] = float(line[3:10].strip())     
            if 'VARIATIONAL EQUATION INTEGRATION STEPS' in line:
                dict_stats['VARIATIONAL EQUATION INTEGRATION STEPS'] = float(line[3:10].strip())     

            if 'LATITUDE =' in line:
                dict_stats['LATITUDE_geocentric'] = float(line[13:21].strip()) 

            if 'LONGITUDE =' in line:
                dict_stats['LONGITUDE_geocentric'] = float(line[39:47].strip())

            if 'HEIGHT =' in line:
                dict_stats['HEIGHT_geocentric_from_SMA'] = float(line[66:82].strip())






        if Verbose_Stats == True:
#             def quicklook_IIEOUTstatsprint(self._iieout_filename, model, sat_name, data_type):
            StatsSection_start = 'CONVERGENCE WITHIN  2.0 PERCENT '
            StatsSection_end   = 'CORRELATION COEFFICIENTS FOR ADJUSTED PARAMETERSAFTER'
            line_no_1 = []
            line_no_2 = []

            # 
            #    FIND THE STATS SECTION

            with open(self._iieout_filename, 'r') as f:
                for line_no, line_text in enumerate(f):
                    if StatsSection_start in line_text :
                        line_no_1.append(line_no)
                    elif StatsSection_end in line_text :
                        line_no_2.append(line_no)


            Build_stats_lines = []
            StatsSection_range = np.arange(line_no_1[0], line_no_2[0]+1)
#                 dict_stats = {}

            # 
            #     PRINT THE STATS SECTION
            for i,val in enumerate(StatsSection_range):
                line = linecache.getline(self._iieout_filename,val)
                Build_stats_lines.append(line)
#                 if valheader in line:
#                         dict_stats[valheader] = line[10:31].strip()

            print('+'+'='*(30)+ ' ' + model.capitalize() +' Run Details' +' '  +'='*(30)+'+')
            print('     Density model: ' + model )
            print('     Satellite: ' + sat_name )
            print('     Data type: ' + data_type )

            for i,val in enumerate(Build_stats_lines):
                if '\n'in val:
                    val = val.replace("\n", "")
                    print(val)
            
            print('+'+'='*(78)+'+')
#                 return()


#             quicklook_IIEOUTstatsprint(self._iieout_filename, model, sat_name, data_type)

        else:
            pass
        end = time.time()
        elapsed = end - start
#         print('save stats to dict: ',elapsed)

        return(dict_stats)
        
        
    def grab_PCE_ascii(self, start, stop):
        
        self.StateVector_epochs_datafile = '/data/data_geodyn/inputs/icesat2/setups/PCE_ascii.txt'

        os.system('bunzip2'+' '+self.StateVector_epochs_datafile+'.bz2')
        
        epoch_start_dt_STR = str(epoch_start_dt)
        date_in_file_flag = False

        with open(self.StateVector_epochs_datafile, 'r') as f:
            for line_no, line_text in enumerate(f):
                
                if epoch_start_dt_STR in line_text:
                    date_in_file_flag= True
                    print('    ','xyzline',line_no,line_text)

                    break
                



#         xyzline = pd.read_csv(self.StateVector_epochs_datafile, 
#                     skiprows = line_no, 
#                     nrows=1,           
#                     sep = '\s+',
#                     dtype=str,
#                     names = [
#                             'Date',
#                             'X',
#                             'Y',
#                             'Z',
#                             'X_dot',
#                             'Y_dot',
#                             'Z_dot',
#                           ],
#                     )
#         print(xyzline['X'].values[0].ljust(20))
#         X     =  xyzline['X'].values[0].ljust(20)     #'  -745933.8926940708'
#         Y     =  xyzline['Y'].values[0].ljust(20)     #'  -4864983.834066438'
#         Z     =  xyzline['Z'].values[0].ljust(20)     #'    4769954.60524261'
#         X_dot =  xyzline['X_dot'].values[0].ljust(20) #'  457.44564954037634'
#         Y_dot =  xyzline['Y_dot'].values[0].ljust(20) #'   5302.381564886811'
#         Z_dot =  xyzline['Z_dot'].values[0].ljust(20) #'    5463.55571622269'
    
#         os.system('bzip2'+' '+self.StateVector_epochs_datafile)
        ##### -------------------------------------------------------------------------------------------
        #### --------------------------------------------------------------------------------------------
        
        
        
        
            
    def getData_asciiXYZ(self):
        return(self.read_ascixyz())

    #----------------------------------------------

    
    def getData_Trajectory_orbfil(self):
        return(self.read_binary_ORBFIL())

    #----------------------------------------------
    
    def getData_adjustedparams_iieout(self):
        return(self.read_adjustedparams_iieout())
    
    #----------------------------------------------
    
    def getData_density_denfile(self):
        return(self.read_density_file())
    #----------------------------------------------
    
    def getData_DragFile(self):
        return(self.read_drag_file())
    
    #----------------------------------------------
    
    def getData_AccelFile(self):
        return(self.read_accel_file())
    
    #----------------------------------------------

    def getData_residsObserved_iieout(self):
        return(self.read_observed_resids())

    #----------------------------------------------

    def getData_residsMeasSumm_iieout(self):
        return(self.read_resid_measurement_summaries())
    #----------------------------------------------

    def getData_RunSummary_iieout(self):
        return(self.read_RunSummary_iieout())

    def getData_PCE(self):
        return(self.grab_PCE_ascii())

#     def getData_UserChoose(self, inputlist):
    
#         '''
#         determine which datasets to return
#         '''
#         print('     Input must be chosen from the following:')
#         print('           Density', '\n',
#               '          AdjustedParams','\n',
#               '          Trajectory_xyz', '\n',
#               '          Residuals_obs', '\n',
#               '          Residuals_summary', '\n',
#               '          Statistics \n')
        
#         for choice in inputlist:

#             if choice == 'AdjustedParams':
#                 self.AdjustedParams      = self.getData_adjustedparams_iieout()
               
#             elif choice == 'Trajectory_xyz':
#                 self.Trajectory_xyz            = self.getData_asciiXYZ()
                
#             elif choice == 'Density':
#                 self.Density             = self.getData_density_denfile()
             
#             elif choice == 'Residuals_obs':
#                 self.Residuals_obs          = self.getData_residsObserved_iieout()
         
#             elif choice ==  'Residuals_summary':
#                 self.Residuals_summary      = self.getData_residsMeasSumm_iieout()
         
#             elif choice == 'Statistics':
#                 self.Statistics = self.getData_stats_endOfFile_iieout()
         
#             else:
#                 print('The requested output [', choice, '] does not match and inputs')

                
# #             print(self.keys())
#         return(self)
    

        
#     def getData_all(self):

#         data_keys = [
#                     'AdjustedParams',
#                     'Trajectory_xyz',
#                     'Density',
#                     'Residuals_obs',
#                     'Residuals_summary',
#                     'Statistics',
#                     ]

#         for choice in data_keys:
#             if choice == 'AdjustedParams':
#                 self.AdjustedParams      = self.getData_adjustedparams_iieout()
#             elif choice == 'Trajectory_xyz':
#                 self.Trajectory_xyz            = self.getData_asciiXYZ()
#             elif choice == 'Density':
#                 self.Density                   = self.getData_density_denfile()
#             elif choice == 'Residuals_obs':
#                 self.Residuals_obs            = self.getData_residsObserved_iieout()
#             elif choice ==  'Residuals_summary':
#                 self.Residuals_summary      = self.getData_residsMeasSumm_iieout()
#             elif choice == 'Statistics':
#                 self.Statistics = self.getData_stats_endOfFile_iieout()
#             else:
#                 print('The requested output [', choice, '] does not match and inputs')

#         self.organize_output_object_keys(data_keys)




#     def getData_Arc(self):

#         data_keys = [
#                     'AdjustedParams',
# #                     'Trajectory_xyz',
# #                     'Density',
#                     'Residuals_obs',
# #                     'Residuals_summary',
# #                     'Statistics',
#                     ]

#         self.AdjustedParams    = {}
#         self.Trajectory_xyz    = {}
#         self.Density           = {}
#         self.Residuals_obs     = {}
#         self.Residuals_summary = {}
#         self.Statistics        = {}
        
#         arc = self.arc_input

        
#         self.set_file_paths_for_multiple_arcs( arc )


#         for choice in data_keys:
#             if choice == 'AdjustedParams':
#                 self.AdjustedParams[arc]      = self.getData_adjustedparams_iieout()
#             elif choice == 'Trajectory_xyz':
#                 self.Trajectory_xyz[arc]            = self.getData_asciiXYZ()
#             elif choice == 'Density':
#                 self.Density[arc]                   = self.getData_density_denfile()
#             elif choice == 'Residuals_obs':
#                 self.Residuals_obs[arc]            = self.getData_residsObserved_iieout()
#             elif choice ==  'Residuals_summary':
#                 self.Residuals_summary[arc]      = self.getData_residsMeasSumm_iieout()
#             elif choice == 'Statistics':
#                 self.Statistics[arc] = self.getData_stats_endOfFile_iieout()
#             else:
#                 print('The requested output [', choice, '] does not match and inputs')

#         self.organize_output_object_keys(data_keys)            

        
        
        
        
        
#     def getData_ArcList(self):

#         data_keys = [
#                     'AdjustedParams',
#                     'Trajectory_xyz',
#                     'Density',
#                     'Residuals_obs',
#                     'Residuals_summary',
#                     'Statistics',
#                     ]

#         self.AdjustedParams    = {}
#         self.Trajectory_xyz    = {}
#         self.Density           = {}
#         self.Residuals_obs     = {}
#         self.Residuals_summary = {}
#         self.Statistics        = {}
        
#         for arc in self.arc_input:
#             self.set_file_paths_for_multiple_arcs( arc )

            
#             for choice in data_keys:
#                 if choice == 'AdjustedParams':
#                     self.AdjustedParams[arc]      = self.getData_adjustedparams_iieout()
#                 elif choice == 'Trajectory_xyz':
#                     self.Trajectory_xyz[arc]            = self.getData_asciiXYZ()
#                 elif choice == 'Density':
#                     self.Density[arc]                   = self.getData_density_denfile()
#                 elif choice == 'Residuals_obs':
#                     self.Residuals_obs[arc]            = self.getData_residsObserved_iieout()
#                 elif choice ==  'Residuals_summary':
#                     self.Residuals_summary[arc]      = self.getData_residsMeasSumm_iieout()
#                 elif choice == 'Statistics':
#                     self.Statistics[arc] = self.getData_stats_endOfFile_iieout()
#                 else:
#                     print('The requested output [', choice, '] does not match and inputs')

#         self.organize_output_object_keys(data_keys)  # this cleans up the extra keys and puts them in a dictionary key called 'run_parameters'
    
    
    
    
    
    
#     def getData_ParallelizeArcs(self):

#         data_keys = [
#                     'AdjustedParams',
#                     'Trajectory_xyz',
#                     'Density',
#                     'Residuals_obs',
#                     'Residuals_summary',
# #                     'Statistics',
#                     ]

#         import multiprocessing
#         import time
#         from multiprocessing import set_start_method
#         set_start_method("spawn")

        
#         in1 = [
#                 ( '030914_2wk' ),
#                 ( '030928_2wk' ),
# #                 ( '031012_2wk' ),
# #                 ( '031026_2wk' ),
# #                 ( '031109_2wk' ),
# #                 ( '031123_2wk' ),
# #                 ( '031207_2wk' ),
# #                 ( '031221_2wk' ),
#                 ]
        
#         pool = multiprocessing.Pool(processes=4)
#         pool.starmap(self.getData_Arc, in1)






    def getData(self):
        
    
        data_keys = self.request_data

        #### Make dictionaries to store arc in a loop
        self.AdjustedParams    = {}
        self.Trajectory_xyz    = {}
        self.Trajectory_orbfil = {}
        self.Density           = {}
        self.Residuals_obs     = {}
        self.Residuals_summary = {}
        self.RunSummary        = {}
        self.DragFile          = {}
        
        num_arcs = np.size(self.arc_input)
        
        ##### Go thru the files once and unzip them
        self.set_file_paths_for_multiple_arcs( self.arc_input[0], 1, True )
        ARC_FILES = self.make_list_of_arcfilenames()
        
        for i in ARC_FILES:
#             print(i)
            if os.path.exists(self.path_to_model+'DENSITY/'):
                os.chdir(self.path_to_model+'DENSITY/')
                os.system('bunzip2 -v '+ i +'.bz2')
                os.system('bunzip2 -v '+ i +'drag_file.bz2')

            if os.path.exists(self.path_to_model+'ORBITS/'):
                os.chdir(self.path_to_model+'ORBITS/')
#                 print(self.path_to_model+'ORBITS/')
#                 print(i+'_orb1.bz2')
                os.system('bunzip2 -v '+i+'_orb1.bz2')

        
        for iarc, arc in enumerate(self.arc_input):
            self.set_file_paths_for_multiple_arcs( arc, iarc, False )
            self.check_if_run_converged(self._iieout_filename)

            print('Loading ', self.den_model , arc )
            
            for choice in data_keys:                
                
                if choice == 'AdjustedParams':
                    self.AdjustedParams[arc]         = self.getData_adjustedparams_iieout()
                elif choice == 'Trajectory_xyz':
                    self.Trajectory_xyz[arc]         = self.getData_asciiXYZ()
                elif choice == 'Density':
                    self.Density[arc]                = self.getData_density_denfile()
                elif choice == 'Residuals_obs':
                    self.Residuals_obs[arc]          = self.getData_residsObserved_iieout()
                elif choice ==  'Residuals_summary':
                    self.Residuals_summary[arc]      = self.getData_residsMeasSumm_iieout()
                elif choice == 'RunSummary':
                    self.RunSummary[arc]             = self.getData_RunSummary_iieout()
                elif choice == 'Trajectory_orbfil':
#                     print('TESTTESTTEST')
                    self.Trajectory_orbfil[arc]      = self.getData_Trajectory_orbfil()
                elif choice == 'DragFile':
                    self.DragFile[arc]               = self.getData_DragFile()
                else:
#                     print('Error in PygeodynReader.getData()')
#                     print('The requested output [', choice, '] does not match any inputs')
                    break
    
#             os.system('bzip2 -v' +' '+self._density_filename)
            
            self.organize_output_object_keys(data_keys, arc, iarc, num_arcs)
            
            
            
        


    def ResidInvestigation_make_common_residsDF(self):

        ###### GET THE PCE DATA:
        StateVector_PCE_datafile = '/data/data_geodyn/inputs/icesat2/setups/PCE_ascii.txt'
        SAT_ID = int(self.__dict__['global_params']['SATID'])
        which_stat = 'CURRENT_VALUE'

        C_1 = {}
        
        for ii,arc in enumerate(self.__dict__['global_params']['arc_input']):

            ####--------------------- Residual  ---------------------

            arc_first_time  = self.__dict__['Trajectory_orbfil'][arc]['data_record']['Date_UTC'].iloc[0]
            arc_last_time   = self.__dict__['Trajectory_orbfil'][arc]['data_record']['Date_UTC'].iloc[-1]

            arc_first_time_str     =  str(arc_first_time)#.replace( "'",' ') 
            arc_last_time_str      =  str(arc_last_time)#.replace( "'",' ') 
            
            
#             print('arc',arc)
#             print('arc_first_time    ',arc_first_time)
#             print('arc_last_time     ',arc_last_time)
#             print('arc_first_time_str',arc_first_time_str)
#             print('arc_last_time_str ',arc_last_time_str)
            
            
            A=[]
            for i,val in enumerate(np.arange(-20,20)):
                A.append(str(pd.to_datetime(arc_first_time)+pd.to_timedelta(val,'s')))
            B=[]
            for i,val in enumerate(np.arange(-20,20)):
                B.append(str(pd.to_datetime(arc_last_time)+pd.to_timedelta(val,'s')))

            ####---------------------------------------------------------
            last_line = False
            with open(StateVector_PCE_datafile, 'r') as f:
                for line_no, line_text in enumerate(f):
                    if any(times in line_text for times in A):
                        first_line = line_no
                    if any(times in line_text for times in B):
                        last_line = line_no
                        break

                if not last_line:
                    last_line = first_line +32220
                    print('No matching lastline time: ',arc_last_time_str, last_line )

            ####   IF YOU GET AN ERROR HERE stating that either first_line or last_line is 
            ####    It is probably an issue with the date in the arc not matching up with the dates given in the PCEfile
            print('Loading PCE data for ...', arc )
            PCE_data = pd.read_csv(StateVector_PCE_datafile, 
                        skiprows = first_line, 
                        nrows=last_line-first_line,           
                        sep = '\s+',
                        dtype=str,
                        names = [
                                'Date',
                                'MJDSECs', 
                                'RSECS', #(fractional secs)
                                'GPS offset', # (UTC - GPS offset (secs))
                                'X_pce',
                                'Y_pce',
                                'Z_pce',
                                'Xdot_pce',
                                'Ydot_pce',
                                'Zdot_pce',
                                'YYMMDDhhmmss',
                                    ],)
            
            PCE_data['Date_pd'] = pd.to_datetime(PCE_data['Date'])
            del PCE_data['YYMMDDhhmmss']
            del PCE_data['MJDSECs']
            del PCE_data['RSECS']
            del PCE_data['GPS offset']
            del PCE_data['Date']

            
            orbfil_arc1 = self.__dict__['Trajectory_orbfil'][arc]['data_record']
            orbfil_arc1['Date_pd'] = pd.to_datetime(orbfil_arc1 ['Date_UTC'])
            
            del orbfil_arc1['Date_UTC']
            del orbfil_arc1['MJDSEC ET']
            del orbfil_arc1['Satellite Geodetic Latitude']
            del orbfil_arc1['Satellite East Longitude']
            del orbfil_arc1['Satellite Height']
            del orbfil_arc1['MJDS_UTC']


            ### C_1 is a dataframe containing all data between the two files where the dates match
            C_1[arc] = pd.merge(left=orbfil_arc1, left_on='Date_pd',
                 right=PCE_data, right_on='Date_pd')

#             print(C_1[arc].columns)
            C_1[arc] = C_1[arc].rename(columns={"Satellite Inertial X coordinate": "X_orbfil",
                                     "Satellite Inertial Y coordinate": "Y_orbfil",
                                     "Satellite Inertial Z coordinate": "Z_orbfil",
                                     "Satellite Inertial X velocity"  : "Xdot_orbfil",
                                     "Satellite Inertial Y velocity"  : "Ydot_orbfil",
                                     "Satellite Inertial Z velocity"  : "Zdot_orbfil",
                                    })
#             print(C_1[arc].columns)

        return(C_1)


    def ResidInvestigation_get_residuals_coordsystems(self, C_1):

        orbit_resids = {} 

        for ii,arc in enumerate(self.__dict__['global_params']['arc_input']):
#             print(arc)

            data_orbfil = {}
            data_PCE    = {}
            resids      = {}
            ### Convert the PCE data to NTW
            print('Converting PCE data to other coordinates...')
            X = C_1[arc]['X_pce'].astype(float)
            Y = C_1[arc]['Y_pce'].astype(float)
            Z = C_1[arc]['Z_pce'].astype(float)
            Xdot = C_1[arc]['Xdot_pce'].astype(float)
            Ydot = C_1[arc]['Ydot_pce'].astype(float)
            Zdot = C_1[arc]['Zdot_pce'].astype(float)
            state_vector = np.transpose(np.array([X, Y, Z, Xdot, Ydot, Zdot]))
            data_PCE['Date'] = C_1[arc]['Date_pd']

            ##### NTW Coordinate System
#             NTW_PCE  = [Convert_cartesian_to_NTW_returnall(x) for x in state_vector]
            NTW_pce =  [Convert_cartesian_to_NTW_returnall(x_pce, 0, True) for x_pce in state_vector]
#             Rvec_ntw_pce = []
            Tmat         = []
            n_pce        = []
            t_pce        = []
            w_pce        = []

            for vec,matrix in NTW_pce:
                Tmat.append(matrix)
                n_pce.append( vec[0])
                t_pce.append( vec[1])
                w_pce.append( vec[2])        
        
            data_PCE['N'] = n_pce
            data_PCE['T'] = t_pce
            data_PCE['W'] = w_pce
            ##### XYZ Coordinate System
            data_PCE['X'] = X
            data_PCE['Y'] = Y
            data_PCE['Z'] = Z
            data_PCE['Xdot'] = Xdot
            data_PCE['Ydot'] = Ydot
            data_PCE['Zdot'] = Zdot
            ##### R theta phi Coordinate System
            data_PCE['R']     = np.sqrt( np.square(X) + 
                                            np.square(Y) +
                                            np.square(Z) )
            data_PCE['theta'] = np.arctan(Y / X)
            data_PCE['phi']   = np.arccos(Z / (np.sqrt( np.square(X) + 
                                            np.square(Y) +
                                            np.square(Z) )))
    
    
        ### Convert the ORBIT FILE data to NTW
            print('Converting OrbitFile data to other coordinates...', arc)



            X = C_1[arc]['X_orbfil']
            Y = C_1[arc]['Y_orbfil']
            Z = C_1[arc]['Z_orbfil']
            Xdot = C_1[arc]['Xdot_orbfil']
            Ydot = C_1[arc]['Ydot_orbfil']
            Zdot = C_1[arc]['Zdot_orbfil']
            state_vector = np.transpose(np.array([X, Y, Z, Xdot, Ydot, Zdot]))
            data_orbfil['Date'] = C_1[arc]['Date_pd']

            ##### NTW Coordinate System
#             NTW_orbfil  = [Convert_cartesian_to_NTW_returnall(x) for x in state_vector]
            NTW_orb  = [Convert_cartesian_to_NTW_returnall(x_orb, Tmat_i, False) for x_orb, Tmat_i in zip(state_vector,Tmat)]
            n_orb        = []
            t_orb        = []
            w_orb        = []

            for vecorb,matrixorb in NTW_orb:
                n_orb.append( vecorb[0])
                t_orb.append( vecorb[1])
                w_orb.append( vecorb[2])        

            data_orbfil['N'] = n_orb
            data_orbfil['T'] = t_orb
            data_orbfil['W'] = w_orb
            ##### XYZ Coordinate System
            data_orbfil['X'] = X
            data_orbfil['Y'] = Y
            data_orbfil['Z'] = Z
            data_orbfil['Xdot'] = Xdot
            data_orbfil['Ydot'] = Ydot
            data_orbfil['Zdot'] = Zdot
            ##### R theta phi Coordinate System
            data_orbfil['R']     = np.sqrt( np.square(X) + 
                                            np.square(Y) +
                                            np.square(Z) )
            data_orbfil['theta'] = np.arctan(Y / X)
            data_orbfil['phi']   = np.arccos(Z / (np.sqrt( np.square(X) + 
                                            np.square(Y) +
                                            np.square(Z) )))




            ### RESIDUALS:
            resids['Date'] = C_1[arc]['Date_pd']

            ##### NTW Coordinate System
            resids['N'] = (np.array(data_PCE['N']) - np.array(data_orbfil['N']))
            resids['T'] = (np.array(data_PCE['T']) - np.array(data_orbfil['T']))
            resids['W'] = (np.array(data_PCE['W']) - np.array(data_orbfil['W']))
            ##### XYZ Coordinate System
            resids['X'] = (np.array(data_PCE['X']) - np.array(data_orbfil['X']))
            resids['Y'] = (np.array(data_PCE['Y']) - np.array(data_orbfil['Y']))
            resids['Z'] = (np.array(data_PCE['Z']) - np.array(data_orbfil['Z']))
            ##### R theta phi Coordinate System
            resids['R']     = (np.array(data_PCE['R'])     - np.array(data_orbfil['R']))
            resids['theta'] = (np.array(data_PCE['theta']) - np.array(data_orbfil['theta']))
            resids['phi']   = (np.array(data_PCE['phi'])   - np.array(data_orbfil['phi']))

            orbit_resids[arc] = {}
            orbit_resids[arc]['data_orbfil'] = data_orbfil
            orbit_resids[arc]['data_PCE']    = data_PCE
            orbit_resids[arc]['resids']      = resids



        return(orbit_resids)


    def STATS_residuals(self, residuals):
        import numpy as np
#         print('size of resids',np.size(residuals))
        
        n = np.size(residuals)
        mean = (1/n)*(np.sum(residuals))
        variance = (1/n)*(np.sum(np.square(residuals)))
        rms = np.sqrt(variance)
        rms_about_zero = np.sqrt((n/(n-1))*variance)
        return(mean,rms,rms_about_zero)

        
        
        
    def post_getData_ValidationMetrics(self):
        
       

        
        
        #### Construct and save the relevent run information and stats for each arc

        data={'ARC'          :  [],
             'ArcLength'     :  [],
             'Total_iters'   :  [],
#              'RMS POS (run)' :  [] ,
#              'RMS VEL (run)' :  [] ,
             'RMS_total_XYZ'     :  [] ,
             'w_RMS_total_XYZ'   :  [] ,
#              'PCEX_NUMBER'   :  [] ,
#              'PCEX_MEAN'     :  [] ,
             'PCEX_RMS'      :  [] ,
#              'PCEX_No.-WTD'  :  [] ,
#              'PCEX_WTD-MEAN' :  [] ,
             'PCEX_WTD-RMS'  :  [] ,
#              'PCEX_TYPE'     :  [] ,
#              'PCEY_NUMBER'   :  [] ,
#              'PCEY_MEAN'     :  [] ,
             'PCEY_RMS'      :  [] ,
#              'PCEY_No.-WTD'  :  [] ,
#              'PCEY_WTD-MEAN' :  [] ,
             'PCEY_WTD-RMS'  :  [] ,
#              'PCEY_TYPE'     :  [] ,
#              'PCEZ_NUMBER'   :  [] ,
#              'PCEZ_MEAN'     :  [] ,
             'PCEZ_RMS'      :  [] ,
#              'PCEZ_No.-WTD'  :  [] ,
#              'PCEZ_WTD-MEAN' :  [] ,
             'PCEZ_WTD-RMS'  :  [] ,
#              'PCEZ_TYPE'     :  [] ,
#              'X_i'    :  [] ,
#              'Y_i'    :  [] ,
#              'Z_i'    :  [] ,
#              'Xdot_i' :  [] ,
#              'Ydot_i' :  [] ,
#              'Zdot_i' :  [] ,
#              'X_f'    :  [] ,
#              'Y_f'    :  [] ,
#              'Z_f'    :  [] ,
#              'Xdot_f' :  [] ,
#              'Ydot_f' :  [] ,
#              'Zdot_f' :  [] ,
            }
        
#         print(self.__dict__['global_params']['arc_input'])
        self.Statistics        = {}
        
        for iarc, arc in enumerate(self.__dict__['global_params']['arc_input']):
           
            #### GET THE TOTAL RMS of the RESIDUALS
            final_iter  = float(self.__dict__['run_parameters'+arc]['total_iterations'])
            index_iter  = self.__dict__['Residuals_summary'][arc].Iter == final_iter
            
            #### Index the component RMS values
            index_PCEX = self.__dict__['Residuals_summary'][arc][index_iter]['TYPE'] =='PCEX'
            index_PCEY = self.__dict__['Residuals_summary'][arc][index_iter]['TYPE'] =='PCEY'
            index_PCEZ = self.__dict__['Residuals_summary'][arc][index_iter]['TYPE'] =='PCEZ'
            #
            rms_PCEX = float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEX]['RMS'])
            rms_PCEY = float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEY]['RMS'])
            rms_PCEZ = float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEZ]['RMS'])
            w_rms_PCEX = float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEX]['WTD-RMS'])
            w_rms_PCEY = float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEY]['WTD-RMS'])
            w_rms_PCEZ = float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEZ]['WTD-RMS'])

            
            #### Total number of observational Residuals
            N = np.sum(np.array(self.__dict__['Residuals_summary'][arc][index_iter]['NUMBER']))
            RMS_total = np.sqrt( ( np.square(rms_PCEX) + np.square(rms_PCEY) + np.square(rms_PCEZ) )/3  )
            w_RMS_total = np.sqrt( ( np.square(w_rms_PCEX) + np.square(w_rms_PCEY) + np.square(w_rms_PCEZ) )/3  )
            
            ### Get Arc length from the run param settings
            txt = self.__dict__['global_params']['run_settings']['arc_length']
            chars = [s for s in [char for char in txt] if s.isdigit()]
            arc_length = int(''.join(chars))

            data['ARC'].append(arc)
            data['ArcLength'].append(int(arc_length))
            data['Total_iters'].append(int(final_iter))
            

            data['RMS_total_XYZ'].append(RMS_total)     
            data['w_RMS_total_XYZ'].append(w_RMS_total)

#             data['PCEX_NUMBER'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEX]['NUMBER'])   )
#             data['PCEX_MEAN'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEX]['MEAN'])     )
            data['PCEX_RMS'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEX]['RMS'])      )
#             data['PCEX_No.-WTD'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEX]['No.-WTD'])  )
#             data['PCEX_WTD-MEAN'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEX]['WTD-MEAN']) )
            data['PCEX_WTD-RMS'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEX]['WTD-RMS']))
#             data['PCEX_TYPE'].append((self.__dict__['Residuals_summary'][arc][index_iter][index_PCEX]['TYPE'].values))
#             data['PCEY_NUMBER'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEY]['NUMBER']))
#             data['PCEY_MEAN'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEY]['MEAN']))
            data['PCEY_RMS'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEY]['RMS']))
#             data['PCEY_No.-WTD'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEY]['No.-WTD']))
#             data['PCEY_WTD-MEAN'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEY]['WTD-MEAN']) )
            data['PCEY_WTD-RMS'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEY]['WTD-RMS']))
#             data['PCEY_TYPE'].append((self.__dict__['Residuals_summary'][arc][index_iter][index_PCEY]['TYPE'].values))
#             data['PCEZ_NUMBER'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEZ]['NUMBER']))
#             data['PCEZ_MEAN'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEZ]['MEAN']))
            data['PCEZ_RMS'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEZ]['RMS']))
#             data['PCEZ_No.-WTD'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEZ]['No.-WTD']))
#             data['PCEZ_WTD-MEAN'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEZ]['WTD-MEAN']))
            data['PCEZ_WTD-RMS'].append(float(self.__dict__['Residuals_summary'][arc][index_iter][index_PCEZ]['WTD-RMS']))
#             data['PCEZ_TYPE'].append((self.__dict__['Residuals_summary'][arc][index_iter][index_PCEZ]['TYPE'].values))
            
            ##### IF FILES NOT CONVERGED, turn off
#             data['RMS POS (run)'].append(self.__dict__['RunSummary'][arc]['RMS POS']) 
#             data['RMS VEL (run)'].append(self.__dict__['RunSummary'][arc]['RMS VEL']) 

#             data['X_f'].append(self.__dict__['RunSummary'][arc]['X POS'])
#             data['Y_f'].append(self.__dict__['RunSummary'][arc]['Y POS'])
#             data['Z_f'].append(self.__dict__['RunSummary'][arc]['Z POS'])
#             data['Xdot_f'].append(self.__dict__['RunSummary'][arc]['X VEL'])
#             data['Ydot_f'].append(self.__dict__['RunSummary'][arc]['Y VEL'])
#             data['Zdot_f'].append(self.__dict__['RunSummary'][arc]['Z VEL'])

    
    
            
        #### Get the OrbFil and PCE data for the datapoints that match in time
        CombinedOrbitsDF  = self.ResidInvestigation_make_common_residsDF()      
        OrbitResids = self.ResidInvestigation_get_residuals_coordsystems(CombinedOrbitsDF)
        CombinedOrbitsDF = CombinedOrbitsDF
        self.OrbitResids = OrbitResids
        
        
        data['N_RMS'] = []
        data['T_RMS'] = []
        data['W_RMS'] = []
        data['RMS_total_NTW'] = []
        
        for iarc, arc in enumerate(self.__dict__['global_params']['arc_input']):
            print('arc',arc)
#             print("OrbitResids[arc]['resids']['N']",OrbitResids[arc]['resids']['N'])
            
            N_mean, N_rms, N_rms_0 = self.STATS_residuals(OrbitResids[arc]['resids']['N'])
            T_mean, T_rms, T_rms_0 = self.STATS_residuals(OrbitResids[arc]['resids']['T'])
            W_mean, W_rms, W_rms_0 = self.STATS_residuals(OrbitResids[arc]['resids']['W'])
            data['N_RMS'].append(N_rms_0)
            data['T_RMS'].append(T_rms_0)
            data['W_RMS'].append(W_rms_0)
        
#             Num_ntw = int(np.size((OrbitResids[arc]['resids']['T'])))
#             print('num of measures N', int(np.size((OrbitResids[arc]['resids']['N']))))
#             print('num of measures T', int(np.size((OrbitResids[arc]['resids']['T']))))
#             print('num of measures W', int(np.size((OrbitResids[arc]['resids']['W']))))
            
            RMS_total_ntw = np.sqrt( ( np.square(N_rms_0) + np.square(T_rms_0) + np.square(W_rms_0) )/3  )
            data['RMS_total_NTW'].append(RMS_total_ntw)
        
        self.Statistics = pd.DataFrame.from_dict(data) 
#         self.Statistics = data
       
    
       
     ### Free up space by removing any ancillary data from the returned Object
        def Pygeodyn_OBJECT_freeupmemory(OBJ):
            SAT_ID = int(OBJ.__dict__['global_params']['SATID'])

            for i,val in enumerate(OBJ.__dict__['Density'].keys()):

                ####-----------------------------------------------------------------
                #### DELETE UNNECESSARY VARS IN DENSITY

#                 del OBJ.__dict__['Density'][val]['Lat']
#                 del OBJ.__dict__['Density'][val]['Lon']
#                 del OBJ.__dict__['Density'][val]['Height (meters)']
                del OBJ.__dict__['Density'][val]['drhodz (kg/m**3/m)']
                del OBJ.__dict__['Density'][val]['flux_daily']
                del OBJ.__dict__['Density'][val]['flux_avg']
                del OBJ.__dict__['Density'][val]['Kp']

                ####-----------------------------------------------------------------
                #### DELETE UNNECESSARY VARS IN Residuals_obs
#                 del OBJ.__dict__['Residuals_obs'][val]['Sat_main']
#                 del OBJ.__dict__['Residuals_obs'][val]['track_1']
#                 del OBJ.__dict__['Residuals_obs'][val]['track_2']
#                 del OBJ.__dict__['Residuals_obs'][val]['Note']
#                 del OBJ.__dict__['Residuals_obs'][val]['Elev1']
#                 del OBJ.__dict__['Residuals_obs'][val]['Elev2']
#                 del OBJ.__dict__['Residuals_obs'][val]['StatSatConfig']
#                 del OBJ.__dict__['Residuals_obs'][val]['Observation']
#                 del OBJ.__dict__['Residuals_obs'][val]['Residual']
#                 del OBJ.__dict__['Residuals_obs'][val]['RatiotoSigma']

                ####-----------------------------------------------------------------
                #### DELETE UNNECESSARY VARIABLES IN AdjustedParams
                ####-----------------------------------------------------------------
                #### DELETE UNNECESSARY VARIABLES IN Trajectory_orbfil
#                 del OBJ.__dict__['Trajectory_orbfil'][val]['header']
#                 del OBJ.__dict__['Trajectory_orbfil'][val]['data_record']['Satellite Geodetic Latitude']
#                 del OBJ.__dict__['Trajectory_orbfil'][val]['data_record']['Satellite East Longitude']
#                 del OBJ.__dict__['Trajectory_orbfil'][val]['data_record']['Satellite Height']
#                 del OBJ.__dict__['Trajectory_orbfil'][val]['data_record']['MJDSEC ET']
            
    
            del OBJ.__dict__['Trajectory_orbfil']
            del OBJ.__dict__['Residuals_summary']
#             del OBJ.__dict__['RunSummary']
            return(OBJ)
    
        self = Pygeodyn_OBJECT_freeupmemory(self)

    


               

        
    def QuickLook_plots(self, plot_num, PLOTTYPE):
        import plotly.graph_objects as go
        from plotly.offline import plot, iplot
        from plotly.subplots import make_subplots
        import plotly.express as px
        import pandas as pd
        import numpy as np
        config = dict({
                        'displayModeBar': True,
                        'responsive': False,
                        'staticPlot': False,
                        'displaylogo': False,
                        'showTips': False,
                        })

        import os

        def get_color(colorscale_name, loc):
            from _plotly_utils.basevalidators import ColorscaleValidator
            # first parameter: Name of the property being validated
            # second parameter: a string, doesn't really matter in our use case
            cv = ColorscaleValidator("colorscale", "")
            # colorscale will be a list of lists: [[loc1, "rgb1"], [loc2, "rgb2"], ...] 
            colorscale = cv.validate_coerce(colorscale_name)

            if hasattr(loc, "__iter__"):
                return [get_continuous_color(colorscale, x) for x in loc]
            return get_continuous_color(colorscale, loc)


        import plotly.colors
        from PIL import ImageColor

        def get_continuous_color(colorscale, intermed):
            """
            Plotly continuous colorscales assign colors to the range [0, 1]. This function computes the intermediate
            color for any value in that range.

            Plotly doesn't make the colorscales directly accessible in a common format.
            Some are ready to use:

                colorscale = plotly.colors.PLOTLY_SCALES["Greens"]

            Others are just swatches that need to be constructed into a colorscale:

                viridis_colors, scale = plotly.colors.convert_colors_to_same_type(plotly.colors.sequential.Viridis)
                colorscale = plotly.colors.make_colorscale(viridis_colors, scale=scale)

            :param colorscale: A plotly continuous colorscale defined with RGB string colors.
            :param intermed: value in the range [0, 1]
            :return: color in rgb string format
            :rtype: str
            """
            if len(colorscale) < 1:
                raise ValueError("colorscale must have at least one color")

            hex_to_rgb = lambda c: "rgb" + str(ImageColor.getcolor(c, "RGB"))

            if intermed <= 0 or len(colorscale) == 1:
                c = colorscale[0][1]
                return c if c[0] != "#" else hex_to_rgb(c)
            if intermed >= 1:
                c = colorscale[-1][1]
                return c if c[0] != "#" else hex_to_rgb(c)

            for cutoff, color in colorscale:
                if intermed > cutoff:
                    low_cutoff, low_color = cutoff, color
                else:
                    high_cutoff, high_color = cutoff, color
                    break

            if (low_color[0] == "#") or (high_color[0] == "#"):
                # some color scale names (such as cividis) returns:
                # [[loc1, "hex1"], [loc2, "hex2"], ...]
                low_color = hex_to_rgb(low_color)
                high_color = hex_to_rgb(high_color)

            return plotly.colors.find_intermediate_color(
                lowcolor=low_color,
                highcolor=high_color,
                intermed=((intermed - low_cutoff) / (high_cutoff - low_cutoff)),
                colortype="rgb",
            )

        cols = get_color("Viridis", np.linspace(0, 1, 5))
        map_cols = np.linspace(0, 1, 5)
        colorscale=[]
        for i,val in enumerate(map_cols):
            colorscale.append([val, cols[i]])

        # Simplify Plotting Schemes:
        col1 =  px.colors.qualitative.Plotly[0]
        col2 =  px.colors.qualitative.Plotly[1]
        col3 =  px.colors.qualitative.Plotly[2]
        col4 =  px.colors.qualitative.Plotly[3]
        col5 =  px.colors.qualitative.Plotly[4]
        col6 =  px.colors.qualitative.Plotly[5]

        if plot_num == 0:
            col = col1
            m_size = 4.5
        elif plot_num == 1:
            col = col2
            m_size = 4
        elif plot_num == 2:
            col = col3
            m_size = 3.5
        elif plot_num == 3:
            col = col4
            m_size = 3
        elif plot_num == 4:
            col = col5
            m_size = 3
        elif plot_num == 5:
            col = col6
            m_size = 3

    ############################################################################
    
        if PLOTTYPE =='DEN':
            fig = make_subplots(
                rows=1, cols=1,
                subplot_titles=(['Sampled Orbit Densities']),
                vertical_spacing = 0.15)

            model_m1 = self.__dict__['global_params']['den_model']
            print(model_m1)
            for ii,arc in enumerate(self.__dict__['global_params']['arc_input'][:]):


                #### INDEX THE DENSITY DF correctly
                vals  = np.arange(self.__dict__['Density'][arc].index[0],self.__dict__['Density'][arc].index[-1]+1)
                df = self.__dict__['Density'][arc].set_index('Date',drop=False ) 
                df['i_vals'] = vals
                index_date = df.loc[df.index.max()]['i_vals'].min()


                str_run_param = 'run_parameters'+ arc
                final_iter = self.__dict__[str_run_param]['str_iteration']
                i_arc = ii+1

                print('----',model_m1,'----')
                print('     mean:    ',np.mean(self.Density[arc]['rho (kg/m**3)']),'----')
                print('     variance:',np.std(self.Density[arc]['rho (kg/m**3)']),'----')
                print()

                if ii==0:
                    legend_flag = True
                    name_str    = model_m1+arc
                else:
                    legend_flag = False
                    name_str    = model_m1+arc


                fig.add_trace(go.Scattergl(  x=self.Density[arc]['Date'][:index_date][:],
                                             y=self.Density[arc]['rho (kg/m**3)'][:index_date][:],
                                             name= name_str,
                                             mode='markers',
                                             opacity=1,
                                             marker=dict(
                                                color=col, 
                                                size=m_size,
                                                        ),
                                             showlegend=legend_flag,
                                          ),
                                              secondary_y=False,
                                               row=1, col=1,
                                          )

            fig.update_layout(legend= {'itemsizing': 'constant'})
            fig.update_yaxes( title="rho (kg/m**3)", type='log', exponentformat= 'power',row=1, col=1)
            fig.update_xaxes( title="Date", row=1, col=1)       
            
            return(fig.show(config=config) )        
    ############################################################################

        elif PLOTTYPE =='DEN_orbavg':

            def orb_avg(den_df, arc):
                #### Find the index for the correct date
                vals  = np.arange(den_df[arc].index[0],den_df[arc].index[-1]+1)
                df = den_df[arc].set_index('Date',drop=False ) 
                df['i_vals'] = vals
                index_date = df.loc[df.index.max()]['i_vals'].min()

                lat = np.asarray(den_df[arc]['Lat'][:index_date])
                time_pd = pd.to_datetime(den_df[arc]['Date'][:index_date])
                i = np.nonzero( lat[1:]*lat[0:-1]  <  np.logical_and(0 , lat[1:] > lat[0:-1] )  )
                i = i[0]

                d_avg = np.zeros(np.size(i))
                height_avg = np.zeros(np.size(i))

                time_avg = []
                d_avg_rolling = []

                roll_avg_count = 0
                for j in range(np.size(i)-1):
                    d_avg[j]      = np.mean(den_df[arc]['rho (kg/m**3)'  ][i[j] : i[j+1]-1  ]  )
                    height_avg[j] = np.mean(den_df[arc]['Height (meters)'][i[j] : i[j+1]-1  ]  )
                    t1 = pd.to_datetime(time_pd[ i[j]    ])
                    t2 = pd.to_datetime(time_pd[ i[j+1]-1])
                    datemiddle = pd.Timestamp(t1) + (pd.Timestamp(t2) - pd.Timestamp(t1)) / 2

                    time_avg.append(datemiddle)

                    if roll_avg_count ==1:
                        d_avg_rolling.append(np.mean([ d_avg[j],  d_avg[j-1]]))
                        roll_avg_count =0

                    roll_avg_count+=1 
                d_avg_rolling.append(np.mean([ d_avg[j],  d_avg[j-1]]))

                return(time_avg, d_avg, d_avg_rolling )
            fig = make_subplots(
                rows=1, cols=1,
                subplot_titles=(['Orbit Averaged Densities']),
                vertical_spacing = 0.15)
            for ii,arc in enumerate(self.__dict__['global_params']['arc_input']):

                vals  = np.arange(self.__dict__['Density'][arc].index[0],self.__dict__['Density'][arc].index[-1]+1)
                df = self.__dict__['Density'][arc].set_index('Date',drop=False ) 
                df['i_vals'] = vals
                index_date = df.loc[df.index.max()]['i_vals'].min()


                time_avg,d_avg, d_avg_rolling = orb_avg(self.Density, arc)


                fig.add_trace(go.Scattergl(x=time_avg,
                                         y=d_avg_rolling,
        #                                  y=d_avg,
        #                                 name= ' Arc ' +str(i_arc) ,
                                         mode='markers',
                                         marker=dict(
                                         color=col,
                                         size=7,),
                                         showlegend=False,
                                           ),
                                           row=1, col=1,
                                           )


                fig.update_yaxes(type="log", exponentformat= 'power',row=1, col=1)
            fig.update_xaxes(title_text="Date", row=1, col=1)
            fig.update_yaxes(title_text="kg/m^3", row=1, col=1)
            fig.update_layout(legend= {'itemsizing': 'constant'})


            
            return(fig.show(config=config) )        
            
    ############################################################################

        elif PLOTTYPE =='NTW_residuals':
            
            coords = ['N','T','W']

            fig = make_subplots(
                rows=3, cols=1,
                subplot_titles=(['Normal (N)', 'In-track (T)','Cross-Track (W)']),
                vertical_spacing = 0.15)

            model_m1 = self.__dict__['global_params']['den_model']
            for ii,arc in enumerate(self.__dict__['global_params']['arc_input']):

                data_resids = self.__dict__['OrbitResids'][arc]['resids']


                fig.update_layout(title=''.join(coords)+' Orbit Residuals',
                                autosize=True,
                                font=dict(size=14),
                                legend= {'itemsizing': 'constant'})

                for i,val in enumerate(coords):
                    fig.add_trace(go.Scattergl(
                                             x=data_resids['Date'][::10],
                                             y=data_resids[val][::10],
                                         name=val+' '+model_m1,
                                         mode='markers',
                                         marker=dict(
                                             color=col,
                                             size=m_size,
                                                    ),
                                         showlegend=False),
                                         secondary_y=False,
                                         row=i+1, col=1)



            fig.update_yaxes( title="meters" ,type="linear" , exponentformat= 'power',row=1, col=1)
            fig.update_yaxes( title="meters" ,type="linear" , exponentformat= 'power',row=2, col=1)
            fig.update_yaxes( title="meters" ,type="linear" , exponentformat= 'power',row=3, col=1)
            fig.update_xaxes( title="Date",  row=3, col=1)

            
            
            return(fig.show(config=config) )        

        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
    def getData_BigData_lowmemory(self):

        import gc
        
        rsw_bool = True
        
    
        data_keys = self.request_data

        #### Make dictionaries to store arc in a loop
        self.AdjustedParams    = {}
        self.Trajectory_xyz    = {}
        self.Trajectory_orbfil = {}
        self.Density           = {}
        self.Residuals_obs     = {}
        self.Residuals_summary = {}
        self.RunSummary        = {}
        self.DragFile          = {}
        self.AccelFile          = {}
        
        data_keys.append('Statistics')
        data_keys.append('OrbitResids')
        ### Init these here so they don't get deleted by the above func
        self.Statistics  = {}
        self.OrbitResids = {} 

        num_arcs = np.size(self.arc_input)
        
        ##### Go thru the files once and unzip them
        self.set_file_paths_for_multiple_arcs( self.arc_input[0], 1, True )
        ARC_FILES = self.make_list_of_arcfilenames()
        
        for i in ARC_FILES:
#             print('arcfiles',i )
            if os.path.exists(self.path_to_model+'DENSITY/'):
                os.chdir(self.path_to_model+'DENSITY/')
                os.system('bunzip2 -v '+ i +'.bz2')
                os.system('bunzip2 -v '+ i +'drag_file.bz2')

            if os.path.exists(self.path_to_model+'ORBITS/'):
                os.chdir(self.path_to_model+'ORBITS/')
                os.system('bunzip2 -v '+i+'_orb1.bz2')

        
        
        #### Construct and save the relevent run information and stats for each arc
        for iarc, arc in enumerate(self.arc_input):

#             print('Arc: ', arc,'.', iarc ,sep='')

            
            self.set_file_paths_for_multiple_arcs( arc, iarc, False )
            self.check_if_run_converged(self._iieout_filename)

            print('Arc: ',self.arcdate_v2)
            
            self.arcnumber = iarc

            for choice in data_keys:                
                if choice == 'AdjustedParams':
                    self.AdjustedParams[self.arcdate_v2]    = self.getData_adjustedparams_iieout()
                elif choice == 'Trajectory_xyz':
                    self.Trajectory_xyz[self.arcdate_v2]    = self.getData_asciiXYZ()
                elif choice == 'Density':
                    self.Density[self.arcdate_v2]           = self.getData_density_denfile()
                elif choice == 'Residuals_obs':
                    self.Residuals_obs[self.arcdate_v2]     = self.getData_residsObserved_iieout()
                elif choice ==  'Residuals_summary':
                    self.Residuals_summary[self.arcdate_v2] = self.getData_residsMeasSumm_iieout()
                elif choice == 'RunSummary':
                    self.RunSummary[self.arcdate_v2]        = self.getData_RunSummary_iieout()
                elif choice == 'Trajectory_orbfil':
#                     print('TESTTESTTEST')
                    self.Trajectory_orbfil[self.arcdate_v2] = self.getData_Trajectory_orbfil()
                
                elif choice == 'DragFile':
                    self.DragFile[self.arcdate_v2]          = self.getData_DragFile()
                
                elif choice == 'AccelFile':
                    self.AccelFile[self.arcdate_v2]          = self.getData_AccelFile()
                
                else:

                    break
                    
            self.organize_output_object_keys(data_keys, self.arcdate_v2, iarc, num_arcs)

            ### Init these here so they don't get deleted by the above func
            if iarc+1 == num_arcs:
                self.arcdate_v2 = self.__dict__['global_params']['arcdate_v2']
            self.Statistics[self.arcdate_v2]  = {}
            self.OrbitResids[self.arcdate_v2] = {} 

            
            #### GET THE TOTAL RMS of the RESIDUALS
            final_iter  = float(self.__dict__['run_parameters'+self.arcdate_v2]['total_iterations'])
            index_iter  = self.__dict__['Residuals_summary'][self.arcdate_v2].Iter == final_iter
            
            #### Index the component RMS values
            index_PCEX = self.__dict__['Residuals_summary'][self.arcdate_v2][index_iter]['TYPE'] =='PCEX'
            index_PCEY = self.__dict__['Residuals_summary'][self.arcdate_v2][index_iter]['TYPE'] =='PCEY'
            index_PCEZ = self.__dict__['Residuals_summary'][self.arcdate_v2][index_iter]['TYPE'] =='PCEZ'
            #
            rms_PCEX = float(self.__dict__['Residuals_summary'][self.arcdate_v2][index_iter][index_PCEX]['RMS'])
            rms_PCEY = float(self.__dict__['Residuals_summary'][self.arcdate_v2][index_iter][index_PCEY]['RMS'])
            rms_PCEZ = float(self.__dict__['Residuals_summary'][self.arcdate_v2][index_iter][index_PCEZ]['RMS'])
            w_rms_PCEX = float(self.__dict__['Residuals_summary'][self.arcdate_v2][index_iter][index_PCEX]['WTD-RMS'])
            w_rms_PCEY = float(self.__dict__['Residuals_summary'][self.arcdate_v2][index_iter][index_PCEY]['WTD-RMS'])
            w_rms_PCEZ = float(self.__dict__['Residuals_summary'][self.arcdate_v2][index_iter][index_PCEZ]['WTD-RMS'])

            
            #### Total number of observational Residuals
            N = np.sum(np.array(self.__dict__['Residuals_summary'][self.arcdate_v2][index_iter]['NUMBER']))
            RMS_total = np.sqrt( ( np.square(rms_PCEX) + np.square(rms_PCEY) + np.square(rms_PCEZ) )/3  )
            w_RMS_total = np.sqrt( ( np.square(w_rms_PCEX) + np.square(w_rms_PCEY) + np.square(w_rms_PCEZ) )/3  )
            
            ### Get Arc length from the run param settings
            txt = self.__dict__['global_params']['run_settings']['arc_length']
            chars = [s for s in [char for char in txt] if s.isdigit()]
            arc_length = int(''.join(chars))

            
            data = { 'ARC'             :  [] ,
                     'ArcLength'       :  [] ,
                     'Total_iters'     :  [] ,
                     'RMS_total_XYZ'   :  [] ,
                     'w_RMS_total_XYZ' :  [] ,
                     'PCEX_RMS'        :  [] ,
                     'PCEX_WTD-RMS'    :  [] ,
                     'PCEY_RMS'        :  [] ,
                     'PCEY_WTD-RMS'    :  [] ,
                     'PCEZ_RMS'        :  [] ,
                     'PCEZ_WTD-RMS'    :  [] ,
                    }
            data['N_RMS']         = []
            data['T_RMS']         = []
            data['W_RMS']         = []
            data['RMS_total_NTW'] = []
            
            if rsw_bool == True:
                data['R_RMS']         = []
                data['S_RMS']         = []
                data['Wrsw_RMS']         = []
                data['RMS_total_RSW'] = []

            data['ARC'].append(self.arcdate_v2)
            data['ArcLength'].append(int(arc_length))
            data['Total_iters'].append(int(final_iter))
            
            data['RMS_total_XYZ'].append(RMS_total)     
            data['w_RMS_total_XYZ'].append(w_RMS_total)

            data['PCEX_RMS'].append(float(self.__dict__['Residuals_summary'][self.arcdate_v2][index_iter][index_PCEX]['RMS'])      )
            data['PCEX_WTD-RMS'].append(float(self.__dict__['Residuals_summary'][self.arcdate_v2][index_iter][index_PCEX]['WTD-RMS']))
            data['PCEY_RMS'].append(float(self.__dict__['Residuals_summary'][self.arcdate_v2][index_iter][index_PCEY]['RMS']))
            data['PCEY_WTD-RMS'].append(float(self.__dict__['Residuals_summary'][self.arcdate_v2][index_iter][index_PCEY]['WTD-RMS']))
            data['PCEZ_RMS'].append(float(self.__dict__['Residuals_summary'][self.arcdate_v2][index_iter][index_PCEZ]['RMS']))
            data['PCEZ_WTD-RMS'].append(float(self.__dict__['Residuals_summary'][self.arcdate_v2][index_iter][index_PCEZ]['WTD-RMS']))
            

############--------------------------------------------------------------------------------------------------        
            #### Get the OrbFil and PCE data for the datapoints that match in time
            CombinedOrbitsDF  = {}
        
        
            ###### GET THE PCE DATA:
            StateVector_PCE_datafile = '/data/data_geodyn/inputs/icesat2/setups/PCE_ascii.txt'
            SAT_ID = int(self.__dict__['global_params']['SATID'])
            which_stat = 'CURRENT_VALUE'

            ####--------------------- Residual  ---------------------
            arc_first_time  = self.__dict__['Trajectory_orbfil'][self.arcdate_v2]['data_record']['Date_UTC'].iloc[0]
            arc_last_time   = self.__dict__['Trajectory_orbfil'][self.arcdate_v2]['data_record']['Date_UTC'].iloc[-1]

            arc_first_time_str     =  str(arc_first_time)#.replace( "'",' ') 
            arc_last_time_str      =  str(arc_last_time)#.replace( "'",' ') 
            
                       
            A=[]
            for i,val in enumerate(np.arange(-20,20)):
                A.append(str(pd.to_datetime(arc_first_time)+pd.to_timedelta(val,'s')))
            B=[]
            for i,val in enumerate(np.arange(-20,20)):
                B.append(str(pd.to_datetime(arc_last_time)+pd.to_timedelta(val,'s')))

            ####---------------------------------------------------------
            last_line = False
            with open(StateVector_PCE_datafile, 'r') as f:
                for line_no, line_text in enumerate(f):
                    if any(times in line_text for times in A):
                        first_line = line_no
                    if any(times in line_text for times in B):
                        last_line = line_no
                        break

                if not last_line:
                    last_line = first_line +32220
                    print('No matching lastline time: ',arc_last_time_str, last_line )

            ####    IF YOU GET AN ERROR HERE stating that either first_line or last_line is 
            ####    It is probably an issue with the date in the arc not matching up with the dates given in the PCEfile
#             print('        Loading PCE datafile...')
            PCE_data = pd.read_csv(StateVector_PCE_datafile, 
                        skiprows = first_line, 
                        nrows=last_line-first_line,           
                        sep = '\s+',
                        dtype=str,
                        names = [
                                'Date',
                                'MJDSECs', 
                                'RSECS', #(fractional secs)
                                'GPS offset', # (UTC - GPS offset (secs))
                                'X_pce',
                                'Y_pce',
                                'Z_pce',
                                'Xdot_pce',
                                'Ydot_pce',
                                'Zdot_pce',
                                'YYMMDDhhmmss',
                                    ],)
            
            PCE_data['Date_pd'] = pd.to_datetime(PCE_data['Date'])
            del PCE_data['YYMMDDhhmmss']
            del PCE_data['MJDSECs']
            del PCE_data['RSECS']
            del PCE_data['GPS offset']
            del PCE_data['Date']

            
            orbfil_arc1 = self.__dict__['Trajectory_orbfil'][self.arcdate_v2]['data_record']
            orbfil_arc1['Date_pd'] = pd.to_datetime(orbfil_arc1 ['Date_UTC'])
            
            del orbfil_arc1['Date_UTC']
            del orbfil_arc1['MJDSEC ET']
            del orbfil_arc1['Satellite Geodetic Latitude']
            del orbfil_arc1['Satellite East Longitude']
            del orbfil_arc1['Satellite Height']
            del orbfil_arc1['MJDS_UTC']


            ### CombinedOrbitsDF is a dataframe containing all data between the two files where the dates match
            CombinedOrbitsDF[self.arcdate_v2] = pd.merge(left=orbfil_arc1, left_on='Date_pd',
                 right=PCE_data, right_on='Date_pd')

#             print(CombinedOrbitsDF[arc].columns)
            CombinedOrbitsDF[self.arcdate_v2] = CombinedOrbitsDF[self.arcdate_v2].rename(columns={"Satellite Inertial X coordinate": "X_orbfil",
                                     "Satellite Inertial Y coordinate": "Y_orbfil",
                                     "Satellite Inertial Z coordinate": "Z_orbfil",
                                     "Satellite Inertial X velocity"  : "Xdot_orbfil",
                                     "Satellite Inertial Y velocity"  : "Ydot_orbfil",
                                     "Satellite Inertial Z velocity"  : "Zdot_orbfil",
                                    })
############--------------------------------------------------------------------------------------------------        
#             OrbitResids = self.ResidInvestigation_get_residuals_coordsystems(CombinedOrbitsDF)

            data_orbfil = {}
            data_PCE    = {}
            resids      = {}
            ### Convert the PCE data to NTW
#             print('        Converting PCE data to other coordinates...')
            X = CombinedOrbitsDF[self.arcdate_v2]['X_pce'].astype(float)
            Y = CombinedOrbitsDF[self.arcdate_v2]['Y_pce'].astype(float)
            Z = CombinedOrbitsDF[self.arcdate_v2]['Z_pce'].astype(float)
            Xdot = CombinedOrbitsDF[self.arcdate_v2]['Xdot_pce'].astype(float)
            Ydot = CombinedOrbitsDF[self.arcdate_v2]['Ydot_pce'].astype(float)
            Zdot = CombinedOrbitsDF[self.arcdate_v2]['Zdot_pce'].astype(float)
            state_vector = np.transpose(np.array([X, Y, Z, Xdot, Ydot, Zdot]))
            data_PCE['Date'] = CombinedOrbitsDF[self.arcdate_v2]['Date_pd']

            ##### NTW Coordinate System
#             NTW_PCE  = [Convert_cartesian_to_NTW_returnall(x) for x in state_vector]
            NTW_pce =  [Convert_cartesian_to_NTW_returnall(x_pce, 0, True) for x_pce in state_vector]
#             Rvec_ntw_pce = []
            Tmat_ntw         = []
            n_pce        = []
            t_pce        = []
            w_pce        = []
            for vec,matrix in NTW_pce:
                Tmat_ntw.append(matrix)
                n_pce.append( vec[0])
                t_pce.append( vec[1])
                w_pce.append( vec[2])        
               
            data_PCE['N'] = n_pce
            data_PCE['T'] = t_pce
            data_PCE['W'] = w_pce
            ##### XYZ Coordinate System
            data_PCE['X'] = X
            data_PCE['Y'] = Y
            data_PCE['Z'] = Z
            data_PCE['Xdot'] = Xdot
            data_PCE['Ydot'] = Ydot
            data_PCE['Zdot'] = Zdot
            
            if rsw_bool == True:
                RSW_pce =  [Convert_cartesian_to_RSW_returnall(x_pce, 0, True) for x_pce in state_vector]
                Tmat_rsw     = []
                r_pce        = []
                s_pce        = []
                wrsw_pce        = []
                for vec,matrix in RSW_pce:
                    Tmat_rsw.append(matrix)
                    r_pce.append( vec[0])
                    s_pce.append( vec[1])
                    wrsw_pce.append( vec[2]) 
                data_PCE['R'] = r_pce
                data_PCE['S'] = s_pce
                data_PCE['Wrsw'] = wrsw_pce            
         ##### R theta phi Coordinate System
#             data_PCE['R']     = np.sqrt( np.square(X) + 
#                                             np.square(Y) +
#                                             np.square(Z) )
#             data_PCE['theta'] = np.arctan(Y / X)
#             data_PCE['phi']   = np.arccos(Z / (np.sqrt( np.square(X) + 
#                                             np.square(Y) +
#                                             np.square(Z) )))
    
    
        ### Convert the ORBIT FILE data to NTW
            X = CombinedOrbitsDF[self.arcdate_v2]['X_orbfil']
            Y = CombinedOrbitsDF[self.arcdate_v2]['Y_orbfil']
            Z = CombinedOrbitsDF[self.arcdate_v2]['Z_orbfil']
            Xdot = CombinedOrbitsDF[self.arcdate_v2]['Xdot_orbfil']
            Ydot = CombinedOrbitsDF[self.arcdate_v2]['Ydot_orbfil']
            Zdot = CombinedOrbitsDF[self.arcdate_v2]['Zdot_orbfil']
            state_vector = np.transpose(np.array([X, Y, Z, Xdot, Ydot, Zdot]))
            data_orbfil['Date'] = CombinedOrbitsDF[self.arcdate_v2]['Date_pd']

            ##### NTW Coordinate System
#             NTW_orbfil  = [Convert_cartesian_to_NTW_returnall(x) for x in state_vector]
            NTW_orb  = [Convert_cartesian_to_NTW_returnall(x_orb, Tmat_ntw_i, False) for x_orb, Tmat_ntw_i in zip(state_vector,Tmat_ntw)]
            n_orb        = []
            t_orb        = []
            w_orb        = []

            for vecorb,matrixorb in NTW_orb:
                n_orb.append( vecorb[0])
                t_orb.append( vecorb[1])
                w_orb.append( vecorb[2])        

            data_orbfil['N'] = n_orb
            data_orbfil['T'] = t_orb
            data_orbfil['W'] = w_orb
            ##### XYZ Coordinate System
            data_orbfil['X'] = X
            data_orbfil['Y'] = Y
            data_orbfil['Z'] = Z
            data_orbfil['Xdot'] = Xdot
            data_orbfil['Ydot'] = Ydot
            data_orbfil['Zdot'] = Zdot
            
            if rsw_bool == True:
                RSW_orb  = [Convert_cartesian_to_RSW_returnall(x_orb, Tmat_rsw_i, False) for x_orb, Tmat_rsw_i in zip(state_vector,Tmat_rsw)]
                Tmat         = []
                r_orb        = []
                s_orb        = []
                wrsw_orb     = []
                for vecorb,matrixorb in RSW_orb:
                    r_orb.append( vecorb[0])
                    s_orb.append( vecorb[1])
                    wrsw_orb.append( vecorb[2]) 
                data_orbfil['R'] = r_orb
                data_orbfil['S'] = s_orb
                data_orbfil['Wrsw'] = wrsw_orb            

            ##### R theta phi Coordinate System
#             data_orbfil['R']     = np.sqrt( np.square(X) + 
#                                             np.square(Y) +
#                                             np.square(Z) )
#             data_orbfil['theta'] = np.arctan(Y / X)
#             data_orbfil['phi']   = np.arccos(Z / (np.sqrt( np.square(X) + 
#                                             np.square(Y) +
#                                             np.square(Z) )))


            ### RESIDUALS:
            resids['Date'] = CombinedOrbitsDF[self.arcdate_v2]['Date_pd']

            ##### NTW Coordinate System
            resids['N'] = (np.array(data_PCE['N']) - np.array(data_orbfil['N']))
            resids['T'] = (np.array(data_PCE['T']) - np.array(data_orbfil['T']))
            resids['W'] = (np.array(data_PCE['W']) - np.array(data_orbfil['W']))
            
            if rsw_bool == True:
                resids['R'] = (np.array(data_PCE['R']) - np.array(data_orbfil['R']))
                resids['S'] = (np.array(data_PCE['S']) - np.array(data_orbfil['S']))
                resids['Wrsw'] = (np.array(data_PCE['Wrsw']) - np.array(data_orbfil['Wrsw']))

            ##### XYZ Coordinate System
#             resids['X'] = (np.array(data_PCE['X']) - np.array(data_orbfil['X']))
#             resids['Y'] = (np.array(data_PCE['Y']) - np.array(data_orbfil['Y']))
#             resids['Z'] = (np.array(data_PCE['Z']) - np.array(data_orbfil['Z']))
            ##### R theta phi Coordinate System
#             resids['R']     = (np.array(data_PCE['R'])     - np.array(data_orbfil['R']))
#             resids['theta'] = (np.array(data_PCE['theta']) - np.array(data_orbfil['theta']))
#             resids['phi']   = (np.array(data_PCE['phi'])   - np.array(data_orbfil['phi']))

            self.OrbitResids[self.arcdate_v2] = {}
            self.OrbitResids[self.arcdate_v2]['data_orbfil'] = data_orbfil
            self.OrbitResids[self.arcdate_v2]['data_PCE']    = data_PCE
            self.OrbitResids[self.arcdate_v2]['resids']      = resids

            
            N_mean, N_rms, N_rms_0 = self.STATS_residuals(self.OrbitResids[self.arcdate_v2]['resids']['N'])
            T_mean, T_rms, T_rms_0 = self.STATS_residuals(self.OrbitResids[self.arcdate_v2]['resids']['T'])
            W_mean, W_rms, W_rms_0 = self.STATS_residuals(self.OrbitResids[self.arcdate_v2]['resids']['W'])
            data['N_RMS'].append(N_rms_0)
            data['T_RMS'].append(T_rms_0)
            data['W_RMS'].append(W_rms_0)
            RMS_total_ntw = np.sqrt( ( np.square(N_rms_0) + np.square(T_rms_0) + np.square(W_rms_0) )/3  )
            data['RMS_total_NTW'].append(RMS_total_ntw)
        
        
            if rsw_bool == True:
                R_mean, R_rms, R_rms_0 = self.STATS_residuals(self.OrbitResids[self.arcdate_v2]['resids']['R'])
                S_mean, S_rms, S_rms_0 = self.STATS_residuals(self.OrbitResids[self.arcdate_v2]['resids']['S'])
                Wrsw_mean, Wrsw_rms, Wrsw_rms_0 = self.STATS_residuals(self.OrbitResids[self.arcdate_v2]['resids']['Wrsw'])
                data['R_RMS'].append(R_rms_0)
                data['S_RMS'].append(S_rms_0)
                data['Wrsw_RMS'].append(W_rms_0)
                RMS_total_rsw = np.sqrt( ( np.square(R_rms_0) + np.square(S_rms_0) + np.square(Wrsw_rms_0) )/3  )
                data['RMS_total_RSW'].append(RMS_total_rsw)

        
        
            self.Statistics[self.arcdate_v2] = pd.DataFrame.from_dict(data) 
       
        
    
                ### FREE UP MEMORY
                ####-----------------------------------------------------------------
                #### DELETE UNNECESSARY VARS IN DENSITY
            del self.__dict__['Density'][self.arcdate_v2]['drhodz (kg/m**3/m)']
            del self.__dict__['Density'][self.arcdate_v2]['flux_daily']
            del self.__dict__['Density'][self.arcdate_v2]['flux_avg']
            del self.__dict__['Density'][self.arcdate_v2]['Kp']

            ####-----------------------------------------------------------------
            #### DELETE UNNECESSARY VARS IN Residuals_obs
            ####-----------------------------------------------------------------
            #### DELETE UNNECESSARY VARIABLES IN AdjustedParams
            ####-----------------------------------------------------------------
            #### DELETE UNNECESSARY VARIABLES IN Trajectory_orbfil

            
            del self.__dict__['Trajectory_orbfil'][self.arcdate_v2]
            del self.__dict__['Residuals_summary'][self.arcdate_v2]

            
            ##### Optional, delete the coordinate systems we dont want
            
            del self.__dict__['OrbitResids'][self.arcdate_v2]['data_orbfil']['X']
            del self.__dict__['OrbitResids'][self.arcdate_v2]['data_orbfil']['Y']
            del self.__dict__['OrbitResids'][self.arcdate_v2]['data_orbfil']['Z']
            del self.__dict__['OrbitResids'][self.arcdate_v2]['data_orbfil']['Xdot']
            del self.__dict__['OrbitResids'][self.arcdate_v2]['data_orbfil']['Ydot']
            del self.__dict__['OrbitResids'][self.arcdate_v2]['data_orbfil']['Zdot']
            del self.__dict__['OrbitResids'][self.arcdate_v2]['data_PCE']['X']
            del self.__dict__['OrbitResids'][self.arcdate_v2]['data_PCE']['Y']
            del self.__dict__['OrbitResids'][self.arcdate_v2]['data_PCE']['Z']
            del self.__dict__['OrbitResids'][self.arcdate_v2]['data_PCE']['Xdot']
            del self.__dict__['OrbitResids'][self.arcdate_v2]['data_PCE']['Ydot']
            del self.__dict__['OrbitResids'][self.arcdate_v2]['data_PCE']['Zdot']
        
            ### IF ON THE LAST ARC, delete this silly thing that got double saved somehow
            if iarc+1 == num_arcs:
                for iarc, arc in enumerate(self.__dict__['global_params']['arc_input']):
#                     self.set_file_paths_for_multiple_arcs( arc, iarc, False )

                    if 'OrbitResids' in self.__dict__['run_parameters'+self.arcdate_v2]:
                        del self.__dict__['run_parameters'+self.arcdate_v2]['OrbitResids']
                    if 'Statistics' in self.__dict__['run_parameters'+self.arcdate_v2]:
                        del self.__dict__['run_parameters'+self.arcdate_v2]['Statistics']
                    
                    if 'request_data' in self.__dict__['run_parameters'+self.arcdate_v2]:
                        del self.__dict__['run_parameters'+self.arcdate_v2]['request_data']
            
            ### Must use garbage collector in conjunction with del to clear memory in python
            gc.collect()
        
### END OF CLASS       
        