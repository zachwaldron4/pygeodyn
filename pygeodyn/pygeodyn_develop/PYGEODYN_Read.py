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

import sys
sys.path.insert(0,'/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_dir/')

### Import the Classes from the Tools
# from util_classtools import Util_Tools
from common_functions          import MJDS_to_YYMMDDHHMMSS, Convert_ET_TDT_to_UTC


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

        #         print('first_sat_first_time', first_sat_first_time)
        #         print('last_sat_last_time  ', last_sat_last_time)




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
        MJDS_UTC = [Convert_ET_TDT_to_UTC(float(x), 37) for x in orbfil_dict['data_record']['MJDSEC ET'] ]

        ##### Calculate the Gregorian Calendar date:
        yymmdd_str = [MJDS_to_YYMMDDHHMMSS(x) for x in MJDS_UTC]

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
#         text_GA_list = ["0GA 9P 11t1",
#                         "0GA 9P 12t1",
#                         "0GA 9P 21t1",
#                         "0GA 9P 22t1",
#                         "0GA 9P 11t2",
#                         "0GA 9P 12t2",
#                         "0GA 9P 21t2",
#                         "0GA 9P 22t2",
#                             ]    
#         print(self.total_iterations)
        SatMain_AdjustedParams = {}
        for i_iter,iterval in enumerate(np.arange(1, self.total_iterations+1)):
#             print(iterval)
            SatMain_AdjustedParams[iterval] = {}
            for isat, satval in enumerate(sat_list):
#                 if self.empirical_accels == True:
#                     pass
#                     for iga, ga_val in enumerate(text_GA_list):
#                         SatMain_AdjustedParams[iterval][satval] = {}
#                         SatMain_AdjustedParams[iterval][satval]['0CD'] = {}
#                         SatMain_AdjustedParams[iterval][satval][ga_val] = {}
#                 else:
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
                if 'DRAG' in line:
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

                data_1stline = linecache.getline(self._iieout_filename,val_lines+1) #
                data_2ndtline = linecache.getline(self._iieout_filename,val_lines+2) #
                data_3rdline = linecache.getline(self._iieout_filename,val_lines+3) #

                apriorival = float(data_1stline[19:38])
                prevval = float(data_2ndtline[19:38])
                currentval  = float(data_3rdline[19:38])
                totalDelta = float(data_2ndtline[42:62])
                currentDelta =  float(data_3rdline[42:62])
                AprioriSigma = float(data_2ndtline[63:78])
                CurrentSigma =  float(data_3rdline[63:78])
                
#                 print('check_iter',check_iter)
#                 print('check_sat',check_sat)
#                 print('text_param_adjusts',text_param_adjusts)
                SatMain_AdjustedParams[check_iter][check_sat][text_param_adjusts] = {'APRIORI_VALUE': apriorival,
                                                     'PREVIOUS_VALUE': prevval,
                                                     'CURRENT_VALUE': currentval,
                                                      'TOTAL_DELTA':totalDelta,
                                                     'CURRENT_DELTA': currentDelta,
                                                     'APRIORI_SIGMA': AprioriSigma,
                                                     'CURRENT_SIGMA': CurrentSigma }
                i+=1
                #### this makes it so that you can properly index the iterations
                i = np.mod(i,np.shape(lines_params)[0])  

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
                                                                        'PREVIOUS_VALUE': prevval,
                                                                        'CURRENT_VALUE': currentval,
                                                                        'TOTAL_DELTA':totalDelta,
                                                                        'CURRENT_DELTA': currentDelta,
                                                                        'APRIORI_SIGMA': AprioriSigma,
                                                                        'CURRENT_SIGMA': CurrentSigma }
        if self.empirical_accels == True:
            print('There are some weird things here for Gen.Acc. Not done yet.')

            if self.DATA_TYPE == 'GPS':
                print('There are some weird things here for Gen.Acc. Not done yet.')
        #         break
            else:
                print('There are some weird things here for Gen.Acc. Not done yet.')

                
     # THE BELOW CODE IS BROKEN AND NEEDS TO BE FIXED IF YOU EVER WANT TO TURN ON THE GENERAL ACCELS
    #
    #             accel_9_dates = []
    #             for i,val in enumerate(card_inputs_range):
    #                     line = linecache.getline(iieout_file,val)            
    #                     if 'ACCEL9    99' in line:
    #                         check_sat = int(line[18:26])
    #                         if check_sat == self.params['SAT_ID']:

    #                             accel_9_dates.append(line[45:56].strip())
    #             #------------------------------------------------------------------            
    #             # this block of code is used to remove the whitespace from the date 
    #             fix_date_string = []
    #             for i, val in enumerate(accel_9_dates):
    #                 if ' ' in val:
    #                     rebuild_string = []
    #                     for ii,valval in enumerate(val):
    #                         if valval == ' ':
    #                             rebuild_string.append(valval.replace(" ", "0"))
    #                         else:
    #                             rebuild_string.append(valval)
    #                     rebuild_string = "".join(rebuild_string)
    #                     fix_date_string.append(rebuild_string)
    #                 else:
    #                     fix_date_string.append(val)
    #         #------------------------------------------------------------------            
    #             accel_9_Dates = pd.to_datetime(fix_date_string, format='%y%m%d%H%M%S')  #YYMMDDHHMMSS

    #                 # AND THIS SPECIFIC LIST OF Gen. ACCELS
    #                 # 1st Value (1 or 2) Indicates direction of 9 parameter general acceleration
    #                     # 1 - Along Track ((R x V) x R)
    #                     # 2 - Cross Track (R x V)
    #                     # 3 - Radial (R)

    #                 #  2nd Value (1 or 2) Indicates type of 9 parameter general acceleration parameter
    #                     # 1 - Cosine coefficient (A)
    #                     # 2 - Sine coefficient (B)
    #                     # 3 - Constant (C)
    #             text_GA_list = ["0GA 9P 11",
    #                             "0GA 9P 12",
    #                             "0GA 9P 21",
    #                             "0GA 9P 22",
    #                             ]    


    #             lines = []
    #             for itt,text_GA_adjusts in enumerate(text_GA_list): 

    #                 with open(iieout_file, 'r') as f:
    #                     for line_no, line_text in enumerate(f):
    #                         if text_GA_adjusts in line_text:
    #                             lines.append(line_no) 

    #                 # Loop thru the lines saved above and grab the data occording to its name 
    #                 # and location in the file
    #                 for il,val_lines in enumerate(lines[::2]):
    #                     check_sat = int(linecache.getline(iieout_file,val_lines+1)[10:18])

    #                     # Read the lines backwars until you hit a header
    #                     find_last_header_range = np.arange(val_lines, val_lines-1000, -1)
    #                     for iiline, iivaline in enumerate(find_last_header_range):
    #                         line_find_header = linecache.getline(iieout_file,iivaline)
    #                         if 'PARAMETER ADJUSTMENT SUMMARY' in line_find_header:
    #                             check_iter =int(line_find_header[57:60])
    #                             break

    #                     data_1stline = linecache.getline(iieout_file,val_lines+1) #
    #                     data_2ndtline = linecache.getline(iieout_file,val_lines+2) #
    #                     data_3rdline = linecache.getline(iieout_file,val_lines+3) #

    #                     apriorival = float(data_1stline[18:41])
    #                     prevval = float(data_2ndtline[18:41])
    #                     currentval  = float(data_3rdline[18:41])
    #                     totalDelta = float(data_2ndtline[42:62])
    #                     currentDelta =  float(data_3rdline[42:62])
    #                     AprioriSigma = float(data_2ndtline[63:78])
    #                     CurrentSigma =  float(data_3rdline[63:78])

    #             #         print('Sat:  ', check_sat)
    #             #         print('Iter: ', check_iter)
    #             #         print('Val:  ', currentval)
    #             #         print('Time: ', date_timedep_cds[itt])

    #                     SatMain_AdjustedParams[check_iter][check_sat][text_GA_adjusts+'t1'][accel_9_Dates[0]] = {'APRIORI_VALUE': apriorival,
    #                                                                             'PREVIOUS_VALUE': prevval,
    #                                                                             'CURRENT_VALUE': currentval,
    #                                                                             'TOTAL_DELTA':totalDelta,
    #                                                                             'CURRENT_DELTA': currentDelta,
    #                                                                             'APRIORI_SIGMA': AprioriSigma,
    #                                                                             'CURRENT_SIGMA': CurrentSigma }
    #                 for il,val_lines in enumerate(lines[1::2]):
    #                     check_sat = int(linecache.getline(iieout_file,val_lines+1)[10:18])

    #                     # Read the lines backwars until you hit a header
    #                     find_last_header_range = np.arange(val_lines, val_lines-1000, -1)
    #                     for iiline, iivaline in enumerate(find_last_header_range):
    #                         line_find_header = linecache.getline(iieout_file,iivaline)
    #                         if 'PARAMETER ADJUSTMENT SUMMARY' in line_find_header:
    #                             check_iter =int(line_find_header[57:60])
    #                             break

    #                     data_1stline = linecache.getline(iieout_file,val_lines+1) #
    #                     data_2ndtline = linecache.getline(iieout_file,val_lines+2) #
    #                     data_3rdline = linecache.getline(iieout_file,val_lines+3) #

    #                     apriorival = float(data_1stline[18:41])
    #                     prevval = float(data_2ndtline[18:41])
    #                     currentval  = float(data_3rdline[18:41])
    #                     totalDelta = float(data_2ndtline[42:62])
    #                     currentDelta =  float(data_3rdline[42:62])
    #                     AprioriSigma = float(data_2ndtline[63:78])
    #                     CurrentSigma =  float(data_3rdline[63:78])

    #             #         print('Sat:  ', check_sat)
    #             #         print('Iter: ', check_iter)
    #             #         print('Val:  ', currentval)
    #             #         print('Time: ', date_timedep_cds[itt])

    #                     SatMain_AdjustedParams[check_iter][check_sat][text_GA_adjusts+'t2'][accel_9_Dates[1]] = {'APRIORI_VALUE': apriorival,
    #                                                                             'PREVIOUS_VALUE': prevval,
    #                                                                             'CURRENT_VALUE': currentval,
    #                                                                             'TOTAL_DELTA':totalDelta,
    #                                                                             'CURRENT_DELTA': currentDelta,
    #                                                                             'APRIORI_SIGMA': AprioriSigma,
    #                                                                             'CURRENT_SIGMA': CurrentSigma }

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
#                                     'flux_daily',
#                                     'flux_avg',
#                                     'Kp',
                                    'X',
                                    'Y',
                                    'Z',
                                    'XDOT',
                                    'YDOT',
                                    'ZDOT',
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
        
        

        
        #--------------------------------------------------------
        end = time.time()
        elapsed = end - start
#         print('Density file end: ',elapsed)
        return(DEN_df)

        
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
        This function reads in the residuals from the massive IIEOUT file.

        For residuals, there are specific station-satellite configurations.  
        It is prudent to read in each configuration and save which satellites make it up.  
        This is much harder to do than simply reading in all resuiduals (as I did before)

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

        return(resid_meas_summry)
        
        
        
    def read_statistics_iieout(self):
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

    def getData_residsObserved_iieout(self):
        return(self.read_observed_resids())

    #----------------------------------------------

    def getData_residsMeasSumm_iieout(self):
        return(self.read_resid_measurement_summaries())
    #----------------------------------------------

    def getData_stats_endOfFile_iieout(self):
        return(self.read_statistics_iieout())

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
        self.Statistics        = {}
        
        num_arcs = np.size(self.arc_input)
        
        ##### Go thru the files once and unzip them
        
        
        self.set_file_paths_for_multiple_arcs( self.arc_input[0], 1, True )
        
        ARC_FILES = self.make_list_of_arcfilenames()
        
        for i in ARC_FILES:
            if os.path.exists(self.path_to_model+'DENSITY/'):
                os.chdir(self.path_to_model+'DENSITY/')
                os.system('bunzip2 -v '+ i +'.bz2')

            if os.path.exists(self.path_to_model+'ORBITS/'):
                os.chdir(self.path_to_model+'ORBITS/')
                os.system('bunzip2 -v '+i+'_orb1.bz2')

        
        for iarc, arc in enumerate(self.arc_input):
            self.set_file_paths_for_multiple_arcs( arc, iarc )
            self.check_if_run_converged(self._iieout_filename)

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
                elif choice == 'Statistics':
                    self.Statistics[arc]             = self.getData_stats_endOfFile_iieout()
                elif choice == 'Trajectory_orbfil':
#                     print('TESTTESTTEST')
                    self.Trajectory_orbfil[arc]      = self.getData_Trajectory_orbfil()
                else:
#                     print('Error in PygeodynReader.getData()')
#                     print('The requested output [', choice, '] does not match any inputs')
                    break
    
#             os.system('bzip2 -v' +' '+self._density_filename)
            
            self.organize_output_object_keys(data_keys, arc, iarc, num_arcs)
            
            
            
#             print('DONE with ARC:' , arc)
#         self.organize_output_object_keys(data_keys, arc)
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        