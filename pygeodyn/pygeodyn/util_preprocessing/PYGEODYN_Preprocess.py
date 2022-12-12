from scipy.io import FortranFile
import numpy as np
import pandas as pd
from collections import namedtuple
import time
# Fortran calls
import subprocess
import os


import sys
sys.path.insert(0,'/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_dir/')
from common_functions          import MJDS_to_YYMMDDHHMMSS


class PygeodynPreprocessing:
    
    
    def __init__(self,  path_binary, path_preprocessing, arc_files):  
        
        # Inputs
#         self.__rvg_filename =  path + filename
        self.path_binary = path_binary
        self.path_preprocessing = path_preprocessing

        self.arc_files = arc_files
    
        # Hardcoded
        self.record_length = 29  # words (2 Bytes per word)
        self.overlap = 5.40027   # hours
        self.tabtab = '     '
        self.time_estimate_onAWS = 0.0005 # ~ num of secs per record ( 56.11/ 109434)

             
        
    def RVGfiles_read_rvg_binary(self):
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

        data_titles = [ 'MJDSEC_secs_timeGPS' ,
                        'RSEC_fractional_secs',
                        'GPS_offset_secs_utc' ,
                        'spare_4' ,
                        'X_statevector_m' ,
                        'Y_statevector_m' ,
                        'Z_statevector_m' ,
                        'XDOT_statevector_m_s' ,
                        'YDOT_statevector_m_s' ,
                        'ZDOT_statevector_m_s' ,
                        'latitude_sat',
                        'longitude_sat',
                        'height_sat_m',
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
                        'Quaternion_QI_J2000_to_ITRF_ECF',
                        'Quaternion_Q2_J2000_to_ITRF_ECF',
                        'Quaternion_Q3_J2000_to_ITRF_ECF',
                        'Quaternion_Q4_J2000_to_ITRF_ECF',
                        'Greenwich_HR_angle',
                        ]

        sentinel_titles = ['delimeter', 
                          'Satellite_ID',
                          'G_IIS_vers',
                          'G_IIE_vers',
                          ]


        __rvg_filename = self.__rvg_filename
        record_len = self.record_length

        #### determine the approximate number of records...
        # Open file
        with open(__rvg_filename,'rb') as f:
            b=f.read()      # read in binary file as bytes
        np_data = np.frombuffer(b)  # create a numpy array
        est_num_records = int((np_data.size/29) - 29*2)
        
#         print(self.tabtab, '----- Loading ',__rvg_filename  )
        print(self.tabtab, '----- The file has ~%i records. Will take ~%i seconds' % (est_num_records,  self.time_estimate_onAWS*est_num_records ) )

        #### Save the data as a dictionary with keys to hold 
        #          1   header   record, 
        #         many  data    records
        #          1   sentinal record
        
        rvg_data = {}
        rvg_data['header']    = {}
        rvg_data['sentinel']  = {}
        rvg_data['data'] = pd.DataFrame(dict(zip(data_titles,np.ones(record_len)*np.nan) ), index=np.arange(0,est_num_records) )

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
                print(self.tabtab, '----- End of file')

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
        del rvg_data['data']['latitude_sat']
        del rvg_data['data']['longitude_sat']
        del rvg_data['data']['height_sat_m']
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
        del rvg_data['data']['Quaternion_QI_J2000_to_ITRF_ECF']
        del rvg_data['data']['Quaternion_Q2_J2000_to_ITRF_ECF']
        del rvg_data['data']['Quaternion_Q3_J2000_to_ITRF_ECF']
        del rvg_data['data']['Quaternion_Q4_J2000_to_ITRF_ECF']
        del rvg_data['data']['Greenwich_HR_angle']
        
        self.rvg_data = rvg_data
#         return(rvg_data)







    def RVG_Files_add_datetime_column(self):
        '''
        This function includes an additional function to convert the MJDSecs to datetime string.
        
        '''
        
#         def MJDS_to_YYMMDDHHMMSS(input_ModJulianDay_secs):
#             '''
#             This function takes modified julian day seconds (MJDS) as input 
#             and returns a date_string in the format YYMMDDHHMMSS.
#             '''

#             #########################################
#             # Define some constants
#             SECDAY              = 86400
#             geodyn_ref_time_mjd = 30000
#             jd_0                = 2400000.5
#             d36525              = 365.25
#             d122                = 122.1
#             d30600              = 30.6001
#             half                = 0.5
#             ib                  = -15
#             d17209              = 1720996.5

#             ######  CONVERT FROM MJDS TO MJD
#             # Inputs:
#             MJDS = input_ModJulianDay_secs
#             #
#             MJD = (MJDS/SECDAY) + geodyn_ref_time_mjd

#             ######  CONVERT FROM MJD TO YMD
#             # Note from zach-- I took this calculation from geodyn...
#             # There is more going on here than I understand, 
#             # but I want to stay on their level of accuracy
#             #
#             JD = MJD + jd_0                  #  Convert to JulianDay
#             c  = int( JD + half ) + 1537     # ??   sorry, i'm   ??
#             nd = int( (c - d122) / d36525 )  # ??   not sure     ??
#             e  = int( d36525 * nd )          # ??   what this    ??
#             nf = int( ( c - e ) / d30600 )   # ??   all is       ??
#             # ----
#             frac = (JD + half) - int( JD + half )           # frac of day leftover
#             iday = c - e - int( d30600 * nf ) + frac        # day
#             imonth  = nf -  1   - 12 * int( nf / 14 )       # month
#             iyyyy = nd - 4715 - int(  ( 7 + imonth ) / 10 ) # YYYY
#             #
#             ##### Use modular division to get 2 digit year
#             iyear =  iyyyy % 100 
#             #
#             #### Return YYMMDD 
#             yymmdd = int(iyear * 10000 + imonth * 100 + iday)


#             ##### Calculate Hours, Minutes, seconds
#             isec_mjd  =  MJDS % 86400

#             ihour    = isec_mjd/3600
#             iminutes = (ihour % 1)*60
#             isec     = (iminutes % 1)*60 

#             ihour_str = str(int((ihour)))
#             iminutes_str  = str(int((iminutes)))
#             isec_str      = str(int(round(isec)))

#             if len(ihour_str)==1:
#                 ihour_str = '0'+ihour_str
#             if len(iminutes_str)==1:
#                 iminutes_str = '0'+iminutes_str
#             if len(isec_str)==1:
#                 isec_str = '0'+isec_str

#             #hhmmss  =  int((ihour*10000) + (iminutes*100) + isec)
#             hhmmss  =  ihour_str + iminutes_str + isec_str
#             YYMMDDHHMMSS = str(yymmdd) + '-' + str(hhmmss)
            
#             return(YYMMDDHHMMSS)

   



        
        rvg_file = self.rvg_data

        # data
        mjdsecs = rvg_file['data']['MJDSEC_secs_timeGPS']

        #convert the MJDS to a usable string.
        yymmdd_str = [MJDS_to_YYMMDDHHMMSS(x) for x in mjdsecs]

        # convert string of dates to datetime for plotting
        dates_without_offset = [pd.to_datetime( x, format='%y%m%d-%H%M%S') for x in yymmdd_str]

        offset = pd.Series(pd.to_timedelta(self.rvg_data['data']['GPS_offset_secs_utc'],'s'))
        
        dates = pd.Series(dates_without_offset) + offset
        
        
        self.rvg_data['data'].insert(0, 'Date', dates)
        self.rvg_data['data']['yymmdd_str'] = yymmdd_str

#         return(dates)




    def RVGfiles_timeoverlap_GetChoppingTime(self):
        '''
        This function retrieves the times in datetime at which the chop will happen
        '''
        file1 = self.rvg_data        
#         (_, _, tot_overlap) = time_overlap(file1, file2)
        tot_overlap = self.overlap
    
    
        file1_start = file1['data']['Date'].iloc[0] 
        file1_end = file1['data']['Date'].iloc[-1] 

        file1_new_start = file1_start + pd.Timedelta(tot_overlap/2, unit='hours')
        file1_new_end = file1_end - pd.Timedelta(tot_overlap/2, unit='hours')

        return(file1_new_start, file1_new_end)




    def RVGfiles_chop_the_ends(self):
        '''
        Chop the ends off the file.
        '''
              
#         print(self.tabtab,'Chopping the overlap times off of the datasets.')
        (file1_new_start, 
        file1_new_end) = self.RVGfiles_timeoverlap_GetChoppingTime()


        df1 = self.rvg_data['data']

        ##### Chop the FRONT off of the FIRST file
        # select all the values greater than our new start and grab the last one 
        val1_front = df1.Date[df1.Date < file1_new_start].iloc[-1]
        indx1_front = df1.Date[df1.Date==val1_front].index.unique()[0]

        ##### Chop the END off of the FIRST file
        # select all the values less than our new start and grab the first one 
        val1_end = df1.Date[df1.Date > file1_new_end].iloc[1]
        indx1_end = df1.Date[df1.Date==val1_end].index.unique()[0]


        df1_new = df1[:indx1_end][indx1_front+1:] # add one index so there is no overlap in time
    
#         self.rvg_data_chopped = df1_new
        return(df1_new)





#     def get_fixed_rvgdata(self):
        
#         # Read in the binary data into a dict w/ pd.dataframes
#         self.RVGfiles_read_rvg_binary()
        
#         # Convert MJDSecs to pandas datetime
#         self.RVG_Files_add_datetime_column()
        
#         # Chop off the ends of the files to eliminate overlap
#         self.RVGfiles_chop_the_ends()
        
        
    def get_timechopped_rvgdata(self):

        self.total_files=np.size(self.arc_files)

        print(self.tabtab,'Running through the pre-processing procedure...')
        print(self.tabtab,'=======================================================')
        print(self.tabtab,'STEP 1: Convert RVG binary files to pandas DataFrame...')
        print(self.tabtab,'=======================================================')
        
        print(self.tabtab, 'Loading and processing %i files will take approx. %.2f minutes. '% (self.total_files, (self.total_files*self.time_estimate_onAWS*109000)/60))
        print(self.tabtab,self.tabtab, 'Not including unzipping/zipping times')

        print()
#         print(self.tabtab,'First we unzip the files... check the console for progess.')
        os.chdir(self.path_binary)

        
        # Initialize a Dataframe 
        df1 = pd.DataFrame()
        
        count=0
        for i, file in enumerate(self.arc_files):
            
            self.filenum  =i+1 
            print(self.tabtab, '--- File %i / %i '% (self.filenum, self.total_files))

            print(self.tabtab, '----- Unzipping file...', file)
#             os.system('gunzip -vr '+file+'.gz')
#             os.system('bunzip2 -v '+file+'.bz2')

            
            self.__rvg_filename =  self.path_binary +'/' + file
            

            # Read in the binary data into a dict w/ pd.dataframes
            self.RVGfiles_read_rvg_binary()

            # Convert MJDSecs to pandas datetime
            self.RVG_Files_add_datetime_column()

            # Chop off the ends of the files to eliminate overlap
            rvg_data_chopped = self.RVGfiles_chop_the_ends()
                       
            if count == 0:
                df1 = rvg_data_chopped
                count += 1
            else:
                # to append df2 at the end of df1 dataframe
                df1 = pd.concat([df1, rvg_data_chopped])
            
            print(self.tabtab,'Zipping file...', file)
#             os.system('gzip -v '+file)

            print()

        self.RVG_FINAL = df1
        
        
    def make_ascii_traj_txt_file_for_pcefortran(self):
        import copy

        
        
#         print(self.tabtab,  "Saving the fixed datasets as an ascii text file... ")
#         print(self.tabtab,"PATH: ",self.path_preprocessing , '/TRAJ.txt' )

        #### First call the functions that convert RVG files to a dataframe
        self.get_timechopped_rvgdata()
    
    
##########   UNCOMMENT THE BELOW TO CONSTRUCT A PCE TEXT FILE
#         ##########----------------------------------------------------------------------------------------------------------------
#         #####     Save a file to be used to amend ELEMS in IISSET
#         print()
#         print(self.tabtab,'=======================================================')
#         print(self.tabtab,'STEP 2: Saving the EPOCH, POS, VEL to a text file....')
#         print(self.tabtab,'=======================================================')
#         print()

# #         print('Saving the EPOCH info and POS+VEL vectors as text file.')
#         df_traj_txt_big = copy.deepcopy(self.RVG_FINAL)

#         #### We need to prepare the dataframe to be saved to a CSV according 
#         #        to the requirements of the fortran code

#         ##### Save as a txt file with pandas built-in function.
#         #### BZIP2 the file and move it to the setup directory
#         df_traj_txt_big.to_csv(self.path_preprocessing + '/PCE_ascii_new.txt', sep=' ', index = False, header=False)
#         os.system('rm'+' '+'/data/data_geodyn/inputs/icesat2/setups/PCE_ascii_new.txt')
#         os.system('rm'+' '+'/data/data_geodyn/inputs/icesat2/setups/PCE_ascii_new.txt.bz2')

#         os.system('bzip2'+' '+'/data/data_geodyn/inputs/icesat2/pre_processing/PCE_ascii_new.txt')
#         os.system('mv'+' '+self.path_preprocessing+'/PCE_ascii_new.txt.bz2'+ ' '+'/data/data_geodyn/inputs/icesat2/setups/PCE_ascii_new.txt.bz2')

        
# #         df_traj_txt_big = 0
#         del df_traj_txt_big
    
#         print('File loc: /data/data_geodyn/inputs/icesat2/setups/PCE_ascii_new.txt.bz2')
#         ##########----------------------------------------------------------------------------------------------------------------




        print()
        print(self.tabtab,'=======================================================')
        print(self.tabtab,'STEP 3: Make ASCII.txt file to be read by FORTRAN code.')
        print(self.tabtab,'=======================================================')
        print()
       
        df_traj_txt = copy.deepcopy(self.RVG_FINAL)

        #### We need to prepare the dataframe to be saved to a CSV according 
        ####        to the requirements of the fortran code
        #### first integer of TRAJ.txt is just the first word 
        ####      of RVG data record converted to an integer 
        df_traj_txt.insert(0, 'first_int', df_traj_txt['MJDSEC_secs_timeGPS'].astype(int))

        del df_traj_txt['Date']
        del df_traj_txt['MJDSEC_secs_timeGPS']
        del df_traj_txt['XDOT_statevector_m_s']
        del df_traj_txt['YDOT_statevector_m_s']
        del df_traj_txt['ZDOT_statevector_m_s']

        #### The first floating point of TRAJ.txt is just 
        #        the sum of words 2 and 3 of the RVG data record
        sum_words_2_and_3 = df_traj_txt['RSEC_fractional_secs'] + df_traj_txt['GPS_offset_secs_utc']
        df_traj_txt.insert(1, 'first_float', sum_words_2_and_3)
        del df_traj_txt['RSEC_fractional_secs'] 
        del df_traj_txt['GPS_offset_secs_utc']

        ##### Save as a txt file with pandas built-in function
        df_traj_txt.to_csv(self.path_preprocessing + '/TRAJ.txt', sep=' ', index = False, header=False)

#         df_traj_txt = 0
        del df_traj_txt

    def call_fortran_pce_converter(self):
        
        print()
        print(self.tabtab,'=======================================================')
        print(self.tabtab,'STEP 4: Call fortran code to construct G2B file. ')
        print(self.tabtab,'=======================================================')
        print()
        
        path_to_data = self.path_preprocessing
        path_to_PCE_fortran ='/data/geodyn_proj/pygeodyn/pygeodyn_develop/util_preprocessing/'
        in_filename  = 'TRAJ.txt'
        out_filename = 'g2b_pce'
        ### CHANGE DIRECTORY to where the fortran code is hosted
        os.chdir(path_to_PCE_fortran)

        #### Write the inputs to the fortran code to a file to be read in variably:
        
        file_FTN_opts = open(path_to_PCE_fortran +"options_fortrancode.txt","w+")
        file_FTN_opts.writelines(path_to_data+'/'+ '\n') 
        file_FTN_opts.writelines(in_filename +'\n')
        file_FTN_opts.writelines(out_filename+'\n')
        file_FTN_opts.close()        
        
        
        
        #### Compile the pce code:
        command_1 = './compile_pce_f.sh'
        subprocess.run(command_1, shell = True)
        print('pce_fortran.f compiled')
        
        #### Execute the pce code
        command_2 = './ExecutePCE.exe > out_pce 2> err_execute'
        subprocess.run(command_2, shell = True)
        print('pce_fortran.f executed')
        print('')
        
        os.system('gzip -vr '+ path_to_data+'/'+out_filename)

        os.system('mv '+ path_to_data+'/'+out_filename+'.gz'+ ' '+'/data/data_geodyn/inputs/icesat2/g2b/')
        os.system('rm'+' '+self.path_preprocessing + '/TRAJ.txt')
        
        if os.path.exists('/data/data_geodyn/inputs/icesat2/g2b/'+out_filename+'.gz'):
            print('The G2B file has been saved to: ','/data/data_geodyn/inputs/icesat2/g2b/',out_filename,'.gz')
        else:
            print('The G2B binary file has been saved to: ',path_to_data,'/',out_filename, sep='')


        
        
        
    def run_preprocess_PCE(self):
        
        self.make_ascii_traj_txt_file_for_pcefortran()
        self.call_fortran_pce_converter()
        
        
        
        
        
        
