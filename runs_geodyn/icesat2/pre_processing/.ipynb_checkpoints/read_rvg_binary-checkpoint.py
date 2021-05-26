from scipy.io import FortranFile
import numpy as np
import pandas as pd


 





def read_rvg_binary(record_length, filename):
    '''
    This function converts the RVG trajectory data to a python friendly format.
    Output is a dict that contains the header, data, and sentinal records for a file.
    
    #------------INFO------------------------
    #
    1. These files are in what is called the **RVG format**. The RVG files are pretty simple to unpack (lol)
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


    __rvg_filename = filename
    record_len = record_length

    #### determine the approximate number of records...
    # Open file
    with open(__rvg_filename,'rb') as f:
        b=f.read()      # read in binary file as bytes
    np_data = np.frombuffer(b)  # create a numpy array
    est_num_records = int((np_data.size/29) - 29*2)

    print('There are ~ %i records in this file.' % (est_num_records) )

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
            print('End of record')
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
    return(rvg_data)


















