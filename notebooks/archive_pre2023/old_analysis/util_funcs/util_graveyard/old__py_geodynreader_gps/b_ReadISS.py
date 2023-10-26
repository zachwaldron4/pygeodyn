import numpy as np
import pandas as pd
from datetime import datetime,timedelta
import os.path
import linecache
import time

import sys  
sys.path.insert(0, '/data/analysis/util_funcs/util_common/')
from datetime_column import make_datetime_column




def Save_AdjustedParameters_ISS(Sat_main, iieout_file, AccelStatus = True):
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
    number and second dimension being a dictionary that contains the above data.
    '''
    
    '''
    The way that this function works is it basically loops through the MASSIVE IIEOOUT
    file and finds the line number for certain keywords (0XPOS, 0YPOS, ... etc.).
    We then grab the data next to each of the keywords (the format is fixed).
    
    if we are looking at GPS tracked data, we must
    '''
    

    # find the line numbers of all the adjustment parameters in the too big iieout file
    text_param_lists = ['0XPOS',
                      '0YPOS',
                      '0ZPOS',
                      '0XVEL',
                      '0YVEL',
                      '0ZVEL',]


    # Identify the lines where we have changed parameters and identify how many 
    # iteration numbers there are.
    lines_params = [] 
    with open(iieout_file, 'r') as f:
        for line_no, line_text in enumerate(f):
            if '0XPOS' in line_text :
                lines_params.append(line_no)

    # Build the dictionary to be index based on iteration number   
    # and initialize each iteration number to also be a dictionary
    SatMain_AdjustedParams = {}
    for i,val in enumerate(np.arange(1, np.size(lines_params)+1)):
        SatMain_AdjustedParams[val] = {}
        SatMain_AdjustedParams[val]['0CD'] = {}
        SatMain_AdjustedParams[val]['0GA'] = {}
        SatMain_AdjustedParams[val]['0GA_t2'] = {}


    # make a list of the dates for the time dependent drag option
    timedep_drag_text = 'DRAG             '+str(Sat_main)

    lines_timedep_cd = [] 
    with open(iieout_file, 'r') as f:
        for line_no, line_text in enumerate(f):
            if timedep_drag_text in line_text :
                lines_timedep_cd.append(line_no)


    date_timedep_cd = []
    for il,val_lines in enumerate(lines_timedep_cd):
        data_line = linecache.getline(iieout_file,val_lines+1) # 
        try:
            int(data_line[45:55])
            date_timedep_cd.append(str(data_line[45:55]))  #YYMMDDHHMMSS
        except:
            pass

    date_timedep_cds = pd.to_datetime(date_timedep_cd, format='%y%m%d%H%S')  #YYMMDDHHMMSS

        
        
    # Search thru the full file for the key words in the above list
    # save the line numbers where those keywords occur
    lines = []
    for text_param_adjusts in text_param_lists: 
        with open(iieout_file, 'r') as f:
            for line_no, line_text in enumerate(f):
                if text_param_adjusts in line_text:
                    lines.append(line_no) 

        # Loop thru the lines saved above and grab the data occording to its name 
        # and location in the file
        Sat_main_lines = [] 
        i=0
        for il,val_lines in enumerate(lines):
            check_sat = int(linecache.getline(iieout_file,val_lines+1)[10:18])
            check_iter = int((linecache.getline(iieout_file,lines_params[i]-3))[57:60])
            if check_sat == Sat_main:
                # print(check_sat)
                # print(check_iter)
                data_1stline = linecache.getline(iieout_file,val_lines+1) #
                data_2ndtline = linecache.getline(iieout_file,val_lines+2) #
                data_3rdline = linecache.getline(iieout_file,val_lines+3) #

                apriorival = float(data_1stline[19:38])
                prevval = float(data_2ndtline[19:38])
                currentval  = float(data_3rdline[19:38])
                totalDelta = float(data_2ndtline[42:62])
                currentDelta =  float(data_3rdline[42:62])
                AprioriSigma = float(data_2ndtline[63:78])
                CurrentSigma =  float(data_3rdline[63:78])

#           APRIORI_VALUE
#           PREVIOUS_VALUE              
#           CURRENT_VALUE              
#           TOTAL_DELTA
#           CURRENT_DELTA
#           APRIORI_SIGMA
#           CURRENT_SIGMA

                
                SatMain_AdjustedParams[check_iter][text_param_adjusts] = {'APRIORI_VALUE': apriorival,
                                                     'PREVIOUS_VALUE': prevval,
                                                     'CURRENT_VALUE': currentval,
                                                      'TOTAL_DELTA':totalDelta,
                                                     'CURRENT_DELTA': currentDelta,
                                                     'APRIORI_SIGMA': AprioriSigma,
                                                     'CURRENT_SIGMA': CurrentSigma }
                i+=1
                i = np.mod(i,np.shape(lines_params)[0])

            else:
                print('Oops! Grabbing data for the wrong satellite: ', str(check_sat))
                continue
        # print('Done saving the following parameters: \n', text_param_lists)


    # FOR NOW THIS IS HARDCODED IN AS 43 CD VALUES....
    text_cd_list = ["0CD   T%02d" %i for i in np.arange(1,43)]

    for itt,text_param_adjusts in enumerate(text_cd_list): 
        lines = []                

        with open(iieout_file, 'r') as f:
            for line_no, line_text in enumerate(f):
                if text_param_adjusts in line_text:
                    lines.append(line_no)

        # Loop thru the lines saved above and grab the data occording to its name 
        # and location in the file
        Sat_main_lines = [] 
        i=0
        for il,val_lines in enumerate(lines):
            check_sat = int(linecache.getline(iieout_file,val_lines+1)[10:18])
            check_iter = int((linecache.getline(iieout_file,lines_params[i]-3))[57:60])
            if check_sat == Sat_main:
                # print(check_sat)
                # print(check_iter)
                data_1stline = linecache.getline(iieout_file,val_lines+1) #
                data_2ndtline = linecache.getline(iieout_file,val_lines+2) #
                data_3rdline = linecache.getline(iieout_file,val_lines+3) #

                apriorival = float(data_1stline[19:38])
                prevval = float(data_2ndtline[19:38])
                currentval  = float(data_3rdline[19:38])
                totalDelta = float(data_2ndtline[42:62])
                currentDelta =  float(data_3rdline[42:62])
                AprioriSigma = float(data_2ndtline[63:78])
                CurrentSigma =  float(data_3rdline[63:78])

                SatMain_AdjustedParams[check_iter]['0CD'][date_timedep_cds[itt]] = {'APRIORI_VALUE': apriorival,
                                                     'PREVIOUS_VALUE': prevval,
                                                     'CURRENT_VALUE': currentval,
                                                      'TOTAL_DELTA':totalDelta,
                                                     'CURRENT_DELTA': currentDelta,
                                                     'APRIORI_SIGMA': AprioriSigma,
                                                     'CURRENT_SIGMA': CurrentSigma }
                i+=1
                i = np.mod(i,np.shape(lines_params)[0])

            else:
                print('Oops! Grabbing data for the wrong satellite: ', str(check_sat))
                continue

    if AccelStatus == True:

        # AND THIS SPECIFIC LIST OF Gen. ACCELS
        # 1st Value (1 or 2) Indicates direction of 9 parameter general acceleration
            # 1 - Along Track ((R x V) x R)
            # 2 - Cross Track (R x V)
            # 3 - Radial (R)

        #  2nd Value (1 or 2) Indicates type of 9 parameter general acceleration parameter
            # 1 - Cosine coefficient (A)
            # 2 - Sine coefficient (B)
            # 3 - Constant (C)
        text_GA_list = ["0GA 9P 11",
                        "0GA 9P 12",
                        "0GA 9P 21",
                        "0GA 9P 22",
                       ]    

        for text_param_adjusts in text_GA_list: 
            lines = []  
            with open(iieout_file, 'r') as f:
                for line_no, line_text in enumerate(f):
                    if text_param_adjusts in line_text:
                        lines.append(line_no)


            # Loop thru the lines saved above and grab the data occording to its name 
            # and location in the file
            Sat_main_lines = [] 
            i=0
            for il,val_lines in enumerate(lines[::2]):
                check_sat = int(linecache.getline(iieout_file,val_lines+1)[10:18])
                check_iter = int((linecache.getline(iieout_file,lines_params[i]-3))[57:60])
                if check_sat == Sat_main:
                    # print(check_sat)
                    # print(check_iter)
                    data_1stline = linecache.getline(iieout_file,val_lines+1) #
                    data_2ndtline = linecache.getline(iieout_file,val_lines+2) #
                    data_3rdline = linecache.getline(iieout_file,val_lines+3) #

                    apriorival = float(data_1stline[19:42])
                    prevval = float(data_2ndtline[19:42])
                    currentval  = float(data_3rdline[19:42])
                    totalDelta = float(data_2ndtline[42:63])
                    currentDelta =  float(data_3rdline[42:63])
                    AprioriSigma = float(data_2ndtline[63:79])
                    CurrentSigma =  float(data_3rdline[63:79])


                    SatMain_AdjustedParams[check_iter]['0GA'][text_param_adjusts] = {'APRIORI_VALUE': apriorival,
                                                         'PREVIOUS_VALUE': prevval,
                                                         'CURRENT_VALUE': currentval,
                                                          'TOTAL_DELTA':totalDelta,
                                                         'CURRENT_DELTA': currentDelta,
                                                         'APRIORI_SIGMA': AprioriSigma,
                                                         'CURRENT_SIGMA': CurrentSigma }
                    i+=1
                    i = np.mod(i,np.shape(lines_params)[0])

                else:
                    print('Oops! Grabbing data for the wrong satellite: ', str(check_sat))
                    continue
            for il,val_lines in enumerate(lines[1::2]):
                check_sat = int(linecache.getline(iieout_file,val_lines+1)[10:18])
                check_iter = int((linecache.getline(iieout_file,lines_params[i]-3))[57:60])
                if check_sat == Sat_main:
                    # print(check_sat)
                    # print(check_iter)
                    data_1stline = linecache.getline(iieout_file,val_lines+1) #
                    data_2ndtline = linecache.getline(iieout_file,val_lines+2) #
                    data_3rdline = linecache.getline(iieout_file,val_lines+3) #

                    apriorival = float(data_1stline[19:42])
                    prevval = float(data_2ndtline[19:42])
                    currentval  = float(data_3rdline[19:42])
                    totalDelta = float(data_2ndtline[42:63])
                    currentDelta =  float(data_3rdline[42:63])
                    AprioriSigma = float(data_2ndtline[63:79])
                    CurrentSigma =  float(data_3rdline[63:79])


                    SatMain_AdjustedParams[check_iter]['0GA_t2'][text_param_adjusts] = {'APRIORI_VALUE': apriorival,
                                                         'PREVIOUS_VALUE': prevval,
                                                         'CURRENT_VALUE': currentval,
                                                          'TOTAL_DELTA':totalDelta,
                                                         'CURRENT_DELTA': currentDelta,
                                                         'APRIORI_SIGMA': AprioriSigma,
                                                         'CURRENT_SIGMA': CurrentSigma }
                    i+=1
                    i = np.mod(i,np.shape(lines_params)[0])

                else:
                    print('Oops! Grabbing data for the wrong satellite: ', str(check_sat))
                    continue
    else:
        pass
#     print('Done')
    # print('Done saving the following parameters: \n ', np.squeeze(text_cd_list))
    return(SatMain_AdjustedParams)





def read_ascixyz(traj_file, YR):

    '''
    This function reads the ascii XYZ trajectory data.

    The ascii XYZ card SHOULD only be giving the FINAL iteration 
    of the GEODYN RUN.

    In a GPS run, there are many satellites being tracked. 
    This function returns a dataframe with time series of the trajectory 
    data with the satellite as one of the columns.
    '''
    #
    # determine where the first data header is (want to skip the file header)
    with open(traj_file, 'r') as f:
        for line_no, line in enumerate(f):
            if 'SPACECRAFT EPHEMERIS FOR SATELLITE' in line:
                first_header_line = line_no
                break
    #
    # read the total number of lines
    line_total = len(open(traj_file).readlines())
    #
    # initialize some lists for iteratively storing
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
    #
    # loops through all lines in the file from the first 
    # data header to the last file line.
    for i,val in enumerate(np.arange(first_header_line+1,line_total+1)):
        line = linecache.getline(traj_file,val)
        #
        # if the line contains the below text, the satellite is changing
        # save the new sat ID.
        if 'SPACECRAFT EPHEMERIS FOR SATELLITE' in line:
            sat_id = line[46:54]
        #
        # the following keywords denote a header in the file and these lines should be skipped
        if "SPACECRAFT" in line:
            continue
        elif "YYMMDD" in line:
            continue
        elif "GREENWICH" in line:
            continue
        elif "SUNLIGHT" in line:
            continue
    #     elif "" in line:
    #         continue
    #         elif "YYMMDD" in line:
    #             continue
        #
        # If the line is NOT a header, save the data out.
        # Note that at this point in the loop we are saving the 
        # data for the above Sat ID.  When we get to a line 
        # with the 'SPACECRAFT EPHEMERIS FOR SATELLITE' text,
        # the satellite ID will change and the data we save 
        # out will be for that new satellite.
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

                # store in a list to catch 'em all
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
    #
    # Save as a dataframe
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
    break
    date = make_datetime_column(traj_xyz_df, YR)
    traj_xyz_df['Date'] = date


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



    return(traj_xyz_df)








def read_density_file_ISS(densityfil):


    DEN_csv = pd.read_csv(densityfil, 
                        skiprows = 1, 
                        names = ['Elapsed Secs',
                                'YYMMDD',
                                'HHMMSS',
                                'Lat',
                                'Lon',
                                'Height (meters)',
                                'rho (kg/m**3)',
                                'delta_rho (kg/m**3/m)',
                                'X',
                                'Y',
                                'Z',
                                'XDOT',
                                'YDOT',
                                'ZDOT',
                              ],
                        sep = '\s+',
                        )

    #The below takes the Double precision format from Fortran (D) and puts an 
    #E in the string to be interpeted as a float by python
    DEN_df = pd.DataFrame(DEN_csv)
    fix_D_decimal_to_E = []
    fix_D_decimal_to_E2 = []

    for i,val in enumerate(DEN_df['rho (kg/m**3)']):
        val2 = DEN_df['delta_rho (kg/m**3/m)'][i]

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
    DEN_df['delta_rho (kg/m**3/m)'] = fix_D_decimal_to_E2

    # Now we must correct the formatting of the HoursMinutesSeconds column
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

    # Here we split up the years, months, and days.
    # we add a '20' in front of the year

    year  = np.zeros(np.size(DEN_df['YYMMDD'].values))
    month = np.zeros(np.size(DEN_df['YYMMDD'].values))
    day   = np.zeros(np.size(DEN_df['YYMMDD'].values))
    hours  = np.zeros(np.size(DEN_df['YYMMDD'].values))
    minutes = np.zeros(np.size(DEN_df['YYMMDD'].values))
    secs  = np.zeros(np.size(DEN_df['YYMMDD'].values))
    start2 = time.time()
    total_num = (np.size(DEN_df['YYMMDD']))

    i = 0
    for index, row in DEN_df.iterrows():
        # print()
        val = DEN_df['YYMMDD'][index].astype(int).astype(str)
        year[i] = ('20' + val[:2])
        month[i] = (val[2:4])
        day[i] = (val[4:])
        hours[i] = str(DEN_df['timeHHMMSS'][index])[:2]
        minutes[i] = str(DEN_df['timeHHMMSS'][index])[2:4]
        secs[i] = str(DEN_df['timeHHMMSS'][index])[4:]
    #     print('year', year[i])
    #     print('month', month[i])
    #     print('day', day[i] )
    #     print('hours', hours[i] )
    #     print('minu',  minutes[i])
    #     print('sec', secs[i])
        i += 1
    # end2 = time.time()
    # elapsed2 = end2 - start2
    # print(i,'/',total_num ,' Elapsed: ',elapsed2     ,   sep ='')

    DEN_df['year']  = year
    DEN_df['month'] = month
    DEN_df['day']   = day
    DEN_df['hours']  = hours
    DEN_df['minutes'] = minutes
    DEN_df['secs']  = secs


    year= list(map(int, DEN_df['year'].values))
    month= list(map(int, DEN_df['month'].values))
    day= list(map(int, DEN_df['day'].values))
    hour= list(map(int, DEN_df['hours'].values))
    minute = list(map(int, DEN_df['minutes'].values))
    second = list(map(int, DEN_df['secs'].values))

    DATE = list(map(datetime, year,month, day, hour,minute,second ))

    DEN_df['Date']  = DATE



    return(DEN_df)



def iteration_number(iieout_file):
    '''
    This function opens the iieout file, and returns the final iteration number
    '''
    with open(iieout_file, 'r') as f:
        for line_no, line in enumerate(f):
            if 'CONVERGENCE' in line:
                line_text = line
                # print(line)
    num_iters = float(line_text[39:42])-1
    return num_iters

def read_observed_resids_gps(iieout_file, Year, VERBOSE_timer = False):
    '''
    This function reads in the residuals from the massive IIEOUT file.


    For residuals, there are specific station-satellite configurations.  
    It is prudent to read in each configuration and save which satellites make it up.  
    This is much harder to do than simply reading in all resuiduals (as I did before)


    '''





    if VERBOSE_timer == True:
        import time
        start = time.time()
    else:
        pass

    #############################
    # - BEGIN RESIDUAL READER -
    #############################


    # How many iterations are in this run?
    iteration = str(int(iteration_number(iieout_file)))
    VERBOSE_timer = True
    #
    # ----------------------------------------------------------------------------------
    #
    # We first find how many observations there are. 
    # We use the observation number to find where to STOP reading in data. 
    # We find this by finding the Summary by Measurment Header
    text_smry_meas = 'RESIDUAL SUMMARY BY MEASUREMENT TYPE FOR ARC  1 INNER ITERATION  '+ (iteration) +' OF GLOBAL ITERATION 1'
    #
    # Loop through the iieout file and find where the above header exists (it should only show up once)
    # For ISS-GPS runs, there are four TYPES of residuals.  We would like to read these in automatically
    #        PCE X
    #        PCE X
    #        PCE X
    #        DSS1WRNG 
    #
    with open(iieout_file, 'r') as f:
        for line_no, line_text in enumerate(f):
            if text_smry_meas in line_text:
                # Save the exact number where this section (based on the header) appears:
                text_smry_meas_line_no = line_no
    #
    # Save out the Observation residual types:
    # I read in the whole section and stop reading in when the first value of the line is no longer 0
    count_lines = 1
    line = linecache.getline(iieout_file,text_smry_meas_line_no+count_lines)
    if int(line[0]) == 1: 
        is_integer = True
    else:
        print('Started out as wrong value in SUMMRY of MEASURMENTS')
        is_integer = False

    while is_integer == True:
        try:
            int(line[0])
    #         print('Its an INTEGER')
            line = linecache.getline(iieout_file,text_smry_meas_line_no+count_lines)
            count_lines += 1
            is_integer = True
        except:
    #         print(line[0])
    #         print('not an INTEGER')
            is_integer = False
            count_lines -= 3
    # print(count_lines)
    # print(linecache.getline(iieout_file,text_smry_meas_line_no+count_lines))
    resid_meas_summry = pd.read_csv(iieout_file, 
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
    for i,val in resid_meas_summry.iterrows():
        try:
            float(val['TYPE2'])
            type_fixed.append(val['TYPE1'])
            pass
        except:
            type_fixed.append(val['TYPE1'] + val['TYPE2'])
    resid_meas_summry['TYPE'] = type_fixed          

    #
    # ----------------------------------------------------------------------------------
    #
    '''
    Now find all the instances of the OBSERVATION RESIDUALS 
    header at the last iteration.  

    We will want to store these into a dictionary and save out:
        - configuration type
        - contributing satellites
        - all residual data
        
    We first grab the start line and end line of the whole observation residual section
    next we loop through each of line in this range and save data if the block number is a     float.
    There are weird headers that have to be accounted for.

    '''
    text_obs_resid = 'OBSERVATION RESIDUALS FOR ARC  1 FOR INNER ITERATION  '+ (iteration)
    end_of_section = 'RESIDUAL SUMMARY BY STATION AND TYPE FOR ARC  1 INNER ITERATION  '+ (iteration)
    lines_list_1 = [] 
    lines_list_2 = []

    # The below grabs the line numbers of the section headers 
    # The Observation Residuals end at the first instance of the Summary by Station
    with open(iieout_file, 'r') as f:
        for line_no, line in enumerate(f):
            if text_obs_resid in line:
                lines_list_1.append(line_no)
            elif end_of_section in line:
                lines_list_2.append(line_no)

    residual_range  = np.arange(lines_list_1[0], lines_list_2[0]+1)
    # block_num_counter = 0

    list_config_type  = []
    list_SAT_main     = []
    list_note         = []
    list_SAT_1        = []
    list_SAT_2        = []
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

    for i,val in enumerate(residual_range):
        line = linecache.getline(iieout_file,val)
        if 'STATION-SATELLITE CONFIGURATION' in line:
    #         print('HEADER Type 1')
            config_type = line[35:44]
            SAT_main = line[54:62]
            SAT_1 = line[72:80]
            SAT_2 = line[90:98]
            note = np.nan
            # print(Stat_Sat_Config[35:43])
            # print(Stat_Sat_Config[54:62])
            # print(Stat_Sat_Config[72:80])
            # print(Stat_Sat_Config[90:98])
        elif 'STATION-SAT CONFIG.' in line:
            if 'DSS1WRNG' in line:
    #             print('HEADER Type 2-1')
                config_type = line[46:56]
                SAT_main = line[65:73]
                note = np.nan
                SAT_1 = line[83:91]
                SAT_2 = line[100:109]
            else:
    #             print('HEADER Type 2-2')
                config_type = line[46:56]
                SAT_main = np.nan
                note = line[55:63]
                SAT_1 = line[65:74]
                SAT_2 = np.nan

        try:
            BLOCK_no = int(line[117:125])
            YYMMDD       = line[1:8]
            HHMM         = line[8:13]
            SEC_UTC      = line[13:23]
            Observation  = line[26:43]
            Residual     = line[46:58]
            RatiotoSigma = line[60:70]
            Elev1        = line[71:84]
            Elev2        = line[85:96]
            OBS_No       = line[106:117]
            Block        = line[117:125]

            list_config_type.append(config_type)
            list_SAT_main.append(SAT_main)
            list_note.append(note)
            list_SAT_1.append(SAT_1)
            list_SAT_2.append(SAT_2)
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

    resids_dict= {'StatSatConfig' : list_config_type,
                  'Sat_main'      : list_SAT_main   ,
                  'SAT_1'         : list_SAT_1      ,
                  'SAT_2'         : list_SAT_2      ,
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
    # Fix the date column
    # Year = 19
    dates = make_datetime_column(resids_df, Year)
    resids_df['Date'] = dates

    # The ratio-to-sigma columns has some weird strings in it
    #        ValueError: could not convert string to float: ' -16.0620*'
    #        remove them
    fix_string = []
    for i,val in enumerate(resids_df['RatiotoSigma']):
        try:
            float(val)
            fix_string.append(val)
        except:
            # print(i, val)
            fix_string.append(val[:-1])

    resids_df['RatiotoSigma'] = fix_string
#   
# Some of the elevations are empty.  Replace the empty strings with nans
#
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
        
    resids_df['Observation']  = resids_df['Observation'].astype(float)
    resids_df['Residual']     = resids_df['Residual'].astype(float)
    resids_df['RatiotoSigma'] = resids_df['RatiotoSigma'].astype(float)


    return(resids_df)






###############################
######  CODE GRAVEYARD: #######
###############################

# def Save_AdjustedParameters_ISS(Sat_main, iieout_file):
#   '''
#   The below code grabs the estimated adjusted parameters for the MAIN satellite for
#   the each iteration.
#   The data is stored in a dictionary and includes the associated statistics:
#           APRIORI  VALUE
#           PREVIOUS VALUE              
#           CURRENT  VALUE              
#           TOTAL DELTA
#           CURRENT DELTA
#           APRIORI SIGMA
#           CURRENT SIGMA
#   The dictionary is multi-dimensional, with the first dimension being iteration 
#   number and second dimension being a dictionary that contains the above data.
#   '''


#   # Tracking_CD = ["T%02d" %i for i in np.arange(1,43)]
#   # find the line numbers of all the adjustment parameters in the too big iieout file
#   text_param_lists = ['0XPOS',
#                       '0YPOS',
#                       '0ZPOS',
#                       '0XVEL',
#                       '0YVEL',
#                       '0ZVEL',]


#   # Identify the lines where we have changed parameters and identify how many 
#   # iteration numbers there are.
#   lines_params = [] 
#   with open(iieout_file, 'r') as f:
#     for line_no, line_text in enumerate(f):
#         if '0XPOS' in line_text :
#           lines_params.append(line_no)

#   # Build the dictionary to be index based on iteration number   
#     # and initialize each iteration number to also be a dictionary
#   lines = []
#   SatMain_AdjustedParams = {}
#   for i,val in enumerate(np.arange(1, np.size(lines_params)+1)):
#     SatMain_AdjustedParams[val] = {}
#     SatMain_AdjustedParams[val]['0CD'] = {}





#   # Search thru the full file for the key words in the above list
#     # save the line numbers where those keywords occur
#   for text_param_adjusts in text_param_lists: 
#     with open(iieout_file, 'r') as f:
#       for line_no, line_text in enumerate(f):
#           if text_param_adjusts in line_text:
#             lines.append(line_no) 
    
#     # Loop thru the lines saved above and grab the data occording to its name 
#     # and location in the file
#     Sat_main_lines = [] 
#     i=0
#     for il,val_lines in enumerate(lines):
#       check_sat = int(linecache.getline(iieout_file,val_lines+1)[10:18])
#       check_iter = int((linecache.getline(iieout_file,lines_params[i]-3))[57:60])
#       if check_sat == Sat_main:
#         # print(check_sat)
#         # print(check_iter)
#         data_1stline = linecache.getline(iieout_file,val_lines+1) #
#         data_2ndtline = linecache.getline(iieout_file,val_lines+2) #
#         data_3rdline = linecache.getline(iieout_file,val_lines+3) #

#         apriorival = float(data_1stline[19:38])
#         prevval = float(data_2ndtline[19:38])
#         currentval  = float(data_3rdline[19:38])
#         totalDelta = float(data_2ndtline[42:62])
#         currentDelta =  float(data_3rdline[42:62])
#         AprioriSigma = float(data_2ndtline[63:78])
#         CurrentSigma =  float(data_3rdline[63:78])
        
#         SatMain_AdjustedParams[check_iter][text_param_adjusts] = [apriorival,
#                                                       prevval,
#                                                       currentval,
#                                                       totalDelta,
#                                                       currentDelta,
#                                                       AprioriSigma,
#                                                       CurrentSigma ]
#         i+=1
#         i = np.mod(i,np.shape(lines_params)[0])
        
#       else:
#         print('Oops! Grabbing data for the wrong satellite: ', str(check_sat))
#         continue
#   # print('Done saving the following parameters: \n', text_param_lists)
      


#   text_cd_list = ["0CD   T%02d" %i for i in np.arange(1,43)]

#   for text_param_adjusts in text_cd_list: 
#     with open(iieout_file, 'r') as f:
#       for line_no, line_text in enumerate(f):
#           if text_param_adjusts in line_text:
#             lines.append(line_no)
      
    
#     # Loop thru the lines saved above and grab the data occording to its name 
#     # and location in the file
#     Sat_main_lines = [] 
#     i=0
#     for il,val_lines in enumerate(lines):
#       check_sat = int(linecache.getline(iieout_file,val_lines+1)[10:18])
#       check_iter = int((linecache.getline(iieout_file,lines_params[i]-3))[57:60])
#       if check_sat == Sat_main:
#         # print(check_sat)
#         # print(check_iter)
#         data_1stline = linecache.getline(iieout_file,val_lines+1) #
#         data_2ndtline = linecache.getline(iieout_file,val_lines+2) #
#         data_3rdline = linecache.getline(iieout_file,val_lines+3) #

#         apriorival = float(data_1stline[19:38])
#         prevval = float(data_2ndtline[19:38])
#         currentval  = float(data_3rdline[19:38])
#         totalDelta = float(data_2ndtline[42:62])
#         currentDelta =  float(data_3rdline[42:62])
#         AprioriSigma = float(data_2ndtline[63:78])
#         CurrentSigma =  float(data_3rdline[63:78])
        
#         SatMain_AdjustedParams[check_iter]['0CD'][text_param_adjusts[-3:]] = [apriorival,
#                                                       prevval,
#                                                       currentval,
#                                                       totalDelta,
#                                                       currentDelta,
#                                                       AprioriSigma,
#                                                       CurrentSigma ]
#         i+=1
#         i = np.mod(i,np.shape(lines_params)[0])
        
#       else:
#         print('Oops! Grabbing data for the wrong satellite: ', str(check_sat))
#         continue
#   # print('Done saving the following parameters: \n ', np.squeeze(text_cd_list))
#   return(SatMain_AdjustedParams)








