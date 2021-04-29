import numpy as np
import pandas as pd
from datetime import datetime,timedelta
import os.path
import linecache

# import sys
# sys.path.append('/data/analysis/util_funcs/util_common/')
# from datetime_column import make_datetime_column



def Save_AdjustedParameters_starlette(Sat_main, iieout_file, AccelStatus = True):
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




class read_ascii_xyz:
    '''
    This class reads the ascii_xyz data and returns information based on user input.
    '''


    def __init__(self, ascii_xyz_file, iieout_file, choose_sat):
      self.ascii_xyz_file = ascii_xyz_file
      self.iieout_file = iieout_file
      # self.text_find_sats = "ARC  1 FOR INNER ITERATION  6 OF GLOBAL ITERATION 1"
      self.choose_sat = choose_sat



    def iteration_number(self):
      '''
      This function opens the iieout file, and returns the final iteration number
      '''
      with open(self.iieout_file, 'r') as f:
        for line_no, line in enumerate(f):
            if 'CONVERGENCE' in line:
              line_text = line
              # print(line)
        num_iters = float(line_text[39:42])-1
        return num_iters


    def find_satIDs(self):
      '''
      This function loops through the ascii_xyz file and returns the satellite ID
      numbers by identifying all the unique satellite IDs
      '''
      numiters = read_ascii_xyz.iteration_number(self)
      text_find_sats = "ARC  1 FOR INNER ITERATION  %d OF GLOBAL ITERATION 1" % int(numiters)
      allsats = []
      with open(self.ascii_xyz_file, 'r') as f:

        for line_no, line in enumerate(f):
            if text_find_sats in line:
              # print(line[90:100])
              allsats.append((int(line[45:54])))
        #         line_no_1 =lines_list 

      SatIDs = []
      for sat in allsats:
          if sat not in SatIDs:
              SatIDs.append(sat)
      return SatIDs

    def get_single_sat_data(self):
        '''
        This function loops through only the final iteration of the axi_xyz file, 
        and returns a dictionary that contains all the data for one single satellite.
        In this function it is a satellite chosen by the user.

        Eventually this should be update to return info for ALL satellites.
        '''
        # First need to construct a dictionary that has all the line numbers where each 
        # satellite appears:

        
        numiters = read_ascii_xyz.iteration_number(self)
        SatIDs_ascii = read_ascii_xyz.find_satIDs(self)
        SatID_dict = {}
        iteration = str(int(numiters))
        for val_sat in SatIDs_ascii:
            # print(val_sat) 
            lines = []
            text = str(val_sat) + " OF ARC  1 FOR INNER ITERATION  "+ iteration
            with open(self.ascii_xyz_file, 'r') as f:
                for line_no, line in enumerate(f):
                    if text in line:
                        lines.append(line_no)


                SatID_dict[val_sat] = lines


        #  Next, we need to loop through and grab the data.
        #  Because of the weird formatting, we search for the satellite header.  
        #  If the header line starts with 1 the next 3 lines are headers and we skip them
        #  If the header line starts with 0, the next line has the data

        data_dict = {}
        isat = self.choose_sat
        iii = 0

        # for iii, isat in enumerate(SatID_dict):

        B = pd.DataFrame(data={'YYMMDD'   :[], # DATE  GREENWICH TIME
                              'HHMM'    :[],
                              'SECONDS' :[],
                              'X'       :[], # INERTIAL CARTESIAN COORDINATES
                              'Y'       :[],
                              'Z'       :[],
                              'XDOT'    :[], # INERTIAL CARTESIAN VELOCITY
                              'YDOT'    :[],
                              'ZDOT'    :[],
                              'LAT'     :[], # GEODETIC EAST SPHEROID
                              'LONG'    :[],
                              'HEIGHT'  :[]})

        # print(SatID_dict)
        # for iline in SatID_dict[isat]:

        with open(self.ascii_xyz_file, 'r') as f: 
        #     for _ in range(iline): 
        #         f.readline() 
        #     line = f.readline() 

                ephems_csv = pd.read_csv(self.ascii_xyz_file, 
                                    skiprows = SatID_dict[isat][0]+1,
        #                             nrows =  SatID_dict[isat][-1]+,
                                    names = ['YYMMDD',
                                            'HHMM',
                                            'SECONDS',
                                            'X',
                                            'Y',
                                            'Z',
                                            'XDOT',
                                            'YDOT',
                                            'ZDOT',
                                            'LAT',
                                            'LONG',
                                            'HEIGHT',
                                          ],
                                    sep = '\s+',
                                    )
        #         elif int(line[0]) == 1:

        #             ephems_csv = pd.read_csv(self.ascii_xyz_file, 
        #                                 skiprows = iline+3,
        #                                 nrows =  3,
        #                                 names = ['YYMMDD',
        #                                         'HHMM',
        #                                         'SECONDS',
        #                                         'X',
        #                                         'Y',
        #                                         'Z',
        #                                         'XDOT',
        #                                         'YDOT',
        #                                         'ZDOT',
        #                                         'LAT',
        #                                         'LONG',
        #                                         'HEIGHT',
        #                                       ],
        #                                 sep = '\s+',
        #                                 )

        #     A = pd.DataFrame(ephems_csv)
        #     B = pd.concat([ B, A])
        B = pd.DataFrame(ephems_csv)
        index_list = []
        for index, row in B.iterrows():
            try:
                float(row['SECONDS'])
            except:
                index_list.append(index)
                continue

        C=B.drop(index_list)

        data_dict[isat] = C
        # print(C)
        date_isat = read_ascii_xyz.make_datetime_column(data_dict[isat], VERBOSE_timer=False) 
        data_dict[isat]['Date'] = date_isat

        return data_dict

    def make_datetime_column(isat_data, VERBOSE_timer):

        if VERBOSE_timer == True:
          import time
          start = time.time()
        else:
          pass

        timeHHMM = [] 
        for i,val in enumerate(isat_data['HHMM'].values.astype(int)):
            # print(len(str(val)))
            if len(str(val)) == 3:
                timehhmm_val = '0'+ str(val)
                timeHHMM.append(timehhmm_val)
            if len(str(val)) == 2:
                timehhmm_val = '00'+ str(val)
                timeHHMM.append(timehhmm_val)
            if len(str(val)) == 4:
                timehhmm_val = str(val)
                timeHHMM.append(timehhmm_val)
            if len(str(val)) == 1:
                timehhmm_val = '000'+ str(val)
                timeHHMM.append(timehhmm_val)
        # print(val)  
        # print('1!!!!', np.shape(timeHHMM))

        isat_data['timeHHMM'] = timeHHMM

        year  = []
        month = []
        day   = []
        hours  = []
        minutes = []
        secs  = []
        microsecs = []
        for i,val in enumerate(isat_data['YYMMDD'].values.astype(int).astype(str)):
            # print(val)
            year.append('200' + val[:1])
            # print(year)
            month.append(val[1:3])
            day.append(val[3:])
            # print('HERE',isat_data['timeHHMM'].values.astype(str)[i][:2])
            hours.append(isat_data['timeHHMM'].values.astype(str)[i][:2])
            minutes.append(isat_data['timeHHMM'].values.astype(str)[i][2:4])
            secs.append(isat_data['SECONDS'].values.astype(str)[i][:2])
            # microsecs.append(isat_data['Sec-UTC-R'][i][3:])

        isat_data['year']  = year
        isat_data['month'] = month
        isat_data['day']   = day
        isat_data['hours']  = hours
        isat_data['minutes'] = minutes
        isat_data['secs']  = secs
        # isat_data['microsecs'] = microsecs
        if VERBOSE_timer == True:
          end = time.time()
          elapsed = end - start
          print("Loop through and extract indiv date vals:",elapsed)
        else:
          pass

        fix_decimal = []
        for i,val in enumerate(isat_data['secs'].astype(str)):
        #     print(i,val)
            if val.find('.') == 1:
        #             print(i, val)
                fix_decimal.append( '0'+val[:-1])
        #             print(newval)
            else:
                fix_decimal.append( val)

        if VERBOSE_timer == True:
          end = time.time()
          elapsed = end - start
          print("Fix decimals in the seconds column:",elapsed)
        else:
          pass

        year= list(map(int, isat_data['year'].values))
        month= list(map(int, isat_data['month'].values))
        day= list(map(int, isat_data['day'].values))
        hour= list(map(int, isat_data['hours'].values))
        minute = list(map(int, isat_data['minutes'].values))
        second = list(map(int, fix_decimal))
        DATE = list(map(datetime, year,month, day, hour,minute,second ))

        if VERBOSE_timer == True:
          end = time.time()
          elapsed = end - start
          print("Put all dates in a single column:",elapsed)
        else:
          pass
        return(DATE)











def read_density_file(densityfil):
  
    VERBOSE_timer = True


    if VERBOSE_timer == True:
      import time
      start = time.time()
    else:
      pass
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

    # if VERBOSE_timer == True:
    #   end = time.time()
    #   elapsed = end - start
    #   print("Loaded file into df:",elapsed)
    # else:
    #   pass

    ''' 
    The below takes the Double precision 
    format from Fortran (D) and puts an 
    E in the string to be interpeted as 
    a float by python
    '''
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


    # if VERBOSE_timer == True:
    #   end = time.time()
    #   elapsed = end - start
    #   print("Fixed strings on data:",elapsed)
    #   print("Beginning the construction of the date column.")
    # else:
    #   pass

    # from datetime import datetime,timedelta

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

    year  = np.zeros(np.size(DEN_df['YYMMDD'].values))
    month = np.zeros(np.size(DEN_df['YYMMDD'].values))
    day   = np.zeros(np.size(DEN_df['YYMMDD'].values))
    hours  = np.zeros(np.size(DEN_df['YYMMDD'].values))
    minutes = np.zeros(np.size(DEN_df['YYMMDD'].values))
    secs  = np.zeros(np.size(DEN_df['YYMMDD'].values))
    start2 = time.time()
    total_num = (np.size(DEN_df['YYMMDD']))

    # for i,val in enumerate(DEN_df['YYMMDD'].values.astype(int).astype(str)):
    #     year[i] = ('20' + val[:2])
    #     month[i] = (val[2:4])
    #     day[i] = (val[4:])
    #     hours[i] = (DEN_df['timeHHMMSS'].values.astype(str)[i][:2])
    #     minutes[i] = (DEN_df['timeHHMMSS'].values.astype(str)[i][2:4])
    #     secs[i] = (DEN_df['timeHHMMSS'].values.astype(str)[i][4:])

    i = 0
    for index, row in DEN_df.iterrows():
        # print()
        val = DEN_df['YYMMDD'][index].astype(int).astype(str)
        year[i] = ('200' + val[:1])
        month[i] = (val[1:3])
        day[i] = (val[3:])
        hours[i] = str(DEN_df['timeHHMMSS'][index])[:2]
        minutes[i] = str(DEN_df['timeHHMMSS'][index])[2:4]
        secs[i] = str(DEN_df['timeHHMMSS'][index])[4:]
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

    if VERBOSE_timer == True:
      end = time.time()
      elapsed = end - start
      # print("Done with fixing the date column formatting:",elapsed)
    else:
      pass


    DEN_df['Date']  = DATE



    return(DEN_df)






def make_datetime_column_resids(resid_df):
    VERBOSE_timer = False
    if VERBOSE_timer == True:
        import time
        start = time.time()
    else:
        pass

    timeHHMM = [] 
    for i,val in enumerate(resid_df['HHMM']):
        if len(val) == 3:
            timehhmm_val = '0'+ val
            timeHHMM.append(timehhmm_val)
        if len(val) == 2:
            timehhmm_val = '00'+ val
            timeHHMM.append(timehhmm_val)
        if len(val) == 4:
            timehhmm_val = val
            timeHHMM.append(timehhmm_val)
        if len(val) == 1:
            timehhmm_val = '000'+ val
            timeHHMM.append(timehhmm_val)

#     np.shape(timeHHMM)
    resid_df['timeHHMM'] = timeHHMM

    year  = []
    month = []
    day   = []
    hours  = []
    minutes = []
    secs  = []
    microsecs = []
    for i,val in enumerate(resid_df['YYMMDD']):
#         print(val[:2])
        year.append('200' + val[:1])
        month.append(val[1:3])
        day.append(val[3:])

        hours.append(resid_df['timeHHMM'][i][:2])
        minutes.append(resid_df['timeHHMM'][i][2:4])

        secs.append(resid_df['Sec-UTC-R'][i][:2])
        microsecs.append(resid_df['Sec-UTC-R'][i][3:])

    resid_df['year']  = year
    resid_df['month'] = month
    resid_df['day']   = day
    resid_df['hours']  = hours
    resid_df['minutes'] = minutes
    resid_df['secs']  = secs
    resid_df['microsecs'] = microsecs
    if VERBOSE_timer == True:
        end = time.time()
        elapsed = end - start
        print("Loop through and extract indiv date vals:",elapsed)
    else:
        pass

    fix_decimal = []
    for i,val in enumerate(resid_df['secs'].astype(str)):
    #     print(i,val)
        if val.find('.') == 1:
    #             print(i, val)
            fix_decimal.append( '0'+val[:-1])
    #             print(newval)
        else:
            fix_decimal.append( val)

    if VERBOSE_timer == True:
        end = time.time()
        elapsed = end - start
        print("Fix decimals in the seconds column:",elapsed)
    else:
        pass

    year= list(map(int, resid_df['year'].values))
    month= list(map(int, resid_df['month'].values))
    day= list(map(int, resid_df['day'].values))
    hour= list(map(int, resid_df['hours'].values))
    minute = list(map(int, resid_df['minutes'].values))
    second = list(map(int, fix_decimal))
    microsecond= list(map(int, resid_df['microsecs'].values))

    DATE = list(map(datetime, year,month, day, hour,minute,second,microsecond ))

    if VERBOSE_timer == True:
        end = time.time()
        elapsed = end - start
#         print("Put all dates in a single column:",elapsed)
    else:
        pass


    return(DATE)


def read_observed_resids_all(iieout_file):

    iteration = str(int(iteration_number(iieout_file)))
    VERBOSE_timer = False



    if VERBOSE_timer == True:
        import time
        start = time.time()
    else:
        pass

    #-------------------------------------------------------------------------------
    '''
    First we need to find how many observations there are:
    '''
    text_smry_meas = 'RESIDUAL SUMMARY BY MEASUREMENT TYPE FOR ARC  1 INNER ITERATION  '+ (iteration) +' OF GLOBAL ITERATION 1'
    with open(iieout_file, 'r') as f:
        for line_no, line_text in enumerate(f):
            if text_smry_meas in line_text:
                text_smry_meas_line_no = line_no         
    text_smry_meas_line = int(linecache.getline(iieout_file,text_smry_meas_line_no+4)[54:63])

    num_observations = np.float(text_smry_meas_line)
    text_obs_resid = 'OBSERVATION RESIDUALS FOR ARC  1 FOR INNER ITERATION  '+ (iteration)
    end_of_section = 'RESIDUAL SUMMARY BY STATION AND TYPE FOR ARC  1 INNER ITERATION  '+ (iteration)

    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------
    '''
    Now find all the instances of the OBSERVATION RESIDUALS 
    header at this iteration.  These are stored in a list.
    '''
    lines_list_1 = [] #np.empty(np.size(num_observations))
    lines_list_2 = [] #np.empty(np.size(num_observations))

    with open(iieout_file, 'r') as f:

        for line_no, line in enumerate(f):
            if text_obs_resid in line:
    #                 print(line_no)
                lines_list_1.append(line_no)
    #         line_no_1 =lines_list 
            elif end_of_section in line:
                lines_list_2.append(line_no)


    # lines = search_iiesout_all_line_numbers(iieout_file, text)
    line_no_1 = lines_list_1[0]
    line_no_2 = lines_list_2[-1]
    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------
    '''
    Use the first and last line numbers from above to select
    the sections of data that contains the observation residuals.
    The outputted csv data are stored as A
    '''
    RESID_OBSERV = pd.read_csv(iieout_file, 
                                      skiprows = line_no_1 + 6 , 
                                        nrows =  int((line_no_2 - line_no_1) ),
                            names = ['YYMMDD',
                                    'HHMM',
                                    'Sec-UTC-R',
                                    'Observation',
                                      'Residual',
                                    'Ratio to sigma',
                                    'Elev1',
                                    'Elev2',
                                    'OBS No.',
                                    'Block'],
                            sep = '\s+',
                            )

    if VERBOSE_timer == True:
        end = time.time()
        elapsed = end - start
        print("Elapsed time after line search setup:",elapsed)
    else:
        pass
    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------
    ''' 
      We now need to fix the data that contains a lot of misplaced
      characters, random strings, and headers
    '''
    A = pd.DataFrame(RESID_OBSERV)  # input
    index_list = []
    for index, row in A.iterrows():
        try:
            float(row['OBS No.'])
            float(row['HHMM'])
        except:
            index_list.append(index)
            continue

    B=A.drop(index_list)
    if VERBOSE_timer == True:
        end = time.time()
        elapsed = end - start
        print("Elapsed time after loop through then drop indecies:",elapsed)
    else:
        pass

    #-------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------
    ''' 
      We locate the index of the last observation number and remove 
      all datapoints that are after it in the DataFrame
    '''
    C = B.reset_index()
    index_drops = np.arange(C[C['OBS No.']==str(int(num_observations))].index[0]+1,  C.index.stop)
    index_drops

    D = C.drop(index_drops)
    if VERBOSE_timer == True:
        end = time.time()
        elapsed = end - start
        print("Elapsed time after dropping all bad indicies after last obs no.:",elapsed)

    dates = make_datetime_column_resids(D)
    D['Date'] = dates

    fix_string = []
    for i,val in enumerate(D['Ratio to sigma']):
        try:
            float(val)
            fix_string.append(val)
        except:
            # print(i, val)
            fix_string.append(val[:-1])

    D['Ratio_to_sigma_fixed'] = fix_string
    return D





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






###############################
######  CODE GRAVEYARD: #######
###############################

# def Save_AdjustedParameters_starlette(Sat_main, iieout_file):
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








