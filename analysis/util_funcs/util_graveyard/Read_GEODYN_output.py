# from netCDF4 import Dataset
import numpy as np 
import pandas as pd
# import os
import datetime

def my_function():
    print("Hello World")



class iieout_read:
    '''
    This class reads the iieout data and returns information based on user input.
    '''
    def __init__(self, iieout_file):
      self.iieout_file = iieout_file
      self.text_iternumber = 'CONVERGENCE'
      self.text_find_sats = "STATION-SATELLITE CONFIGURATION  DSS1WRNG           9806701"




    def find_satIDs(self):
      allsats = []
      with open(self.iieout_file, 'r') as f:

        for line_no, line in enumerate(f):
            if self.text_find_sats in line:
              allsats.append(int(line[90:100]))

      SatIDs = []
      for sat in allsats:
          if sat not in SatIDs:
              SatIDs.append(sat)
      return SatIDs




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
          print(val_sat) 
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
      for iline in SatID_dict[isat]:
      
        with open(self.ascii_xyz_file, 'r') as f: 
          for _ in range(iline): 
              f.readline() 
          line = f.readline() 
          if int(line[0]) == 0:

            ephems_csv = pd.read_csv(self.ascii_xyz_file, 
                                    skiprows = iline+1,
                                    nrows =  3,
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
          elif int(line[0]) == 1:

            ephems_csv = pd.read_csv(self.ascii_xyz_file, 
                                    skiprows = iline+3,
                                    nrows =  3,
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

        A = pd.DataFrame(ephems_csv)
        B = pd.concat([ B, A])

      index_list = []
      for index, row in B.iterrows():
          try:
              float(row['HHMM'])
          except:
              index_list.append(index)
              continue

      C=B.drop(index_list)

      data_dict[isat] = C
      # print(C)
      date_isat = read_ascii_xyz.make_datetime_column(data_dict[isat], VERBOSE_timer=True) 
      data_dict[isat]['Date'] = date_isat

      return data_dict

    def make_datetime_column(isat_data, VERBOSE_timer):
        # isat_data = data_dict[isat] 
        # VERBOSE_timer=True


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
            year.append('20' + val[:2])
            month.append(val[2:4])
            day.append(val[4:])
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

        DATE = list(map(datetime.datetime, year,month, day, hour,minute,second ))

        if VERBOSE_timer == True:
          end = time.time()
          elapsed = end - start
          print("Put all dates in a single column:",elapsed)
        else:
          pass
        return(DATE)




class read_residuals_iieout:
    '''
    This class reads the iieout data and returns the observation residuals.
    '''
    def __init__(self, iieout_file, VERBOSE_timer):
      self.iieout_file = iieout_file
      self.VERBOSE_timer =VERBOSE_timer

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

# find the satellites in the GEODYN Run:

    def find_Sat_IDs_resids(self):
        text="STATION-SATELLITE CONFIGURATION  DSS1WRNG           9806701"
        allsats = []
        with open(self.iieout_file, 'r') as f:

          for line_no, line in enumerate(f):
              if text in line:
                # print(line[90:100])
                allsats.append(int(line[70:81]) )
                # print(line)
                
          #         line_no_1 =lines_list 

        SatIDs = []
        for sat in allsats:
            if sat not in SatIDs:
                SatIDs.append(sat)
        iteration = read_residuals_iieout.iteration_number(self)
        text_obs_resid = 'OBSERVATION RESIDUALS FOR ARC  1 FOR INNER ITERATION  '+ str(int(iteration))

        lines_list = [] #np.empty(np.size(num_observations))
        with open(self.iieout_file, 'r') as f:

            for line_no, line in enumerate(f):
                if text_obs_resid in line:
                        # print(line_no)
                    lines_list.append(line_no)
        #         line_no_1 =lines_list 

        # lines = search_iiesout_all_line_numbers(iieout_file, text)
        line_no_1 = lines_list[0]
        line_no_2 = lines_list[-1]

        # find the satellites in the GEODYN Run:

        text="STATION-SATELLITE CONFIGURATION  DSS1WRNG           9806701"
        allsats = []
        with open(self.iieout_file, 'r') as f:

          for line_no, line in enumerate(f):
            if text in line:
              # print(line[90:100])
              allsats.append(int(line[70:81]) )
              # print(line)
            
        #         line_no_1 =lines_list 

        SatIDs = []
        for sat in allsats:
          if sat not in SatIDs:
              SatIDs.append(sat)

        return SatIDs


    def make_datetime_column(resid_df, self):
        if self.VERBOSE_timer == True:
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

        np.shape(timeHHMM)
        resid_df['timeHHMM'] = timeHHMM

        year  = []
        month = []
        day   = []
        hours  = []
        minutes = []
        secs  = []
        microsecs = []
        for i,val in enumerate(resid_df['YYMMDD']):
          year.append('20' + val[:2])
          month.append(val[2:4])
          day.append(val[4:])

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
        if self.VERBOSE_timer == True:
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

        if self.VERBOSE_timer == True:
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

        DATE = list(map(datetime.datetime, year,month, day, hour,minute,second,microsecond ))

        if self.VERBOSE_timer == True:
          end = time.time()
          elapsed = end - start
          print("Put all dates in a single column:",elapsed)
        else:
          pass


        return(DATE)

    def read_observed_resids_by_sat(self ):

        # def read_obs_residuals(iieout_file, iteration, VERBOSE_timer):
        iteration = str(int(read_residuals_iieout.iteration_number(self)))
        # VERBOSE_timer = True


        if self.VERBOSE_timer == True:
          import time
          start = time.time()
        else:
          pass

        #-------------------------------------------------------------------------------

        #-------------------------------------------------------------------------------
        #-------------------------------------------------------------------------------
        '''
          Now find all the instances of the OBSERVATION RESIDUALS 
          header at this iteration.  These are stored in a list.
        '''

        text_obs_resid = 'OBSERVATION RESIDUALS FOR ARC  1 FOR INNER ITERATION  '+ str(int(iteration))
        SatIDs = read_residuals_iieout.find_Sat_IDs_resids(self)

        lines_list = [] #np.empty(np.size(num_observations))
        with open(self.iieout_file, 'r') as f:

            for line_no, line in enumerate(f):
                if text_obs_resid in line:
        #                 print(line_no)
                    lines_list.append(line_no)
        #         line_no_1 =lines_list 

        import time
        start = time.time()

        init_df = pd.DataFrame(data={'YYMMDD'   :[],
                                'HHMM'    :[],
                                'Sec-UTC-R' :[],
                                'Observation'       :[],
                                'Residual'       :[],
                                'Ratio to sigma'       :[],
                                'Elev1'    :[],
                                'Elev2'    :[],
                                'OBS No.'    :[],
                                'Block'     :[],})

        dict_sat = {}
        for i in SatIDs:
          dict_sat[i]= init_df



        for i,iline in enumerate(lines_list):

          with open(self.iieout_file, 'r') as f: 
            for _ in range(iline+1): 
                f.readline() 
            line = f.readline()
            sat_line = int(line[70:81])   #int(line[90:100])
            print(line)
            print(sat_line)

          RESID_OBSERV = pd.read_csv(self.iieout_file, 
                                              skiprows = lines_list[0] + 6 , 
                                              nrows =  int((lines_list[0 + 1]-6 - lines_list[0]-7) ),
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
          
          A = pd.DataFrame(RESID_OBSERV)
          B = dict_sat[sat_line]
          
          dict_sat[sat_line] =  pd.concat([ B, A])
          end = time.time()
          elapsed = end - start
          print("Elapsed time:",elapsed)

          print(i ,'/', str(len(lines_list)))
          return RESID_OBSERV


    def read_observed_resids_all(self):

      iteration = str(int(read_residuals_iieout.iteration_number(self)))

      if self.VERBOSE_timer == True:
        import time
        start = time.time()
      else:
        pass

      #-------------------------------------------------------------------------------
      '''
        First we need to find how many observations there are:
      '''
      text_smry_meas = 'RESIDUAL SUMMARY BY MEASUREMENT TYPE FOR ARC  1 INNER ITERATION  '+ (iteration) +' OF GLOBAL ITERATION 1'
      with open(self.iieout_file, 'r') as f:
          for line_no, line in enumerate(f):
              if text_smry_meas in line:
      #                 print(line_no)
      #                 lines_list= (line_no)
                  RESID_OBSERV = pd.read_csv(self.iieout_file, 
                                          skiprows = line_no+2 ,  # to 53917
                                            nrows =  4,
                                names = ['num','NUMBER', 'MEAN','RMS','No.WTD', 'wtd-mean','wtd-rms','Type'],
                                sep = '\s+',
                                )
      num_observations = np.float(RESID_OBSERV.num.sum())
      text_obs_resid = 'OBSERVATION RESIDUALS FOR ARC  1 FOR INNER ITERATION  '+ (iteration)
      num_observations = num_observations
      #-------------------------------------------------------------------------------
      #-------------------------------------------------------------------------------
      '''
        Now find all the instances of the OBSERVATION RESIDUALS 
        header at this iteration.  These are stored in a list.
      '''
      lines_list = [] #np.empty(np.size(num_observations))
      with open(self.iieout_file, 'r') as f:

          for line_no, line in enumerate(f):
              if text_obs_resid in line:
      #                 print(line_no)
                  lines_list.append(line_no)
      #         line_no_1 =lines_list 

      # lines = search_iiesout_all_line_numbers(iieout_file, text)
      line_no_1 = lines_list[0]
      line_no_2 = lines_list[-1]
      #-------------------------------------------------------------------------------
      #-------------------------------------------------------------------------------
      '''
        Use the first and last line numbers from above to select
        the sections of data that contains the observation residuals.
        The outputted csv data are stored as A
      '''
      RESID_OBSERV = pd.read_csv(self.iieout_file, 
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

      if self.VERBOSE_timer == True:
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
      if self.VERBOSE_timer == True:
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
      index_drops = np.arange(last_index, np.shape(C["OBS No."]))
      index_drops

      D = C.drop(index_drops)
      if self.VERBOSE_timer == True:
        end = time.time()
        elapsed = end - start
        print("Elapsed time after dropping all bad indicies after last obs no.:",elapsed)

      dates = read_residuals_iieout.make_datetime_column(D, self)
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

      return(D)
  





