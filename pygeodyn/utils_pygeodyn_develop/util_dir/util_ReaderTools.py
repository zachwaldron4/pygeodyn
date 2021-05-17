#### ----------------------------------------
#### Import modules:
#### -----------------
import numpy as np
import pandas as pd
    #### Computer function
import os
import os.path
import sys
import subprocess
import shutil
import time
    #### modules for reading and converting data
import linecache
from datetime import datetime,timedelta
import copy


#### ----------------------------------------
#### Plotting modules:
#### -----------------
import plotly.graph_objects as go
from plotly.offline import plot, iplot
from plotly.subplots import make_subplots
import plotly.express as px
#### ----------------------------------------
#### ----------------------------------------
#### ----------------------------------------






class UtilReader_Tools:

    '''
    The MakeDatetime class will be inherited by all classes in the primary GEODYN reader
    '''
    def __init__(self):  # read in the input and store as members
        pass
    

    def make_datetime_column(self, df, YR):
       
        YR = int(str(YR)[-2:])

        VERBOSE_timer = False
        if VERBOSE_timer == True:
            start = time.time()
        else:
            pass


        df['YYMMDD'] = df['YYMMDD'].str.strip()
        df['HHMM']   = df['HHMM'].str.strip()


        timeHHMM = [] 
        for i,val in enumerate(df['HHMM']):
        #         print(len(val))
            if len(val) == 3:
                timehhmm_val = '0'+ val
                timeHHMM.append(timehhmm_val)
            elif len(val) == 2:
                timehhmm_val = '00'+ val
                timeHHMM.append(timehhmm_val)
            elif len(val) == 1:
                timehhmm_val = '000'+ val
                timeHHMM.append(timehhmm_val)
            elif len(val) == 4:
                timehhmm_val = val
                timeHHMM.append(timehhmm_val)
            elif len(val) == 0:
        #             timehhmm_val = val
        #             timeHHMM.append(timehhmm_val)
                pass

        df['timeHHMM'] = timeHHMM

        YYMMDD_list   = df['YYMMDD'].astype(int).astype(str)
        timeHHMM_list = df['timeHHMM'].astype(str)
        SEC_UTC_list  = df['SEC_UTC'].astype(str)

        if YR < 10:
            year    = ['200' + x[:1]  for x in YYMMDD_list]
            month   = [        x[1:3] for x in YYMMDD_list]
            day     = [        x[3:]  for x in YYMMDD_list]
            hours   = [        x[:2]  for x in timeHHMM_list]
            minutes = [        x[2:4]  for x in timeHHMM_list]
            secs    = [        x[:2] for x in SEC_UTC_list]
            millsecs= [        x[3:]  for x in SEC_UTC_list]
        else:
            year    = ['20' + x[:2]  for x in YYMMDD_list]
            month   = [        x[2:4] for x in YYMMDD_list]
            day     = [        x[4:]  for x in YYMMDD_list]
            hours   = [        x[:2]  for x in timeHHMM_list]
            minutes = [        x[2:4]  for x in timeHHMM_list]
            secs    = [        x[:2] for x in SEC_UTC_list]
            millsecs= [        x[3:]  for x in SEC_UTC_list]


        df['year']  = year
        df['month'] = month
        df['day']   = day
        df['hours']  = hours
        df['minutes'] = minutes
        df['secs']  = secs
        df['millsecs'] = millsecs

        fix_decimal = []
        for i,val in enumerate(df['secs'].astype(str)):
            if val.find('.') == 1:
                fix_decimal.append( '0'+val[:-1])
            else:
                fix_decimal.append( val)

        year        = list(map(int, df['year'].values))
        month       = list(map(int, df['month'].values))
        day         = list(map(int, df['day'].values))
        hour        = list(map(int, df['hours'].values))
        minute      = list(map(int, df['minutes'].values))
        second      = list(map(int, fix_decimal))
        millsecs = list(map(int, df['millsecs'].values))

        DATE = list(map(datetime, year,month, day, hour,minute,second,millsecs ))
        return(DATE)
    
    
    def iteration_number(self, iieout_filename):
        '''
        This function opens the iieout file, and returns the final iteration number
        '''
        with open(iieout_filename, 'r') as f:
            for line_no, line in enumerate(f):
                if 'CONVERGENCE' in line:
                    line_text = line
                    # print(line)
        num_iters = float(line_text[38:42])-1
        self.total_iterations = int(num_iters)

        if len(str(self.total_iterations)) == 1:
             self.str_iteration = ' '+str(self.total_iterations)  # add a space if the iteration number is not double digit
        else:
             self.str_iteration =     str(self.total_iterations)
        return(self.total_iterations, self.str_iteration)
    
    
    
    def organize_output_object_keys(self, data_keys):    
        ''' 
        This function cleans the keys that are stored in the object:   
         The run parameters are stored in their own dimension under 'run_parameters'.
                     RUNOBJECT.__dict__['run_parameters']
         The remaining datasets are left in the 1st dimension
                     RUNOBJECT.__dict__.keys()
        '''
        data_keys.append('run_parameters')
        to_move_and_delete = []
        self.run_parameters    = {}

        for ii,i in enumerate(self.__dict__.keys()):
            if i in data_keys:
                pass
            else:
                to_move_and_delete.append(i)
                self.run_parameters[i] =  self.__dict__[i]
                
        for i_del in to_move_and_delete:         
            del self.__dict__[i_del]    
        return(self)
            
            
