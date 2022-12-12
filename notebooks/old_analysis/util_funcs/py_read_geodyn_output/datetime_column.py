import numpy as np
import pandas as pd
from datetime import datetime,timedelta
import os.path
import linecache
import time

# df =traj_xyz_df
# YR = 19



def make_datetime_column(df, YR):
    
    YR = int(str(YR)[-2:])

#     print(YR)
    
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

#     print(type(year[0])) 
#     print(type(month[0]))
#     print(type(day[0]))
#     print(type(hour[0]))
#     print(type(minute[0]))
#     print(type(second[0]))
#     print(type(millsecs[0]))
    
    DATE = list(map(datetime, year,month, day, hour,minute,second,millsecs ))


    return(DATE)