import numpy as np 
import pandas as pd
from datetime import datetime,timedelta





def read_density_file(densityfil):
    DEN_csv = pd.read_csv(densityfil, 
                        names = ['Elapsed Secs',
                                'YYMMDD',
                                'HHMMSS',
                                'Lat',
                                'Lon',
                                'Height (meters)',
                                'rho (kg/m**3)',
                                'delta_rho (kg/m**3/m)',
                              ],
                        sep = '\s+',
                        )

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



    from datetime import datetime,timedelta

    VERBOSE_timer = True
        # def make_datetime_column(isat_data, VERBOSE_timer):
    # isat_data = data_dict[isat] 
    # VERBOSE_timer=True


    if VERBOSE_timer == True:
      import time
      start = time.time()
    else:
      pass

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
    for i,val in enumerate(DEN_df['YYMMDD'].values.astype(int).astype(str)):
        year[i] = ('20' + val[:2])
        month[i] = (val[2:4])
        day[i] = (val[4:])
        hours[i] = (DEN_df['timeHHMMSS'].values.astype(str)[i][:2])
        minutes[i] = (DEN_df['timeHHMMSS'].values.astype(str)[i][2:4])
        secs[i] = (DEN_df['timeHHMMSS'].values.astype(str)[i][4:])

    DEN_df['year']  = year
    DEN_df['month'] = month
    DEN_df['day']   = day
    DEN_df['hours']  = hours
    DEN_df['minutes'] = minutes
    DEN_df['secs']  = secs

    if VERBOSE_timer == True:
      end = time.time()
      elapsed = end - start
      print("Loop through and extract indiv date vals:",elapsed)
    else:
      pass


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
      print("Put all dates in a single column:",elapsed)
    else:
      pass

    DEN_df['Date']  = DATE



    return(DEN_df)


