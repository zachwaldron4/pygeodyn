import numpy as np
import pandas as pd
from datetime import datetime,timedelta
import os.path
import linecache


def read_density_file(densityfil):
    '''
    The density file doesn't change if you change satellite or run type.
    The below function works well as a common function.
    '''

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
