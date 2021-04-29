import numpy as np
import pandas as pd

def orb_avg(den_df, arc):
    lat = np.asarray(den_df[arc]['Lat'])
    time_pd = pd.to_datetime(den_df[arc]['Date'])
    i = np.nonzero( lat[1:]*lat[0:-1]  <  np.logical_and(0 , lat[1:] > lat[0:-1] )  )
    i = i[0]

    d_avg = np.zeros(np.size(i))
    height_avg = np.zeros(np.size(i))
    time_avg = []

    d_avg_rolling = []
    
    roll_avg_count = 0
    for j in range(np.size(i)-1):
    #     print(j+1)
        d_avg[j] = np.mean(  den_df[arc]['rho (kg/m**3)'][   i[j] : i[j+1]-1  ]  )
        height_avg[j] = np.mean(  den_df[arc]['Height (meters)'][   i[j] : i[j+1]-1  ]  )
        mean_time=time_pd[   i[j] : i[j+1]-1  ].mean() 
        time_avg.append(  mean_time)

        if roll_avg_count ==2:
            d_avg_rolling.append(np.mean([ d_avg[j],  d_avg[j-1]]))
            
            roll_avg_count =0
            
        roll_avg_count+=1 
        
    return(time_avg, d_avg, d_avg_rolling )
    