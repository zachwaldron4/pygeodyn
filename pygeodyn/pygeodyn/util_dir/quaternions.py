# Coordinate Transformation of the quaternions: (SBF → RSW) to (SBF→ J2000)
# - SPIRE Default Quaternions
#     - SBF → Orbit Level frame  (RSW)
#         - Quaternions convert from spacecraft reference frame to orbit level frame: (Just RSW)
#             - Z nadir = -r/|r|            (where r is s/c position vector)   ——     **Neg of Radial (R_hat)**
#             - Y = -(r x v) / | r x v |     (where v is s/c velocity vector)  ——    **Neg of Cross Track (W_hat)**
#             - X = Y x Z                (where 'x' is vector cross product)   ——     **Along Track (S_hat)**
            
#             A vector, **u**, in the spacecraft frame, has coordinates **u' = quq*** in the local
#             level frame.  *(where q is the full vector q=[qx, qy, qz, qw])*
            
#             pvi:  Position and velocity in ECI coordinates (km)
#             X, Y, Z, Xdot, Ydot, Zdot
            
# - GEODYN requirement
#     - SBF→ J2000 Earth Equator and Equinox Frame (ECI)
    
# Transformation Steps

# 1. construct the RSWtoECI transformation matrix  $M_{(RSW \rightarrow ECI)}$ using the `pvi` (XYZ, XYZ_dots in ECI ) line of the Spire attitude files
#     - details
        
        
# 2. convert this transformation matrix into a quaternion $\vec{q \,}_{(RSW \rightarrow ECI)}$ (see example.m)
# 3. multiply this quaternion to the one provided by Spire (i.e., q_SBFtoRSW) to get $\vec{q \,}_{(SBF \rightarrow ECI)} = \vec{q \,}_{(RSW \rightarrow ECI)} * \vec{q \,}_{(SBF \rightarrow RSW)}$ (see example.m and quat_mult.m). 
# 4. use SLERP technique to interpolate to the appropriate time cadence (extract from GEODYN or find elsewhere)    



import numpy as np
import pandas as pd
from datetime import datetime,timedelta
import gc

    
    
    
def quat_mult(q1,q2):
    r"""Multiply two quaternions q1 and q2
        Quaternions are column or row vectors with q = [qx,qy,qz,qw],
        Where qw is the scalar part
        See https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation#Quaternions

    Parameters
    ----------
    q1 : 4x1 numpy array
    q2 : 4x1 numpy array

    Returns
    -------
    q  : 4x1 numpy array
    """

    if len(q1)!= 4 or len(q2)!=4:
        print("error-- input quaternions must have length=4" )
        sys.exit(0)
    if q1.shape[0]!=q2.shape[0]: #or q1.shape[1]!=q2.shape[1]:
        print("error-- input quaternions must have consistent dimensions")
        sys.exit(0)

    ### Initialize output quaternion
    q = np.zeros(np.size(q1))
    ### Compute scalar component
    q[3] = q1[3]*q2[3] - np.dot(q1[0:3],q2[0:3])
    ### Compute vector component
    q[0:3] = np.multiply(q1[3],q2[0:3]) + np.multiply(q2[3],q1[0:3]) + np.cross(q1[0:3],q2[0:3])
    return(q)







def quat_trans_SBFtoRSW_to_SBFtoECI(pos_eci, vel_eci, q_SBFtoRSW,
                                              verbose=False):
    r"""Transform Quaternions from SBFtoRSW to SBFtoECI
            SBF - Spacecraft Body Fixed
            RSW - Satellite Coordinate System-Orbit Level
                    (R)adial, (S) Along Track, (W) Cross-Track
            ECI - Earth Centered Inertial Coordinates, typically J2000

    Transformation Steps: 
        (see e.g., https://en.wikipedia.org/wiki/Rotation_matrix#Quaternion)
        1.  Construct the RSWtoECI transformation matrix M_RSWtoECI 
            using the pvi line of the Spire attitude files.    
        2.  Convert this transformation matrix into a quaternion q_RSWtoECI 
        3.  Multiply quaternion (q_RSWtoECI) by that of Spire (q_SBFtoRSW)
            to get q_SBFtoECI = q_RSWtoECI*q_SBFtoRSW.

    Parameters
    ----------
    pos_eci    : 3x1 numpy array
                 S/C position vector in ECI
    vel_eci    : 3x1 numpy array
                 S/C velocity vector in ECI
    q_SBFtoRSW : 4x1 numpy array
                 Quaternions representing a transformation from SBFtoRSW
    **kwargs
        verbose : logical
            
    Returns
    -------
    q_SBFtoRSW : 4x1 numpy array
        Quaternions representing a transformation from SBFtoECI
    """
    
    pos_eci    = np.matrix(pos_eci[:,None]) # make vertical
    vel_eci    = np.matrix(vel_eci[:,None]) # make vertical
    
    ##### 1. Calculate RSW->ECI transition matrix
    M_RSWtoECI      = np.matrix(np.zeros((3,3) ))
    M_RSWtoECI[:,2] = (-1*pos_eci) / np.sqrt(np.sum(np.square(pos_eci)))
    M_RSWtoECI[:,1] = np.cross(M_RSWtoECI[:,2], vel_eci, axis=0 ) \
                      / np.sqrt(np.sum(np.square(vel_eci)) )
    M_RSWtoECI[:,0] = np.cross(M_RSWtoECI[:,1], M_RSWtoECI[:,2], axis=0  ) 
    if verbose: print("Expecting:") 
    if verbose: print(" 0     0    -1")
    if verbose: print(" 0     1     0")
    if verbose: print(" 1     0     0")
    if verbose: print("M_RSWtoECI \n", M_RSWtoECI)

    #####  2. Construce q_RSWtoECI quaternion from M_RSWtoECI tranformation matrix
    ###     (see e.g., https://en.wikipedia.org/wiki/Rotation_matrix#Quaternion)
    r = np.sqrt(1 + np.trace(M_RSWtoECI))
    s = 1/(2*r)
    q_RSWtoECI = np.array([(M_RSWtoECI[2,1]-M_RSWtoECI[1,2])*s,
                           (M_RSWtoECI[0,2]-M_RSWtoECI[2,0])*s,
                           (M_RSWtoECI[1,0]-M_RSWtoECI[0,1])*s,
                            r/2                               ])
    ##### Test 1:  
    ###   pos_rsw should be 6378.137*[0;0;-1], 
    ###   from this we can test to see if we recover pos_eci 
    ###   with the quaternion conjugation: pos_eci = q*pos_rsw*q'
    inv_q_RSWtoECI = np.append(-1*q_RSWtoECI[0:3] , q_RSWtoECI[3] ) 
    pos_rsw        = np.array([0,0,-6378.137,0]) # treat vector as a quaternion with zero scalar value
    pos_eci_test   = quat_mult(q_RSWtoECI  ,   quat_mult(pos_rsw,   inv_q_RSWtoECI)   )
    if verbose: print()
    if verbose: print('Expecting\n', np.array([6.3781,         0,   -0.,         0])*1e3 )
    if verbose: print("pos_eci_test \n",pos_eci_test)

    ##### Test 2:
    ###   vel_rsw should be 7*[1;0;0], 
    ###   from this we can test to see if we recover vel_eci
    ###   with the quaternion conjugation: vel_eci = q*vel_rsw*q'
    inv_q_RSWtoECI = np.append(-1*q_RSWtoECI[0:3],q_RSWtoECI[3])
    vel_rsw = np.array([7,0,0,0])  # treat vector as a quaternion with zero scalar value
    vel_eci_test = quat_mult(q_RSWtoECI,quat_mult(vel_rsw,inv_q_RSWtoECI))
    if verbose: print( )
    if verbose: print("Expecting\n",np.array([0.,         0.,   7.,         0.]) )
    if verbose: print("vel_eci_test \n",vel_eci_test)

    ##### 3.  Multiply quaternions to get q_SBFtoECI
    q_SBFtoECI = quat_mult(q_RSWtoECI,q_SBFtoRSW)
    if verbose: print()
    if verbose: print("q_SBFtoECI \n",q_SBFtoECI)
        
    return(q_SBFtoECI)











def call_slerp_SpireAtt(SpireDF, start_date, stop_date, interval ):
    """Call the scipy slerp function to interpolate Spire quaternions.

    [qx, qy, qz, qw]

    Interpolate from the inconsistent Spire quaternion time cadence to 
    a linearly spaced time cadence so that the quaternions can be written
    to a GEODYN External Attitude file.
    

    Args:
        SpireDF (pandas dataframe): The Spire attitude quaternions and times.
            Times are TDT time 'tim (tdt)', and quaternions are SBF 'q (sbf)'.
        startEpoch (str): Epoch start time (must be <10 seconds before 
            intended arc epoch start); "2018-11-08 23:00:00"  
        stopEpoch (str): Epoch stop time (must be <10 seconds after 
            intended arc epoch stop); "2018-11-10 01:00:00"  
        interval (int): Intended interval (i.e., time cadence/step size)
        
    Returns:
        extatt_quats(dict): Return a dictionary containing the linearly spaced
            TDT times from start_date to stop_date given user input interval, 
            Slerp'd quaternions at the above times for SBF-->J2000.
    """
    
    from scipy.spatial.transform import Rotation as R
    from scipy.spatial.transform import Slerp
    
    
    print(f"start_date {start_date}")
    print(f"stop_date {stop_date}")



    ### Make linearly spaced time series from the Epoch Start to Epoch End 
    ### given some time cadence (interval).
    # startDT = pd.to_datetime(start_date, format='%Y-%m-%d %H:%M:%S')
    # stopDT  = pd.to_datetime(stop_date,  format='%Y-%m-%d %H:%M:%S')
    startDT = pd.to_datetime(start_date, format='ISO8601')
    stopDT  = pd.to_datetime(stop_date,  format='ISO8601')

    freq_str = str(int(interval))+"s"
    times_linspace = pd.date_range(start=startDT, end=stopDT, freq=freq_str)
    times_linspace = [pd.Timestamp(date).to_pydatetime()
                                for date in times_linspace ]


    ### Remove any duplicates or repeats that may corrupt the interpolation
    ###  Slerp require STRICTLY INCREASING array
    SpireDF = SpireDF.drop_duplicates(subset=["date_tdt"], keep='first'\
                ).sort_values(by='date_tdt'\
                                ).reset_index(drop=True)

    
    print(SpireDF['date_tdt'].values)
    
    ### Simplify variable name
    Spire_dates = SpireDF['date_tdt'].values
    
    ###### Must convert times to their unix times. 
    ###    Spire data times 
    unixtimes = [ pd.Timestamp(date) for date in Spire_dates ]
    tim_unix_spire =  [ ts.value/10**9  for ts in unixtimes ]
    
    ###    Desired Times to which we are interpolating
    unixtimes = pd.Series([ pd.Timestamp(date)  for date in times_linspace ])
    tim_unix_interp =  [ ts.value/10**9  for ts in unixtimes ]
    
    #### Load the Spire Quaternions as Scipy." ".Rotation    
    key_rots = R.from_quat(SpireDF['q_SBF_to_J2000'] .values.tolist() )
    
    #### Construct a Slerp interpolation object    
    slerp_obj = Slerp(tim_unix_spire,key_rots)

    #### Interpolate the quaternions to desired time series
    interp_rots = slerp_obj(tim_unix_interp)

    extatt_quats = {}
    ### Recover the dates from unix time
    extatt_quats['date_tdt'] =  [pd.to_datetime(
                datetime.strftime(datetime.fromtimestamp(ts), '%y%m%d%H%M%S.%f'),
                    format ='%y%m%d%H%M%S.%f' )
                                 for ts in tim_unix_interp ]

    ### Recover the interpolated quaternions
    extatt_quats['q_SBF_to_J2000'] = interp_rots.as_quat()

    return(extatt_quats)




def interpolate_nearest_neighbor():
    '''
    This code is not fully implemented, rather it is being stored.
    
    Obsolete Option:
        Do a Nearest Neighbor Interpolation at a 10 second cadence 
          1. Make a linearly spaced array of dates from the Epoch Start 
             to Epoch End
          2. For all values in the date array, find the times closest to
             them in the SPIRE DF and make that the value.

    '''
    
    ##### datetime ##### ##### datetime ##### ##### datetime ##### 
    ### Initialize the Interpolated Data that will be stored
    num = np.size(Dates_10s)
    qx_sbf_interpd = np.zeros( num ) 
    qy_sbf_interpd = np.zeros( num ) 
    qz_sbf_interpd = np.zeros( num ) 
    qw_sbf_interpd = np.zeros( num ) 
    Spire_dates = SpireDF['tim (gps)'].values
    Spire_dates = [ pd.Timestamp(date).to_pydatetime()  for date in Spire_dates ]
    Spire_qx    = SpireDF['qx (sbf)'].values.astype(float)
    Spire_qy    = SpireDF['qy (sbf)'].values.astype(float)
    Spire_qz    = SpireDF['qz (sbf)'].values.astype(float)
    Spire_qw    = SpireDF['qw (sbf)'].values.astype(float)

    ### NEAREST DATETIME -----------------------------------------------
    def nearest(items, pivot):
        return min(items, key=lambda x: abs(x - pivot))
    ## Loop through the 
    for i,dateval in enumerate(Dates_10s):
        ### Find the closest date
        date_near = nearest(Spire_dates, dateval)
        res = (date_list == pd.Timestamp(date_near)).argmax()
       
        qx_sbf_interpd[i] = SpireDF['qx (sbf)'][res]
        qy_sbf_interpd[i] = SpireDF['qy (sbf)'][res]
        qz_sbf_interpd[i] = SpireDF['qz (sbf)'][res]
        qw_sbf_interpd[i] = SpireDF['qw (sbf)'][res]

    ##### PANDAS ##### ##### PANDAS ##### ##### PANDAS ##### ##### PANDAS #####
    
    ### Initialize the Interpolated Data that will be stored
    num = np.size(Dates_10s)

    qx_sbf_interpd = np.zeros( num ) 
    qy_sbf_interpd = np.zeros( num ) 
    qz_sbf_interpd = np.zeros( num ) 
    qw_sbf_interpd = np.zeros( num ) 

    Spire_dates = SpireDF['tim (gps)'].values
    Spire_dates = [ pd.Timestamp(date).to_pydatetime()  for date in Spire_dates ]
    Spire_qx    = SpireDF['qx (sbf)'].values.astype(float)
    Spire_qy    = SpireDF['qy (sbf)'].values.astype(float)
    Spire_qz    = SpireDF['qz (sbf)'].values.astype(float)
    Spire_qw    = SpireDF['qw (sbf)'].values.astype(float)
    ### NEAREST DATETIME -----------------------------------------------
    def nearest(items, pivot):
        return min(items, key=lambda x: abs(x - pivot))
    ## Loop through the 
    for i,dateval in enumerate(Dates_10s):
        ### Find the closest date
        date_near = nearest(Spire_dates, dateval)
        res = (date_list == pd.Timestamp(date_neafr)).argmax()
        ### There are some instances of repeats dates which produce
    #     if np.size(index_near)>1:
    #         index_near = index_near.min()
        qx_sbf_interpd[i] = SpireDF['qx (sbf)'][res]
        qy_sbf_interpd[i] = SpireDF['qy (sbf)'][res]
        qz_sbf_interpd[i] = SpireDF['qz (sbf)'][res]
        qw_sbf_interpd[i] = SpireDF['qw (sbf)'][res]

    return



