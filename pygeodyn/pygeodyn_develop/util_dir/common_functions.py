import numpy as np


def MJDS_to_YYMMDDHHMMSS(input_ModJulianDay_secs):
    '''
    This function takes modified julian day seconds (MJDS) as input 
    and returns a date_string in the format YYMMDDHHMMSS.
    '''

    #########################################
    # Define some constants
    SECDAY              = 86400
    geodyn_ref_time_mjd = 30000
    jd_0                = 2400000.5
    d36525              = 365.25
    d122                = 122.1
    d30600              = 30.6001
    half                = 0.5
    ib                  = -15
    d17209              = 1720996.5

    ######  CONVERT FROM MJDS TO MJD
    # Inputs:
    MJDS = input_ModJulianDay_secs
    #
    MJD = (MJDS/SECDAY) + geodyn_ref_time_mjd

    ######  CONVERT FROM MJD TO YMD
    # Note from zach-- I took this calculation from geodyn...
    # There is more going on here than I understand, 
    # but I want to stay on their level of accuracy
    #
    JD = MJD + jd_0                  #  Convert to JulianDay
    c  = int( JD + half ) + 1537     # ??   sorry, i'm   ??
    nd = int( (c - d122) / d36525 )  # ??   not sure     ??
    e  = int( d36525 * nd )          # ??   what this    ??
    nf = int( ( c - e ) / d30600 )   # ??   all is       ??
    # ----
    frac = (JD + half) - int( JD + half )           # frac of day leftover
    iday = c - e - int( d30600 * nf ) + frac        # day
    imonth  = nf -  1   - 12 * int( nf / 14 )       # month
    iyyyy = nd - 4715 - int(  ( 7 + imonth ) / 10 ) # YYYY
    #
    ##### Use modular division to get 2 digit year
    iyear =  iyyyy % 100 
    #
    #### Return YYMMDD 
    yymmdd = int(iyear * 10000 + imonth * 100 + iday)


    ##### Calculate Hours, Minutes, seconds
    isec_mjd  =  MJDS % 86400

    ihour    = isec_mjd/3600
    iminutes = (ihour % 1)*60
    isec     = (iminutes % 1)*60 

    ihour_str     = str(int((ihour)))
    iminutes_str  = str(int((iminutes)))
    isec_str      = str(int(round(isec)))

    if len(ihour_str)==1:
        ihour_str = '0'+ihour_str
    if len(iminutes_str)==1:
        iminutes_str = '0'+iminutes_str
    if len(isec_str)==1:
        isec_str = '0'+isec_str

    #hhmmss  =  int((ihour*10000) + (iminutes*100) + isec)
    hhmmss  =  ihour_str + iminutes_str + isec_str
    YYMMDDHHMMSS = str(yymmdd) + '-' + str(hhmmss)


    return(YYMMDDHHMMSS)







def Convert_ET_TDT_to_UTC(terrestrial_time_mjdsec, leap_seconds):
    '''
    ET is Ephemeris Time and has been numerically equivalent to 
    Terrestral Time (TT) or Terrestral Dynamic Time (TDT) since mid 1970’s.

    TT is distinct from the time scale often used as a 
        basis for civil purposes, Coordinated Universal Time (UTC).
        TT is indirectly the basis of UTC, via 
        International Atomic Time (TAI). 
        Because of the historical difference between TAI and ET 
        when TT was introduced, TT is approximately 32.184 s
        ahead of TAI.

        ??ET - A1 = 32.1496183??

        TDT = TAI + 32.184  
        TAI = UTC + dAT  
            where dAT is the total algebraic sum of leap seconds 

            As of 1 January 2017,
            TAI is ahead of UTC   by 37 seconds.
            TAI is ahead of GPS   by 19 seconds.
            GPS is ahead of UTC   by 18 seconds.


    Convert ET to UTC:
        UTC  =  TT - dAT - 32.184 s  
     '''



    TT  = terrestrial_time_mjdsec
    dAT = leap_seconds

    UTC = TT - dAT - 32.184
    mjdsecs_UTC = UTC

    return(mjdsecs_UTC)








def Convert_cartesian_to_RSW(state_vector):
    '''
    ###### The Satellite Coordinate System: RSW
    ### often used to describe orbital errors, relative positions, and displacements of satellite orbits. 
    ### The RSW system moves with the satellite and is sometimes given the letters RTN (radial, transverse, and normal). 
    ### The R axis always points from the Earth’s center along the radius vector toward the satellite as it moves through the orbit. 
    ### The S axis points in the direction of (but not necessarily parallel to) the velocity vector and is perpendicular to the radius vector—an important distinction.
    ### The S axis is usually not aligned with the velocity vector except for circular orbits or for elliptical orbits at apogee and perigee. In

    ### Radial positions and displacements are parallel to the position vector (along the R axis).
    ### Along-track or transverse displacements are normal to the position vector (along the S axis).
    ### Finally, cross-track positions are normal to the plane defined by the current position and velocity vectors (along the W axis).


    ##### Radial component:
    ###    Radial positions and displacements are parallel
    ###    to the position vector (along the R axis).


    ##### Along track or Transverse
    ###   normal to the position vector (along the S axis)

    ##### Cross track
    ###   normal to the plane defined by the current 
    ###   position and velocity vectors (along the W axis)

    '''

    r_vec = state_vector[:3]    # np.array([X,   Y,   Z   ])
    v_vec = state_vector[-3:]    # np.array([Xdot,Ydot,Zdot])
    r_vec_norm = np.linalg.norm(r_vec)

    R_hat = r_vec/r_vec_norm
    W_hat = (np.cross(r_vec,v_vec)) / np.linalg.norm(np.cross(r_vec,v_vec))
    S_hat = np.cross(W_hat, R_hat )


    ###  
    ##         R_vec_ijk = T * R_vec_rsw
    ###
    ###    where, T = [ R_hat | S_hat | W_hat ]
    ###
    ###    so...
    ###
    ###        R_vec_rsw =   R_vec_ijk * T^-1
    transmat_RSW =  np.transpose(np.array([R_hat, S_hat, W_hat ]))
    inverse_transmat_RSW = np.linalg.inv(transmat_RSW)


    r_vec_RSW = np.matmul(inverse_transmat_RSW, r_vec,  )
    r = r_vec_RSW[0]
#     s = r_vec_RSW[1]
#     w = r_vec_RSW[2]
    return r

def Convert_cartesian_to_RSW_returnall(state_vector):
    '''
    ###### The Satellite Coordinate System: RSW
    ### often used to describe orbital errors, relative positions, and displacements of satellite orbits. 
    ### The RSW system moves with the satellite and is sometimes given the letters RTN (radial, transverse, and normal). 
    ### The R axis always points from the Earth’s center along the radius vector toward the satellite as it moves through the orbit. 
    ### The S axis points in the direction of (but not necessarily parallel to) the velocity vector and is perpendicular to the radius vector—an important distinction.
    ### The S axis is usually not aligned with the velocity vector except for circular orbits or for elliptical orbits at apogee and perigee. In

    ### Radial positions and displacements are parallel to the position vector (along the R axis).
    ### Along-track or transverse displacements are normal to the position vector (along the S axis).
    ### Finally, cross-track positions are normal to the plane defined by the current position and velocity vectors (along the W axis).


    ##### Radial component:
    ###    Radial positions and displacements are parallel
    ###    to the position vector (along the R axis).


    ##### Along track or Transverse
    ###   normal to the position vector (along the S axis)

    ##### Cross track
    ###   normal to the plane defined by the current 
    ###   position and velocity vectors (along the W axis)

    '''

    r_vec = state_vector[:3]    # np.array([X,   Y,   Z   ])
    v_vec = state_vector[-3:]    # np.array([Xdot,Ydot,Zdot])
    r_vec_norm = np.linalg.norm(r_vec)

    R_hat = r_vec/r_vec_norm
    W_hat = (np.cross(r_vec,v_vec)) / np.linalg.norm(np.cross(r_vec,v_vec))
    S_hat = np.cross(W_hat, R_hat )


    ###  
    ##         R_vec_ijk = T * R_vec_rsw
    ###
    ###    where, T = [ R_hat | S_hat | W_hat ]
    ###
    ###    so...
    ###
    ###        R_vec_rsw =   R_vec_ijk * T^-1
    transmat_RSW =  np.transpose(np.array([R_hat, S_hat, W_hat ]))
    inverse_transmat_RSW = np.linalg.inv(transmat_RSW)


    r_vec_RSW = np.matmul(inverse_transmat_RSW, r_vec  )
    r = r_vec_RSW[0]
    s = r_vec_RSW[1]
    w = r_vec_RSW[2]
    return r,s,w



def Convert_cartesian_to_NTW(state_vector):
    '''
    ###### The Satellite Coordinate System: NTW
    ###        often used to describe orbital errors, relative positions, 
               and displacements of satellite orbits. 
    
    ### The NTW system moves with the satellite.
    ###     T axis is tangential to the orbit and always points to the velocity vector. 
    ###     N axis lies in the orbital plane, normal to the velocity vector.
    ###     W axis is normal to the orbital plane (as in the RSW system)
    
    ### NOTE: We define in-track or tangential displacements as deviations along
              the T axis. In-track errors are not the same as along-track variations
              in the RSW system. One way to remember the distinction is that the 
              in-track errors are in the direc- tion of the velocity, whereas 
              along-track variations are simply along the velocity vector.
    '''

    r_vec = state_vector[:3]    # np.array([X,   Y,   Z   ])
    v_vec = state_vector[-3:]    # np.array([Xdot,Ydot,Zdot])
    v_vec_norm = np.linalg.norm(v_vec)

    T_hat = v_vec/v_vec_norm
    W_hat = (np.cross(r_vec,v_vec)) / np.linalg.norm(np.cross(r_vec,v_vec))
    N_hat = np.cross(T_hat, W_hat)


    transmat_NTW =  np.transpose(np.array([N_hat, T_hat, W_hat ]))
    inverse_transmat_NTW = np.linalg.inv(transmat_NTW)


    r_vec_NTW = np.matmul(inverse_transmat_NTW, r_vec  )
    
#     n = r_vec_NTW[0]
    t = r_vec_NTW[1]
#     w = r_vec_NTW[2]
    return t


def Convert_cartesian_to_NTW_returnall(state_vector):
    '''
    ###### The Satellite Coordinate System: NTW
    ###        often used to describe orbital errors, relative positions, 
               and displacements of satellite orbits. 
    
    ### The NTW system moves with the satellite.
    ###     T axis is tangential to the orbit and always points to the velocity vector. 
    ###     N axis lies in the orbital plane, normal to the velocity vector.
    ###     W axis is normal to the orbital plane (as in the RSW system)
    
    ### NOTE: We define in-track or tangential displacements as deviations along
              the T axis. In-track errors are not the same as along-track variations
              in the RSW system. One way to remember the distinction is that the 
              in-track errors are in the direc- tion of the velocity, whereas 
              along-track variations are simply along the velocity vector.
    '''

    r_vec = state_vector[:3]    # np.array([X,   Y,   Z   ])
    v_vec = state_vector[-3:]    # np.array([Xdot,Ydot,Zdot])
    v_vec_norm = np.linalg.norm(v_vec)

    T_hat = v_vec/v_vec_norm
    W_hat = (np.cross(r_vec,v_vec)) / np.linalg.norm(np.cross(r_vec,v_vec))
    N_hat = np.cross(T_hat, W_hat)


    transmat_NTW =  np.transpose(np.array([N_hat, T_hat, W_hat ]))
    inverse_transmat_NTW = np.linalg.inv(transmat_NTW)


    r_vec_NTW = np.matmul(inverse_transmat_NTW, r_vec  )
    
    n = r_vec_NTW[0]
    t = r_vec_NTW[1]
    w = r_vec_NTW[2]
    return n, t, w



def Pygeodyn_OBJECT_freeupmemory(OBJ):
    SAT_ID = int(OBJ.__dict__['global_params']['SATID'])

    for i,val in enumerate(OBJ.__dict__['Density'].keys()):
        
        ####-----------------------------------------------------------------
        #### DELETE UNNECESSARY VARS IN DENSITY
        
        del OBJ.__dict__['Density'][val]['Lat']
        del OBJ.__dict__['Density'][val]['Lon']
#         del OBJ.__dict__['Density'][val]['X']
#         del OBJ.__dict__['Density'][val]['Y']
#         del OBJ.__dict__['Density'][val]['Z']
#         del OBJ.__dict__['Density'][val]['XDOT']
#         del OBJ.__dict__['Density'][val]['YDOT']
#         del OBJ.__dict__['Density'][val]['ZDOT']
        del OBJ.__dict__['Density'][val]['Height (meters)']
        del OBJ.__dict__['Density'][val]['drhodz (kg/m**3/m)']
        
        
        ####-----------------------------------------------------------------
        #### DELETE UNNECESSARY VARS IN Residuals_obs
        
        del OBJ.__dict__['Residuals_obs'][val]['Sat_main']
        del OBJ.__dict__['Residuals_obs'][val]['track_1']
        del OBJ.__dict__['Residuals_obs'][val]['track_2']
        del OBJ.__dict__['Residuals_obs'][val]['Note']
        del OBJ.__dict__['Residuals_obs'][val]['Elev1']
        del OBJ.__dict__['Residuals_obs'][val]['Elev2']
        ####
        del OBJ.__dict__['Residuals_obs'][val]['StatSatConfig']
        del OBJ.__dict__['Residuals_obs'][val]['Observation']
        del OBJ.__dict__['Residuals_obs'][val]['Residual']
        del OBJ.__dict__['Residuals_obs'][val]['RatiotoSigma']

         
        
        
        
        
        
        ####-----------------------------------------------------------------
        #### DELETE UNNECESSARY VARIABLES IN AdjustedParams
        
        iterations = OBJ.__dict__['run_parameters'+val]['total_iterations']
        for iters in np.arange(1, iterations):
#             print(iters)
            if iters == iterations:
                del OBJ.__dict__['AdjustedParams'][val][iters][SAT_ID]['0XPOS']
                del OBJ.__dict__['AdjustedParams'][val][iters][SAT_ID]['0YPOS']
                del OBJ.__dict__['AdjustedParams'][val][iters][SAT_ID]['0ZPOS']
#                 del OBJ.__dict__['AdjustedParams'][val][iters][SAT_ID]['0XPOS']

                pass
            else:
                try:
                    del OBJ.__dict__['AdjustedParams'][val][iters]
                except:
                    pass
        
        
        ####-----------------------------------------------------------------
        #### DELETE UNNECESSARY VARIABLES IN Trajectory_orbfil
        
        del OBJ.__dict__['Trajectory_orbfil'][val]['header']
        del OBJ.__dict__['Trajectory_orbfil'][val]['data_record']['Satellite Geodetic Latitude']
        del OBJ.__dict__['Trajectory_orbfil'][val]['data_record']['Satellite East Longitude']
        del OBJ.__dict__['Trajectory_orbfil'][val]['data_record']['Satellite Height']
        del OBJ.__dict__['Trajectory_orbfil'][val]['data_record']['MJDSEC ET']
        
        
        
    #### For big data non-sense
#     del OBJ.__dict__['AdjustedParams']
#    del OBJ.__dict__['Trajectory_orbfil']
    del OBJ.__dict__['Density']
#     del OBJ.__dict__['Residuals_obs']
    del OBJ.__dict__['Residuals_summary']

        
    return(OBJ)
