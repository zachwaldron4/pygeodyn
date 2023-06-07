import numpy as np
import pandas as pd

from pygeodyn.util_dir.time_systems import *


path_iau06 ='/data/SatDragModelValidation/pygeodyn/pygeodyn/util_dir/data_iau06'
file_iers_iau06eop = path_iau06+'/finals2000A.daily.extended.dat.2010_on' 

file_iers_iau76eop = path_iau06+"/iau76_finals.daily.extended"


def Convert_cartesian_to_RSW(state_vector):
    '''
    ###### The Satellite Coordinate System: RSW ### often used to describe
    orbital errors, relative positions, and displacements of satellite orbits.
    ### The RSW system moves with the satellite and is sometimes given the
    letters RTN (radial, transverse, and normal).  ### The R axis always points
    from the Earth’s center along the radius vector toward the satellite as it
    moves through the orbit.  ### The S axis points in the direction of (but not
    necessarily parallel to) the velocity vector and is perpendicular to the
    radius vector—an important distinction.  ### The S axis is usually not
    aligned with the velocity vector except for circular orbits or for
    elliptical orbits at apogee and perigee. In

    ### Radial positions and displacements are parallel to the position vector
    (along the R axis).  ### Along-track or transverse displacements are normal
    to the position vector (along the S axis).  ### Finally, cross-track
    positions are normal to the plane defined by the current position and
    velocity vectors (along the W axis).


    ##### Radial component: ###    Radial positions and displacements are
    parallel ###    to the position vector (along the R axis).


    ##### Along track or Transverse ###   normal to the position vector (along
    the S axis)

    ##### Cross track ###   normal to the plane defined by the current ###
    position and velocity vectors (along the W axis)

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

def Convert_cartesian_to_RSW_returnall(state_vector, Tmat_input, PCE_bool = True):
    '''
    ###### The Satellite Coordinate System: RSW ### often used to describe
    orbital errors, relative positions, and displacements of satellite orbits.
    ### The RSW system moves with the satellite and is sometimes given the
    letters RTN (radial, transverse, and normal).  ### The R axis always points
    from the Earth’s center along the radius vector toward the satellite as it
    moves through the orbit.  ### The S axis points in the direction of (but not
    necessarily parallel to) the velocity vector and is perpendicular to the
    radius vector—an important distinction.  ### The S axis is usually not
    aligned with the velocity vector except for circular orbits or for
    elliptical orbits at apogee and perigee. In

    ### Radial positions and displacements are parallel to the position vector
    (along the R axis).  ### Along-track or transverse displacements are normal
    to the position vector (along the S axis).  ### Finally, cross-track
    positions are normal to the plane defined by the current position and
    velocity vectors (along the W axis).


    ##### Radial component: ###    Radial positions and displacements are
    parallel ###    to the position vector (along the R axis).

    ##### Along track or Transverse ###   normal to the position vector (along
    the S axis)

    ##### Cross track ###   normal to the plane defined by the current ###
    position and velocity vectors (along the W axis)

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
    
    if PCE_bool == True:
        transmat_RSW  =  [[ R_hat[0] , S_hat[0] , W_hat[0] ],
                          [ R_hat[1] , S_hat[1] , W_hat[1] ],
                          [ R_hat[2] , S_hat[2] , W_hat[2] ] ]
    else:
        transmat_RSW = Tmat_input

    
#     transmat_RSW =  np.transpose(np.array([R_hat, S_hat, W_hat ]))
    inverse_transmat_RSW = np.linalg.inv(transmat_RSW)


    r_vec_RSW = np.matmul(inverse_transmat_RSW, r_vec  )
    r = r_vec_RSW[0]
    s = r_vec_RSW[1]
    w = r_vec_RSW[2]
    return r_vec_RSW, transmat_RSW #r,s,w



# def Convert_cartesian_to_NTW_getT(state_vector):
#     '''
#     ###### The Satellite Coordinate System: NTW
#     ###        often used to describe orbital errors, relative positions, 
#                and displacements of satellite orbits. 
    
#     ### The NTW system moves with the satellite.
#     ###     T axis is tangential to the orbit and always points to the velocity vector. 
#     ###     N axis lies in the orbital plane, normal to the velocity vector.
#     ###     W axis is normal to the orbital plane (as in the RSW system)
    
#     ### NOTE: We define in-track or tangential displacements as deviations along
#               the T axis. In-track errors are not the same as along-track variations
#               in the RSW system. One way to remember the distinction is that the 
#               in-track errors are in the direc- tion of the velocity, whereas 
#               along-track variations are simply along the velocity vector.
#     '''

#     r_vec = state_vector[:3]    # np.array([X,   Y,   Z   ])
#     v_vec = state_vector[-3:]    # np.array([Xdot,Ydot,Zdot])
#     v_vec_norm = np.linalg.norm(v_vec)

#     T_hat = v_vec/v_vec_norm
#     W_hat = (np.cross(r_vec,v_vec)) / np.linalg.norm(np.cross(r_vec,v_vec))
#     N_hat = np.cross(T_hat, W_hat)


#     transmat_NTW =  np.transpose(np.array([N_hat, T_hat, W_hat ]))
#     inverse_transmat_NTW = np.linalg.inv(transmat_NTW)


#     r_vec_NTW = np.matmul(inverse_transmat_NTW, r_vec  )
    
# #     n = r_vec_NTW[0]
#     t = r_vec_NTW[1]
# #     w = r_vec_NTW[2]
#     return t


# def Convert_cartesian_to_NTW_returnall(state_vector):
#     '''
#     ###### The Satellite Coordinate System: NTW
#     ###        often used to describe orbital errors, relative positions, 
#                and displacements of satellite orbits. 
    
#     ### The NTW system moves with the satellite.
#     ###     T axis is tangential to the orbit and always points to the velocity vector. 
#     ###     N axis lies in the orbital plane, normal to the velocity vector.
#     ###     W axis is normal to the orbital plane (as in the RSW system)
    
#     ### NOTE: We define in-track or tangential displacements as deviations along
#               the T axis. In-track errors are not the same as along-track variations
#               in the RSW system. One way to remember the distinction is that the 
#               in-track errors are in the direc- tion of the velocity, whereas 
#               along-track variations are simply along the velocity vector.
#     '''

#     r_vec = state_vector[:3]    # np.array([X,   Y,   Z   ])
#     v_vec = state_vector[-3:]    # np.array([Xdot,Ydot,Zdot])
#     v_vec_norm = np.linalg.norm(v_vec)

#     T_hat = v_vec/v_vec_norm
#     W_hat = (np.cross(r_vec,v_vec)) / np.linalg.norm(np.cross(r_vec,v_vec))
#     N_hat = np.cross(T_hat, W_hat)


#     transmat_NTW =  np.transpose(np.array([N_hat, T_hat, W_hat ]))
#     inverse_transmat_NTW = np.linalg.inv(transmat_NTW)


#     r_vec_NTW = np.matmul(inverse_transmat_NTW, r_vec  )
    
#     n = r_vec_NTW[0]
#     t = r_vec_NTW[1]
#     w = r_vec_NTW[2]
#     return(n, t, w)


def Convert_cartesian_to_NTW_returnall(state_vector, Tmat_input, PCE_bool = True):
    '''
    ###### The Satellite Coordinate System: NTW 
    # ###        often used to
    describe orbital errors, relative positions, 
               and displacements of satellite orbits. 
    
    ### The NTW system moves with the satellite.  ###     T axis is tangential
    to the orbit and always points to the velocity vector.  ###     N axis lies
    in the orbital plane, normal to the velocity vector.  ###     W axis is
    normal to the orbital plane (as in the RSW system)
    
    ### NOTE: We define in-track or tangential displacements as deviations along
              the T axis. In-track errors are not the same as along-track
              variations in the RSW system. One way to remember the distinction
              is that the in-track errors are in the direc- tion of the
              velocity, whereas along-track variations are simply along the
              velocity vector.
    '''

#     print('state_vector', state_vector)
    
    r_vec = state_vector[:3]    
    v_vec = state_vector[-3:]   
    v_vec_norm = np.linalg.norm(v_vec, ord=2)

    T_hat = v_vec/v_vec_norm
    W_hat = (np.cross(r_vec,v_vec)) / np.linalg.norm(np.cross(r_vec,v_vec), ord=2)
    N_hat = np.cross(T_hat, W_hat)
    
    ### Transformation matrix
#     transmat_NTW =  np.transpose(np.array([N_hat, T_hat, W_hat ]))
#     transmat_NTW =  np.vstack([N_hat, T_hat, W_hat ])
    if PCE_bool == True:
        transmat_NTW  =  [[ N_hat[0] , T_hat[0] , W_hat[0] ],
                          [ N_hat[1] , T_hat[1] , W_hat[1] ],
                          [ N_hat[2] , T_hat[2] , W_hat[2] ] ]
    else:
        transmat_NTW = Tmat_input
    
#     from scipy.linalg import det
#     det_transmat_NTW = det(transmat_NTW)
#     print('transmat_NTW      ',transmat_NTW )
#     print('det(transmat_NTW) ',det_transmat_NTW )

    inverse_transmat_NTW = np.linalg.inv(transmat_NTW)

    r_vec_NTW = np.matmul(inverse_transmat_NTW, r_vec  )
    
    n = r_vec_NTW[0]
    t = r_vec_NTW[1]
    w = r_vec_NTW[2]
    
#     data_ntw = (n, t, w)
    return r_vec_NTW, transmat_NTW




def iau76_get_eop_vals(year,mon,day,
                 hr,minute,sec):
        
        #Maybe need to use EOPCO4
         # dir:  https://hpiers.obspm.fr/iers/eop/eopc04/
         # full time: https://hpiers.obspm.fr/iers/eop/eopc04/eopc04.dX_dY.12h.84-now
         # https://hpiers.obspm.fr/eoppc/eop/eopc04/C04.guide.pdf

    def readline_finals_iau76_data(line):
        """Parse a single line from the iau76_finals.daily.extended file.

        File sourced from IERS Rapid Service/Prediction Center (U.S. Naval Observatory)
        https://maia.usno.navy.mil/products/daily.htm

        finals.daily.extended
        Contains: Polar motion, UT1-UTC, and length of day since 02 January 1973
                    with dψ & dε using 1976 Precession/1980 Nutation Theory updated
                    daily (with 1 year of predictions).

        #
        The format of the finals.data, finals.daily, and finals.all files is:
        Col.#    Format  Quantity
        -------  ------  -------------------------------------------------------------
        1-2      I2      year (to get true calendar year, add 1900 for MJD<=51543 or add 2000 for MJD>=51544)
        3-4      I2      month number
        5-6      I2      day of month
        7        X       [blank]
        8-15     F8.2    fractional Modified Julian Date (MJD UTC)
        16       X       [blank]
        17       A1      IERS (I) or Prediction (P) flag for Bull. A polar motion values
        18       X       [blank]
        19-27    F9.6    Bull. A PM-x (sec. of arc)
        28-36    F9.6    error in PM-x (sec. of arc)
        37       X       [blank]
        38-46    F9.6    Bull. A PM-y (sec. of arc)
        47-55    F9.6    error in PM-y (sec. of arc)
        56-57    2X      [blanks]
        58       A1      IERS (I) or Prediction (P) flag for Bull. A UT1-UTC values
        59-68    F10.7   Bull. A UT1-UTC (sec. of time)
        69-78    F10.7   error in UT1-UTC (sec. of time)
        79       X       [blank]
        80-86    F7.4    Bull. A LOD (msec. of time) -- NOT ALWAYS FILLED
        87-93    F7.4    error in LOD (msec. of time) -- NOT ALWAYS FILLED
        94-95    2X      [blanks]
        96       A1      IERS (I) or Prediction (P) flag for Bull. A nutation values
        97       X       [blank]
        98-106   F9.3    Bull. A dPSI (msec. of arc)
        107-115  F9.3    error in dPSI (msec. of arc)
        116      X       [blank]
        117-125  F9.3    Bull. A dEPSILON (msec. of arc)
        126-134  F9.3    error in dEPSILON (msec. of arc)
        135-144  F10.6   Bull. B PM-x (sec. of arc)
        145-154  F10.6   Bull. B PM-y (sec. of arc)
        155-165  F11.7   Bull. B UT1-UTC (sec. of time)
        166-175  F10.3   Bull. B dPSI (msec. of arc)
        176-185  F10.3   Bull. B dEPSILON (msec. of arc)
        """
        eop ={}
        eop['year']    = int(  line[1-1  :  2]) # I2    -year (to get true calendar year, 
                                                #        add 1900 for MJD<=51543 or add 2000 for MJD>=51544)
        eop['month']   = int(  line[3-1  :  4]) # I2    -month number
        eop['day']     = int(  line[5-1  :  6]) # I2    -day of month
        eop['mjd_utc'] = float(line[8-1  : 15]) # F8.2  -fractional Modified Julian Date (MJD UTC)
        #
        eop['flag_pm___A'] = str(  line[17-1     ]) # A1    -IERS (I) or Prediction (P) flag for Bull. A polar motion values
        eop['pmx_______A'] = float(line[19-1 : 27]) # F9.6  -Bull. A PM-x (sec. of arc)
        eop['pmxerr____A'] = float(line[28-1 : 36]) # F9.6  -err  in PM-x (sec. of arc)
        eop['pmy_______A'] = float(line[38-1 : 46]) # F9.6  -Bull. A PM-y (sec. of arc)
        eop['pmyerr____A'] = float(line[47-1 : 55]) # F9.6  -err  in PM-y (sec. of arc)
        #
        eop['flag_dut1_A'] = str(  line[58-1     ]) # A1    -IERS (I) or Prediction (P) flag for Bull. A UT1-UTC values
        eop['dut1______A'] = float(line[59-1 : 68]) # F10.7 -Bull. A UT1-UTC (sec. of time)
        eop['dut1err___A'] = float(line[69-1 : 78]) # F10.7 -err  in UT1-UTC (sec. of time)
        eop['lod_______A'] = float(line[80-1 : 86]) # F7.4  -Bull. A LOD (msec. of time) -- NOT ALWAYS FILLED
        eop['loderr____A'] = float(line[87-1 : 93]) # F7.4  -err in  LOD (msec. of time) -- NOT ALWAYS FILLED
        #
        eop['flag__nut_A'] = str(  line[96-1     ]) # A1    -IERS (I) or Prediction (P) flag for Bull. A nutation values
        eop['dpsi__nut_A'] = float(line[98-1 :106]) # F9.3  -Bull. A dPSI (msec. of arc)
        eop['dpsierr___A'] = float(line[107-1:115]) # F9.3  -error in dPSI (msec. of arc)
        eop['deps__nut_A'] = float(line[117-1:125]) # F9.3  -Bull. A dEPSILON (msec. of arc)
        eop['depserr___A'] = float(line[126-1:134]) # F9.3  -error in dEPSILON (msec. of arc)
        #
        eop['pmx_______B'] = float(line[135-1:144]) # F10.6 -Bull. B PM-x (sec. of arc)
        eop['pmy_______B'] = float(line[145-1:154]) # F10.6 -Bull. B PM-y (sec. of arc)
        eop['dut1______B'] = float(line[155-1:165]) # F11.7 -Bull. B UT1-UTC (sec. of time)
        eop['dpsi__nut_B'] = float(line[166-1:175]) # F10.3 -Bull. B dPSI (msec. of arc)
        eop['deps__nut_B'] = float(line[176-1:185]) # F10.3 -Bull. B dEPSILON (msec. of arc)

        return(eop)

    
    ### Determine the MJD at 00:00:00 
    jd, jdfrac = jday(year,mon,day,0,0,0)
    mjd  = jd+jdfrac - 2400000.5


    if 'eop_dict' not in globals():   #read_once_flag==False:    
        # print('---Reading EOP data and saving as dict global vars.')

        global eop_dict

        ### Save data to dictionary if its within the range of MJD dates 
        # (end is set to december 2nd 2022)
        eop_dict = {}
        with open(file_iers_iau76eop, 'r') as f:
            for line in f:
                if float(line[8-1  : 15]) < mjd:
                    pass
                elif float(line[8-1  : 15]) > 59915.0:
                    pass
                else:
                    eop_dict[float(line[8-1  : 15])] = line


    line = eop_dict[mjd]
    if float(line[8-1  : 15]) == mjd:
        eop = readline_finals_iau76_data(line)

    else:
        print('teme-2-eci transformation')
        print('   - Val not in dict, reading EOP the long way...')
        #### Scan the EOP file to find the correct day/line
        with open(file_iers_iau76eop, 'r') as f:
            # f.seek(line_no)
            
            for line_no,line in enumerate(f):
                #### Find the correct day
                if float(line[8-1  : 15]) == mjd:
                    eop = readline_finals_iau76_data(line)
                    break





    ### convert arc seconds to radians
    conv  = np.pi / (180.0*3600.0)   
    #                                                        # converted unit
    dut1  = eop['dut1______B']              # delta of ut1-utc      (sec)
    dat   = get_leapseconds(year,mon,day)            # delta of tai-utc      (sec)
    lod   = eop['lod_______A']*1e-3         # excess length of day  (sec)
    xp    = eop['pmx_______B']*conv         # polar motion coeff    (rad)
    yp    = eop['pmy_______B']*conv         # polar motion coeff    (rad)
    ddpsi    = eop['dpsi__nut_B']*1e-3*conv  # dpsi correction       (rad)
    ddeps    = eop['deps__nut_B']*1e-3*conv  # deps correction       (rad)

    return(dut1, dat, lod, xp, yp, ddpsi, ddeps)






def iau1976_precess( ttt, opt ):
    """Account for the effects of precession.

    Calulates the transformation matrix that accounts for the effects
    of precession. Both the 1980 and 2006 theories are handled. 
    Note that the required parameters differ a little.
    
    Args:
        ttt (float):  Julian centuries of tt 
        opt (str):    Method option ('01', '02', '96', '80')

    Outputs:
        prec (3x3 matrix):  Transformation matrix for mod - j2000 (80 only)
        psia (float):       Cannonical precession angle    [rad]    (00 only)
        wa   (float):       Cannonical precession angle    [rad]    (00 only)
        ea   (float):       Cannonical precession angle    [rad]    (00 only)
        xa   (float):       Cannonical precession angle    [rad]    (00 only)

    locals:
        ttt2    - ttt squared 
        ttt3    - ttt cubed 
        zeta    - precession angle                  [rad]
        z       - precession angle                  [rad]
        theta   - precession angle                  [rad]
        oblo    - obliquity value at j2000 epoch 
    
    Comments:
        - Authored,  Jun 2002: David Vallado (matlab)
        - Revision, Feb 2005: Vallado, consolidate with iau 2000  
        - Revision, Jan 2023: Waldron, transcribed to Python     
    """
    ###% " to rad
    convrt = np.pi / (180.0*3600.0)
    ttt2= ttt * ttt
    ttt3= ttt2 * ttt

    prec      = np.array(np.zeros((3,3) ))
    # prec      = np.matrix(np.zeros((3,3) ))

    ###% ------------------- fk4 b1950 precession angles --------------------
    if opt == '50':
        ###% ---- Seidelmann pg 107 ##% for these calls, ttt will come in with
        #the current jd ##%            t1= 0.0; %(ttt -
        #2433282.42345905)/365242.198782;  % set start as B1850, 0.0 to simplify
        ###%            t2= (ttt - 2396758.203)/365242.198782;  % uses B1850 ##%
        #ttt = t2 - t1; ##%            ttt2 = ttt * ttt; ##%            ttt3 =
        #ttt * ttt2; ##%            fprintf(1,'50prec %15.9f  \n',ttt); ##% exp
        #supp 61 pg 38 ##%            psia = 50.3708 + 0.0050 * ttt; ##%
        #wa = 0.0; ##%            ea = 0.0; ##%            xa = 0.1247 - 0.0188
        #* ttt; ##%            zeta =  (23035.545 + 139.720*t1 + 0.060
        #*t1*t1)*ttt + (30.240-0.270*t1)*ttt2 + 17.995*ttt3; % " ##%
        #theta=  (20051.12 - 85.29*t1 - 0.37 *t1*t1)*ttt + (-42.65-0.37*t1)*ttt2
        #- 41.80*ttt3; ##%            z    =  (23035.545 + 139.720*t1 + 0.060
        #*t1*t1)*ttt + (109.480+0.390*t1)*ttt2 + 18.325*ttt3; ##% ---- Newcomb
        #Exp Supp 61 approach, but see GTDS pg 3-17 ##% Exp Supp 61 says use
        #1900? but gtds says use 1950.  ##% for these calls, ttt will come in
        #with the current jd
        t1= 0.0   ###%(ttt - 2415020.31352)/36524.2198782;  % set start as B1900, 0.0 to simplify
        ###%            t2= (ttt - 2415020.31352)/36525;  % uses B1900
        t2= (ttt - 2433282.42345905)/36525   ###% uses B1950
        ###%            ttt = t2 - t1;
        ###%            ttt2 = ttt * ttt;
        ###%            ttt3 = ttt * ttt2;
        ###% exp supp 61 pg 38
        psia = 50.3708 + 0.0050 * ttt
        wa = 0.0  ### not sure which one is which...
        ea = 84428.26 -   46.845*ttt - 0.00059*ttt2 + 0.00181*ttt3
        xa = 0.1247 - 0.0188 * ttt
        # print(f'50prec {ttt:15.9f}  \n')
        ###% seems like Exp supp 61 is ok with 1900 as epoch, and Seidlemann is
        #ok with listed measr, ##%            zeta =  (2304.25 + 1.396*t1)*ttt +
        #0.302*ttt2 + 0.018*ttt3; % " ##%            theta=  (2004.682 -
        #0.853*t1)*ttt -0.426*ttt2 - 0.042*ttt3; ##%            z    =  (2304.25
        #+ 1.396*t1)*ttt + 1.093*ttt2 + 0.018*ttt3;
        ###% GTDS pg 3-17 using days from 1950 - avoids long rpecession
        ###% constants...
        zeta =  2304.9969*ttt + 0.302*ttt2 + 0.01808*ttt3 ###% "
        theta=  2004.2980*ttt -0.425936*ttt2 - 0.0416*ttt3
        z    =  2304.9969*ttt + 1.092999*ttt2 + 0.0192*ttt3

        ### initialize the precession matrix

        ###% tp-008 36-45
        ###% ttt is tropical centruies from 1950 36524.22 days
        prec[0,0] =  1.0 - 2.9696e-4 * ttt2 - 1.3e-7 * ttt3
        prec[0,1] =  2.234941e-2 * ttt + 6.76e-6 * ttt2 - 2.21e-6 * ttt3
        prec[0,2] =  9.7169e-3 * ttt - 2.07e-6 * ttt2 - 9.6e-7 * ttt3
        prec[1,0] =  -1*prec[0,1]
        prec[1,1] =  1.0 - 2.4975e-4 * ttt2 - 1.5e-7 * ttt3
        prec[1,2] =  -1*1.0858e-4 * ttt2
        prec[2,0] =  -1*prec[0,2]
        prec[2,1] =     prec[1,2]
        prec[2,2] =  1.0 - 4.721e-5 * ttt2

        ###% pass these back out for testing
        psia = zeta
        wa = theta
        ea = z
    else:
        ###% ------------------- iau 76 precession angles --------------------
        if opt == '80':
            ###%     fprintf(1,'80prec %15.9f  \n',ttt);
            psia =             5038.7784*ttt - 1.07259*ttt2 - 0.001147*ttt3; ###% "
            wa   = 84381.448                 + 0.05127*ttt2 - 0.007726*ttt3;
            ea   = 84381.448 -   46.8150*ttt - 0.00059*ttt2 + 0.001813*ttt3;
            xa   =               10.5526*ttt - 2.38064*ttt2 - 0.001125*ttt3;

            zeta =             2306.2181*ttt + 0.30188*ttt2 + 0.017998*ttt3; ###% "
            theta=             2004.3109*ttt - 0.42665*ttt2 - 0.041833*ttt3;
            z    =             2306.2181*ttt + 1.09468*ttt2 + 0.018203*ttt3;
            ###% ------------------ iau 06 precession angles -------------------
        else:
            oblo =  84381.406  ###% "
            psia =  (((( -0.0000000951 * ttt + 0.000132851 ) * ttt - 0.00114045 ) * ttt - 1.0790069 ) * ttt + 5038.481507 ) * ttt ###; % "
            wa   =  ((((  0.0000003337 * ttt - 0.000000467 ) * ttt - 0.00772503 ) * ttt + 0.0512623 ) * ttt -    0.025754 ) * ttt + oblo
            ea   =  (((( -0.0000000434 * ttt - 0.000000576 ) * ttt + 0.00200340 ) * ttt - 0.0001831 ) * ttt -   46.836769 ) * ttt + oblo
            xa   =  (((( -0.0000000560 * ttt + 0.000170663 ) * ttt - 0.00121197 ) * ttt - 2.3814292 ) * ttt +   10.556403 ) * ttt

            zeta =  (((( -0.0000003173 * ttt - 0.000005971 ) * ttt + 0.01801828 ) * ttt + 0.2988499 ) * ttt + 2306.083227 ) * ttt + 2.650545 ###;% "
            theta=  (((( -0.0000001274 * ttt - 0.000007089 ) * ttt - 0.04182264 ) * ttt - 0.4294934 ) * ttt + 2004.191903 ) * ttt
            z    =  ((((  0.0000002904 * ttt - 0.000028596 ) * ttt + 0.01826837 ) * ttt + 1.0927348 ) * ttt + 2306.077181 ) * ttt - 2.650545

    ###% convert units to rad
    psia = psia  * convrt ###% rad
    wa   = wa    * convrt
    ea   = ea    * convrt
    xa   = xa    * convrt

    zeta = zeta  * convrt
    theta= theta * convrt
    z    = z     * convrt
    ###%iauhelp='y';
    ###%fprintf(1,'pr %11.7f  %11.7f  %11.7fdeg \n',zeta*180/pi,theta*180/pi,z*180/pi );
    ###if iauhelp == 'y':
    ###fprintf(1,'pr %11.7f  %11.7f  %11.7f %11.7fdeg \n',psia*180/pi,wa*180/pi,ea*180/pi,xa*180/pi );
    ###fprintf(1,'pr %11.7f  %11.7f  %11.7fdeg \n',zeta*180/pi,theta*180/pi,z*180/pi );

    if opt=='80' or opt=='06':
#         print('test')
        coszeta  = np.cos(zeta)
        sinzeta  = np.sin(zeta)
        costheta = np.cos(theta)
        sintheta = np.sin(theta)
        cosz     = np.cos(z)
        sinz     = np.sin(z)

        ###% ----------------- form matrix  mod to j2000 -----------------
        prec[0,0] =  coszeta * costheta * cosz - sinzeta * sinz
        prec[0,1] =  coszeta * costheta * sinz + sinzeta * cosz
        prec[0,2] =  coszeta * sintheta
        prec[1,0] = -sinzeta * costheta * cosz - coszeta * sinz
        prec[1,1] = -sinzeta * costheta * sinz + coszeta * cosz
        prec[1,2] = -sinzeta * sintheta
        prec[2,0] = -sintheta * cosz
        prec[2,1] = -sintheta * sinz
        prec[2,2] =  costheta

        ###% ----------------- do rotations instead ----------------------
        ###%             fprintf(1,'prec matrix \n');
        ###%             prec
        ###%             fprintf(1,'prec rotations z \n');
        ###%             p1 = rot3mat( z );
        ###%             p2 = rot2mat( -theta );
        ###%             p3 = rot3mat( zeta );
        ###%             prec1 = p3*p2*p1
        ###%
        ###%             fprintf(1,'prec rotations w \n');
        ###%             a4  = rot3mat(-xa);
        ###%             a5  = rot1mat(wa);
        ###%             a6  = rot3mat(psia);
        ###%             a7  = rot1mat(-ea);
        ###%             prec2 = a7*a6*a5*a4
    else:  ###%if %(strcmp(opt,'50') ~= 1)
        pass
        ###%             oblo = oblo * convrt; % " to rad
        ###%             a4  = rot3mat(-xa);
        ###%             a5  = rot1mat(wa);
        ###%             a6  = rot3mat(psia);
        ###%             a7  = rot1mat(-oblo);
        ###%             prec3 = a7*a6*a5*a4
#     end
    return(prec,psia,wa,ea,xa)


def iau76_80_loadmat():
    """Loads and initializes nutation matricies from file.
    
    This function initializes the nutation matricies needed for reduction
    calculations. The routine needs the filename of the files as input.
    
    Args:
        none
        nut80.dat   data file

    Outputs:
        iar80 (np.array): Integers for fk5 1980
        rar80 (np.array): Reals for fk5 1980             [rad]
    
    Locals:
        convrt      - conversion factor to degrees
        
    Comments:
        - Authored,  May 2002: David Vallado, (matlab)
        - Revision, Jan 2023: Zach Waldron, transcribed to Python     
    """

    ###% ------------------------  implementation   -------------------
    ###% 0.0001" to rad
    convrt = 0.0001 * np.pi / (180*3600.0)

    #     load nut80.dat;

    nut80_df = pd.read_csv(path_iau06+'/nut80.dat', sep = '\s+',
               names = ['iar80_1','iar80_2','iar80_3','iar80_4','iar80_5',
                        'rar80_1','rar80_2','rar80_3','rar80_4','rar80_5',
                       ]) 

    # iar80 = np.matrix(np.zeros((106,5) ))
    # rar80 = np.matrix(np.zeros((106,5) ))
    iar80 = np.array(np.zeros((106,5) ))
    rar80 = np.array(np.zeros((106,5) ))
    #
    iar80[:,0] = np.transpose(nut80_df['iar80_1'].values)
    iar80[:,1] = np.transpose(nut80_df['iar80_2'].values)
    iar80[:,2] = np.transpose(nut80_df['iar80_3'].values)
    iar80[:,3] = np.transpose(nut80_df['iar80_4'].values)
    iar80[:,4] = np.transpose(nut80_df['iar80_5'].values)
    #
    rar80[:,0] = np.transpose(nut80_df['rar80_1'].values)*convrt
    rar80[:,1] = np.transpose(nut80_df['rar80_2'].values)*convrt
    rar80[:,2] = np.transpose(nut80_df['rar80_3'].values)*convrt
    rar80[:,3] = np.transpose(nut80_df['rar80_4'].values)*convrt
    rar80[:,4] = np.transpose(nut80_df['rar80_5'].values)

    return(iar80,rar80) 





def iau06_delauney_fundarg(ttt, opt):
    """calulates delauany variables and planetary values for several theories
    
    This function calulates the delauany variables and planetary values for
    several theories.
    
    Args:
        ttt (float):   Julian centuries of tt 
        opt (string):  Method option ('06', '02', '96', '80')
    
    Outputs:
        l           - delaunay element               rad 
        ll          - delaunay element               rad 
        f           - delaunay element               rad 
        d           - delaunay element               rad 
        omega       - delaunay element               rad 
        planetary longitudes                         rad
    
    locals:
        ttt2,ttt3,  - powers of ttt
    
    Comments:
        - Authored,  Jul 2004: David Vallado, (matlab)
        - Revision, Feb 2005: David Vallado, consolidate with iau 2000
        - Revision, Jan 2023: Zach Waldron, transcribed to Python     
    """
    
    deg2rad = np.pi/180.0

    ### ---- determine coefficients for iau 2000 nutation theory ----
    ttt2 = ttt*ttt
    ttt3 = ttt2*ttt
    ttt4 = ttt2*ttt2

    ### ---- iau 2006 theory
    if opt == '06':
        ### ------ form the delaunay fundamental arguments in deg
        l    =  134.96340251  + ( 1717915923.2178 *ttt + \
                 31.8792 *ttt2 + 0.051635 *ttt3 - 0.00024470 *ttt4 ) / 3600.0
        l1   =  357.52910918  + (  129596581.0481 *ttt - \
                  0.5532 *ttt2 - 0.000136 *ttt3 - 0.00001149*ttt4 )  / 3600.0
        f    =   93.27209062  + ( 1739527262.8478 *ttt - \
                 12.7512 *ttt2 + 0.001037 *ttt3 + 0.00000417*ttt4 )  / 3600.0
        d    =  297.85019547  + ( 1602961601.2090 *ttt - \
                  6.3706 *ttt2 + 0.006593 *ttt3 - 0.00003169*ttt4 )  / 3600.0
        omega=  125.04455501  + (   -6962890.5431 *ttt + \
                  7.4722 *ttt2 + 0.007702 *ttt3 - 0.00005939*ttt4 )  / 3600.0

        ### ------ form the planetary arguments in deg
        lonmer  = 252.250905494  + 149472.6746358  *ttt
        lonven  = 181.979800853  +  58517.8156748  *ttt
        lonear  = 100.466448494  +  35999.3728521  *ttt
        lonmar  = 355.433274605  +  19140.299314   *ttt
        lonjup  =  34.351483900  +   3034.90567464 *ttt
        lonsat  =  50.0774713998 +   1222.11379404 *ttt
        lonurn  = 314.055005137  +    428.466998313*ttt
        lonnep  = 304.348665499  +    218.486200208*ttt
        precrate=   1.39697137214*ttt + 0.0003086*ttt2

    ### ---- iau 2000b theory
    if opt == '02':
        ### ------ form the delaunay fundamental arguments in deg
        l    =  134.96340251  + ( 1717915923.2178 *ttt ) / 3600.0
        l1   =  357.52910918  + (  129596581.0481 *ttt ) / 3600.0
        f    =   93.27209062  + ( 1739527262.8478 *ttt ) / 3600.0
        d    =  297.85019547  + ( 1602961601.2090 *ttt ) / 3600.0
        omega=  125.04455501  + (   -6962890.5431 *ttt ) / 3600.0

        ###% ------ form the planetary arguments in deg
        lonmer  = 0.0
        lonven  = 0.0
        lonear  = 0.0
        lonmar  = 0.0
        lonjup  = 0.0
        lonsat  = 0.0
        lonurn  = 0.0
        lonnep  = 0.0
        precrate= 0.0

    ### ---- iau 1996 theory
    if opt == '96':
        l    =  134.96340251  + ( 1717915923.2178 *ttt + \
                 31.8792 *ttt2 + 0.051635 *ttt3 - 0.00024470 *ttt4 ) / 3600.0
        l1   =  357.52910918  + (  129596581.0481 *ttt - \
                  0.5532 *ttt2 - 0.000136 *ttt3 - 0.00001149*ttt4 )  / 3600.0
        f    =   93.27209062  + ( 1739527262.8478 *ttt - \
                 12.7512 *ttt2 + 0.001037 *ttt3 + 0.00000417*ttt4 )  / 3600.0
        d    =  297.85019547  + ( 1602961601.2090 *ttt - \
                  6.3706 *ttt2 + 0.006593 *ttt3 - 0.00003169*ttt4 )  / 3600.0
        omega=  125.04455501  + (   -6962890.2665 *ttt + \
                  7.4722 *ttt2 + 0.007702 *ttt3 - 0.00005939*ttt4 )  / 3600.0
        ### ------ form the planetary arguments in deg
        lonmer  = 0.0
        lonven  = 181.979800853  +  58517.8156748  *ttt   ###%% deg
        lonear  = 100.466448494  +  35999.3728521  *ttt
        lonmar  = 355.433274605  +  19140.299314   *ttt
        lonjup  =  34.351483900  +   3034.90567464 *ttt
        lonsat  =  50.0774713998 +   1222.11379404 *ttt
        lonurn  = 0.0
        lonnep  = 0.0
        precrate=   1.39697137214*ttt + 0.0003086*ttt2
  

     ### ---- iau 1980 theory
    if opt == '80':
        l = ((((0.064) * ttt + 31.310) * ttt + 1717915922.6330) * ttt) / 3600.0 + 134.96298139
        l1 = ((((-0.012) * ttt - 0.577) * ttt + 129596581.2240) * ttt) / 3600.0 + 357.52772333
        f = ((((0.011) * ttt - 13.257) * ttt + 1739527263.1370) * ttt) / 3600.0 + 93.27191028
        d = ((((0.019) * ttt - 6.891) * ttt + 1602961601.3280) * ttt) / 3600.0 + 297.85036306
        omega = ((((0.008) * ttt + 7.455) * ttt - 6962890.5390) * ttt) / 3600.0 + 125.04452222

        ###% ------ form the planetary arguments in deg
        lonmer  = 252.3 + 149472.0 *ttt
        lonven  = 179.9 +  58517.8 *ttt   ###%% deg
        lonear  =  98.4 +  35999.4 *ttt
        lonmar  = 353.3 +  19140.3 *ttt
        lonjup  =  32.3 +   3034.9 *ttt
        lonsat  =  48.0 +   1222.1 *ttt
        lonurn  =   0.0
        lonnep  =   0.0
        precrate=   0.0

    ###% ---- convert units to rad
    l    = np.fmod( l,360.0  )     * deg2rad ###%% rad
    l1   = np.fmod( l1,360.0  )    * deg2rad
    f    = np.fmod( f,360.0  )     * deg2rad
    d    = np.fmod( d, 360.0  )     * deg2rad
    omega= np.fmod( omega,360.0  ) * deg2rad

    lonmer= np.fmod( lonmer,360.0 ) * deg2rad  ###%% rad
    lonven= np.fmod( lonven,360.0 ) * deg2rad
    lonear= np.fmod( lonear,360.0 ) * deg2rad
    lonmar= np.fmod( lonmar,360.0 ) * deg2rad
    lonjup= np.fmod( lonjup,360.0 ) * deg2rad
    lonsat= np.fmod( lonsat,360.0 ) * deg2rad
    lonurn= np.fmod( lonurn,360.0 ) * deg2rad
    lonnep= np.fmod( lonnep,360.0 ) * deg2rad
    precrate= np.fmod( precrate,360.0 ) * deg2rad
    ###%iauhelp='y';
            ###%if iauhelp == 'y'
            ###%fprintf(1,'fa %11.7f  %11.7f  %11.7f  %11.7f  %11.7f deg \n',l*180/pi,l1*180/pi,f*180/pi,d*180/pi,omega*180/pi );
            ###%fprintf(1,'fa %11.7f  %11.7f  %11.7f  %11.7f deg \n',lonmer*180/pi,lonven*180/pi,lonear*180/pi,lonmar*180/pi );
            ###%fprintf(1,'fa %11.7f  %11.7f  %11.7f  %11.7f deg \n',lonjup*180/pi,lonsat*180/pi,lonurn*180/pi,lonnep*180/pi );
            ###%fprintf(1,'fa %11.7f  \n',precrate*180/pi );
             ###% end;
           ###% test if they are equivalent
           ###% most around 1e-10, but some at 1e-6
    ###%         oo3600 = 1.0 / 3600.0;
    ###%         deg2rad = pi / 180.0;
    ###%         ttt = 0.34698738576;
    ###%         twopi = 2.0 * pi;
    ###%         lonmer = mod((908103.259872 + 538101628.688982 * ttt) * oo3600,360)*deg2rad;
    ###%         lonven = mod((655127.283060 + 210664136.433548 * ttt) * oo3600,360)*deg2rad;
    ###%         lonear = mod((361679.244588 + 129597742.283429 * ttt) * oo3600,360)*deg2rad;
    ###%         lonmar = mod((1279558.798488 + 68905077.493988 * ttt) * oo3600,360)*deg2rad;
    ###%         lonjup = mod((123665.467464 + 10925660.377991 * ttt) * oo3600,360)*deg2rad;
    ###%         lonsat = mod((180278.799480 + 4399609.855732 * ttt) * oo3600,360)*deg2rad;
    ###%         lonurn = mod((1130598.018396 + 1542481.193933 * ttt) * oo3600,360)*deg2rad;
    ###%         lonnep = mod((1095655.195728 + 786550.320744 * ttt) * oo3600,360)*deg2rad;
    ###%         precrate = ((1.112022 * ttt + 5028.8200) * ttt) * oo3600*deg2rad;
    ###% 
    ###%         lonmer1 = mod (4.402608842 + 2608.7903141574 * ttt , twopi);
    ###%         lonven1 = mod (3.176146697 + 1021.3285546211 * ttt , twopi);
    ###%         lonear1 = mod(1.753470314 + 628.3075849991 * ttt , twopi);
    ###%         lonmar1 = mod(6.203480913 + 334.0612426700 * ttt , twopi);
    ###%         lonjup1 = mod(0.599546497 + 52.9690962641 * ttt , twopi);
    ###%         lonsat1 = mod(0.874016757 + 21.3299104960 * ttt , twopi);
    ###%         lonurn1 = mod(5.481293872 + 7.4781598567 * ttt , twopi);
    ###%         lonnep1 = mod(5.311886287 + 3.8133035638 * ttt , twopi);
    ###%         precrate1 = (0.024381750 + 0.00000538691 * ttt ) *ttt;
    ###% 
    ###%         lonmer-lonmer1
    ###%         lonven-lonven1
    ###%         lonear-lonear1
    ###%         lonmar-lonmar1
    ###%         lonjup-lonjup1
    ###%         lonsat-lonsat1
    ###%         lonurn-lonurn1
    ###%         lonnep-lonnep1
    ###%         precrate-precrate1
    ###% 
    ###% 
    return(l, l1, f, d, omega,lonmer, lonven, lonear, lonmar, lonjup, lonsat, lonurn, lonnep, precrate)





def iau1976_nutation(ttt, ddpsi, ddeps):
    """Account for effects of nutation using the IAU 1976/FK5 Theory.
       
    This function calulates the transformation matrix that accounts for the
    effects of nutation.
    
    Args:
        ttt   (float)  Julian centuries of tt
        ddpsi (float)  Delta psi correction to GCRF   rad
        ddeps (float)  Delta eps correction to GCRF   rad
    
    Outputs:
        deltapsi (float)      Nutation angle                  rad
        trueeps  (float)      True obliquity of the ecliptic  rad
        meaneps  (float)      Mean obliquity of the ecliptic  rad
        omega    (float)                                      rad
        nut      (np.matrix)  Transformation matrix for tod - mod
    
    locals:
        iar80       - integers for fk5 1980
        rar80       - reals for fk5 1980
        ttt2        - ttt squared
        ttt3        - ttt cubed
        l           -                                rad
        ll          -                                rad
        f           -                                rad
        d           -                                rad
        deltaeps    - change in obliquity            rad
    
    Comments:
        - Authored,  Jun 2002: David Vallado, (matlab)
        - Revision, Feb 2005: David Vallado, consolidate with iau 2000
        - Revision, Jan 2023: Zach Waldron, transcribed to Python     
    """
    deg2rad = np.pi/180.0

    iar80,rar80 = iau76_80_loadmat()  ###% coeff in deg

    ###% ---- determine coefficients for iau 1980 nutation theory ----
    ttt2= ttt*ttt
    ttt3= ttt2*ttt

    meaneps = -46.8150 *ttt - 0.00059 *ttt2 + 0.001813 *ttt3 + 84381.448
    meaneps = np.fmod( meaneps/3600.0, 360.0 )
    meaneps = meaneps * deg2rad

    l, l1, f, d, omega,lonmer, lonven, \
    lonear, lonmar, lonjup, lonsat,    \
    lonurn, lonnep, precrate            = iau06_delauney_fundarg( ttt, '80' )
    # print(f'NUTATION del arg {l*180/np.pi:11.7f}  {l1*180/np.pi:11.7f}  {f*180/np.pi:11.7f}  {d*180/np.pi:11.7f}  {omega*180/np.pi:11.7f} \n');

    deltapsi= 0.0
    deltaeps= 0.0


    for i in np.arange(105,-1,-1):  # range to the zeroth index, backwards

    #     print(i)
        tempval= iar80[i,0]*l + iar80[i,1]*l1 + iar80[i,2]*f + \
                 iar80[i,3]*d + iar80[i,4]*omega
        deltapsi= deltapsi + (rar80[i,0]+rar80[i,1]*ttt) * np.sin( tempval )
        deltaeps= deltaeps + (rar80[i,2]+rar80[i,3]*ttt) * np.cos( tempval )
    
    # print('deltapsi',deltapsi)
    ###% --------------- find nutation parameters --------------------
    deltapsi = np.fmod( deltapsi + ddpsi, 2.0 * np.pi )
    deltaeps = np.fmod( deltaeps + ddeps, 2.0 * np.pi )
    trueeps  = meaneps + deltaeps

    # print(f'meaneps {meaneps*180/np.pi:11.7f} dp  {deltapsi*180/np.pi:11.7f} de  {deltaeps*180/np.pi:11.7f} te  {trueeps*180/np.pi:11.7f}  ttt  {ttt:11.7f} \n')

    cospsi  = np.cos(deltapsi)
    sinpsi  = np.sin(deltapsi)
    coseps  = np.cos(meaneps)
    sineps  = np.sin(meaneps)
    costrueeps = np.cos(trueeps)
    sintrueeps = np.sin(trueeps)

    # nut = np.matrix(np.zeros((3,3) ))
    nut = np.array(np.zeros((3,3) ))

    nut[0,0] =  cospsi
    nut[0,1] =  costrueeps * sinpsi
    nut[0,2] =  sintrueeps * sinpsi
    nut[1,0] = -1*coseps * sinpsi
    nut[1,1] =  costrueeps * coseps * cospsi + sintrueeps * sineps
    nut[1,2] =  sintrueeps * coseps * cospsi - sineps * costrueeps
    nut[2,0] = -1*sineps * sinpsi
    nut[2,1] =  costrueeps * sineps * cospsi - sintrueeps * coseps
    nut[2,2] =  sintrueeps * sineps * cospsi + costrueeps * coseps

###%         fprintf(1,'nut matrix \n');
###%         nut
###%         fprintf(1,'nut rotations \n');
###%         n1 = rot1mat( trueeps );
###%         n2 = rot3mat( deltapsi );
###%         n3 = rot1mat( -meaneps );
###%         nut1 = n3*n2*n1


    return(deltapsi, trueeps, meaneps, omega, nut)




def iau1976_gstime(jdut1):
    """
    %                           function gstime
    %
    %  this function finds the greenwich sidereal time (iau-82).
    %
    %  author        : david vallado                  719-573-2600    7 jun 2002
    %
    %  revisions
    %                -
    %
    %  inputs          description                    range / units
    %    jdut1       - julian date of ut1             days from 4713 bc
    %
    %  outputs       :
    %    gst         - greenwich sidereal time        0 to 2pi rad
    %
    %  locals        :
    %    temp        - temporary variable for reals   rad
    %    tut1        - julian centuries from the
    %                  jan 1, 2000 12 h epoch (ut1)
    %
    %  coupling      :
    %
    %  references    :
    %    vallado       2007, 193, Eq 3-43
    %
    % gst = gstime(jdut1);
    % -----------------------------------------------------------------------------
    """

    twopi      = 2.0*np.pi
    deg2rad    = np.pi/180.0

    ###% ------------------------  implementation   ------------------
    tut1= ( jdut1 - 2451545.0 ) / 36525.0

    temp = - 6.2e-6 * tut1 * tut1 * tut1 + 0.093104 * tut1 * tut1  \
           + (876600.0 * 3600.0 + 8640184.812866) * tut1 + 67310.54841

    ###% 360/86400 = 1/240, to deg, to rad
    temp = np.fmod( temp*deg2rad/240.0,twopi )

    ###% ------------------------ check quadrants --------------------
    if  temp < 0.0 :
        temp = temp + twopi

    gst = temp

    return(gst)







def iau1976_sidereal(jdut1, deltapsi, meaneps, omega, lod, eqeterms ):
    """% ----------------------------------------------------------------------------
    %
    %                           function sidereal
    %
    %  this function calulates the transformation matrix that accounts for the
    %    effects of sidereal time. Notice that deltaspi should not be mod'ed to a
    %    positive number because it is multiplied rather than used in a
    %    trigonometric argument.
    %
    %  author        : david vallado                  719-573-2600   25 jun 2002
    %
    %  revisions
    %    vallado     - fix units on kinematic terms                   5 sep 2002
    %    vallado     - add terms                                     30 sep 2002
    %    vallado     - consolidate with iau 2000                     14 feb 2005
    %
    %  inputs          description                    range / units
    %    jdut1       - julian centuries of ut1        days
    %    deltapsi    - nutation angle                 rad
    %    meaneps     - mean obliquity of the ecliptic rad
    %    omega       - long of asc node of moon       rad
    %    lod         - length of day                  sec
    %    eqeterms    - terms for ast calculation      0,2
    %
    %  outputs       :
    %    st          - transformation matrix for pef - tod
    %    stdot       - transformation matrix for pef - tod rate
    %
    %  locals        :
    %    gmst        - mean greenwich sidereal time   0 to 2pi rad
    %    ast         - apparent gmst                   0 to 2pi rad
    %    hr          - hour                           hr
    %    min         - minutes                        min
    %    sec         - seconds                        sec
    %    temp        - temporary vector
    %    tempval     - temporary variable
    %
    %  coupling      :
    %
    %  references    :
    %    vallado       2013, 223-224
    %
    % [st, stdot]  = sidereal (jdut1, deltapsi, meaneps, omega, lod, eqeterms );
    % ----------------------------------------------------------------------------
    """

    
    ###% ------------------------ find gmst --------------------------
    gmst = iau1976_gstime( jdut1 )

    ###% ------------------------ find mean ast ----------------------
    ###% after 1997, kinematic terms apply as well as gemoetric in eqe
    if jdut1 > 2450449.5  and eqeterms > 0:
        # print('sidereal-- jdut1 > 2450449.5  and eqeterms > 0')
        ast= gmst + deltapsi* np.cos(meaneps) \
            + 0.00264*np.pi /(3600*180)*np.sin(omega) \
            + 0.000063*np.pi /(3600*180)*np.sin(2.0 *omega)
    else:
        # print('sidereal-- else')
        ast= gmst + deltapsi* np.cos(meaneps)
    

    ast = np.fmod(ast, 2.0*np.pi)
    thetasa    = 7.29211514670698e-05 * (1.0  - lod/86400.0 )
    omegaearth = thetasa

    # print(f'st gmst {gmst*180/np.pi:11.8f} ast {ast*180/np.pi:11.8f} ome  {omegaearth*180/np.pi:11.8f} \n')

    # st = np.matrix(np.zeros((3,3) ))
    # stdot = np.matrix(np.zeros((3,3) ))
    st = np.array(np.zeros((3,3) ))
    stdot = np.array(np.zeros((3,3) ))

    st[0,0] =  np.cos(ast)
    st[0,1] = -1*np.sin(ast)
    st[0,2] =  0.0
    st[1,0] =  np.sin(ast)
    st[1,1] =  np.cos(ast)
    st[1,2] =  0.0
    st[2,0] =  0.0
    st[2,1] =  0.0
    st[2,2] =  1.0

    
    ###% compute sidereal time rate matrix
    stdot[0,0] = -1*omegaearth * np.sin(ast)
    stdot[0,1] = -1*omegaearth * np.cos(ast)
    stdot[0,2] =  0.0
    stdot[1,0] =    omegaearth * np.cos(ast)
    stdot[1,1] = -1*omegaearth * np.sin(ast)
    stdot[1,2] =  0.0
    stdot[2,0] =  0.0
    stdot[2,1] =  0.0
    stdot[2,2] =  0.0
        
    return(st,stdot)



def iau1976_Convert_ecef2eci( recef,vecef,aecef,ttt,jdut1,lod,xp,yp,eqeterms,ddpsi,ddeps ):
    """Convert from ECEF frame to ECI frame using IAU-76/FK5.
    
    This function transforms a vector from the earth fixed (ECEF, ITRF) frame,
    to the ECI mean equator mean equinox (j2000) frame.
    
        
    Args:
        recef (3x1, np.array):  Position vector earth fixed     (km)
        vecef (3x1, np.array):  Velocity vector earth fixed     (km/s)
        aecef (3x1, np.array):  Acceleration vector earth fixed (km/s**2)
        ttt      (float):  Julian centuries of tt               (centuries)
        jdut1    (float):  Julian date of ut1                (days from 4713 bc)
        lod      (float):  Excess length of day                 (sec)
        xp       (float):  Polar motion coefficient             (rad)
        yp       (float):  Polar motion coefficient             (rad)
        eqeterms (float):  Terms for ast calculation            (0,2)
        ddpsi    (float):  Delta psi correction to gcrf         (rad)
        ddeps    (float):  Delta eps correction to gcrf         (rad)
    
      Outputs
        reci (3x1, np.array):  Position vector eci              (km)
        veci (3x1, np.array):  Velocity vector eci              (km/s)
        aeci (3x1, np.array):  Acceleration vector eci          (km/s2)
    
      locals:
        deltapsi    - nutation angle                 rad
        trueeps     - true obliquity of the ecliptic rad
        meaneps     - mean obliquity of the ecliptic rad
        omega       -                                rad
        prec        - matrix for mod - eci 
        nut         - matrix for tod - mod 
        st          - matrix for pef - tod 
        stdot       - matrix for pef - tod rate
        pm          - matrix for ecef - pef 
    
    Comments:
        - Authored,  Sep 2002: David Vallado, (matlab)  
        - Revision, Sep 2002: David Vallado, add terms for AST calculation              
        - Revision, Feb 2005: David Vallado, consolidate with iau 2000  
        - Revision, Jan 2023: Zach Waldron, transcribed to Python    

    """

    ###% ---- find matrices
    prec,psia,wa,ea,xa = iau1976_precess( ttt, '80' )

    deltapsi,trueeps,meaneps,omega,nut = iau1976_nutation(ttt,ddpsi,ddeps)

    st,stdot = iau1976_sidereal(jdut1,deltapsi,meaneps,omega,lod,eqeterms )

    pm = iau06_polarmotion(xp,yp,ttt,'80')

    ###% ---- perform transformations
    thetasa= 7.29211514670698e-05 * (1.0  - lod/86400.0 )
    omegaearth = np.array([0, 0, thetasa])[:,None]

    ### "@" is matrix multiplication
    rpef = pm@recef
    reci = prec@nut@st@rpef

    vpef = pm @ vecef
    veci = prec@nut@st@(vpef + np.cross(omegaearth,rpef, axis=0))

    # print(f'   r_pef ',rpef)
    # print(f'   v_pef ',vpef)
    ###% veci1 = prec*nut * (stdot*recef + st*pm*vecef)  % alt approach using
    #sidereal rate


    temp = np.cross(omegaearth,rpef, axis=0)
    ###% two additional terms not needed if satellite is not on surface
    ###% of the Earth
    aeci = prec@nut@st@( pm@aecef ) \
            + np.cross(omegaearth,temp, axis=0) \
            + 2.0*np.cross(omegaearth,vpef, axis=0)

    
    return(reci,veci,aeci) 
            






def teme2eci(rteme, vteme, ateme, ttt, ddpsi, ddeps, calc_accel = False):
    """Transforms from ECI-TEME to ECI-J2000

    This function transforms a vector from the true equator mean equinox system,
    (teme) to the mean equator mean equinox (j2000) system.

      author        : david vallado                  719-573-2600   30 oct 2017

      inputs          description                    range / units
        rteme       - position vector of date
                        true equator, mean equinox   km
        vteme       - velocity vector of date
                        true equator, mean equinox   km/s
        ateme       - acceleration vector of date (if available, else set to 0)
                        true equator, mean equinox   km/s2
        ttt         - julian centuries of tt         centuries
        ddpsi       - delta psi correction to gcrf   rad
        ddeps       - delta eps correction to gcrf   rad

      outputs:
        reci        - position vector eci            km
        veci        - velocity vector eci            km/s
        aeci        - acceleration vector eci        km/s2

      locals:
        prec        - matrix for eci - mod
        nutteme     - matrix for mod - teme - an approximation for nutation
        eqeg        - rotation for equation of equinoxes (geometric terms only)
        tm          - combined matrix for teme2eci

      coupling:
       precess      - rotation for precession        eci - mod
       nutation     - rotation for nutation          eci - tod
    """

    
    rteme = np.asarray(rteme)[:,None]
    vteme = np.asarray(vteme)[:,None]

    prec, psia, wa, ea, xa = iau1976_precess( ttt, '80' )

    deltapsi, trueeps, meaneps, omega, nut = iau1976_nutation(ttt, ddpsi, ddeps )

    ### ------------------------ find eqeg ----------------------
    ### rotate teme through just geometric terms 
    eqeg = deltapsi * np.cos(meaneps)

    eqeg = np.fmod(eqeg, 2.0*np.pi)

    eqe    = np.array(np.zeros( (3,3) ) )

    
    eqe[0,0] =    np.cos(eqeg)
    eqe[0,1] =    np.sin(eqeg)
    eqe[0,2] =    0.0
    eqe[1,0] = -1*np.sin(eqeg)
    eqe[1,1] =    np.cos(eqeg)
    eqe[1,2] =    0.0
    eqe[2,0] =    0.0
    eqe[2,1] =    0.0
    eqe[2,2] =    1.0

    tm = prec @ nut @ eqe.conj().transpose()

    reci = tm @ rteme
    veci = tm @ vteme
    if calc_accel:
        aeci = tm @ ateme
        return(reci, veci, aeci)
    else:
        return(reci, veci, None)







            
            
def iau06_polarmotion( xp, yp, ttt, opt ):
    """Account for polar motion

    This function calulates the transformation matrix that accounts for polar
    motion. Both the 1980 and 2000 theories are handled. 
    Note that the rotation order is different between 1980 and 2000.
    
    Args:          
        xp  (float)  Polar motion coefficient       (rad)
        yp  (float)  Polar motion coefficient       (rad)
        ttt (float)  Julian centuries of tt         (00 theory only)
        opt (float)  Method option                  ('01', '02', '80')
    
    Outputs:
        pm (np.matrix)  Transformation matrix for ecef - pef
    
    locals:
        convrt      - conversion from arcsec to rad
        sp          - s prime value
    
    Comments:
        - Authored,  Jun 2002: David Vallado, (matlab)
        - Revision, Feb 2005: David Vallado, consolidate with iau 2000
        - Revision, Jan 2023: Zach Waldron, transcribed to Python     
    """


    cosxp = np.cos(xp)
    sinxp = np.sin(xp)
    cosyp = np.cos(yp)
    sinyp = np.sin(yp)

    # pm    = np.matrix(np.zeros((3,3) ))
    pm    = np.array(np.zeros((3,3) ))

    if (opt == '80'):
        pm[0,0] =  cosxp
        pm[0,1] =  0.0
        pm[0,2] = -sinxp
        pm[1,0] =  sinxp * sinyp
        pm[1,1] =  cosyp
        pm[1,2] =  cosxp * sinyp
        pm[2,0] =  sinxp * cosyp
        pm[2,1] = -sinyp
        pm[2,2] =  cosxp * cosyp

    else:
        convrt = np.pi / (3600.0*180.0)
        ###% approximate sp value in rad
        sp = -47.0e-6 * ttt * convrt
        cossp = np.cos(sp)
        sinsp = np.sin(sp)

        ###% form the matrix
        pm[0,0] =  cosxp * cossp
        pm[0,1] = -cosyp * sinsp + sinyp * sinxp * cossp
        pm[0,2] = -sinyp * sinsp - cosyp * sinxp * cossp
        pm[1,0] =  cosxp * sinsp
        pm[1,1] =  cosyp * cossp + sinyp * sinxp * sinsp
        pm[1,2] =  sinyp * cossp - cosyp * sinxp * sinsp
        pm[2,0] =  sinxp
        pm[2,1] = -sinyp * cosxp
        pm[2,2] =  cosyp * cosxp

    return(pm)


def iau06_load_nut_mats():
    """Loads and initializes IAU-2006 nutation matricies from a file.
    
      This function initializes the matricies needed for iau 2006 reduction
        calculations. The routine uses the files listed as inputs, but they are
        are not input to the routine as they are static files.
    
    Args:
        none
        iau00x.dat  - file for x coefficient
        iau00y.dat  - file for y coefficient
        iau00s.dat  - file for s coefficient
        iau00n.dat  - file for nutation coefficients
        iau00pl.dat (notused) - file for planetary nutation coefficients
        iau00gs.dat - file for gmst coefficients
    
    Outputs:
        axs0        - real coefficients for x           rad
        a0xi        - integer coefficients for x
        ays0        - real coefficients for y           rad
        a0yi        - integer coefficients for y
        ass0        - real coefficients for s           rad
        a0si        - integer coefficients for s
        apn         - real coefficients for nutation    rad
        apni        - integer coefficients for nutation
        ape         - real coefficients for obliquity   rad
        apei        - integer coefficients for obliquity
        agst        - real coefficients for gst         rad
        agsti       - integer coefficients for gst
    
    locals:
        convrt      - conversion factor to radians
        i           - index

    Comments:
        - Authored, Jul 2004: David Vallado, written in MATLAB
        - Revision, Apr 2011: David Vallado, update for iau2006 conventions
        - Revision, Jan 2023: Zach Waldron,  transcribed to Python     
    """

    ### convert arcseconds to rad
    convrtu= (0.000001*np.pi) /(180.0*3600.0);  ###% if micro arcsecond
    convrtm= (0.001*np.pi) /(180.0*3600.0);     ###% if milli arcsecond

    ### -----------------------------------------------------------------
    ###  note that since all these coefficients have only a single
    ###  decimal place, one could store them as integres, and then simply
    ###  divide by one additional power of ten. it would make memeory
    ###  storage much smaller and potentially faster.
    ### -----------------------------------------------------------------

    ### Load the X values
    #
    iau06x_df = pd.read_csv(path_iau06+'/iau06x.dat', sep = '\s+',header=None)
    nums_a0xi = np.arange(3,17)
    # axs0 = np.matrix(np.zeros( (iau06x_df.shape[0],2) ))
    # a0xi = np.matrix(np.zeros( (iau06x_df.shape[0],len(nums_a0xi)) ))
    axs0 = np.array(np.zeros( (iau06x_df.shape[0],2) ))
    a0xi = np.array(np.zeros( (iau06x_df.shape[0],len(nums_a0xi)) ))
    
    ### X Reals
    # axs0[:,0] = iau06x_df[1].values[:,None]*convrtu   #rad
    # axs0[:,1] = iau06x_df[2].values[:,None]*convrtu   #rad
    axs0[:,0] = iau06x_df[1].values*convrtu   #rad
    axs0[:,1] = iau06x_df[2].values*convrtu   #rad
    ### X Integers
    for i in nums_a0xi:
        a0xi[:,i-3] = iau06x_df[i].values     #rad
    del nums_a0xi, iau06x_df

    ### Load the Y values
    ###
    iau06y_df = pd.read_csv(path_iau06+'/iau06y.dat', sep = '\s+',header=None)
    nums_a0yi = np.arange(3,17)
    # ays0 = np.matrix(np.zeros( (iau06y_df.shape[0],2) ))
    # a0yi = np.matrix(np.zeros( (iau06y_df.shape[0],len(nums_a0yi)) ))
    ays0 = np.array(np.zeros( (iau06y_df.shape[0],2) ))
    a0yi = np.array(np.zeros( (iau06y_df.shape[0],len(nums_a0yi)) ))
    ### Y Reals
    ays0[:,0] = iau06y_df[1].values*convrtu   #rad
    ays0[:,1] = iau06y_df[2].values*convrtu   #rad
    ### Y Integers
    for i in nums_a0yi:
        a0yi[:,i-3] = iau06y_df[i].values     #rad
    del nums_a0yi, iau06y_df

    ### Load the S values
    ###
    iau06s_df = pd.read_csv(path_iau06+'/iau06s.dat', sep = '\s+',header=None)
    nums_a0si = np.arange(3,17)
    # ass0 = np.matrix(np.zeros( (iau06s_df.shape[0],2) ))
    # a0si = np.matrix(np.zeros( (iau06s_df.shape[0],len(nums_a0si)) ))
    ass0 = np.array(np.zeros( (iau06s_df.shape[0],2) ))
    a0si = np.array(np.zeros( (iau06s_df.shape[0],len(nums_a0si)) ))
    ### s Reals
    ass0[:,0] = iau06s_df[1].values*convrtu   #rad
    ass0[:,1] = iau06s_df[2].values*convrtu   #rad
    ### s Integers
    for i in nums_a0si:
        a0si[:,i-3] = iau06s_df[i].values     #rad
    del nums_a0si, iau06s_df


    ###% nutation values old approach iau2003
    # load iau03n.dat;
    # apni = iau03n(:,1:5);
    # apn  = iau03n(:,7:14);
    # for i=1:size(apn)
    #     apn(i,1)= apn(i,1) * convrtm;
    #     apn(i,2)= apn(i,2) * convrtm;
    #     apn(i,3)= apn(i,3) * convrtm;
    #     apn(i,4)= apn(i,4) * convrtm;
    #     apn(i,5)= apn(i,5) * convrtm;
    #     apn(i,6)= apn(i,6) * convrtm;
    #     apn(i,7)= apn(i,7) * convrtm;
    #     apn(i,8)= apn(i,8) * convrtm;
    # end;

    # ###% planetary nutation values
    # load iau03pl.dat;
    # appli = iau03pl(:,2:15);
    # appl  = iau03pl(:,17:21);  ###% 21 is extra
    # for i=1:size(appl)
    #     appl(i,1)= appl(i,1) * convrtm;
    #     appl(i,2)= appl(i,2) * convrtm;
    #     appl(i,3)= appl(i,3) * convrtm;
    #     appl(i,4)= appl(i,4) * convrtm;
    # end;


    # ###% nutation values planetary now included new iau2006
    # ###%       load iau00n.dat;  ###% luni-solar
    # ###%       apn  = iau00n(:,2:3);
    # ###%       apni   = iau00n(:,4:17);
    # ###%       for i=1:size(apn)
    # ###%           apn(i,1)= apn(i,1) * convrtu;
    # ###%           apn(i,2)= apn(i,2) * convrtu;
    # ###%       end;
    # ###%
    # ###%       load iau00e.dat;  ###% planetary
    # ###%       ape  = iau00n(:,2:3);
    # ###%       apei   = iau00n(:,4:17);
    # ###%       for i=1:size(ape)
    # ###%           ape(i,1)= ape(i,1) * convrtu;
    # ###%           ape(i,2)= ape(i,2) * convrtu;


    ### Load the GMST Values
    ###
    # iau06gs_df = pd.read_csv(path_iau06+'/iau06gs.dat', sep = '\s+',header=None)
    # nums_agst  = np.arange(3,17)
    # # agst  = np.matrix(np.zeros( (iau06gs_df.shape[0],2) ))
    # # agsti = np.matrix(np.zeros( (iau06gs_df.shape[0],len(nums_agst)) ))
    # agst  = np.array(np.zeros( (iau06gs_df.shape[0],2) ))
    # agsti = np.array(np.zeros( (iau06gs_df.shape[0],len(nums_agst)) ))
    # ### Reals
    # agst[:,0] = iau06gs_df[1].values[:,None]*convrtu   #rad
    # agst[:,1] = iau06gs_df[2].values[:,None]*convrtu   #rad
    # ### Integers
    # for i in nums_agst:
    #     agsti[:,i-3] = iau06gs_df[i].values[:,None]     #rad
    # del nums_agst, iau06gs_df

    return(axs0, a0xi, ays0, a0yi, ass0, a0si)#, agst, agsti)





# read_once_flag = False
def iau06_precess_nutate_xys(ttt, ddx, ddy):
    """Account for the effects of precession-nutation with IAU-2006/2000, CIO
        
    This function calulates the transformation matrix that accounts for the
    effects of precession-nutation in the IAU-2006/2000, CIO based theory.
        
    Args:         
        ttt   (float): Julian centuries of tt
        ddx   (float): EOP correction for x           rad
        ddy   (float): EOP correction for y           rad
    
    outputs:
        nut (3x3 np.matrix): Transformation matrix for ire-gcrf
        x   (float):         Coordinate of Celestial Intermediate Pole (rad)
        y   (float):         Coordinate of CIP              (rad)
        s   (float):         Coordinate                     (rad)
    
    locals:
        axs0        - real coefficients for x        rad
        a0xi        - integer coefficients for x
        ays0        - real coefficients for y        rad
        a0yi        - integer coefficients for y
        ass0        - real coefficients for s        rad
        a0si        - integer coefficients for s
        apn         - real coefficients for nutation rad
        apni        - integer coefficients for nutation
        appl        - real coefficients for planetary nutation rad
        appli       - integer coefficients for planetary nutation
        ttt2,ttt3,  - powers of ttt
        l           - delaunay element               rad
        ll          - delaunay element               rad
        f           - delaunay element               rad
        d           - delaunay element               rad
        omega       - delaunay element               rad
        deltaeps    - change in obliquity            rad
        many others
    
    coupling:
        iau00in     - initialize the arrays
        fundarg     - find the fundamental arguments
    
    Comments:
        - Authored, Jul 2004: David Vallado, written in MATLAB
        - Revision, Feb 2005: David Vallado, consolidate with iau 2000
        - Revision, Jan 2023: Zach Waldron,  transcribed to Python     
    """
    
    #### FROM ZACH--- somewhere in this function is giving a slight innaccuracy
    #                 for X and Y
    
    
    ### " to rad
    convrt  = np.pi / (180.0*3600.0)
    deg2rad = np.pi / 180.0

    ttt2 = ttt  * ttt
    ttt3 = ttt2 * ttt
    ttt4 = ttt2 * ttt2
    ttt5 = ttt3 * ttt2

    if 'axs0' not in globals():   #read_once_flag==False:    
        # print('---Reading nutation data and saving as global vars.')

        global axs0
        global a0xi
        global ays0
        global a0yi
        global ass0
        global a0si

        (axs0, a0xi,\
        ays0, a0yi,\
        ass0, a0si)  = iau06_load_nut_mats()
        # agst, agsti) = iau06_load_nut_mats()

        # read_once_flag = True
    else:
        pass
    
    ### Need something to tell program to only run the above if the lists don't exists


    # import sys
    # sys.exit


    opt = '06'  ### 02 - 2000a, 96 - 1996 theory, 80-1980 theory
    l, l1, f, d, omega,     \
    lonmer, lonven, lonear, \
    lonmar, lonjup, lonsat, \
    lonurn, lonnep, precrate = iau06_delauney_fundarg( ttt, opt )
    #
#     print(f'delauany')
#     print(f'  l     {l/deg2rad:11.7f}    ')
#     print(f'  l1    {l1/deg2rad:11.7f}   ')
#     print(f'  f     {f/deg2rad:11.7f}    ')
#     print(f'  d     {d/deg2rad:11.7f}    ')
#     print(f'  omega {omega/deg2rad:11.7f}')
#     print(f'\n   ')
    
#     print(f' planetary')
#     print(f'   lonmer   {lonmer/deg2rad:11.7f}   ')
#     print(f'   lonven   {lonven/deg2rad:11.7f}   ')
#     print(f'   lonear   {lonear/deg2rad:11.7f}   ')
#     print(f'   lonmar   {lonmar/deg2rad:11.7f}   ')
#     print(f'   lonjup   {lonjup/deg2rad:11.7f}   ')
#     print(f'   lonsat   {lonsat/deg2rad:11.7f}   ')
#     print(f'   lonurn   {lonurn/deg2rad:11.7f}   ')
#     print(f'   lonnep   {lonnep/deg2rad:11.7f}   ')
#     print(f'   precrate {precrate/deg2rad:11.7f} ')


# def add_slow(a0xi,
#         l,l1,f,d,omega,lonmer,lonven,lonear,lonmar,lonjup,lonsat,lonurn,lonnep,precrate):
#     xsum0 = 0.0
#     for i in np.arange(1306-1,-1,-1):  # range to the zeroth index, backwards
#         tempval =  a0xi[i, 1-1]*l      \
#                  + a0xi[i, 2-1]*l1     \
#                  + a0xi[i, 3-1]*f      \
#                  + a0xi[i, 4-1]*d      \
#                  + a0xi[i, 5-1]*omega  \
#                  + a0xi[i, 6-1]*lonmer + a0xi[i, 7-1]*lonven  \
#                  + a0xi[i, 8-1]*lonear + a0xi[i, 9-1]*lonmar  \
#                  + a0xi[i,10-1]*lonjup + a0xi[i,11-1]*lonsat  \
#                  + a0xi[i,12-1]*lonurn + a0xi[i,13-1]*lonnep  \
#                  + a0xi[i,14-1]*precrate
#         #
#         xsum0 = xsum0 + axs0[i,1-1]*np.sin(tempval)+axs0[i,2-1]*np.cos(tempval)
#     return(xsum0)
# import timeit

# def add_fast(a0xi,
#         l,l1,f,d,omega,lonmer,lonven,lonear,lonmar,lonjup,lonsat,lonurn,lonnep,precrate):
#     xsum0 = [axs0[i,1-1]*np.sin(a0xi[i, 1-1]*l      \
#                                      + a0xi[i, 2-1]*l1     \
#                                      + a0xi[i, 3-1]*f      \
#                                      + a0xi[i, 4-1]*d      \
#                                      + a0xi[i, 5-1]*omega  \
#                                      + a0xi[i, 6-1]*lonmer + a0xi[i, 7-1]*lonven  \
#                                      + a0xi[i, 8-1]*lonear + a0xi[i, 9-1]*lonmar  \
#                                      + a0xi[i,10-1]*lonjup + a0xi[i,11-1]*lonsat  \
#                                      + a0xi[i,12-1]*lonurn + a0xi[i,13-1]*lonnep  \
#                                      + a0xi[i,14-1]*precrate )\
#         +axs0[i,2-1]*np.cos(a0xi[i, 1-1]*l      \
#                           + a0xi[i, 2-1]*l1     \
#                           + a0xi[i, 3-1]*f      \
#                           + a0xi[i, 4-1]*d      \
#                           + a0xi[i, 5-1]*omega  \
#                           + a0xi[i, 6-1]*lonmer + a0xi[i, 7-1]*lonven  \
#                           + a0xi[i, 8-1]*lonear + a0xi[i, 9-1]*lonmar  \
#                           + a0xi[i,10-1]*lonjup + a0xi[i,11-1]*lonsat  \
#                           + a0xi[i,12-1]*lonurn + a0xi[i,13-1]*lonnep  \
#                           + a0xi[i,14-1]*precrate  )    for i in np.arange(1306-1,-1,-1)]
#     return(sum(xsum0))

# print(timeit.timeit('add_fast', 'from __main__ import add_fast', number=10**8))
# print(timeit.timeit('add_slow', 'from __main__ import add_slow', number=10**8))



    ### ---------------- first find x
    ### the iers code puts the constants in here, however
    ### don't sum constants in here because theyre larger than last few terms
    xsum0 = 0.0
    for i in np.arange(1306-1,-1,-1):  # range to the zeroth index, backwards
        tempval =  a0xi[i, 1-1]*l      \
                 + a0xi[i, 2-1]*l1     \
                 + a0xi[i, 3-1]*f      \
                 + a0xi[i, 4-1]*d      \
                 + a0xi[i, 5-1]*omega  \
                 + a0xi[i, 6-1]*lonmer + a0xi[i, 7-1]*lonven  \
                 + a0xi[i, 8-1]*lonear + a0xi[i, 9-1]*lonmar  \
                 + a0xi[i,10-1]*lonjup + a0xi[i,11-1]*lonsat  \
                 + a0xi[i,12-1]*lonurn + a0xi[i,13-1]*lonnep  \
                 + a0xi[i,14-1]*precrate
        #
        xsum0 = xsum0 + axs0[i,1-1]*np.sin(tempval)+axs0[i,2-1]*np.cos(tempval)

    xsum1 = 0.0
    ### Note that the index changes here to j. This is because the a0xi etc
    ### indicies go from 1 to 1600, but there are 5 groups. The i index counts through each
    ### calculation, and j takes care of the individual summations. note that
    ### this same process is used for y and s.
    for j in np.arange(253-1,-1,-1): # 253: -1 : 1
        i = 1306 + j 
        #
        tempval =  a0xi[i, 1-1]*l     \
                 + a0xi[i, 2-1]*l1    \
                 + a0xi[i, 3-1]*f     \
                 + a0xi[i, 4-1]*d     \
                 + a0xi[i, 5-1]*omega \
                 + a0xi[i, 6-1]*lonmer + a0xi[i, 7-1]*lonven \
                 + a0xi[i, 8-1]*lonear + a0xi[i, 9-1]*lonmar \
                 + a0xi[i,10-1]*lonjup + a0xi[i,11-1]*lonsat \
                 + a0xi[i,12-1]*lonurn + a0xi[i,13-1]*lonnep \
                 + a0xi[i,14-1]*precrate
        xsum1 = xsum1 + axs0[i,1-1]*np.sin(tempval) + axs0[i,2-1]*np.cos(tempval)

    xsum2 = 0.0
    for j in np.arange(35,-1,-1): #36: -1 : 1
        i = 1306 + 253 + j
        tempval =  a0xi[i, 1-1]*l      \
                 + a0xi[i, 2-1]*l1     \
                 + a0xi[i, 3-1]*f      \
                 + a0xi[i, 4-1]*d      \
                 + a0xi[i, 5-1]*omega  \
                 + a0xi[i, 6-1]*lonmer + a0xi[i, 7-1]*lonven \
                 + a0xi[i, 8-1]*lonear + a0xi[i, 9-1]*lonmar \
                 + a0xi[i,10-1]*lonjup + a0xi[i,11-1]*lonsat \
                 + a0xi[i,12-1]*lonurn + a0xi[i,13-1]*lonnep \
                 + a0xi[i,14-1]*precrate
        xsum2 = xsum2 + axs0[i,1-1]*np.sin(tempval)+axs0[i,2-1]*np.cos(tempval)

    xsum3 = 0.0
    for j in np.arange(4-1,-1,-1): #4: -1 : 1
        i = 1306 + 253 + 36 + j
        tempval =  a0xi[i, 1-1]*l     \
                 + a0xi[i, 2-1]*l1    \
                 + a0xi[i, 3-1]*f     \
                 + a0xi[i, 4-1]*d     \
                 + a0xi[i, 5-1]*omega \
                 + a0xi[i, 6-1]*lonmer + a0xi[i, 7-1]*lonven \
                 + a0xi[i, 8-1]*lonear + a0xi[i, 9-1]*lonmar \
                 + a0xi[i,10-1]*lonjup + a0xi[i,11-1]*lonsat \
                 + a0xi[i,12-1]*lonurn + a0xi[i,13-1]*lonnep \
                 + a0xi[i,14-1]*precrate
        xsum3 = xsum3 + axs0[i,1-1]*np.sin(tempval) + axs0[i,2-1]*np.cos(tempval)

    xsum4 = 0.0
    for j in np.arange(1-1,-1,-1): # 1: -1 : 1
        i = 1306 + 253 + 36 + 4 + j
        tempval =  a0xi[i, 1-1]*l      \
                 + a0xi[i, 2-1]*l1     \
                 + a0xi[i, 3-1]*f      \
                 + a0xi[i, 4-1]*d      \
                 + a0xi[i, 5-1]*omega  \
                 + a0xi[i, 6-1]*lonmer + a0xi[i, 7-1]*lonven \
                 + a0xi[i, 8-1]*lonear + a0xi[i, 9-1]*lonmar \
                 + a0xi[i,10-1]*lonjup + a0xi[i,11-1]*lonsat \
                 + a0xi[i,12-1]*lonurn + a0xi[i,13-1]*lonnep \
                 + a0xi[i,14-1]*precrate
        xsum4 = xsum4+axs0[i, 1-1]*np.sin(tempval)+axs0[i, 2-1]*np.cos(tempval)

    x =  -0.016617            \
        + 2004.191898  * ttt  \
        - 0.4297829    * ttt2 \
        - 0.19861834   * ttt3 \
        - 0.000007578  * ttt4 \
        + 0.0000059285 * ttt5 ## arcseconds
    x =  x*convrt + xsum0      \
                  + xsum1*ttt  \
                  + xsum2*ttt2 \
                  + xsum3*ttt3 \
                  + xsum4*ttt4  ### rad

    
    # ###if iauhelp == 'y'
    # ###    fprintf(1,'xys x %14.12f  %14.12f  %14.12f  %14.12f  %14.12f \n',
    # xsum0/deg2rad,xsum1/deg2rad,xsum2/deg2rad,xsum3/deg2rad,xsum4/deg2rad );
    # ###end;

    ###% ---------------- now find y
    ysum0 = 0.0
    for i in np.arange(962-1,-1,-1): #= 962: -1 : 1
        tempval =  a0yi[i, 1-1]*l      \
                 + a0yi[i, 2-1]*l1     \
                 + a0yi[i, 3-1]*f      \
                 + a0yi[i, 4-1]*d      \
                 + a0yi[i, 5-1]*omega  \
                 + a0yi[i, 6-1]*lonmer + a0yi[i, 7-1]*lonven \
                 + a0yi[i, 8-1]*lonear + a0yi[i, 9-1]*lonmar \
                 + a0yi[i,10-1]*lonjup + a0yi[i,11-1]*lonsat \
                 + a0yi[i,12-1]*lonurn + a0yi[i,13-1]*lonnep \
                 + a0yi[i,14-1]*precrate
        ysum0 = ysum0 + ays0[i, 1-1]*np.sin(tempval) + ays0[i, 2-1]*np.cos(tempval)

    ysum1 = 0.0
    for j in np.arange( 277-1,-1,-1): #= 277: -1 : 1
        i = 962 + j
        tempval =  a0yi[i, 1-1]*l      \
                 + a0yi[i, 2-1]*l1     \
                 + a0yi[i, 3-1]*f      \
                 + a0yi[i, 4-1]*d      \
                 + a0yi[i, 5-1]*omega  \
                 + a0yi[i, 6-1]*lonmer + a0yi[i, 7-1]*lonven \
                 + a0yi[i, 8-1]*lonear + a0yi[i, 9-1]*lonmar \
                 + a0yi[i,10-1]*lonjup + a0yi[i,11-1]*lonsat \
                 + a0yi[i,12-1]*lonurn + a0yi[i,13-1]*lonnep \
                 + a0yi[i,14-1]*precrate
        ysum1 = ysum1 + ays0[i,1-1]*np.sin(tempval) + ays0[i,2-1]*np.cos(tempval)

    ysum2 = 0.0
    for j in np.arange( 30-1,-1,-1): #= 30: -1 : 1
        i = 962 + 277 + j
        tempval =  a0yi[i, 1-1]*l      \
                 + a0yi[i, 2-1]*l1     \
                 + a0yi[i, 3-1]*f      \
                 + a0yi[i, 4-1]*d      \
                 + a0yi[i, 5-1]*omega  \
                 + a0yi[i, 6-1]*lonmer + a0yi[i, 7-1]*lonven \
                 + a0yi[i, 8-1]*lonear + a0yi[i, 9-1]*lonmar \
                 + a0yi[i,10-1]*lonjup + a0yi[i,11-1]*lonsat \
                 + a0yi[i,12-1]*lonurn + a0yi[i,13-1]*lonnep \
                 + a0yi[i,14-1]*precrate
        ysum2 = ysum2 + ays0[i,1-1]*np.sin(tempval) + ays0[i,2-1]*np.cos(tempval)

    ysum3 = 0.0
    for j in np.arange( 5-1,-1,-1): #= 5: -1 : 1
        i = 962 + 277 + 30 + j
        tempval =  a0yi[i, 1-1]*l      \
                 + a0yi[i, 2-1]*l1     \
                 + a0yi[i, 3-1]*f      \
                 + a0yi[i, 4-1]*d      \
                 + a0yi[i, 5-1]*omega  \
                 + a0yi[i, 6-1]*lonmer + a0yi[i, 7-1]*lonven \
                 + a0yi[i, 8-1]*lonear + a0yi[i, 9-1]*lonmar \
                 + a0yi[i,10-1]*lonjup + a0yi[i,11-1]*lonsat \
                 + a0yi[i,12-1]*lonurn + a0yi[i,13-1]*lonnep \
                 + a0yi[i,14-1]*precrate
        ysum3 = ysum3 + ays0[i,1-1]*np.sin(tempval) + ays0[i,2-1]*np.cos(tempval)

    ysum4 = 0.0
    for j in np.arange( 1-1,-1,-1): #= 1: -1 : 1
        i = 962 + 277 + 30 + 5 + j
        tempval =  a0yi[i, 1-1]*l      \
                 + a0yi[i, 2-1]*l1     \
                 + a0yi[i, 3-1]*f      \
                 + a0yi[i, 4-1]*d      \
                 + a0yi[i, 5-1]*omega  \
                 + a0yi[i, 6-1]*lonmer + a0yi[i, 7-1]*lonven \
                 + a0yi[i, 8-1]*lonear + a0yi[i, 9-1]*lonmar \
                 + a0yi[i,10-1]*lonjup + a0yi[i,11-1]*lonsat \
                 + a0yi[i,12-1]*lonurn + a0yi[i,13-1]*lonnep \
                 + a0yi[i,14-1]*precrate
        ysum4 = ysum4 + ays0[i,1-1]*np.sin(tempval) + ays0[i,2-1]*np.cos(tempval)

    y =   -0.006951            \
         - 0.025896     * ttt  \
         - 22.4072747   * ttt2 \
         + 0.00190059   * ttt3 \
         + 0.001112526  * ttt4 \
         + 0.0000001358 * ttt5

    y = y*convrt + ysum0      \
                 + ysum1*ttt  \
                 + ysum2*ttt2 \
                 + ysum3*ttt3 \
                 + ysum4*ttt4  ### rad

    ###if iauhelp == 'y' fprintf(1,'xys y %14.12f  %14.12f  %14.12f  %14.12f
    #    %14.12f
    #\n',ysum0/deg2rad,ysum1/deg2rad,ysum2/deg2rad,ysum3/deg2rad,ysum4/deg2rad
    #); ##end;

    ###% ---------------- now find s
    ssum0 = 0.0
    for i in np.arange(33-1,-1,-1): #= 33: -1 : 1
        tempval =  a0si[i, 1-1]*l      \
                 + a0si[i, 2-1]*l1     \
                 + a0si[i, 3-1]*f      \
                 + a0si[i, 4-1]*d      \
                 + a0si[i, 5-1]*omega  \
                 + a0si[i, 6-1]*lonmer + a0si[i, 7-1]*lonven  \
                 + a0si[i, 8-1]*lonear + a0si[i, 9-1]*lonmar  \
                 + a0si[i,10-1]*lonjup + a0si[i,11-1]*lonsat  \
                 + a0si[i,12-1]*lonurn + a0si[i,13-1]*lonnep  \
                 + a0si[i,14-1]*precrate
        ssum0 = ssum0 + ass0[i, 1-1]*np.sin(tempval) + ass0[i, 2-1]*np.cos(tempval)

    ssum1 = 0.0
    for j in np.arange(3-1,-1,-1): #= 3: -1 : 1
        i = 33 + j
        tempval =  a0si[i, 1-1]*l      \
                 + a0si[i, 2-1]*l1     \
                 + a0si[i, 3-1]*f      \
                 + a0si[i, 4-1]*d      \
                 + a0si[i, 5-1]*omega  \
                 + a0si[i, 6-1]*lonmer + a0si[i, 7-1]*lonven  \
                 + a0si[i, 8-1]*lonear + a0si[i, 9-1]*lonmar  \
                 + a0si[i,10-1]*lonjup + a0si[i,11-1]*lonsat  \
                 + a0si[i,12-1]*lonurn + a0si[i,13-1]*lonnep  \
                 + a0si[i,14-1]*precrate
        ssum1 = ssum1 + ass0[i,1-1]*np.sin(tempval) + ass0[i,2-1]*np.cos(tempval)

    ssum2 = 0.0
    for j in np.arange(25-1,-1,-1): #= 25: -1 : 1
        i = 33 + 3 + j
        tempval =  a0si[i, 1-1]*l      \
                 + a0si[i, 2-1]*l1     \
                 + a0si[i, 3-1]*f      \
                 + a0si[i, 4-1]*d      \
                 + a0si[i, 5-1]*omega  \
                 + a0si[i, 6-1]*lonmer + a0si[i, 7-1]*lonven  \
                 + a0si[i, 8-1]*lonear + a0si[i, 9-1]*lonmar  \
                 + a0si[i,10-1]*lonjup + a0si[i,11-1]*lonsat  \
                 + a0si[i,12-1]*lonurn + a0si[i,13-1]*lonnep  \
                 + a0si[i,14-1]*precrate    
        ssum2 = ssum2 + ass0[i,1-1]*np.sin(tempval) + ass0[i,2-1]*np.cos(tempval)

    ssum3 = 0.0
    for j in np.arange(4-1,-1,-1): #= 4: -1 : 1
        i = 33 + 3 + 25 + j
        tempval =  a0si[i, 1-1]*l      \
                 + a0si[i, 2-1]*l1     \
                 + a0si[i, 3-1]*f      \
                 + a0si[i, 4-1]*d      \
                 + a0si[i, 5-1]*omega  \
                 + a0si[i, 6-1]*lonmer + a0si[i, 7-1]*lonven  \
                 + a0si[i, 8-1]*lonear + a0si[i, 9-1]*lonmar  \
                 + a0si[i,10-1]*lonjup + a0si[i,11-1]*lonsat  \
                 + a0si[i,12-1]*lonurn + a0si[i,13-1]*lonnep  \
                 + a0si[i,14-1]*precrate    
        ssum3 = ssum3 + ass0[i,1-1]*np.sin(tempval) + ass0[i,2-1]*np.cos(tempval)

    ssum4 = 0.0
    for j in np.arange(1-1,-1,-1): #= 1: -1 : 1
        i = 33 + 3 + 25 + 4 + j
        tempval =  a0si[i, 1-1]*l      \
                 + a0si[i, 2-1]*l1     \
                 + a0si[i, 3-1]*f      \
                 + a0si[i, 4-1]*d      \
                 + a0si[i, 5-1]*omega  \
                 + a0si[i, 6-1]*lonmer + a0si[i, 7-1]*lonven  \
                 + a0si[i, 8-1]*lonear + a0si[i, 9-1]*lonmar  \
                 + a0si[i,10-1]*lonjup + a0si[i,11-1]*lonsat  \
                 + a0si[i,12-1]*lonurn + a0si[i,13-1]*lonnep  \
                 + a0si[i,14-1]*precrate    
        ssum4 = ssum4 + ass0[i,1-1]*np.sin(tempval) + ass0[i,2-1]*np.cos(tempval)

    s =  0.000094          \
       + 0.00380865 * ttt  \
       - 0.00012268 * ttt2 \
       - 0.07257411 * ttt3 \
       + 0.00002798 * ttt4 \
       + 0.00001562 * ttt5 
    ###% ...
    ###%            + 0.00000171*ttt*sin(omega) + 0.00000357*ttt*cos(2.0*omega) ...
    ###%            + 0.00074353*ttt2*sin(omega) + 0.00005691*ttt2*sin(2.0*(f-d+omega)) ...
    ###%            + 0.00000984*ttt2*sin(2.0*(f+omega)) - 0.00000885*ttt2*sin(2.0*omega);
    s = -1*x*y*0.5 + s*convrt + ssum0    \
                              + ssum1*ttt  \
                              + ssum2*ttt2 \
                              + ssum3*ttt3 \
                              + ssum4*ttt4  ### rad

    ### add corrections if available
    x = x + ddx
    y = y + ddy

    ### ---------------- now find a
    a = 0.5 + 0.125*(x*x + y*y) ### units take on whatever x and y are

    # print(f"06xys  x  {x/deg2rad:14.12f} ")
    # print(f"y  {y/deg2rad:14.12f} ")
    # print(f" s {s/deg2rad:14.12f} ")
    # print(f" a {a/deg2rad:14.12f} deg")
    # #
    # print(f"06xys  x  {x/deg2rad*3600:14.12f}")
    # print(f" y  {y/deg2rad*3600:14.12f}")
    # print(f" s {s/deg2rad*3600:14.12f}")
    # print(f" a {a/deg2rad:14.12f} deg")

    # nut1    = np.matrix(np.zeros((3,3) ))
    nut1    = np.array(np.zeros((3,3) ))

    ###% ----------------- find nutation matrix ----------------------
    nut1[1-1,1-1] = 1.0 - a*x*x
    nut1[1-1,2-1] =   -1.*a*x*y
    nut1[1-1,3-1] = x
    nut1[2-1,1-1] =   -1.*a*x*y
    nut1[2-1,2-1] = 1.0 - a*y*y
    nut1[2-1,3-1] =  y
    nut1[3-1,1-1] = -1*x
    nut1[3-1,2-1] = -1*y
    nut1[3-1,3-1] = 1.0 - a*(x*x + y*y)
    ###%nut1

    nut2 = np.eye(3)
    nut2[1-1,1-1] =    np.cos(s)
    nut2[2-1,2-1] =    np.cos(s)
    nut2[1-1,2-1] =    np.sin(s)
    nut2[2-1,1-1] = -1*np.sin(s)

    nut = nut1@nut2

###%   the matrix apears to be orthogonal now, so the extra processing is not needed.
###%     if (x ~= 0.0) && (y ~= 0.0)
###%         e = atan2(y,x);
###%       else
###%         e = 0.0;
###%       end;
###%     d = atan( sqrt((x^2 + y^2) / (1.0-x^2-y^2)) );
###%     nut1 = rot3mat(-e)*rot2mat(-d)*rot3mat(e+s)

    return(x,y,s,nut)


def iau06_earthrotationangle(jdut1):
    """Account effects of sidereal time via the earth rotation angle
 
    This function calulates the transformation matrix that accounts 
    for the effects of sidereal time via the earth rotation angle.

    Args:
        jdut1 (float):       Julian date of ut1      (days)

    Outputs :
        st (3x3 np.matrix):  Transformation matrix for pef-ire

    locals :
        tdut1       - julian centuries of ut1        (days)
        era         - earth rotation angle           (rad)

    Comments:
        - Authored, Jul 2004: David Vallado, (matlab)
        - Revision, Jan 2023: Zach Waldron, transcribed to Python     
    """

    twopi = 2.0 * np.pi
    
    ### julian centuries of ut1
    tut1d= jdut1 - 2451545.0

    era = twopi * ( 0.7790572732640 + 1.00273781191135448 * tut1d )
    era = np.fmod(era,twopi)
#     print(f'era{era*180/np.pi:11.7f}')

    # st    = np.matrix(np.zeros( (3,3) ) )
    st    = np.array(np.zeros( (3,3) ) )


    ### transformation matrix
    st[1-1,1-1] =  np.cos(era)
    st[1-1,2-1] = -1*np.sin(era)
    st[1-1,3-1] =  0.0

    st[2-1,1-1] =  np.sin(era)
    st[2-1,2-1] =  np.cos(era)
    st[2-1,3-1] =  0.0

    st[3-1,1-1] =  0.0
    st[3-1,2-1] =  0.0
    st[3-1,3-1] =  1.0

    return(st)




def iau06_Convert_ecef2eci(recef,vecef,aecef,ttt,jdut1,lod,xp,yp,option,\
                             ddx, ddy, calc_accel=False ):
    """Convert from ECEF frame to ECI frame using IAU-2006/2000, CIO based.
        
    This function transforms a vector from the earth centered earth fixed (ITRF)
    frame, to the ECI mean equator mean equinox (GCRF).
    
    Args:
        recef (3x1, np.array):  Position vector earth fixed     (km)
        vecef (3x1, np.array):  Velocity vector earth fixed     (km/s)
        aecef (3x1, np.array):  Acceleration vector earth fixed (km/s**2)
        ttt   (float):  Julian centuries of tt               (centuries)
        jdut1 (float):  Julian date of ut1                (days from 4713 bc)
        lod   (float):  Excess length of day                 (sec)
        xp    (float):  Polar motion coefficient             (arc rad)
        yp    (float):  Polar motion coefficient             (arc rad)
        option(float):  Which approach to use      (a-2000a, b-2000b, c-2000xys)
        ddx   (float):  eop correction for x                 (rad)
        ddy   (float):  eop correction for y                 (rad)
    
    Outputs
        reci (3x1, np.array):  Position vector eci              (km)
        veci (3x1, np.array):  Velocity vector eci              (km/s)
        aeci (3x1, np.array):  Acceleration vector eci          (km/s2)

    locals:
        pm          - transformation matrix for itrf-pef
        st          - transformation matrix for pef-ire
        nut         - transformation matrix for ire-gcrf
    coupling:
        iau00pm      - rotation for polar motion                 itrf-pef
        iau06_earthrotationangle     - rotation for earth rotyation              pef-ire
        iau06_precess_nutate_xys     - rotation for precession/nutation          ire-gcrf

    Comments:
        - Authored, Jul 2004: David Vallado, (written in matlab)  
        - Revision, Jan 2023: Zach Waldron, transcribed to Python   

    """

    ###% ---- ceo based, iau2006
    if option == 'cio':
        '''Use the CIO based option for IAU-1006/2000'''
        x,y,s,pnb = iau06_precess_nutate_xys(ttt, ddx, ddy)
        st        = iau06_earthrotationangle(jdut1)

    ###% ---- class equinox based, 2000a
    # if option == 'a'
    #     [ deltapsi, pnb, prec, nut, l, l1, f, d, omega, ...
    #         lonmer, lonven, lonear, lonmar, lonjup, lonsat, lonurn, lonnep, precrate ...
    #         ] = iau06pna (ttt);
    #     [gst,st] = iau06gst(jdut1, ttt, deltapsi, l, l1, f, d, omega, ...
    #         lonmer, lonven, lonear, lonmar, lonjup, lonsat, lonurn, lonnep, precrate);
    # end;

    ###% ---- class equinox based, 2000b
    # if option == 'b'
    #     [ deltapsi, pnb, prec, nut, l, l1, f, d, omega, ...
    #         lonmer, lonven, lonear, lonmar, lonjup, lonsat, lonurn, lonnep, precrate ...
    #         ] = iau06pnb (ttt);
    #     [gst,st] = iau06gst(jdut1, ttt, deltapsi, l, l1, f, d, omega, ...
    #         lonmer, lonven, lonear, lonmar, lonjup, lonsat, lonurn, lonnep, precrate);
    # end;


    pm = iau06_polarmotion(xp,yp,ttt,'06')

    ### ---- Setup parameters for velocity transformations
    thetasa= 7.29211514670698e-05 * (1.0  - lod/86400.0)
    omegaearth = np.array([0, 0, thetasa])[:,None]

    ### ---- Perform transformations
        ## "@" is matrix multiplication

    rpef = pm@recef
    reci = pnb@st@rpef
    vpef = pm @ vecef
    veci = pnb@st@(vpef + np.cross(omegaearth,rpef, axis=0))
#     print(f'   r_pef ',rpef)
#     print(f'   v_pef ',vpef)

    if calc_accel:
        temp = np.cross(omegaearth,rpef, axis=0)    
        aeci = pnb@st@( pm@aecef ) \
                + np.cross(omegaearth,temp, axis=0) \
                + 2.0*np.cross(omegaearth,vpef, axis=0)
        return(reci,veci,aeci)

    else:
        return(reci,veci,None)







def iau06_get_eop_vals(year,mon,day,
                 hr,minute,sec):
        
        #Maybe need to use EOPCO4
         # dir:  https://hpiers.obspm.fr/iers/eop/eopc04/
         # full time: https://hpiers.obspm.fr/iers/eop/eopc04/eopc04.dX_dY.12h.84-now
         # https://hpiers.obspm.fr/eoppc/eop/eopc04/C04.guide.pdf

    def readline_finals2000Adata(line):
        """Parse a single line from the finals2000a.data file.

        File sourced from IERS Rapid Service/Prediction Center (U.S. Naval Observatory)
        https://maia.usno.navy.mil/products/daily.htm
        Contains: Polar motion, UT1-UTC, and length of day since 02 January 1973 
                    with dX & dY using IAU2000A Nutation/Precession Theory updated \
                    daily (with 1 year of predictions).
        #
        The format of the finals2000A.data, finals2000A.daily, and finals2000A.all files is:
        Col.#    Format  Quantity
        -------  ------  -------------------------------------------------------------
        1-2      I2      year (to get true calendar year, add 1900 for MJD<=51543 or add 2000 for MJD>=51544)
        3-4      I2      month number
        5-6      I2      day of month
        7        X       [blank]
        8-15     F8.2    fractional Modified Julian Date (MJD UTC)
        16       X       [blank]
        17       A1      IERS (I) or Prediction (P) flag for Bull. A polar motion values
        18       X       [blank]
        19-27    F9.6    Bull. A PM-x (sec. of arc)
        28-36    F9.6    error in PM-x (sec. of arc)
        37       X       [blank]
        38-46    F9.6    Bull. A PM-y (sec. of arc)
        47-55    F9.6    error in PM-y (sec. of arc)
        56-57    2X      [blanks]
        58       A1      IERS (I) or Prediction (P) flag for Bull. A UT1-UTC values
        59-68    F10.7   Bull. A UT1-UTC (sec. of time)
        69-78    F10.7   error in UT1-UTC (sec. of time)
        79       X       [blank]
        80-86    F7.4    Bull. A LOD (msec. of time) -- NOT ALWAYS FILLED
        87-93    F7.4    error in LOD (msec. of time) -- NOT ALWAYS FILLED
        94-95    2X      [blanks]
        96       A1      IERS (I) or Prediction (P) flag for Bull. A nutation values
        97       X       [blank]
        98-106   F9.3    Bull. A dX wrt IAU2000A Nutation (msec. of arc), Free Core Nutation NOT Removed
        107-115  F9.3    error in dX (msec. of arc)
        116      X       [blank]
        117-125  F9.3    Bull. A dY wrt IAU2000A Nutation (msec. of arc), Free Core Nutation NOT Removed
        126-134  F9.3    error in dY (msec. of arc)
        135-144  F10.6   Bull. B PM-x (sec. of arc)
        145-154  F10.6   Bull. B PM-y (sec. of arc)
        155-165  F11.7   Bull. B UT1-UTC (sec. of time)
        166-175  F10.3   Bull. B dX wrt IAU2000A Nutation (msec. of arc)
        176-185  F10.3   Bull. B dY wrt IAU2000A Nutation (msec. of arc)

        """
        eop ={}
        eop['year']    = int(  line[1-1  :  2]) # I2    -year (to get true calendar year, 
                                                #        add 1900 for MJD<=51543 or add 2000 for MJD>=51544)
        eop['month']   = int(  line[3-1  :  4]) # I2    -month number
        eop['day']     = int(  line[5-1  :  6]) # I2    -day of month
        eop['mjd_utc'] = float(line[8-1  : 15]) # F8.2  -fractional Modified Julian Date (MJD UTC)
        #
        eop['flag_pm___A'] = str(  line[17-1     ]) # A1    -IERS (I) or Prediction (P) flag for Bull. A polar motion values
        eop['pmx_______A'] = float(line[19-1 : 27]) # F9.6  -Bull. A PM-x (sec. of arc)
        eop['pmxerr____A'] = float(line[28-1 : 36]) # F9.6  -err  in PM-x (sec. of arc)
        eop['pmy_______A'] = float(line[38-1 : 46]) # F9.6  -Bull. A PM-y (sec. of arc)
        eop['pmyerr____A'] = float(line[47-1 : 55]) # F9.6  -err  in PM-y (sec. of arc)
        #
        eop['flag_dut1_A'] = str(  line[58-1     ]) # A1    -IERS (I) or Prediction (P) flag for Bull. A UT1-UTC values
        eop['dut1______A'] = float(line[59-1 : 68]) # F10.7 -Bull. A UT1-UTC (sec. of time)
        eop['dut1err___A'] = float(line[69-1 : 78]) # F10.7 -err  in UT1-UTC (sec. of time)
        eop['lod_______A'] = float(line[80-1 : 86]) # F7.4  -Bull. A LOD (msec. of time) -- NOT ALWAYS FILLED
        eop['loderr____A'] = float(line[87-1 : 93]) # F7.4  -err in  LOD (msec. of time) -- NOT ALWAYS FILLED
        #
        eop['flag__nut_A'] = str(  line[96-1     ]) # A1    -IERS (I) or Prediction (P) flag for Bull. A nutation values
        eop['dX____nut_A'] = float(line[98-1 :106]) # F9.3  -Bull. A dX wrt IAU2000A Nutation (msec. of arc), Free Core Nutation NOT Removed
        eop['dXerr_nut_A'] = float(line[107-1:115]) # F9.3  -err  in dX (msec. of arc)
        eop['dY____nut_A'] = float(line[117-1:125]) # F9.3  -Bull. A dY wrt IAU2000A Nutation (msec. of arc), Free Core Nutation NOT Removed
        eop['dYerr_nut_A'] = float(line[126-1:134]) # F9.3  -err  in dY (msec. of arc)
        #
        eop['pmx_______B'] = float(line[135-1:144]) # F10.6 -Bull. B PM-x (sec. of arc)
        eop['pmy_______B'] = float(line[145-1:154]) # F10.6 -Bull. B PM-y (sec. of arc)
        eop['dut1______B'] = float(line[155-1:165]) # F11.7 -Bull. B UT1-UTC (sec. of time)
        eop['dX____nut_B'] = float(line[166-1:175]) # F10.3 -Bull. B dX wrt IAU2000A Nutation (msec. of arc)
        eop['dY____nut_B'] = float(line[176-1:185]) # F10.3 -Bull. B dY wrt IAU2000A Nutation (msec. of arc)

        return(eop)
    
    ### Determine the MJD at 00:00:00 
    jd, jdfrac = jday(year,mon,day,0,0,0)
    mjd  = jd+jdfrac - 2400000.5
#     print(f'MJD={mjd}')



    # def read_reverse_order(file_name):
    #     # Open file for reading in binary mode
    #     with open(file_name, 'rb') as read_obj:
    #         # Move the cursor to the end of the file
    #         read_obj.seek(0, os.SEEK_END)
    #         # Get the current position of pointer i.e eof
    #         pointer_location = read_obj.tell()
    #         # Create a buffer to keep the last read line
    #         buffer = bytearray()
    #         # Loop till pointer reaches the top of the file
    #         while pointer_location >= 0:
    #             # Move the file pointer to the location pointed by pointer_location
    #             read_obj.seek(pointer_location)
    #             # Shift pointer location by -1
    #             pointer_location = pointer_location -1
    #             # read that byte / character
    #             new_byte = read_obj.read(1)
    #             # If the read byte is new line character then it means one line is read
    #             if new_byte == b'\n':
    #                 # Fetch the line from buffer and yield it
    #                 yield buffer.decode()[::-1]
    #                 # Reinitialize the byte array to save next line
    #                 buffer = bytearray()
    #             else:
    #                 # If last read character is not eol then add it in buffer
    #                 buffer.extend(new_byte)
    #         # As file is read completely, if there is still data in buffer, then its the first line.
    #         if len(buffer) > 0:
    #             # Yield the first line too
    #             yield buffer.decode()[::-1]


    # def save_rows(filename):
    #     big_dict = {}
    #     keys = []
    #     lines = []
    #     with open(filename) as f:
    #         for line in f:
    #             line = line.strip().split(',')
    #             # d = dict(zip(keys, line))
    #             keys.append(line[8-1  : 15])
    #             lines.append(line)
        
    #     return dicts

    if 'eop_dict' not in globals():   #read_once_flag==False:    
        # print('---Reading EOP data and saving as dict global vars.')

        global eop_dict

        ### Save data to dictionary if its within the range of MJD dates 
        # (end is set to december 2nd 2022)
        eop_dict = {}
        with open(file_iers_iau06eop, 'r') as f:
            for line in f:
                if float(line[8-1  : 15]) < mjd:
                    pass
                elif float(line[8-1  : 15]) > 59915.0:
                    pass
                else:
                    eop_dict[float(line[8-1  : 15])] = line

    # #### Scan the EOP file to find the correct day/line
    # with open(file_iers_iau06eop, 'r') as f:
    #     # f.seek(line_no)
    #     for line_no,line in enumerate(f):
    #         #### Find the correct day
    #         if float(line[8-1  : 15]) == mjd:
    #             eop = readline_finals2000Adata(line)
    #             break

    line = eop_dict[mjd]
    if float(line[8-1  : 15]) == mjd:
        eop = readline_finals2000Adata(line)

    else:
        print('Val not in dict, reading EOP the long way...')
        #### Scan the EOP file to find the correct day/line
        with open(file_iers_iau06eop, 'r') as f:
            # f.seek(line_no)
            
            for line_no,line in enumerate(f):
                #### Find the correct day
                if float(line[8-1  : 15]) == mjd:
                    eop = readline_finals2000Adata(line)
                    break





    ### convert arc seconds to radians
    conv  = np.pi / (180.0*3600.0)   
    #
    dut1  = eop['dut1______B']            # delta of ut1-utc      (sec)
    dat   = get_leapseconds(year,mon,day)          # delta of tai-utc      (sec)
    lod   = eop['lod_______A']*1e-3       # excess length of day  (sec)
    xp    = eop['pmx_______B']*conv  # polar motion coeff    (rad)
    yp    = eop['pmy_______B']*conv  # polar motion coeff    (rad)
    dx    = eop['dX____nut_B']*1e-3*conv  # X correction          (rad)
    dy    = eop['dY____nut_B']*1e-3*conv  # Y correction

    return(dut1, dat, lod, xp, yp, dx, dy)




def call_teme2eci(r_teme, v_teme, aecef,
                        year, mon, day,
                        hr, minute, sec, calc_accel=False):
    '''Preps inputs and calls the ECI-TEME to ECI-J2000 transformation routine.
    
    Technically uses the iau_76/80 formulation.
    
    '''
    

    dat = get_leapseconds(year, mon, day)
    #
    #### Get the earth orientation parameters from iau76
    dut1, dat, lod,\
    xp, yp,\
    ddpsi, ddeps = iau76_get_eop_vals(year,mon,day,
                                    hr,minute,sec)
    #
    #### Convert time from utc to all the others    
    timezone = 0             # offset to utc             (0 .. 23 hr)
    (ut1,      # ut1       - universal time              (sec)
    tut1,      # tut1      - julian centuries of ut1
    jdut1,     # jdut1     - julian date (days only)     (days from 4713 bc)
    jdut1frac, # jdut1Frac - julian date (frac of day)   (days from 0 hr of day)
    utc,       # utc       - coordinated universal time  (sec)
    tai,       # tai       - atomic time                 (sec)
    tt,        # tdt       - terrestrial dynamical time  (sec)
    ttt,       # ttdt      - julian centuries of tdt
    jdtt,      # jdtt      - julian date (days only)     (days from 4713 bc)
    jdttfrac,  # jdttFrac  - julian date (frac of day)   (days from 0 hr of day)
    tdb,       # tdb       - terrestrial barycentric time(sec)
    ttdb,      # ttdb      - julian centuries of tdb
    jdtdb,     # jdtdb     - julian date of tdb          (days from 4713 bc)
               # jdtdbFrac   - julian date (frac of day) (days from 0hr of day)
    jdtdbfrac) = convtime( year, mon, day, hr, minute, sec,    
                           timezone, dut1, dat )

    (reci, veci, _)= teme2eci(r_teme, v_teme, None,\
                              ttt, ddpsi, ddeps,   \
                              calc_accel=False)
    return(reci, veci)




def iau06_Call_ecef2eci(recef, vecef, aecef,
                        year, mon, day,
                        hr, minute, sec, calc_accel=False):

    #### Get the earth orientation parameters
    dut1, dat, lod, xp, yp, dx, dy = iau06_get_eop_vals(year,mon,day,
                                                 hr,minute,sec)

    #### Convert time from utc to all the others    
    timezone = 0             # offset to utc             (0 .. 23 hr)
    #
    (ut1,      # ut1       - universal time              (sec)
    tut1,      # tut1      - julian centuries of ut1
    jdut1,     # jdut1     - julian date (days only)     (days from 4713 bc)
    jdut1frac, # jdut1Frac - julian date (frac of day)   (days from 0 hr of day)
    utc,       # utc       - coordinated universal time  (sec)
    tai,       # tai       - atomic time                 (sec)
    tt,        # tdt       - terrestrial dynamical time  (sec)
    ttt,       # ttdt      - julian centuries of tdt
    jdtt,      # jdtt      - julian date (days only)     (days from 4713 bc)
    jdttfrac,  # jdttFrac  - julian date (frac of day)   (days from 0 hr of day)
    tdb,       # tdb       - terrestrial barycentric time(sec)
    ttdb,      # ttdb      - julian centuries of tdb
    jdtdb,     # jdtdb     - julian date of tdb          (days from 4713 bc)
               # jdtdbFrac   - julian date (frac of day) (days from 0hr of day)
    jdtdbfrac) = convtime( year, mon, day, hr, minute, sec,    
                           timezone, dut1, dat )

    ### Make arrays into vertical vectors
    recef = np.array(recef)[:,None]
    vecef = np.array(vecef)[:,None]
    if calc_accel:
        aecef = np.array(aecef)[:,None]
    #
        reci, veci, aeci = iau06_Convert_ecef2eci(recef, vecef, aecef, \
                                                        ttt, jdut1+jdut1frac,\
                                                        lod, xp, yp, 'cio',  \
                                                        dx, dy, calc_accel=True
                                                    )
        return(reci, veci, aeci)
    else:
        reci, veci, _ = iau06_Convert_ecef2eci(recef, vecef, None, \
                                                        ttt, jdut1+jdut1frac,\
                                                        lod, xp, yp, 'cio',  \
                                                        dx, dy, calc_accel=False
                                                    )
    
        return(reci, veci, _)



















'''TEST-teme2ecef     ========================================================
from pygeodyn.util_dir.time_systems import time_gps_to_utc,\
                                       get_leapseconds,\
                                       jday,\
                                       convtime
from pygeodyn.util_dir.coordinate_systems import iau76_get_eop_vals,\
                                                 teme2eci

year   = 2004       #date_utc[0].year
mon    = 4          #date_utc[0].month
day    = 6          #date_utc[0].day
hr     = 7          #date_utc[0].hour
minute = 51         #date_utc[0].minute
sec    = 28.386009  #date_utc[0].second
#------------------------------------------
# timezone=0;
# order = 106;
# eqeterms = 2; % use the kinematic eqe terms after 1997



### get the leapseconds for time of interest
jd, jdfrac = jday(year,mon,day,
                  0,0,0)
dat = get_leapseconds(jd+jdfrac-2400000.5)

# date_utc = [time_gps_to_utc(time, dat) for time in SpireDF['tim (gps)'][:1]]

#------------------------------------------
# r_ecef = [-1033.4793830,  7901.2952754, 6380.3565958] 
# v_ecef = [-3.225636520,  -2.872451450,  5.531924446]

#
r_teme = [5094.18016210,  6127.64465950, 6380.34453270]    
v_teme = [-4.7461314870,  0.7858180410, 5.5319312880]
#


#### Get the earth orientation parameters
dut1, dat, lod,\
xp, yp,\
ddpsi, ddeps = iau76_get_eop_vals(year,mon,day,
                                hr,minute,sec)

conv = np.pi / (180*3600)
dut1_book = -0.4399619
dat_book  = 32
lod_book  =  0.0015563        #  % sec
ddpsi_book = -0.052195 * conv #  % " to rad
ddeps_book = -0.003875 * conv #

print(f"ddpsi  ")
print(f"    Calc'd: {ddpsi} ")
print(f"    expect: {ddpsi_book} ")
print(f"ddeps  ")
print(f"    Calc'd: {ddeps} ")
print(f"    expect: {ddeps_book} ")
print(f"lod  ")
print(f"    Calc'd: {lod} ")
print(f"    expect: {lod_book} ")
print(f"xp  ")
print(f"    Calc'd: {xp} ")
print(f"    expect: {xp_book} ")
print(f"yp  ")
print(f"    Calc'd: {yp} ")
print(f"    expect: {yp_book} ")
print(f"dut1  ")
print(f"    Calc'd: {dut1} ")
print(f"    expect: {dut1_book} ")
print(f"dat  ")
print(f"    Calc'd: {dat} ")
print(f"    expect: {dat_book} ")


#### Convert time from utc to all the others    
timezone = 0             # offset to utc             (0 .. 23 hr)
#
(ut1,      # ut1       - universal time              (sec)
tut1,      # tut1      - julian centuries of ut1
jdut1,     # jdut1     - julian date (days only)     (days from 4713 bc)
jdut1frac, # jdut1Frac - julian date (frac of day)   (days from 0 hr of day)
utc,       # utc       - coordinated universal time  (sec)
tai,       # tai       - atomic time                 (sec)
tt,        # tdt       - terrestrial dynamical time  (sec)
ttt,       # ttdt      - julian centuries of tdt
jdtt,      # jdtt      - julian date (days only)     (days from 4713 bc)
jdttfrac,  # jdttFrac  - julian date (frac of day)   (days from 0 hr of day)
tdb,       # tdb       - terrestrial barycentric time(sec)
ttdb,      # ttdb      - julian centuries of tdb
jdtdb,     # jdtdb     - julian date of tdb          (days from 4713 bc)
           # jdtdbFrac   - julian date (frac of day) (days from 0hr of day)
jdtdbfrac) = convtime( year, mon, day, hr, minute, sec,    
                       timezone, dut1, dat )


reci, veci, _ = teme2eci(r_teme, v_teme, None, ttt, ddpsi, ddeps,  calc_accel=False)


# J2000 2wo corrIAU-76/FK55102.509600006123.011520006378.13630000

#GCRF 2 w corrIAU-76/FK5   5102.50895790 6123.01140070   6378.13692820
#GCRF 2 w corrIAU-76/FK5   -4.7432201570    0.7905364970    5.533755727
# print("r", reci)
# print("v", veci)
print(f' r calc [{reci[0,0]:14.9f}, {reci[1,0]:14.9f}, {reci[2,0]:14.9f}]' )
print(f' r book [{ 5102.508959:14.9f}, { 6123.011403:14.9f}, { 6378.136925:14.9f}]' )
print(f' r book [{ 5102.50895790:14.9f}, { 6123.01140070:14.9f}, { 6378.13692820:14.9f}]' )

print(f' v calc [{veci[0,0]:14.9f}, {veci[1,0]:14.9f}, {veci[2,0]:14.9f}]' )
print(f' v book [{ -4.74322016:14.9f}, { 0.79053650:14.9f}, { 5.53375573:14.9f}]' )


===============================================================================
'''