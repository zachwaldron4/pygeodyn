import numpy as np



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

def Convert_cartesian_to_RSW_returnall(state_vector, Tmat_input, PCE_bool = True):
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
