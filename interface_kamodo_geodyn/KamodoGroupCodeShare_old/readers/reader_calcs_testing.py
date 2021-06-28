# -*- coding: utf-8 -*-
"""
Created on Fri May 14 15:42:55 2021

@author: rringuet

generalized reader calculations
TEC, hmF2, NmF2, ...
"""
import numpy as np


'''example execution assuming rho_e.shape = time, height, lat, lon:
rho_e_singletime_arrays = [rho_e[i] for i in range(rho_e.shape[0])]
TEC = np.array([C.calc_TEC_singletime(rho_e, gitm._height) for rho_e in rho_e_singletime_arrays])

Within 4e-05 percent of results from GITM reader: (GITM-this)/this. 
'''
def calc_TEC(rho_e, height):  #adapted from GITM filereader
    '''
    A routine to calculate the 2D VTEC at one timestep.
    To perform these calculations, electron density (rho_e) must be one of
    the available data types with shape (height,lat,lon).
    '''
    import scipy.integrate as integ

    print(len(height))
    print(rho_e.shape)
    nheight, nlat, nlon = rho_e.shape
    temp = np.array(rho_e * 1.0e-16)#,
    TEC = np.zeros((nlat, nlon), dtype=np.float64)
    for ilon in range(nlon):
        for ilat in range(nlat):
            # Integrate electron density over altitude, not including
            # ghost cells
            vtec = integ.simps(temp[2:-2,ilat,ilon],
                               height[2:-2]*1000, "avg")
            TEC[ilat,ilon] = vtec
    return TEC

'''example execution assuming rho_e.shape = time, height, lat, lon:
rho_e_singletime_arrays = [rho_e[i] for i in range(rho_e.shape[0])]
tmp = np.array([C.calc_TEC_singletime(rho_e, gitm._height) for rho_e in rho_e_singletime_arrays])
NmF2 = np.array([tmp[i][0] for i in range(len(tmp))])
hmF2 = np.array([tmp[i][1] for i in range(len(tmp))])

Within 4e-05 percent of results from GITM reader: (GITM-this)/this. 
'''
def calc_NmF2_hmF2(rho_e, height): #adapted from GITM filereader
    '''
    A routine to calculate the 2D ionospheric parameters at one timestep: hmF2, NmF2.
    To perform these calculations, electron density (rho_e) must be one of
    the available data types with shape (height,lat,lon).
    '''
    from scipy.interpolate import interp1d
    from scipy.signal import argrelextrema

    nheight, nlat, nlon = rho_e.shape
    NmF2_result=np.zeros((nlat,nlon), dtype=np.float64)
    hmF2_result=np.zeros((nlat,nlon), dtype=np.float64)
    alt = np.linspace(min(height[2:-2]),max(height[2:-2]),1000)
    for ilon in range(nlon):
        for ilat in range(nlat):

            # Interpolate over the electron density altitude profile
            eprof = interp1d(height[2:-2], rho_e[2:-2,ilat,ilon], kind="cubic")
            edens = eprof(alt)

            # Find the local maxima of the electron density profile
            emax = argrelextrema(edens, np.greater)
            emax_list = list(emax[0])
            saddle = False
            if len(emax_list) == 0:
                # No local maxima were found, try identifying
                # saddle points
                saddle = True
            elif len(emax_list) == 1:   #emax_list is a number
                if max(edens) == edens[emax_list[0]]:
                    # Only one maxima exists and is realistic. Sometimes
                    # a profile will have inflection points instead of
                    # local maxima and this can confuse the routine
                    NmF2_result[ilat,ilon] = edens[emax_list[0]]
                    hmF2_result[ilat,ilon] = alt[emax_list[0]]
                else:
                    saddle = True
            elif alt[emax_list[-1]] < 120.0:
                saddle = True
            else:
                # More than one maxima exists.  Seperate hmF2 from hmF1
                # and spurious local maxima
                NmF2 = list(edens[emax_list])
                HmF2 = list(alt[emax_list])

                # If the global maximum is over 200 km,
                # this is the F2 peak
                eindex = NmF2.index(max(NmF2))
                if HmF2[eindex] <= 200.0 and HmF2[eindex] == min(HmF2):
                    # The global max may be the F1 peak, see if the
                    # secondary (or lessor) maxima is at the upper
                    # limit of the model.  If so, remove this point
                    # from consideration
                    if max(HmF2) > height[-5]:
                        eindex = HmF2.index(max(HmF2))
                        emax_list.pop(eindex)
                        NmF2.pop(eindex)
                        HmF2.pop(eindex)
                        eindex = NmF2.index(max(NmF2))

                    if len(emax_list) > 1:
                        # If there are multiple maxima after the upper
                        # boundary has been removed, choose the largest
                        # maxima above 200 km since the hmF1 is often
                        # larger than the hmF2
                        emax_list.pop(eindex)
                        NmF2.pop(eindex)
                        eindex = NmF2.index(max(NmF2))

                # Set the hmF2 and NmF2 (#emax_list is a number)
                NmF2_result[ilat,ilon] = edens[emax_list[eindex]]
                hmF2_result[ilat,ilon] = alt[emax_list[eindex]]

            if saddle:
                # It is difficult to find saddle points.  Examine
                # the rate of change of density
                delta_alt = alt[1] - alt[0] # Equally spaced
                edens_dot = np.diff(edens) / delta_alt

                # Identify inflection points by looking for
                # minima in the derivative of electron density.
                edot_min = argrelextrema(edens_dot, np.less)
                emin_list = list(edot_min[0])
                edens_min = list(edens[emin_list])
                emax = np.max(edens_min)
                eindex = emin_list[edens_min.index(emax)]

                # Assign the inflection with the largest
                # electron density to the ion peak
                #emax_list is a number
                NmF2_result[ilat,ilon] = edens[eindex]
                hmF2_result[ilat,ilon] = alt[eindex]
    return NmF2_result, hmF2_result

''' -------------------- Code in testing below this line---------------------'''
def calc_magdi(BF_east, BF_north, BF_vertical, BF_magnitude, lon, lat):
    '''
    This routine was adapted from the original GITM file reader. 
    BF_east, BF_north, BF_vertical, and BF_magnitude all have the same shape:
        [lon.shape, lat.shape, alt.shape]
    
    GITM 3DION and 3DMAG files contain the magnetic field in
    North-East-Vertical coordinates.  This routine computes the magnetic
    inclination and declination.
    '''
    import math

    #check that dimensions match lon and lat shape given
    check=True
    for item in [BF_east, BF_north, BF_vertical, BF_magnitude]:
        if (item.shape[0]!=lon.shape) or (item.shape[1]!=lat.shape):
            print('Shape does not match!')
            print('Array shape: ', item.shape)
            check=False
    if not check:
        print('Lon, Lat shapes: ', lon.shape, lat.shape)
        print('Returning 0 and exiting routine.')
        return 0

    #initialize calculation values
    dec_frac = BF_east / BF_north
    inc_sign = -1.0 * BF_vertical / abs(BF_vertical)
    inc_pmag = BF_north**2 + BF_east**2

    for ilon in range(len(lon)):
        for ilat in range(len(lat)):
            for ialt,df in enumerate(dec_frac[ilon,ilat]):
                dec_frac[ilon,ilat,ialt] = math.atan(df) * 180.0 / np.pi
                i = (inc_sign[ilon,ilat,ialt]
                     * math.sqrt(inc_pmag[ilon,ilat,ialt])
                     / BF_magnitude[ilon,ilat,ialt])
                inc_pmag[ilon,ilat,ialt] = math.acos(i) * 180.0 / np.pi
                if inc_pmag[ilon,ilat,ialt] > 90.0:
                    inc_pmag[ilon,ilat,ialt] -= 180.0

    magdi={'Declination': dec_frac, 'Inclination': inc_pmag}

    return magdi      

def calc_magvel(BF_east, BF_north, BF_vertical, BF_magnitude, data_in):
    '''
    Adapted from the original GITM file reader. data_in is a dictionary
    of variable data where the keys are the variable names and the value is 
    an array of the variable data. The variable names should be of the
    format varname_east, varname_north, varname_up for individual components
    of each varname. Otherwise, the variable will be treated as a magnitude
    (the else statement below will be executed).
    
    Gitm 3DIon files contain the magnetic coordinates that allow the
    field-aligned and field-perpendicular velocities to be computed.
    The 3DIon file must be from the same run as the 3DAll file so that
    the geographic grid is the same.  If a 3D Ion file was not produced
    in the original run, don't fret!  You can get one by using the same
    UAM.in file.  Unless the magnetic field is varying secularly, you
    can get away with producing just one 3DIon file.  It is better to have
    a matching 3D Ion file for every 3D All file, however.
    '''

    # Compute the field-aligned unit vector in East, North,
    # and Vertical coordinates
    bhat_e = BF_east / BF_magnitude
    bhat_n = BF_north / BF_magnitude
    bhat_v = BF_vertical / BF_magnitude

    # Compute the zonal unit vector in East, North, Vertical coord
    mag = np.sqrt(np.square(BF_east)
                  + np.square(BF_north))
    zhat_e = BF_north / mag
    zhat_n = BF_east / mag
    # zhat_v is identically zero
    '''Is this true for all models?'''

    # Compute the meridional unit vector in East, North, Vertical coord
    mhat_e = (-BF_east*BF_vertical/(mag * BF_magnitude))
    mhat_n = (-BF_north*BF_vertical/ (mag * BF_magnitude))
    mhat_v = mag / BF_magnitude

    # Compute the magnetic coordinate velocities for each overlapping
    # latitude, longitude, and altitude.  Also include the mag coord.
    magvel_data, calculated_names = {}, []
    for item in list(data_in.keys()):   #loop through given variable names
        if item not in calculated_names:  #skip variables calc is done for
            if (item.split('_')[-1] in ['east','north','up']): 
                # extract the directional names of the same variable
                varname = ''.join([t+'_' for t in  item.split('_')[:-1]]).strip('_')
                east = varname+'_east'
                north = varname+'_north'
                up = varname+'_up'
                
                #add directional versions to done list
                calculated_names.extend([east, north, up])
            
                #build new names for velocities
                par = varname+'_par'
                zon = varname+'_zon'
                mer = varname+'_mer'

                #calculate velocities
                vp=bhat_e*data_in[east]+bhat_n*data_in[north]+bhat_v*data_in[up]
                vz=zhat_e*data_in[east]+zhat_n*data_in[north]
                vm=mhat_e*data_in[east]+mhat_n*data_in[north]+mhat_v*data_in[up]
            else: 
                #add variable name to done list
                calculated_names.append(item)
                
                #build new names for velocities
                par = varname+'_par'
                zon = varname+'_zon'
                mer = varname+'_mer'
                
                #calculate velocities
                vp=bhat_v*data_in[item]
                vz=0.0*data_in[item]
                vm=mhat_v*data_in[item]
            #store data for each mag. coord. based vel component
            magvel_data[par] = vp
            magvel_data[zon] = vz
            magvel_data[mer] = vm
  
    return magvel_data

