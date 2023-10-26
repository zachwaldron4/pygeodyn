# -*- coding: utf-8 -*-
"""
Created on Fri Apr 23 17:09:14 2021

@author: rringuet

Most needed functions to support the SatelliteFlythrough and SingleSatelliteFlythrough
softwares. The corresponding height function to be inverted by CalcIlev
will need to be labeled H_ilev for ilev, H_ilev1 for the ilev1 variation, etc.

______________________________________________________________________________
|NOTES REGARDING THE DIFFUSIVE EQUILLIBRIUM EXTRAPOLATION
|=============================================================================
|                          By Zach Waldron --- April 2022
|-----------------------------------------------------------------------------
|    1) Comments have been added where this code has been modified to
|        implement the top boundary extrapolation.  
|        These have been denoted as:          
|
|            ### DIFF_EQ_EXTRAP EDIT: note note note  
|
|        Or in more detail as:
|
|            ### =============================================================    
|            ###     DIFF_EQ_EXTRAP EDIT
|            ###  ------------------------------------------------------------
|            ###     Title/summary of edit
|            ###  ------------------------------------------------------------
|            ###     Notes notes notes... notes notes notes... 
|            ###     Notes notes notes... notes notes notes... 
|            ###  ------------------------------------------------------------
|            code
|            code
|            code
|            ### =============================================================    
|        
|-----------------------------------------------------------------------------
|        1a) Summarized,  the functions that were modified are:
|          
|                Model_SatelliteFlythrough()
|                    - FOR TIEGCM --- added check for N2 constituent
|                                --- need to calculate manually if not present
|                    - for all requested values above the top boundary, update
|                       the density and mass mixing ratios using the diffusive
|                       equillibrium equation
|                
|                Model_FlyAway()
|                    - return the max_height from the sat_tracks() function
|                    - add max_height to the results dict
|                            results['max_height'] = max_height
|
|                sat_tracks()   
|                    - return the max_height from the call_CalcIlev() function 
|                    - add max_height to sat_tracks return
|
|                call_CalcIlev()   
|                    - return the max_height from the CalcIlev function 
|                    - add max_height to call_CalcIlev return
|
|                CalcIlev()
|                    - in the "if height>max_height" conditional, 
|                       return the max_height
|                    - return the max_height in the normal function return
|
|-----------------------------------------------------------------------------
|    2) Comments that denote modifications related to interfacing to the 
|        GEODYN/Pygeodyn program (most commonly to remove various printing)
|        are denoted as:
|
|            ### GEODYN EDIT: note note note  
|
|        Or in more detail as:
|
|        ### =================================================================    
|        ###     GEODYN EDIT
|        ###  ----------------------------------------------------------------
|        ###     Title/summary of edit
|        ###  ----------------------------------------------------------------
|        ###     Notes notes notes... notes notes notes... 
|        ###     Notes notes notes... notes notes notes... 
|        ###  ----------------------------------------------------------------
|        code
|        code
|        code
|        ### =================================================================
|
|-----------------------------------------------------------------------------
|    3) While the below function is not used, it represents 
|       the method to perform a top boundary density extrapolation 
|       assuming diffusive equilibrium
|             ####### ----------------------------------------
|             #######  DIFFUSIVE EQUILIBRIUM EXTRAPOLATION
|             ####### ----------------------------------------
|             ### Prepare constants and functions
|             R_e = 6.371008e6     # meters  
|             k   = 1.38064852e-23 # [J/K]
|             m_p = 1.6726e-27     # [kg]
|             g_0 = 9.807          # [m/s^2]
|             def gravity(h):
|                 ### For h in same units as R_e
|                 return g_0*pow(R_e/(R_e + h), 2)
|             def scale_height(T, height, molecular_mass):
|                 '''Scale height in meters, assuming mks'''
|                 return k*T/(gravity(height)*molecular_mass*m_p)
|     
|             def DIFF_EQUI_EXTRAP(results_dict, height_request ):  
|                 i = results_dict['c3'].index(height_request/1000.)
|                 print(i)
|                 print(height_request)
|     
|                 den_O1 = results_dict['mmr_O'][i]  * results_dict['rho'][i]*1000.  # [kg/m^3]
|                 den_O2 = results_dict['mmr_O2'][i] * results_dict['rho'][i]*1000.  # [kg/m^3]
|                 den_HE = results_dict['mmr_He'][i] * results_dict['rho'][i]*1000.  # [kg/m^3]
|                 den_N2 = results_dict['mmr_N2'][i] * results_dict['rho'][i]*1000.  # [kg/m^3] 
|                 neutral_temp = results_dict['T_n'][i]      # neutral temperature [K]
|     
|                 # height_request is the altitude in meters
|                 # top_boundary_m is the max model altitude in meters
|                 alt_m = height_request
|                 top_boundary_m = results_dict['max_height'][i]
|                        ## All calculations done in SI Units
|                        ## The calc is done as an update to the 
                         ##  "top boundary" density
|                 H_O1    = scale_height(neutral_temp, alt_m, 16.)   
|                 den_O1 *= np_exp(-(alt_m - top_boundary_m)/H_O1)
|                 H_O2    = scale_height(neutral_temp, alt_m, 32.) 
|                 den_O2 *= np_exp(-(alt_m - top_boundary_m)/H_O2)
|                 H_HE = scale_height(neutral_temp, alt_m, 4.)                                
|                 den_HE *= np_exp(-(alt_m - top_boundary_m)/H_HE)                         
|                 H_N2 = scale_height(neutral_temp, alt_m, 28.)  
|                 den_N2 *= np_exp(-(alt_m - top_boundary_m)/H_N2)
|     
|                 ### Return rho in units of g/cm^3
|                 rho = den_O1 + den_O2 + den_HE + den_N2    
|     
|                 ### Update the mass mixing ratio values 
|                 ###  above the top boundary
|                 results_dict['mmr_O'][i]  = den_O1/(results_dict['rho'][i]*1000.)
|                 results_dict['mmr_O2'][i] = den_O2/(results_dict['rho'][i]*1000.)
|                 results_dict['mmr_He'][i] = den_HE/(results_dict['rho'][i]*1000.)
|                 results_dict['mmr_N2'][i] = den_N2/(results_dict['rho'][i]*1000.)
|                 results_dict['rho'][i]    = rho/1000.      
|                 return(results_dict)
|-----------------------------------------------------------------------------
|     end notes from Zach.
|_____________________________________________________________________________
"""

from numpy import vectorize, array, linspace, diff, where, isnan, float64
from numpy import concatenate, argmin, argsort, unique, ndarray, NaN, nanmax, nanmin
from numpy import abs as npabs
from time import perf_counter
from os.path import basename, isfile
from datetime import datetime, timedelta, timezone
import kamodo_ccmc.flythrough.model_wrapper as MW
from kamodo_ccmc.flythrough.utils import ConvertCoord
from astropy.constants import R_earth  #to convert from radius to height in CalcIlev

### DIFF_EQ_EXTRAP EDIT: add numpy functions
from numpy import zeros as np_zeros
from numpy import size as np_size
from numpy import abs as npabs
from numpy import exp as np_exp

@vectorize
def ts_to_hrs(time_val, filedate):
    '''Convert array of timestamps to hours since midnight of filedate string'''
    
    file_datetime = datetime.strptime(filedate+' 00:00:00', '%Y-%m-%d %H:%M:%S')
    return (datetime.utcfromtimestamp(time_val)-file_datetime).total_seconds()/3600.

@vectorize
def hrs_to_ts(time_val, filedate):
    '''Convert array of hours since midnight of filedate string to timestamps'''
    
    file_datetime = datetime.strptime(filedate+' 00:00:00', '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc)    
    return datetime.timestamp(file_datetime+timedelta(hours=time_val))

def ts_to_ISOstring(utc_ts):
    '''Convert timestamp to string of format 2017-05-28T00:00:00'''
    
    return datetime.utcfromtimestamp(utc_ts).isoformat()

def check_plot_dir(plot_dir):
    '''If plot_dir does not exist, create it'''
    from os import path, mkdir
    
    if not path.isdir(plot_dir): mkdir(plot_dir)
    return

def write_timescsv(csv_filename, times):
    '''writes times dict from day_files to csv for faster execution time next time.''' 
    
    data_out = open(csv_filename, 'w')
    data_out.write('# '+csv_filename)
    data_out.write('\n#file_date, filename, datetimes[0], datetimes[1], filetimes[0], filetimes[1], dt')
    for key in times.keys():
        data_out.write('\n'+key+','+''.join([f'{value},' for value in times[key]]).strip(','))
    data_out.close()    
    return

def read_timescsv(csv_filename):
    '''reads times dict from csv_filename for faster execution time.'''
    
    times = {}
    data_in = open(csv_filename, 'r')
    lines = data_in.readlines()
    data_in.close()
    for line in lines[2:]:
        data_line = line.strip('\n').split(',')
        times[data_line[0]] = data_line[1:4]
        times[data_line[0]].extend([float64(data_line[4]), float64(data_line[5]), 
                                   float(data_line[6])])
    return times

# def day_files(file_pattern, model, call_type):
#     '''Retrieve file times. Convert files if necessary.'''

#     #file_pattern could be a list if more than one pattern in file_dir exists   
#     if not isinstance(file_pattern, str):  #if a list/array of strings (GITM/similar)
#         files, times = file_pattern, {}  #run reader with file_prefixes given
#     else: 
#         from glob import glob
#         files, times = sorted(glob(file_pattern)), {}
        
#     #collect only time information from files for full time range
#     reader = MW.Model_Reader(model)
#     for f in files:
#         k = reader(f, variables_requested=[], filetime=True, fulltime=True, printfiles=False)
#         if hasattr(k, 'conversion_test'): 
#             if not k.conversion_test:
#                 continue  #if file conversion errors, skip file_pattern
#         if call_type=='normal':
#             file_date = k.datetimes[0][0:10]  #'YYYY-MM-DD'
#         elif call_type=='single':
#             file_date = k.datetimes[0][0:13].replace(' ','_')  #'YYYY-MM-DD_HH'
#         if file_date not in times.keys():  #prevent overwriting
#             times[file_date] = [f,k.datetimes[0],k.datetimes[1],
#                             k.filetimes[0], k.filetimes[1], k.dt]
#         else: 
#             file_date+='_'+k.datetimes[0][11:13] #'YYYY-MM-DD_HH'
#             times[file_date] = [f,k.datetimes[0],k.datetimes[1],
#                 k.filetimes[0], k.filetimes[1], k.dt]
#     return times
def day_files(file_pattern, model, call_type):
    '''Retrieve file times. Convert files if necessary.'''

    #file_pattern could be a list if more than one pattern in file_dir exists   
    if not isinstance(file_pattern, str):  #if a list/array of strings (GITM/similar)
        files, times = file_pattern, {}  #run reader with file_prefixes given
    else: 
        from glob import glob
        files, times = sorted(glob(file_pattern)), {}
        
    #collect only time information from files for full time range
    reader = MW.Model_Reader(model)
    
#     print('zach SF_utilities.day_files()-- files \n', files)
    
    for f in files:
        k = reader(f, variables_requested=[], filetime=True, fulltime=True, printfiles=False)
        if hasattr(k, 'conversion_test'): 
            if not k.conversion_test:
                continue  #if file conversion errors, skip file_pattern
        if call_type=='normal':
            file_date = k.datetimes[0][0:10]  #'YYYY-MM-DD'
        elif call_type=='single':
            file_date = k.datetimes[0][0:13].replace(' ','_')  #'YYYY-MM-DD_HH'
        if file_date not in times.keys():  #prevent overwriting
            times[file_date] = [f,k.datetimes[0],k.datetimes[1],
                            k.filetimes[0], k.filetimes[1], k.dt]
        else: 
            file_date+='_'+k.datetimes[0][11:13] #'YYYY-MM-DD_HH'
            times[file_date] = [f,k.datetimes[0],k.datetimes[1],
                k.filetimes[0], k.filetimes[1], k.dt]
    return times

# def check_timescsv(file_pattern, model, call_type='normal'):
#     '''check for times csv file, write if not found in file_dir or if outdated'''
    
#     #file_pattern could be a list if more than one pattern in file_dir exists   
#     if not isinstance(file_pattern, str):  #if a list/array of strings (GITM/similar)
#         sample_pattern = file_pattern[0]  #
#     else: 
#         sample_pattern = file_pattern
        
#     #determine csv filename
#     sample_prefix = basename(sample_pattern)
#     file_dir = sample_pattern.split(sample_prefix)[0]
#     if call_type=='normal':
#         csv_filename = file_dir+model+'_times.csv'
#     elif call_type=='single':
#         csv_filename = file_dir+model+'_singletimes.csv'
#     #print('csv_filename', csv_filename)
    
#     #if file DNE or outdated, write and return, else read and return
#     if not isfile(csv_filename):
#         times = day_files(file_pattern, model, call_type)
#         write_timescsv(csv_filename, times)
#     else:
#         times = read_timescsv(csv_filename)
        
#         #compare file contents to data in dir
#         oldfile_pattern = [value[0] for key, value in times.items()]  #file_patterns in times.csv file
#         if len(oldfile_pattern)==len(file_pattern):  #same length -> compare contents
#             compare = sum(oldfile_pattern==file_pattern)
#             if compare==len(file_pattern):  #if match, then return
#                 return times
#             else:  #not matching -> delete old file and recreate
#                 times = day_files(file_pattern, model, call_type)
#                 write_timescsv(csv_filename, times)                
#         else: #different lengths -> delete old file and recreate
#             times = day_files(file_pattern, model, call_type)
#             write_timescsv(csv_filename, times) 
                
#     return times
def check_timescsv(file_pattern, model, call_type='normal'):
    '''check for times csv file, write if not found in file_dir or if outdated'''
    
    #file_pattern could be a list if more than one pattern in file_dir exists   
    if not isinstance(file_pattern, str):  #if a list/array of strings (GITM/similar)
        sample_pattern = file_pattern[0]  #
    else: 
        sample_pattern = file_pattern
        
    #determine csv filename
    sample_prefix = basename(sample_pattern)
    file_dir = sample_pattern.split(sample_prefix)[0]
    if call_type=='normal':
        csv_filename = file_dir+model+'_times.csv'
    # elif call_type=='single':
    #     csv_filename = file_dir+model+'_singletimes.csv'
    #print('csv_filename', csv_filename)
    
    #if file DNE or outdated, write and return, else read and return
    if not isfile(csv_filename):
        times = day_files(file_pattern, model, call_type)
        write_timescsv(csv_filename, times)
    else:
        print('reading times csv')
        times = read_timescsv(csv_filename)
        
        # if model=='GITM':

        #compare file contents to data in dir
        oldfile_pattern = [value[0] for key, value in times.items()]  #file_patterns in times.csv file
        if len(oldfile_pattern)==len(file_pattern):  #same length -> compare contents
            compare = sum(oldfile_pattern==file_pattern)
            if compare==len(file_pattern):  #if match, then return
                return times
            else:  #not matching -> delete old file and recreate
                times = day_files(file_pattern, model, call_type)
                write_timescsv(csv_filename, times)                
        else: #different lengths -> delete old file and recreate
            times = day_files(file_pattern, model, call_type)
            write_timescsv(csv_filename, times) 
                
    return times

    
def save_times(file_patterns, sat_time, model, verbose=False):
    '''Adjust times between files to filetime within half of dt in seconds (7.5min by default).
    Works for models with one day of data per file.'''

    times = check_timescsv(file_patterns, model) 
    #print(times)
    #look for sat_times not in files
    l_idx, file_dates = 0, list(times.keys())
    for i in range(len(file_dates)):  #filter out times not in files
        idx = where((sat_time>=times[file_dates[i]][3]) & (sat_time<=times[file_dates[i]][4]\
                                                       +times[file_dates[i]][5]))[0]  #end_time+dt
        times[file_dates[i]].append(idx)   
        if i>0:  #remove indices from previous idx list if it occurs in this one
            #prefer time to be after beg time and not in dt section after end time
            tmp_idx = [ival for ival in times[file_dates[i-1]][6] if ival not in idx]
            times[file_dates[i-1]][6] = tmp_idx  

    #collect indices into one array for plotting
    net_idx = array(concatenate(tuple([times[file_date][6] for file_date in times.keys()])), dtype=int)
    l_idx = len(net_idx)
    test_idx = unique(net_idx)
    if len(test_idx)!=l_idx: 
        print(l_idx, len(test_idx))
        raise AttributeError("net_idx has repeating values. Idx filtering didn't work right.")
    
    #print errors for any remaining 'bad' times
    nbad_times = len(sat_time)-l_idx
    #print('save_times function', len(sat_time), l_idx)
    if nbad_times>0:
        print(f'{nbad_times} times are not in model output files and are excluded from the flythrough.')
        
    return sat_time, times, net_idx


''' Using CTIPe object for these tests:
#speed test for CalcIlev section, for two step process, one satellite position at a time
#outputs n_stepd, bounds of height as delta_h, bounds of ilev for delta_ilev, ilev, 
#    and processing time
t, height, lat, lon = sat_track[0]
sample_ilev = linspace(1,15,15,dtype=float)   #global variable
for i in [10,50,100,250,500]:
    start = ti.time()
    rough_height = array([ctipe.H([t, ilev, lat, lon])[0] for ilev in sample_ilev])
    ilev_range = sort(sample_ilev[argsort(abs(height-rough_height))[0:2]])
    test_ilev = linspace(ilev_range[0],ilev_range[1],i,dtype=float)
    finer_height = array([ctipe.H([t, ilev, lat, lon])[0] for ilev in test_ilev])
    ilev_idx = argmin(abs(height-finer_height))
    print(i, finer_height[ilev_idx+1]-finer_height[ilev_idx-1], 
          test_ilev[ilev_idx+1]-test_ilev[ilev_idx-1], test_ilev[ilev_idx],ti.time()-start)

OUTPUT: (i, delta_h, delta_ilev, ilev, calc_time)
10 10061.973765432136 0.2222222222222232 12.777777777777779 0.01804208755493164
50 1848.1176303854445 0.040816326530611846 12.775510204081632 0.04254770278930664
100 914.7248877665261 0.020202020202019 12.787878787878787 0.07263469696044922
250 363.68579875055 0.008032128514056325 12.783132530120483 0.20418334007263184
500 181.47848474729108 0.004008016032063466 12.783567134268537 0.3631880283355713
-> Too slow to execute repeatedly
   

#speed test with one step process, same outputs
t, height, lat, lon = sat_track[0]
for i in [10,50,100,500,1000,5000,10000]:
    test_ilev = linspace(1,15,n,dtype=float)  #global variable each time
    start = ti.time()
    test_track = array([[t, ilev, lat, lon] for ilev in test_ilev])
    finer_height = ctipe.H(test_track)
    ilev_idx = argmin(abs(height-finer_height))
    print(i, finer_height[ilev_idx+1]-finer_height[ilev_idx-1], 
          test_ilev[ilev_idx+1]-test_ilev[ilev_idx-1], test_ilev[ilev_idx],ti.time()-start)
    
OUTPUT: (i, delta_h, delta_ilev, ilev, calc_time)
10 152055.1091820988 3.1111111111111107 13.444444444444445 0.0
50 25873.64682539692 0.571428571428573 12.714285714285714 0.0025177001953125
100 12806.148428731773 0.28282828282828376 12.737373737373737 0.0010514259338378906
500 2540.6987864619005 0.056112224448899184 12.783567134268537 0.0
1000 1269.0777722166968 0.02802802802802873 12.785785785785786 0.006573677062988281
5000 253.6124613811844 0.005601120224044465 12.784756951390278 0.002038717269897461
10000 126.79354879941093 0.0028002800280031437 12.783578357835783 0.010225057601928711  *chosen method
-> Not accurate to within 200 km until i=10000, but better speed than 1st option


#faster 2-step process?
t, height, lat, lon = sat_track[0]
sample_ilev = linspace(1,15,15,dtype=float)   #global variable
for i in [10,50,100,250,500,1000,5000,10000]:
    start = ti.time()
    rough_track = array([[t, ilev, lat, lon] for ilev in sample_ilev])
    rough_height = ctipe.H(rough_track)
    ilev_range = sort(sample_ilev[argsort(abs(height-rough_height))[0:2]])
    test_ilev = linspace(ilev_range[0],ilev_range[1],i,dtype=float)
    finer_track = array([[t, ilev, lat, lon] for ilev in test_ilev])
    finer_height = ctipe.H(finer_track)
    ilev_idx = argmin(abs(height-finer_height))
    print(i, finer_height[ilev_idx+1]-finer_height[ilev_idx-1], 
          test_ilev[ilev_idx+1]-test_ilev[ilev_idx-1], test_ilev[ilev_idx],ti.time()-start)
    
OUTPUT: (i, delta_h, delta_ilev, ilev, calc_time)
10 10061.973765432136 0.2222222222222232 12.777777777777779 0.0
50 1848.1176303854445 0.040816326530611846 12.775510204081632 0.0
100 914.7248877665261 0.020202020202019 12.787878787878787 0.0
250 363.68579875055 0.008032128514056325 12.783132530120483 0.015614032745361328
500 181.47848474729108 0.004008016032063466 12.783567134268537 0.0   
1000 90.64841230114689 0.0020020020020012907 12.783783783783784 0.0
5000 18.115175812970847 0.0004000800160035567 12.783956791358271 0.0
10000 9.056682057096623 0.00020002000199959014 12.783978397839784 0.015622377395629883

-> Accurate to within 200 km for i=500, and too fast to time it. Best option out of first 3.
Freezes when including i values above 500. Not sure why. Gives answer if I hit enter a few times.

-> Comparing method 2 (i=10000) and 3 (i=500) in execution showed a large time difference.
Method 2 took about 150 seconds while method 3 took about 16 seconds. Choosing method 3.

#Method 2
sample_ilev = linspace(1,15,10000, dtype=float)
def CalcIlev2(H, t, height, lat, lon):
    
    finer_height = H(array([[t, ilev, lat, lon] for ilev in sample_ilev]))
    ilev_idx = argmin(abs(height-finer_height))
    return sample_ilev[ilev_idx] 
''' 
 
def CalcIlev(H, Hunit, t, c1_val, c2_val, height, ilev_grid, z_unit, high_res,model, verbose=False):
    '''Approximate ilev by inverting the chosen height function for one sat point.
    high_res default is 20 meters. (high_res), input height is in meters.'''
    
    # print('z_unit',z_unit)
    # print('height',height)
    #if input height not an altitude, then convert. Else convert to meters.
    if z_unit=='R_E':  #given height is a radius. convert
        radius = height
        height = ((radius - 1.0)*R_earth.value )+ 641  #in meters (ZACH ADDED due to numerical error)
    elif z_unit=='km':
        height*=1000.
    elif z_unit=='cm':
        height/=100.
    #else assume in meters
    
    #input height is in meters, time is in hours since midnight.
    #convert for common function output units. Add as more are discovered.
    if Hunit=='m':
        Hconv=1
    elif Hunit=='cm':
        Hconv=1./100.  #convert from cm to m
    elif Hunit=='km':
        Hconv=1000.   #convert from km to m
    
    #get height function output for the ilev range allowed in model
    #sample_ilev = linspace(min_ilev,max_ilev,75,dtype=float)  #typical range is 15


    # print('height',height)
    # print(f"lon {c1_val}" )
    # if model=='CTIPe' and c1_val < 0:
    #     c1_val=c1_val+360.

    max_height={}
    ### DIFF_EQ_EXTRAP EDIT: need to remove the top couple of pressure levels in TIEGCM
                            #### this change needs to be reflected in the max_height for the extrap to work
    if model=='TIEGCM': #or model=='CTIPe':
        ilev_grid=ilev_grid[:-2]
        if ilev_grid[0]==-6.75:
            height_levcoord = 'midpoint'
        else:
            height_levcoord = 'ilev'

    if model=='CTIPe': #or model=='CTIPe':
        # ilev_grid=ilev_grid[:-1]
        ilev_grid=ilev_grid
        height_levcoord ='ilev'    
        
    rough_height = H(array([[t, c1_val, c2_val, ilev] for ilev in ilev_grid]))*Hconv
    max_height, min_height = nanmax(rough_height), nanmin(rough_height)  #save for output, ignore NaNs if possible
    #will return NaN if all values are NaNs
    # print(rough_height)
    if isnan(max_height) or isnan(min_height):  #this happens if one or more of the coordinates is out of range (typically time)
        if verbose: print('Coordinate(s) are out of range:', t, c1_val, c2_val)
        # print('Coordinate(s) are out of range:', t, c1_val, c2_val, ilev_grid)
        # print("all nans encountered")
        # print(f" max_height={max_height},  min_height={min_height}")
        return NaN, NaN, NaN
    
#     print('ilev_grid: ', ilev_grid)
    # print('requested height: ', height, ' | Max_height: ',max_height)
    #handle requested heights outside of the allowed range at given t, c1, c2 coordinate
    if height>max_height:  #max first
        ### DIFF_EQ_EXTRAP EDIT: Add the maximum height  
        # print('requested height: ', height, ' | Max_height: ',max_height)

        if verbose: print('Given height is above maximum pressure level. Returning max possible pressure level instead')
        test_height, idx = NaN, 0
        while isnan(test_height):  #ignore top of ilev_grid in case of grid mismatch (e.g. some TIEGCM data)
            idx -= 1  #start from end/max of ilev_grid values
            max_ilev = ilev_grid[idx]
            test_height = H([t, c1_val, c2_val, max_ilev])
            
        ### DIFF_EQ_EXTRAP EDIT: Add the maximum height  
        return max_ilev, abs(height-max_height), max_height
    
    if height<min_height:  #then min
        if verbose: print('Given height is below minimum pressure level. Returning min possible pressure level instead')
        print('Given height is below minimum pressure level. Returning min possible pressure level instead')
        test_height, idx = NaN, -1
        while isnan(test_height):  #ignore top of ilev_grid in case of grid mismatch (e.g. some TIEGCM data)
            idx += 1  #start from beginning/min of ilev_grid values
            min_ilev = ilev_grid[idx]
            test_height = H([t, c1_val, c2_val, min_ilev])
        return min_ilev, abs(height-min_height),  max_height

    #continue with numerical inversion
    ilev_idx = argsort(npabs(height-rough_height))[0] #first value may not be in center of curve
    if ilev_idx==len(ilev_grid)-1: ilev_idx-=1  #use end instead to avoid errors
    test_ilev = linspace(ilev_grid[ilev_idx-1],ilev_grid[ilev_idx+1],100,dtype=float) 
    finer_height = H(array([[t, c1_val, c2_val, ilev] for ilev in test_ilev]))*Hconv
    
    #loop through process until requested resolution is achieved
    loop_count, final_res = 0, npabs(height-finer_height).min() 
    while final_res>high_res: 
        ilev_idx = argsort(npabs(height-finer_height))[0]
        if ilev_idx==len(test_ilev)-1: ilev_idx-=1  #use end instead to avoid errors
        test_ilev = linspace(test_ilev[ilev_idx-1],test_ilev[ilev_idx+1],100,dtype=float) 
        finer_height = H(array([[t, c1_val, c2_val, ilev] for ilev in test_ilev]))*Hconv
        #initial_res = final_res
        final_res = npabs(height-finer_height).min()
        #limit iterations to prevent infinite loops
        loop_count+=1
        if loop_count>10:# and (final_res-initial_res)/final_res<0.01: 
            break
    
    #output info for inspection
    if verbose or loop_count>10 or isnan(npabs(height-finer_height).min()): 
        print(f'\nRequested height: {height:.3f} m')
        print(f'Maximum allowed height: {max_height:.3f} m')
        print(f'Number of loops required to acheive resolution: {loop_count}')
        print('Time, c1, c2, height:', t, c1_val, c2_val, height)
        print(f'Calculated equivalent pressure level: {test_ilev[argmin(npabs(height-finer_height))]}')
        print(f'Height resolution achieved: {npabs(height-finer_height).min():.5f} m\n')
    
    ### DIFF_EQ_EXTRAP EDIT: Add the maximum height  
    return [test_ilev[argmin(npabs(height-finer_height))], npabs(height-finer_height).min(), max_height]

def call_CalcIlev(ilev_string, kamodo_object, sat_track0, z_unit, high_res):
    '''ilev agnostic method to gather parameters and call CalcIlev with less code.'''
    
    #retrieve parameters for CalcIlev call
    ilev_grid = getattr(kamodo_object, '_'+ilev_string)
    Hfunc = getattr(kamodo_object, 'H_'+ilev_string) 
    Hunit = kamodo_object.variables['H_'+ilev_string]['units']
    

    # model = kamodo_object.modelname
    # if model=='TIEGCM': #or model=='CTIPe':
    #     ilev_grid=ilev_grid[:-3]
    #     # Hfunc = getattr(kamodo_object, 'H_'+ilev_string)[:-2] 

    # if model=='CTIPe': #or model=='CTIPe':
    #     ilev_grid=ilev_grid[:-2]

    # print(kamodo_object.__dir__())   
    # print(kamodo_object.__dir__())   
    #### ZACH EDIT
    print('SF_utilities/call_CalcIlev():  ilev_grid = ',ilev_grid,  sep='')
    print('SF_utilities/call_CalcIlev():  Hfunc = ',Hfunc,  sep='')

    
    ### ==================================================================    
    ###     DIFF_EQ_EXTRAP EDIT
    ###  -----------------------------------------------------------------
    ###     Add the maximum height to be returned to the results dict
    ###  -----------------------------------------------------------------                  
    #for each call version, call CalcIlev and build satellite track with pressure level
    ### DIFF_EQ_EXTRAP EDIT: Add the maximum height  
#     sat_ilev, height_res = array([CalcIlev(Hfunc, Hunit, *sat_position, ilev_grid, 
#                        z_unit, high_res, verbose=False) for sat_position in sat_track0]).T
    
#     print('SF_utilities/call_CalcIlev(): sat_track0 alt  ',sat_track0[0],'',  sep='')
#     print('SF_utilities/call_CalcIlev(): sat_track0 alt  ',sat_track0[0][3],' Re',  sep='')
#     print('SF_utilities/call_CalcIlev(): sat_track0 alt  ',sat_track0[0][3]*6378100,' m',  sep='')
    
    ### DIFF_EQ_EXTRAP EDIT: Need model name in CalcIlev() to handle the TIEGCM top boundary removal
    model = kamodo_object.modelname



    # print(array([CalcIlev(Hfunc, Hunit, *sat_position, ilev_grid, 
    #                    z_unit, high_res,model, verbose=False) for sat_position in sat_track0]).T)


    sat_ilev, height_res, max_height = array([CalcIlev(Hfunc, Hunit, *sat_position, ilev_grid, 
                       z_unit, high_res,model, verbose=False) for sat_position in sat_track0]).T


    if len(height_res)>1:
        #Give user feedback about range of height resolution for height to ilev conversion
        #clean_height = array([val for val in height_res if not isnan(val)]) #remove NaN values
        max_res = nanmax(height_res)
        ### GEODYN EDIT: remove print
#         print(f'\nBest height resolution achieved: {nanmin(height_res):.5f} m')
#         print(f'Worst height resolution achieved: {max_res:.5f} m')
        if max_res>high_res: 
            idx = where(height_res>high_res)[0]
            print(f'{len(idx)} requested coordinates are out of range.')
            #print(array(sat_track0)[idx][-1])
#         print()
    else:
        ### GEODYN EDIT: remove print
#         print(f'\nHeight resolution achieved: {height_res[0]:.5f} m\n')
        max_res = height_res
    #if max_res>high_res: print('Files:', kamodo_object.filename)
    
    ### DIFF_EQ_EXTRAP EDIT: Add the maximum height  
    return sat_ilev, max_height


def sat_tracks(variable_list, kamodo_object, sat_time, c1, c2, c3, z_unit, 
               z_dependencies, high_res, verbose=False):
    '''Calculate satellite tracks for interpolation'''
                

    # max_height = {}
    
    #Create satellite tracks with appropriate inputs
    model = kamodo_object.modelname
    ilev_list = MW.Var_ilev(model)
    sat_track = {}  #initialize list of satellite tracks
    if '3D' in z_dependencies.keys():
        if verbose: print('Building height-independent satellite track.')
        sat_track['3D']=[[t, c1_val, c2_val] for t, c1_val, c2_val in zip(
            sat_time,c1,c2)]
    if '4D' in z_dependencies.keys():  #if function requires height (in km)
        if verbose: print('Building height-dependent satellite track (km).')
        sat_track['4D']=[[t, c1_val, c2_val, c3_val] for t, c1_val, c2_val, c3_val in zip(
            sat_time,c1,c2,c3)]
        #### ZACH EDIT:
        ###   need to add the max_height for the non-pressure level case
        if model =='GITM':
            max_height_val =  585.6*1000.  # m
            max_height = array([max_height_val for sat_position in sat_track['4D']])
#             print('Using GITM- max_height is ', max_height,' m')
#             print('      need to implement method to find max height automatically ')
        
    if sum([True if key in ilev_list else False for key in z_dependencies.keys()])>0:  
        #if ilev, ilev1, or milev is required for at least one variable
        if verbose: print('Converting height to pressure level and building satellite track.')
        start = perf_counter()  #Input h in meters
        sat_track0 = [[t, c1_val, c2_val, c3_val] for t, c1_val, c2_val, c3_val in zip(
            sat_time,c1,c2,c3)]
        for ilev_string in ilev_list:
            if ilev_string in z_dependencies.keys():
                # print("ilev_string", ilev_string)
                if len(z_dependencies[ilev_string])>0:  #ignore empty lists
                    #add new track type to dictionary
                    ### ==================================================================    
                    ###     DIFF_EQ_EXTRAP EDIT
                    ###  -----------------------------------------------------------------
                    ###     Add the maximum height to be returned to the results dict
                    ###  -----------------------------------------------------------------                  
                    ### DIFF_EQ_EXTRAP EDIT: Original code  
#                     sat_ilev = call_CalcIlev(ilev_string, kamodo_object, sat_track0, 
#                                              z_unit, high_res)    
                    ### DIFF_EQ_EXTRAP EDIT: Add the maximum height  
                    sat_ilev, max_height = call_CalcIlev(ilev_string, 
                                                         kamodo_object, 
                                                         sat_track0, 
                                                         z_unit,
                                                         high_res)    

                    sat_track[ilev_string]=[[t, c1_val, c2_val, ilev_val] for \
                                            t, c1_val, c2_val, ilev_val in zip(
                        sat_time,c1,c2,sat_ilev)]
                if verbose: print(f'{ilev_string} track added')              
        if verbose: print(f'Conversion took {perf_counter()-start} s for {len(sat_time)} positions.')
    ### DIFF_EQ_EXTRAP EDIT: Add the maximum height  
    return sat_track, max_height

def Model_FlyAway(reader, filename, variable_list, sat_time, c1, c2, c3, 
                  z_unit, z_dependencies, high_res, verbose=False):
    '''Perform flythrough for one day of data and one coordinate system.'''
 
    #create kamodo object, initialize some variables
    var_list = variable_list.copy()  #save copy before it gets altered by the reader
    kamodo_object = reader(filename, variables_requested=variable_list, gridded_int=False)
    
    #remove requested variables not found in data from variable list and z_dependencies
    newvar_list = [var for var in var_list if var in kamodo_object.variables.keys()]
    z_dependencies = {key:[var for var in value if var in newvar_list] \
                      for key, value in z_dependencies.items()}
    
    ### ==================================================================    
    ###     DIFF_EQ_EXTRAP EDIT
    ###  -----------------------------------------------------------------
    ###     Add the maximum height to be returned to the results dict
    ###  -----------------------------------------------------------------
    
#     print('ZACH - Model_FlyAway ')

    #create satellite tracks of types needed based on vertical dependencies
    ### DIFF_EQ_EXTRAP EDIT: original code  
#     sat_track = sat_tracks(newvar_list, kamodo_object, sat_time, c1, c2, c3,
#                            z_unit, z_dependencies, high_res, verbose=verbose)
    ### DIFF_EQ_EXTRAP EDIT: Add the maximum height  
    sat_track, max_height = sat_tracks(variable_list, kamodo_object, sat_time, c1, c2, c3,
                           z_unit, z_dependencies, high_res, verbose=verbose)

#     print(sat_track)
    ### ZACH EDIT
    # print('max_height          ', max_height)
#     print('coord converted c3  ', (c3 - 1.0)*R_earth.value)
#     print()
    #retrieve interpolator and interpolate data for each variable, using track 
    #type appropriate for each variable. 
    #print('Model_FlyAway2',sat_track.keys())
    #for var in var_list:
    #    print(var,[key for key, value in z_dependencies.items() if var in value][0])
    
    # results = {var: kamodo_object[var](sat_track[[key for key, value in \
    #                                          z_dependencies.items() if var in value][0]])\
    #            for var in newvar_list}
    
    results = {}
    for var in newvar_list:
        for key, value in z_dependencies.items():
            if var in value:
                selected_key = key
                break
        if var[0:3] == "rho" or var[0:2] == "N_" or var[0:3] == "mmr": 
            print(var)
            results[var] = np_exp(kamodo_object[var](sat_track[selected_key]))
        else:
            results[var] = kamodo_object[var](sat_track[selected_key])


    ### DIFF_EQ_EXTRAP EDIT: Add the maximum height to the results dict    

    # # print('max_height',max_height)
    # model = kamodo_object.modelname
    # if model== "CTIPe":
    #     results['max_height'] =  max_height['ilev']
    # if model== "TIEGCM":
    #     results['max_height'] =  max_height['ilev']




    results['max_height'] = max_height

    del kamodo_object   #save memory
    return results


def coordinate_systems(model, sat_time, c1, c2, c3, variable_list, coord_type, coord_grid):
    '''Detect what coordinate system is needed per variable, convert and return per type.'''
    
    #determine coordinate types needed, convert to alternative coordinates if necessary
    var_dict = MW.Model_Variables(model, return_dict=True)  #{varname:[desc, int, coord_name, grid_type, coord_list, unit]}
    var_coord_strs = unique([value[2].split('_')[0]+','+value[3] for key, value in var_dict.items() \
                             if key in variable_list])   #'SPH,sph','MAG,sph','GDZ,sph', etc

#     print('zach- coordinate_systems() var_dict', var_dict)
#     print('zach- coordinate_systems() var_coord_strs', var_coord_strs)
    
    if len(var_coord_strs)!=1 or var_coord_strs[0]!=(coord_type+','+coord_grid):  #then coordinate conversion needed
        #print('CoordConv check',coord_type+','+coord_grid, var_coord_strs, model, variable_list)
        new_coords={coord_name:[[key for key, value in var_dict.items() \
                                 if (value[2].split('_')[0]+','+value[3]==coord_name)\
                                     and key in variable_list]] \
                    for coord_name in var_coord_strs}  #key is coord type 'name,type'
                    #first value is a list of the variable names needing those coordinates
        #print('Performing needed coordinate conversions using SpacePy.')
        for key in new_coords.keys():   #e.g. key= 'GDZ,sph'
            #convert to needed coordinates, in/out order NO LONGER depends on coord_grid
            #can't use net_idx because indices won't line up anymore with split by files
            alt_c1, alt_c2, alt_c3, units_out = \
                    ConvertCoord(sat_time,c1,c2,c3,coord_type,coord_grid,*key.split(','))
            new_coords[key].extend([alt_c1,alt_c2,alt_c3])  #elements 1, 2, 3
            #print(key, alt_c1.min(), alt_c1.max(), alt_c2.min(), alt_c2.max(), 
            #      alt_c3.min(), alt_c3.max())
            #print(coord_type, coord_grid, sat_time, c1, c2, c3)
            #print(key, sat_time, alt_c1, alt_c2, alt_c3)            
            
            #determine unit of z coordinate. needed for conversion to ilev
            if key=='GDZ,sph': z_unit='km'
            else: z_unit='R_E'
            new_coords[key].append(z_unit)  #element 4
            #print('z_unit:', z_unit)
    else:
        new_coords = {coord_type+','+coord_grid:[variable_list,c1,c2,c3]}   
        
        #determine unit of z coordinate. needed for conversion to ilev
        if coord_type=='GDZ' and coord_grid=='sph': z_unit='km'
        else: z_unit='R_E'
        new_coords[coord_type+','+coord_grid].append(z_unit)  #element 4
        #print('z_unit:', z_unit)
    
    #determine z_dependency of relevant variables for each coordinate system    
    for key in new_coords.keys():
        z_dependencies = {}
        if 3 in [len(value[4]) for keyv, value in var_dict.items() if keyv in new_coords[key][0]]:
            z_dependencies['3D'] = [keyv for keyv, value in var_dict.items() if len(value[4])==3\
                                     and keyv in new_coords[key][0]]
        end_4coords = unique([value[4][-1] for keyv, value in var_dict.items() \
                      if keyv in new_coords[key][0] and len(value[4])==4])  #e.g. ['radius','ilev','height']
        if len(end_4coords)>0:  #if some variables in lista are 4D
            ilev_list = MW.Var_ilev(model)
            ilev_coords = list(unique([ilev for ilev in end_4coords if ilev in ilev_list]))
            if len(ilev_coords)>0: 
                for ilev_type in ilev_coords:
                    z_dependencies[ilev_type] = [keyv for keyv, value in var_dict.items()\
                                                 if value[4][-1]==ilev_type and \
                                                     keyv in new_coords[key][0]]  #add ilev dependencies
            if len(ilev_coords)<len(end_4coords):  #if there are other dependencies
                z_dependencies['4D'] = [keyv for keyv, value in var_dict.items()\
                                                 if value[4][-1] not in ilev_list \
                                                     and value[4][-1] in end_4coords\
                                                     and keyv in new_coords[key][0]]
        new_coords[key].append(z_dependencies) #element 5   
        #print('z_den:', new_coords[key][0], z_dependencies)    
    
    # if model== 'CTIPe':
    #     conv_alt = (new_coords['SPH,sph'][3][0] - 1.0)*R_earth.value        
    #     diff_conv_alt =  (conv_alt*1e-3) - c3
    #     print('input_alt: ', c3[0],' | converted alt: ',conv_alt*1e-3   ,sep='') 

    #     print('                  conversion is off by ',diff_conv_alt[0],'km ')


    return new_coords

# def coordinate_systems(model, sat_time, c1, c2, c3, variable_list, coord_type, coord_grid):
#     '''Detect what coordinate system is needed per variable, convert and return per type.'''
    
#     #determine coordinate types needed, convert to alternative coordinates if necessary
#     var_dict = MW.Model_Variables(model, return_dict=True)  #{varname:[desc, int, coord_name, grid_type, coord_list, unit]}
#     var_coord_strs = unique([value[2].split('_')[0]+','+value[3] for key, value in var_dict.items() \
#                              if key in variable_list])   #'SPH,sph','MAG,sph','GDZ,sph', etc
#     if len(var_coord_strs)!=1 or var_coord_strs[0]!=(coord_type+','+coord_grid):  #then coordinate conversion needed
#         #print('CoordConv check',coord_type+','+coord_grid, var_coord_strs, model, variable_list)
#         new_coords={coord_name:[[key for key, value in var_dict.items() \
#                                  if (value[2].split('_')[0]+','+value[3]==coord_name)\
#                                      and key in variable_list]] \
#                     for coord_name in var_coord_strs}  #key is coord type 'name,type'
#                     #first value is a list of the variable names needing those coordinates
#         #print('Performing needed coordinate conversions using SpacePy.')
#         for key in new_coords.keys():   #e.g. key= 'GDZ,sph'
#             #convert to needed coordinates, in/out order NO LONGER depends on coord_grid
#             #can't use net_idx because indices won't line up anymore with split by files
#             alt_c1, alt_c2, alt_c3, units_out = \
#                     ConvertCoord(sat_time,c1,c2,c3,coord_type,coord_grid,*key.split(','))
#             new_coords[key].extend([alt_c1,alt_c2,alt_c3])  #elements 1, 2, 3
#             #print(key, alt_c1.min(), alt_c1.max(), alt_c2.min(), alt_c2.max(), 
#             #      alt_c3.min(), alt_c3.max())
#             #print(coord_type, coord_grid, sat_time, c1, c2, c3)
#             #print(key, sat_time, alt_c1, alt_c2, alt_c3)            
            
#             #determine unit of z coordinate. needed for conversion to ilev
#             if key=='GDZ,sph': z_unit='km'
#             else: z_unit='R_E'
#             new_coords[key].append(z_unit)  #element 4
#             #print('z_unit:', z_unit)
#     else:
#         new_coords = {coord_type+','+coord_grid:[variable_list,c1,c2,c3]}   
        
#         #determine unit of z coordinate. needed for conversion to ilev
#         if coord_type=='GDZ' and coord_grid=='sph': z_unit='km'
#         else: z_unit='R_E'
#         new_coords[coord_type+','+coord_grid].append(z_unit)  #element 4
#         #print('z_unit:', z_unit)
    
#     #determine z_dependency of relevant variables for each coordinate system    
#     for key in new_coords.keys():
#         z_dependencies = {}
#         if 3 in [len(value[4]) for keyv, value in var_dict.items() if keyv in new_coords[key][0]]:
#             z_dependencies['3D'] = [keyv for keyv, value in var_dict.items() if len(value[4])==3\
#                                      and keyv in new_coords[key][0]]
#         end_4coords = unique([value[4][-1] for keyv, value in var_dict.items() \
#                       if keyv in new_coords[key][0] and len(value[4])==4])  #e.g. ['radius','ilev','height']
#         if len(end_4coords)>0:  #if some variables in lista are 4D
#             ilev_list = MW.Var_ilev(model)
#             ilev_coords = list(unique([ilev for ilev in end_4coords if ilev in ilev_list]))
#             if len(ilev_coords)>0: 
#                 for ilev_type in ilev_coords:
#                     z_dependencies[ilev_type] = [keyv for keyv, value in var_dict.items()\
#                                                  if value[4][-1]==ilev_type and \
#                                                      keyv in new_coords[key][0]]  #add ilev dependencies
#             if len(ilev_coords)<len(end_4coords):  #if there are other dependencies
#                 z_dependencies['4D'] = [keyv for keyv, value in var_dict.items()\
#                                                  if value[4][-1] not in ilev_list \
#                                                      and value[4][-1] in end_4coords\
#                                                      and keyv in new_coords[key][0]]
#         new_coords[key].append(z_dependencies) #element 5   
        
#     #### ZACH EDIT            
    # if model== 'CTIPe':
    #     conv_alt = (new_coords['SPH,sph'][3][0] - 1.0)*R_earth.value        
    #     diff_conv_alt =  (conv_alt*1e-3) - c3
    #     print('input_alt: ', c3[0],' | converted alt: ',conv_alt*1e-3   ,sep='') 

    #     print('                  conversion is off by ',diff_conv_alt[0],'km ')

#     return new_coords

### ==================================================================    
###     DIFF_EQ_EXTRAP EDIT
###  -----------------------------------------------------------------
###     Prepare some constants and functions
###  -----------------------------------------------------------------
###
###  -----------------------------------------------------------------
R_e = 6.371008e6     # meters  
k   = 1.38064852e-23 # [J/K]
m_p = 1.6726e-27     # [kg]
g_0 = 9.807          # [m/s^2]
def gravity(h):
    """For h in same units as R_e"""
    return g_0*pow(R_e/(R_e + h), 2)

def scale_height(T, height, molecular_mass):
    '''Scale height in meters, assuming mks'''
    return k*T/(gravity(height)*molecular_mass*m_p)
### ==================================================================    


def Model_SatelliteFlythrough(model, file_dir, variable_list, sat_time, c1, c2, 
                              c3, coord_type, coord_grid, high_res, verbose=False):
    '''
    Execute flythrough for model data. Returns results_dict.
    results_dict is a dictionary of the interpolated data for the entire data set
        sorted by variable name.
    file_dir is a string indicating where the data files are located.
    variable_list is a list of strings of the desired variable names. 
    sat_time is an array of timestamp values.
    c1, c2, c3 = x, y, z or lon, lat, height (or radius)
    if x, y, z, then must be in R_E units
    if radius -> R_E. if height -> km
    '''

    #Check that sat data is all the same length, will error if not
    if max(diff(array([len(sat_time),len(c3),len(c2),len(c1)])))>0:
        raise AttributeError (f'Satellite arrays or lists must all be the same length.\
                              Current array lengths are {len(sat_time)}, {len(c1)},\
                               {len(c2)}, and {len(c3)}') 
                               
    #reader prefers converted filename, even if it does not exist. will create if no wrapped data found.
    file_patterns = MW.FileSearch(model, file_dir)
    reader = MW.Model_Reader(model)  #Kamodo gets imported here
    #print(file_patterns)
            
    #match trajectory times to model data output files
    sat_time, times, net_idx = save_times(file_patterns, sat_time, model, verbose=verbose)
    
    #initialize results dictionary with given trajectory
    results_dict = {'utc_time': sat_time[net_idx], 'c1': c1[net_idx],
                    'c2': c2[net_idx], 'c3': c3[net_idx],
                    'net_idx': net_idx}  #index for comparison with other data from real satellite
    ### ZACH EDIT
#     print('ZACH-SF_utilities/Model_SatelliteFlythrough():  ',coord_type,coord_grid,  sep='')
    
#     print('ZACH-: model         ',model)
#     print('ZACH-: sat_time      ',sat_time)
#     print('ZACH-: c1            ',c1)
#     print('ZACH-: c2            ',c2)
#     print('ZACH-: c3            ',c3)
#     print('ZACH-: variable_list ',variable_list)
#     print('ZACH-: coord_type    ',coord_type)
#     print('ZACH-: coord_grid    ',coord_grid)
#     print('  ')

    
    #perform coordinate conversions and sort variables by coordinate systems
    coord_dict = coordinate_systems(model, sat_time, c1, c2, c3, variable_list, coord_type, coord_grid)
    
    # print('zach coord_dict  ',coord_dict)

    #perform flythroughs
    if verbose: print('Interpolating through model data... \n \n',end="")
    #print(coord_dict['GDZ,sph'][0], coord_dict['GDZ,sph'][5])
    interp_time = perf_counter()
    
#     print('zach 1 SF_utilities/Model_SatelliteFlythrough():  coord_dict.keys() = ',coord_dict.keys(),  sep='')

    for key in coord_dict.keys():
        ### ZACH EDIT
        #interpolate requested data for each day. FlyAway is specific to each wrapper
        #reader, file_name, variable_list, sat_time in hrs, c1, c2, c3, z_unit, z_dependencies, high_res
        list_results = [Model_FlyAway(reader, times[file_date][0], coord_dict[key][0], 
                                          ts_to_hrs(sat_time[times[file_date][6]], file_date.split('_')[0]),
                                          coord_dict[key][1][times[file_date][6]], 
                                          coord_dict[key][2][times[file_date][6]], 
                                          coord_dict[key][3][times[file_date][6]], 
                                          coord_dict[key][4], coord_dict[key][5],
                                          high_res, verbose=verbose) \
                            for file_date in times.keys() if len(sat_time[times[file_date][6]])>0]
        
        #get new variable list from results dictionaries
        newvar_list = []
        [newvar_list.extend(list(results.keys())) for results in list_results]
#         print('Zach newvar_list',newvar_list)
#         print('Zach list_results',list_results)
            
        #collect interpolated data into the same dictionary
        for var in newvar_list:  #sort and combine arrays for the same variable
#             print('Zach var', var)
            results_dict[var] = concatenate(tuple([results[var] for results in list_results]))
            
        ### ==================================================================    
        ###     DIFF_EQ_EXTRAP EDIT
        ###  -----------------------------------------------------------------
        ###     Add check for presense of N2 if using TIEGCM
        ###  -----------------------------------------------------------------
        ###     At the time of modifying this code, the CCMC-RoR tiegcm 
        ###     output files did not include the N2 mmr in their output. 
        ###     We check if its there and can calculate it manually as: 
        ###             1 - sum(the other constituent mmrs)
        ###  -----------------------------------------------------------------
        if (model == 4) or (model== 'TIEGCM'):
            if "mmr_N2" not in newvar_list:
                if verbose: print('Will have to calc N2 manually')
                flag_add_mmrN2 = True
                results_dict['mmr_N2'] = np_zeros(np_size( results_dict['mmr_O2']))
            else:
                flag_add_mmrN2 = False
        ### ==================================================================    
     
    ### ==================================================================    
    ###     DIFF_EQ_EXTRAP EDIT
    ###  -----------------------------------------------------------------
    ###     Implement the top boundary extrapolation
    ###  -----------------------------------------------------------------
    ###     We loop through all returned values for height 
    ###     and implement the top boundary extrapolation for 
    ###     any value that is above the maxiumum altitude,
    ###     updating the values accordingly.
    ###  -----------------------------------------------------------------

#     print('zach-- results_dict',results_dict.keys())
#     print('zach-- results_dict',results_dict.keys())
    
    for i, height_request in enumerate(results_dict['c3']*1000.):  ## this converts alt from km to m



        # print(results_dict['max_height'])



        if height_request >  results_dict['max_height'][i]:
            if verbose: print('         Correct with top boundary extrap. ')

            # rho comes in as g/cm^3 
#             rho_SIunit = results_dict['rho'][i]*1000.   #### CONVERT TO SI UNIT (kg/m^3)
            
            ### DIFF_EQ_EXTRAP EDIT: Perform extrapolation for TIEGCM case  
            if (model == 4) or (model== 'TIEGCM'):
    
                den_O1 = results_dict['mmr_O'][i]  * results_dict['rho'][i]*1000.  # [kg/m^3]
                den_O2 = results_dict['mmr_O2'][i] * results_dict['rho'][i]*1000.  # [kg/m^3]
                den_HE = results_dict['mmr_He'][i] * results_dict['rho'][i]*1000.  # [kg/m^3]
                if flag_add_mmrN2 == True:
                    results_dict['mmr_N2'][i] = 1 - (results_dict['mmr_O'][i]  + 
                                                     results_dict['mmr_O2'][i] + 
                                                     results_dict['mmr_He'][i])
                    den_N2 = results_dict['mmr_N2'][i] * results_dict['rho'][i]*1000.
                else:
                    den_N2 = results_dict['mmr_N2'][i] * results_dict['rho'][i]*1000.  # [kg/m^3] 
                neutral_temp = results_dict['T_n'][i]      # neutral temperature [K]

                # height_request is the altitude in m
                # top_boundary_m is the max  model altitude in meters
                alt_m = height_request
                top_boundary_m = results_dict['max_height'][i]
                        ## All calculations done in SI Units
                        ## the calc is done as an update to the "top boundary" density
                H_O1    = scale_height(neutral_temp, alt_m, 16.)   
                den_O1 *= np_exp(-(alt_m - top_boundary_m)/H_O1)
                H_O2    = scale_height(neutral_temp, alt_m, 32.) 
                den_O2 *= np_exp(-(alt_m - top_boundary_m)/H_O2)
                H_HE = scale_height(neutral_temp, alt_m, 4.)                                
                den_HE *= np_exp(-(alt_m - top_boundary_m)/H_HE)                         
                H_N2 = scale_height(neutral_temp, alt_m, 28.)  
                den_N2 *= np_exp(-(alt_m - top_boundary_m)/H_N2)

                #### Convert the constituent mass densities to number densities:
                #results_dict['nden_O'][i]  = ( den_O1/(m_p*16))*0.000001   ## convert from 1/m^3 to 1/cm^3
                #results_dict['nden_O2'][i] = ( den_O2/(m_p*32))*0.000001   ## 
                #results_dict['nden_He'][i] = ( den_HE/(m_p*4 ))*0.000001   ## 
                #results_dict['nden_N2'][i] = ( den_N2/(m_p*28))*0.000001   ## 

                ### Return rho in units of g/cm^3
                rho = den_O1 + den_O2 + den_HE + den_N2    

                ### update the mass mixing ratio values above the top boundary
                results_dict['rho'][i]    = rho/1000.
                results_dict['mmr_O'][i]  = den_O1/(results_dict['rho'][i]*1000.)
                results_dict['mmr_O2'][i] = den_O2/(results_dict['rho'][i]*1000.)
                results_dict['mmr_He'][i] = den_HE/(results_dict['rho'][i]*1000.)
                results_dict['mmr_N2'][i] = den_N2/(results_dict['rho'][i]*1000.)
                   
                
            ### DIFF_EQ_EXTRAP EDIT: Perform extrapolation for CTIPe case  
            elif (model == 0) or (model== 'CTIPe'):
#                 'N_O',
#                 'N_O2',
#                 'N_N2',
#                 'T'
                
                den_O1 = results_dict['N_O'][i]  * m_p * 16. # [kg/m^3]
                den_O2 = results_dict['N_O2'][i] * m_p * 32. # [kg/m^3]
                den_N2 = results_dict['N_N2'][i] * m_p * 28. # [kg/m^3]
                neutral_temp = results_dict['T'][i]          # neutral temp [K]

                # height_request is the altitude in m
                # top_boundary_m is the max  model altitude in meters
                alt_m = height_request
                top_boundary_m = results_dict['max_height'][i]
                        ## All calculations done in SI Units
                        ## the calc is done as an update to the "top boundary" density
                H_O1    = scale_height(neutral_temp, alt_m, 16.)   
                den_O1 *= np_exp(-(alt_m - top_boundary_m)/H_O1)
                H_O2    = scale_height(neutral_temp, alt_m, 32.) 
                den_O2 *= np_exp(-(alt_m - top_boundary_m)/H_O2)
                H_N2 = scale_height(neutral_temp, alt_m, 28.)  
                den_N2 *= np_exp(-(alt_m - top_boundary_m)/H_N2)


                ### Return rho in units of g/cm^3
                rho = den_O1 + den_O2  + den_N2    

                ### update the mass mixing ratio values above the top boundary
                                
                results_dict['N_O'][i]  = den_O1/(m_p * 16.)
                results_dict['N_O2'][i] = den_O2/(m_p * 32.)
                results_dict['N_N2'][i] = den_N2/(m_p * 28.)
                results_dict['rho'][i]  = rho
            if verbose: print('         Done with top boundary extrap. ')

        else:
            if (model == 4) or (model== 'TIEGCM'):
                if flag_add_mmrN2 == True:
                    results_dict['mmr_N2'][i] = 1 - (results_dict['mmr_O'][i]  + 
                                                     results_dict['mmr_O2'][i] + 
                                                     results_dict['mmr_He'][i]) 
    
    ### ============Done with top boundary extrap.========================    
    if verbose: print(f'done in {perf_counter()-interp_time:.5f} s.')
    return results_dict





def Prepare_Files(model, file_dir, call_type='normal'):
    '''Return a list of the required height input for each variable. Create wrapped files if needed.'''
    
    #Determine possible file patterns. Create wrapped files if needed.
    file_patterns = MW.FileSearch(model, file_dir, call_type=call_type)
#     print('zach-- Prepare_Files(): \n',file_patterns)

    
    times = check_timescsv(file_patterns, model, call_type=call_type)  #reader creates converted files if DNE
    #     print('Files prepared for run.')
    return 



#----------Code below is for Single Satellite Flythrough software--------------------------------

def find_singlefiletime(file_patterns, sat_time, model):
    '''Find file containing given single time. Adjust if within dt seconds.'''
    
    #t_time = perf_counter()
    times = check_timescsv(file_patterns, model, call_type='single')  #Retrieve file times
    #print('day_files timing:', perf_counter()-t_time)
    
    #Check if time is not in a file. Break if not.
    filename=''
    for file_date in times.keys():  
        if ((sat_time>=times[file_date][3]) & (sat_time<=times[file_date][4]\
                                                       +times[file_date][5])):  #end_time+dt
            filename = times[file_date][0]  #overwrites on purpose if time is found in later file
            #better for time to be in file than after end but in dt section
            
    #if time not found in file, print stuff and break, else return filename
    if filename=='':  
        print('\nCheck that time fits in file time ranges:')
        print(f'Data time (UTC): {sat_time}')
        print('Filename, Min DateTime, Max DateTime, Min Time (UTC), Max Time (UTC)')
        for file_date in times.keys(): 
            print (times[file_date][0].split('/')[-1].split('\\')[-1], times[file_date][3:5])
        raise AttributeError('No file found with the time given.')

    return filename    

def Single_DzCalc(results, sat_positions, kamodo_object, variable_list, z_dependence, dz):
    '''fly satellite through model data, per position
    sat_time, c1, c2, and c3 are all floats, not arrays
    z_dependence = dict '''
    
    if sum(dz)==0: return results       
    for i in range(len(variable_list)):
        sat_pos = sat_positions[[key for key, value in z_dependence.items() \
                                  if variable_list[i] in value][0]][0]
        if dz[i]==1 and len(sat_pos)==3:
            print(f'{variable_list[i]} has no vertical dependency. Vertical partial derivative not calculated.')
            continue
        elif dz[i]==1 and len(sat_pos)==4:  #calculate dz for +/- 1% of vertical value
            #check for undefined value at original position
            if isnan(kamodo_object[variable_list[i]](sat_pos)):
                print('Interpolated value at given position is undefined. Cannot calculate Dz for',
                      variable_list[i])
                continue
        
            #get +/-1% positions and interpolate
            sat_pos_up = [*sat_pos[0:3],sat_pos[3]*1.01]
            sat_pos_dn = [*sat_pos[0:3],sat_pos[3]*0.99]
            value_up = kamodo_object[variable_list[i]](sat_pos_up)[0]
            value_dn = kamodo_object[variable_list[i]](sat_pos_dn)[0]
            
            #if one is NaN (out of range), then replace with original value
            if isnan(value_up): value_up = kamodo_object[variable_list[i]](sat_pos)[0]
            elif isnan(value_dn): value_dn = kamodo_object[variable_list[i]](sat_pos)[0]
            
            #calculate diff and store
            results[variable_list[i]+'_dz'] = value_up-value_dn
    
    return results

def Single_FlyAway(model, file_dir, variable_list, dz, sat_time, c1, c2, c3, 
                   coord_type, coord_grid, high_res):
    '''Fly given satellite position through model code.'''
    #t0 = perf_counter()
    #time_array=[]

    #find file with sat_time 
    file_patterns = MW.FileSearch(model, file_dir, call_type='single')
    filename = find_singlefiletime(file_patterns, sat_time, model)
    #time_array.append(['test0a',perf_counter()-t0])
    #t1 = perf_counter()
    
    #get ctipe object for requested variable (+H too), remove variables not found
    var_list = variable_list  #save a copy because it gets altered
    reader = MW.Model_Reader(model)  #imports Kamodo here
    kamodo_object = reader(filename, variables_requested=variable_list, 
                           gridded_int=False)
    newvar_list = [var for var in var_list if var in kamodo_object.variables.keys()]
    #time_array.append(['test1',perf_counter()-t1])
    #t2 = perf_counter()    
    
    #convert to arrays if needed
    if not isinstance(sat_time, ndarray): sat_time = array([sat_time])
    if not isinstance(c1, ndarray): c1 = array([c1])
    if not isinstance(c2, ndarray): c2 = array([c2])
    if not isinstance(c3, ndarray): c3 = array([c3])
    
    #convert time to hrs since midnight
    str_date = datetime.strftime(kamodo_object.filedate, format='%Y-%m-%d')
    model_sat_time = ts_to_hrs(sat_time, str_date)
    #time_array.append(['test2',perf_counter()-t2])
    #t3 = perf_counter()        
    
    #deal with diff coordinate systems
    coord_dict = coordinate_systems(model, sat_time, c1, c2, c3, newvar_list, 
                                    coord_type, coord_grid)
    #time_array.append(['test3',perf_counter()-t3])
    #t4 = perf_counter()  
    #print(coord_dict)      
    
    #get tracks and interpolate for coordinate systems separately
    results_dict={}
    for key in coord_dict.keys():
        
        #get all types of tracks needed: variable list, ko, time_hrs, c1, c2, c3, z_unit, z_depend, high_res
        sat_positions = sat_tracks(coord_dict[key][0], kamodo_object, model_sat_time, 
                               *coord_dict[key][1:], high_res)  
                
        #perform interpolation with correct version of satellite position, store in dict
        results = {var: kamodo_object[var](sat_positions[[key for key, value in \
                                          coord_dict[key][5].items() if var in value][0]])[0]\
                  for var in coord_dict[key][0]}        
        
        #collect interpolated data into the same dictionary
        for var in coord_dict[key][0]:  #will be different variables in each loop
            results_dict[var] = results[var]
            
        #if requested, approximate dz for variables with +/-1% diff in vertical coordinate
        if sum(dz)>0:   #results, positions, ko, var_list, z_depend, dz
            results_dict = Single_DzCalc(results_dict, sat_positions, kamodo_object, 
                                         coord_dict[key][0], coord_dict[key][5], dz)
    #time_array.append(['test4',perf_counter()-t4])
    #print('test4', perf_counter()-t4)
    #print()
    #for i in range(len(time_array)):
    #    print(time_array[i])
    #print()
    return results_dict



#------ Code below here is for possible link directly into fortran------------
def test_validobject(kamodo_object, sat_time):   
    ''' Determine if a new ctipe object is needed bsed on the time given'''
    
    if isinstance(kamodo_object, list):  
        return True  #if a list, then ctipe DNE, get a new one
    elif (sat_time>=kamodo_object.filetimes[0]) and (sat_time<=kamodo_object.filetimes[1]):
        return False  #sat_time is within known time range of file, use current ctipe
    else:  #sat_time is not within known time range of file, get a new ctipe object
        return True
