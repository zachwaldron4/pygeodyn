import plotly.graph_objects as go
from plotly.offline import plot, iplot
from plotly.subplots import make_subplots
import plotly.express as px
import plotly.io as pio   ### Allows you to save plotly figs
import pandas as pd
import numpy as np


config = dict({
                'displayModeBar': False,
                'responsive': False,
                'staticPlot': False,
                'displaylogo': False,
                'showTips': False,
                })




# Simplify Plotting Schemes:
# col_msis2     =  "#2ca02c"  # 'tab:green'
# col_jb2008    =  "#ff7f0e"  # 'tab:orange'
# col_dtm2020   =  "#d62728"  # 'tab:red'
# col_tiegcm_oc =  "#17becf"  # 'tab:cyan'
# col_hasdm_oc  =  "#1f77b4"  # 'tab:blue'
# col_ctipe_oc  =  "#9467bd"  # 'tab:purple'
# col_gitm      =  '#e377c2'  # 'tab:pink'

# x_annot_val = 1.1
font_dict=dict(family='Arial',size=14,color='black')
font_annot=dict(family='Arial',size=11,color='black')


# def get_plot_params(model_name_string):
#     '''
#     INPUT:   
#         Plot number, model_name string, x_annot_val
    
#     RETURN:
#         col, x_annot, y_annot1, y_annot2, m_size,   
#     '''
        
#     if model_name_string == 'msis2':
#         col=col_msis2
#     if model_name_string == 'manual_msis2':
#         col=col_msis2
#     elif model_name_string == 'dtm2020_o':
#         col=col_dtm2020
#     elif model_name_string == 'manual_dtm2020_o':
#         col=col_dtm2020
#     elif model_name_string == 'jb2008':
#         col=col_jb2008
#     elif model_name_string == 'manual_jb2008':
#         col=col_jb2008
#     elif model_name_string == 'tiegcm_oc':
#         col=col_tiegcm_oc
#     elif model_name_string == 'manual_tiegcm_oc':
#         col=col_tiegcm_oc
#     elif model_name_string == 'hasdm_oc':
#         col=col_hasdm_oc
#     elif model_name_string == 'ctipe_oc':
#         col=col_ctipe_oc
#     elif model_name_string == 'manual_ctipe_oc':
#         col=col_ctipe_oc
#     elif model_name_string == 'gitm':
#         col=col_gitm
#     elif model_name_string == 'manual_gitm':
#         col=col_gitm
#     elif model_name_string == 'manual_ensembleAvg':
#         col='black'
#     elif model_name_string == 'manual_ensembleWgtAvg':
#         col='black'
#     elif model_name_string == 'manual_ensembleMed':
#         col='blue'

#     ### Old Models
#     elif model_name_string == 'dtm87':
#         col='grey'
#     elif model_name_string == 'jaachia71':
#         col='grey'
#     elif model_name_string == 'msis86':
#         col='grey'
#     elif model_name_string == 'msis00':
#         col='tan'
        
#     return(col)


    

###############################################################################




def orbit_avg_generic(Dates, rho, Lats ):
    lat     = np.asarray(Lats[:])
    time_pd = pd.to_datetime(Dates[:])

    # identify ascending equatorial pass as start of orbit
    a     = np.logical_and( lat[1:]*lat[0:-1]  < 0 , lat[1:] > lat[0:-1] )
    tup_i = a.ravel().nonzero( )
    i     = np.squeeze(tup_i)
    # initialize arrays
    d_avg      = np.zeros(np.size(i))
    time_avg   = []  
    for j in range(np.size(i)-1):
        # average density over orbital passes
        d_avg[j]      = np.mean(rho[i[j] : i[j+1]-1  ]  )
        # average dates
        # t1 = pd.to_datetime(time_pd[ i[j]    ])
        # t2 = pd.to_datetime(time_pd[ i[j+1]-1])
        t1 = pd.to_datetime(time_pd.values[ i[j]    ])
        t2 = pd.to_datetime(time_pd.values[ i[j+1]-1])
        datemiddle = pd.Timestamp(t1) + (pd.Timestamp(t2) - pd.Timestamp(t1)) / 2
        time_avg.append(datemiddle)
        
    return(time_avg, d_avg )








def calc_percent_change(a, b):
    from numpy import absolute as np_abs
    return(  ( (b-a)/np_abs(a) )*100)





def get_continuous_densities(OBJECT, choose_model,scale_cadence ,verbose=False):
    den_dict            =   {}
    den_dict['dates']   =   []
    den_dict['lat'  ]   =   []
    den_dict['den'  ]   =   []
    
    for ii,arc in enumerate(OBJECT[choose_model]['global_params']['arc_input']):
        epochstart = OBJECT[choose_model]['global_params']['prms']['epoch_start'][ii]
        epochstop  = OBJECT[choose_model]['global_params']['prms']['epoch_stop'][ii]
        hrs        = pd.to_datetime(epochstart, format='%Y-%m-%d %H:%M:%S').hour
        frachours  = (hrs/24)
        #
        if len(arc) == 9:
            maneuv_indicator = arc[8]
        else:
            maneuv_indicator = ''

        frachours=0.875

        arc_type =  OBJECT[choose_model]['global_params']['prms']['arc_type']
        if arc_type == "Nominal30hr_and_AB":
            arc_name =arc[:8]+ maneuv_indicator
        else:
            arc_name =arc[:8]+('%.3f'%frachours).lstrip('0')+ maneuv_indicator

           
        # Truncate the ends of the arc for regular days
#         print(pd.to_datetime(arc, format = '%Y.%j'))
#         print(pd.to_datetime(arc, format = '%Y.%j')+pd.to_timedelta(24, 'h')-pd.to_timedelta(1, 'm'))
#         start_arc = pd.to_datetime(epochstart, format='%Y-%m-%d %H:%M:%S') + pd.to_timedelta(scale_cadence,'h')
#         end_arc   = pd.to_datetime(epochstop , format='%Y-%m-%d %H:%M:%S') - pd.to_timedelta(scale_cadence,'h')

        start_arc = OBJECT[choose_model]['run_parameters'+arc_name]['prms_arc']['scaleparameter_times'][0]
        end_arc   = OBJECT[choose_model]['run_parameters'+arc_name]['prms_arc']['scaleparameter_times'][-1]
        # start_arc = pd.to_datetime(arc[:8], format='%Y.%j')
        # end_arc = pd.to_datetime(arc[:8], format = '%Y.%j')+pd.to_timedelta(24, 'h')-pd.to_timedelta(1, 'm')
        if verbose:
            print('arc'      , arc)
            print('start_arc', start_arc)
            print('end_arc'  , end_arc)
            print()

        ### SELECT ONLY THE MIDDLE 24-HOURS
        A = OBJECT[choose_model]['Density'][arc_name].query( \
                f"{start_arc.year}"         \
               +f"{start_arc.month:02d}"    \
               +f"{start_arc.day:02d}"      \
               +f"{start_arc.hour:02d}"     \
               +f"{start_arc.minute:02d}"   \
               +f"{start_arc.second:02d}"   \
               +f" <= Date < "                     \
               +f"{end_arc.year}"       \
               +f"{end_arc.month:02d}"  \
               +f"{end_arc.day:02d}"    \
               +f"{end_arc.hour:02d}"   \
               +f"{end_arc.minute:02d}" \
               +f"{end_arc.second:02d}" \
            )

        
        len_dates = np.shape(A['rho (kg/m**3)'])[0]

        for it in np.arange(0,len_dates):
            den_dict['den'].append(A['rho (kg/m**3)'].iloc[it])
            den_dict['lat'].append(A['Lat'].iloc[it])
            den_dict['dates'].append(A['Date'].iloc[it])

            
            
            
    return(den_dict)



def get_continuous_scaled_densities(OBJECT, model_dict, choose_model, scale_cadence):
    den_dict                =   {}
    den_dict['dates'    ]   =   []
    den_dict['lon'      ]   =   []
    den_dict['lat'      ]   =   []
    den_dict['alt'      ]   =   []
#     den_dict['denscaled']   =   []
    den_dict['dens']   =   []
    if scale_cadence ==24:
        den_dict['denscaled']=[]


    for ii,arc in enumerate(OBJECT[choose_model]['global_params']['arc_input']):
        epochstart = OBJECT[choose_model]['global_params']['prms']['epoch_start'][ii]
        epochstop = OBJECT[choose_model]['global_params']['prms']['epoch_stop'][ii]
        hrs        = pd.to_datetime(epochstart, format='%Y-%m-%d %H:%M:%S').hour
        frachours  =(hrs/24)
        #
        if len(arc) == 9:
            maneuv_indicator = arc[8]
        else:
            maneuv_indicator = ''

        arc_type = OBJECT[choose_model]['global_params']['prms']['arc_type']
        if arc_type == "Nominal30hr_and_AB":
            arc_name =arc[:8] + maneuv_indicator
        else:
            arc_name =arc[:8]+('%.3f'%frachours).lstrip('0')
        #
        # start_arc  = pd.to_datetime(epochstart, format='%Y-%m-%d %H:%M:%S') + pd.to_timedelta(scale_cadence,'h')
        # end_arc   = pd.to_datetime(epochstop, format='%Y-%m-%d %H:%M:%S') - pd.to_timedelta(scale_cadence,'h')
        # start_arc = pd.to_datetime(OBJECT[choose_model]['run_parameters'+arc_name]['prms_arc']['scaleparameter_times'][0])
        if scale_cadence == 24:
            start_arc = pd.to_datetime(arc_name,format="%Y.%j")
            end_arc   = pd.to_datetime(arc_name,format="%Y.%j") + pd.to_timedelta(24,'h')
        else:
            start_arc = pd.to_datetime(OBJECT[choose_model]['run_parameters'+arc_name]['prms_arc']['scaleparameter_times'][0])
            end_arc   = pd.to_datetime(OBJECT[choose_model]['run_parameters'+arc_name]['prms_arc']['scaleparameter_times'][-2])

        # print(end_arc)
        ### SELECT ONLY THE MIDDLE 24-HOURS
        A = OBJECT[choose_model]['Density'][arc_name].query( \
                f"{start_arc.year}"         \
               +f"{start_arc.month:02d}"    \
               +f"{start_arc.day:02d}"      \
               +f"{start_arc.hour:02d}"     \
               +f"{start_arc.minute:02d}"   \
               +f"{start_arc.second:02d}"   \
               +f" <= Date <= "                     \
               +f"{end_arc.year}"       \
               +f"{end_arc.month:02d}"  \
               +f"{end_arc.day:02d}"    \
               +f"{end_arc.hour:02d}"   \
               +f"{end_arc.minute:02d}" \
               +f"{end_arc.second:02d}" \
            )
        len_dates = np.shape(A['rho (kg/m**3)'])[0]

        #24hour period
        for it in np.arange(0,len_dates):
#             den_dict['denscaled'].append(\
#                     OBJECT[choose_model]['Density'][arc]['rho (kg/m**3)'].iloc[it]\
#                     *model_dict[choose_model]['ScalingFactors'][ii])
            den_dict['dens'].append(A['rho (kg/m**3)'].iloc[it])
            den_dict['lon'].append( A['Lon'].iloc[it])
            den_dict['lat'].append( A['Lat'].iloc[it])
            den_dict['alt'].append( A['Height (meters)'].iloc[it])
            den_dict['dates'].append( A['Date'].iloc[it])

            
            if scale_cadence ==24:
                den_dict['denscaled'].append(\
                        A['rho (kg/m**3)'].iloc[it]*model_dict[choose_model]['ScalingFactors'][ii])
    if scale_cadence ==24:
        pass        
    else:

        sfdate_list = model_dict[choose_model]['ScalingFactor_times']
        sf_list     = model_dict[choose_model]['ScalingFactors']
        df          = pd.DataFrame.from_dict(den_dict)


        scaled_densities = []
        scaled_dates = []
        scaled_lats = []
        scaled_lons = []
        scaled_alts = []
        for isf, valsf in enumerate(sfdate_list):
            # print(isf, valsf)
            if isf == 0:
                A=df.query(f" dates <= "              \
                    +f"{sfdate_list[isf].year}"       \
                    +f"{sfdate_list[isf].month:02d}"  \
                    +f"{sfdate_list[isf].day:02d}"    \
                    +f"{sfdate_list[isf].hour:02d}"   \
                    +f"{sfdate_list[isf].minute:02d}" \
                    +f"{sfdate_list[isf].second:02d}" \
                    )

            elif isf == len(sfdate_list)-1:
                A=df.query( f"{sfdate_list[isf-1].year}"       \
                        +f"{sfdate_list[isf-1].month:02d}"  \
                        +f"{sfdate_list[isf-1].day:02d}"    \
                        +f"{sfdate_list[isf-1].hour:02d}"   \
                        +f"{sfdate_list[isf-1].minute:02d}" \
                        +f"{sfdate_list[isf-1].second:02d}" \
                        +f" <= dates "                     \
                    )
            else:
                # select the values of density corresponding to SF datetimes
                A=df.query( f"{sfdate_list[isf].year}"         \
                        +f"{sfdate_list[isf].month:02d}"    \
                        +f"{sfdate_list[isf].day:02d}"      \
                        +f"{sfdate_list[isf].hour:02d}"     \
                        +f"{sfdate_list[isf].minute:02d}"   \
                        +f"{sfdate_list[isf].second:02d}"   \
                        +f" <= dates <= "                     \
                        +f"{sfdate_list[isf+1].year}"       \
                        +f"{sfdate_list[isf+1].month:02d}"  \
                        +f"{sfdate_list[isf+1].day:02d}"    \
                        +f"{sfdate_list[isf+1].hour:02d}"   \
                        +f"{sfdate_list[isf+1].minute:02d}" \
                        +f"{sfdate_list[isf+1].second:02d}" \
                        )
            ## Apply the scaling factor
            scaled_densities.extend(A['dens'].values*sf_list[isf])
            scaled_dates.extend(A['dates'].values)
            scaled_lats.extend(A['lat'].values)
            scaled_lons.extend(A['lon'].values)
            scaled_alts.extend(A['alt'].values)
            
        den_dict['denscaled'] = scaled_densities
        den_dict['datescaled'] = pd.to_datetime(scaled_dates)
        den_dict['lat_indx'] = scaled_lats
        den_dict['lon_indx'] = scaled_lons
        den_dict['alt_indx'] = scaled_alts
                
    return(den_dict)




def get_month_num(monthmodel):
    if monthmodel[:3] == 'oct':
        monthnum=10
    if monthmodel[:3] == 'nov':
        monthnum=11
    if monthmodel[:3] == 'dec':
        monthnum=12
    
    return(monthnum)

def calc_rho_ScaledEnsembleWgtAvg(models_dens,run_dict, month_list, run_list):
    '''
    Calculates the scaled density ensemble weighted average.
    Requires the dictionary of scaled densities (models_dens) as input.
    Requires the dictionary of weights (run_dict) as input.
    '''

    for imonth,month in enumerate(month_list):
        print(f"Processing {month}")
        len_dates = len(models_dens[month+run_list[0]]['datescaled'])

        # dates                 = np.zeros(len_dates)
        rho_scaledEnsembleAvg = np.zeros(len_dates)
        std_scaledEnsembleAvg = np.zeros(len_dates)
        lon_wgt_avg           = np.zeros(len_dates)
        lat_wgt_avg           = np.zeros(len_dates)
        alt_wgt_avg           = np.zeros(len_dates)



        for it,val in enumerate(models_dens[month+run_list[0]]['datescaled']):
            ## Get the index of the weight directly from the timestamp
            indx_wgt = pd.to_datetime(val).strftime(format='%Y.%j')

            ### Initialize date level list
            it_models = []
            it_lon    = []
            it_lat    = []
            it_alt    = []
            it_wgts   = []


            ###  At each date collect the `rho`, `lats`, `wgts` for each model
                # print(month)
            for i,model in enumerate(run_list):
                    # print(model)        
            
            # for model in models_dens.keys():  ### only select models with correct month 
                it_models.append(models_dens[month+model]['denscaled'][it] )
                it_lon.append(   models_dens[month+model]['lon_indx'][it] )
                it_lat.append(   models_dens[month+model]['lat_indx'][it] )
                it_alt.append(   models_dens[month+model]['alt_indx'][it] )
                it_wgts.append(  run_dict[month+model]['Weight'][indx_wgt] )

            #### Average the model densities at each time, each model weighted 
            x_bar_wgt  = np.average(it_models, weights=it_wgts) 
            rho_scaledEnsembleAvg[it] =  x_bar_wgt   

            lon_wgt_avg[it] = np.average(it_lon, weights=it_wgts) 
            lat_wgt_avg[it] = np.average(it_lat, weights=it_wgts) 
            alt_wgt_avg[it] = np.average(it_alt, weights=it_wgts) 


            ### Standard deviation (with weights)
            M = np.count_nonzero(it_wgts)
            std_numerator   = sum([ it_wgts[i]*((it_models[i] - x_bar_wgt)**2) for i in range(len(it_wgts))])
            std_denominator = ((M-1)/M)*sum(it_wgts)
            std_scaledEnsembleAvg[it]=  np.sqrt( std_numerator/std_denominator) 


        # 'x' denotes the ensemble weighted avg'

        models_dens[month+'Rho_x'] = {}
        models_dens[month+'Rho_x']['date']   = models_dens[month+run_list[0]]['datescaled']
        models_dens[month+'Rho_x']['lon']     = lon_wgt_avg
        models_dens[month+'Rho_x']['lat']     = lat_wgt_avg
        models_dens[month+'Rho_x']['alt']     = alt_wgt_avg
        models_dens[month+'Rho_x']['Rho_x']   = rho_scaledEnsembleAvg
        models_dens[month+'Rho_x']['Rho_std'] = std_scaledEnsembleAvg

    return(models_dens)







def normalize_density_msis2(sat_data, sat_name, alt_norm):
    import pandas as pd
    import numpy as np 
    import sys  
    from scipy.io import loadmat  #allows us to read in .mat files
    from datetime import datetime, timedelta
    import gc

    #### MAKE MSIS Take the 3HR Ap values
    from pymsis import msis
    SWI_option = [1.0]*25
    SWI_option[8] = -1.0
    
    if sat_name =='GRACE-FO':
        rhoname  = 'dens_x'
        datename = 'Date'
    if sat_name =='ICESat-2':
        rhoname  = 'Rho_x'
        datename = 'date'

        
        
    from netCDF4 import Dataset
    import numpy as np 
    import os
    import pandas as pd
    # import numpy  as np

    def read_nc_file( filename, variables):
        ''' This fu#### clear up some space        
    truncate_date    = np.logical_and(gpi_data['Date'].year>=2018 , gpi_data['Date'].year<=2019 )
    truncate_date3hr =  np.logical_and(gpi_data['Date_3hrAp'].year>=2018 , gpi_data['Date_3hrAp'].year<=2019 )

    gpi_data['Date_3hrAp'] = gpi_data['Date_3hrAp'][truncate_date3hr]
    gpi_data['Ap']         = np.array(gpi_data['Ap'])[truncate_date3hr]
    gpi_data['Date']       = gpi_data['Date'][truncate_date]
    gpi_data['Ap_dailyavg'] =  np.array(gpi_data['Ap_dailyavg'])[truncate_date]
    gpi_data['f107d_earth'] = np.array(gpi_data['f107d_earth'])[truncate_date]
    gpi_data['f107a_earth'] = np.array(gpi_data['f107a_earth'])[truncate_date]

    del gpi_data['DOY']
    del gpi_data['kp']
    del gpi_data['f107d']
    del gpi_data['f107a']
    del gpi_data['year_day']
    nction reads the TIEGCM .nc files and saves the given input variables to a dictionary.
            The breakloop feature is here so that if the file doesn't exist the code can still continue.  '''
        status = os.path.exists(filename)

        if status == True:
            data = {}
            for i, var_names in enumerate(variables):
                ncid =  Dataset(filename,"r+", format="NETCDF4")# filename must be a string
                varData = ncid.variables
                data[var_names] = np.array(varData[var_names])  
        elif status == False:
            print('No File Found', filename )
            breakloop = True
            data = 0
            return( data , breakloop)
        breakloop = False
        return(data,breakloop )



    #################################################################
    ##########    First fix the F10.7 values to be MgII    ##########
    #################################################################
    Kp_to_Ap = {0.    : 0  ,
                0.1   : 0  ,
                0.3   : 2  ,
                0.7   : 3  ,
                 1.   : 4  ,
                 1.3  : 5  ,
                 1.7  : 6  ,
                 2.   : 7  ,
                 2.3  : 9  ,
                 2.7  : 12 ,
                 3.   : 15 ,
                 3.3  : 18 ,
                 3.7  : 22 ,
                 4.   : 27 ,
                 4.3  : 32 ,
                 4.7  : 39 ,
                 5    : 48 ,
                 5.3  : 56 ,
                 5.7  : 67 ,
                 6.   : 80 ,
                 6.3  : 94 ,
                 6.7  : 111,
                 7    : 132,
                 7.3  : 154,
                 7.7  : 179,
                 8.   : 207,
                 8.3  : 236,
                 8.7  : 300,
                 9.   : 400}

    path_to_gpi = '/data/SatDragModelValidation/data/inputs/atmos_models/geo_phys_indicies/gpi_1960001-2021243_f107aDaily.nc'
    variables    =  ['year_day', 'f107d', 'f107a', 'kp']
    gpi_data    =  read_nc_file(path_to_gpi, variables)
    gpi_data    =  gpi_data[0]
    gpi_data['Date']= pd.to_datetime( gpi_data['year_day'], format='%Y%j')

    #### Make an array of datetimes for the 3hr Kps
    doy=[]
    Date_3hrAp = []
    Ap = []
    Ap_dailyavg = []

    for i,val in enumerate(gpi_data['Date']):
        doy.append(str(gpi_data['year_day'][i])[-3:])

        n=0
        avg_ap = []
        for ii,kpval in enumerate(gpi_data['kp'][i]):
            Ap.append( Kp_to_Ap[np.around(kpval,1)])
            avg_ap.append( Kp_to_Ap[np.around(kpval,1)])

            if ii==0:
                Date_3hrAp.append(val)
            else:
                Date_3hrAp.append(val + pd.Timedelta(hours=3*n))
            n+=1

        Ap_dailyavg.append(np.mean(avg_ap))
    gpi_data['Date_3hrAp']=  pd.to_datetime(Date_3hrAp)
    gpi_data['DOY']= doy
    gpi_data['Ap']= Ap
    gpi_data['Ap_dailyavg']= Ap_dailyavg

    #### Account for the F10.7 at earth (instead of referenced at 1AU)
    f107d_earth = []
    f107a_earth = []
    for i,val in enumerate(gpi_data['Date']):
        iday = int(gpi_data['DOY'][i])

        theta0 = 2 * np.pi * (iday)/365.
        sfeps = 1.000110 + 0.034221*np.cos(theta0)+0.001280* np.sin(theta0) +0.000719*np.cos(2.*theta0)+0.000077*np.sin(2.*theta0)

        f107d_earth.append(sfeps * gpi_data['f107d'][i])
        f107a_earth.append(sfeps * gpi_data['f107a'][i])

    gpi_data['f107d_earth'] = f107d_earth
    gpi_data['f107a_earth'] = f107a_earth





    #### clear up some space        
    truncate_date    = np.logical_and(gpi_data['Date'].year>=2018 , gpi_data['Date'].year<=2019 )
    truncate_date3hr =  np.logical_and(gpi_data['Date_3hrAp'].year>=2018 , gpi_data['Date_3hrAp'].year<=2019 )

    gpi_data['Date_3hrAp'] = gpi_data['Date_3hrAp'][truncate_date3hr]
    gpi_data['Ap']         = np.array(gpi_data['Ap'])[truncate_date3hr]
    gpi_data['Date']       = gpi_data['Date'][truncate_date]
    gpi_data['Ap_dailyavg'] =  np.array(gpi_data['Ap_dailyavg'])[truncate_date]
    gpi_data['f107d_earth'] = np.array(gpi_data['f107d_earth'])[truncate_date]
    gpi_data['f107a_earth'] = np.array(gpi_data['f107a_earth'])[truncate_date]

    del gpi_data['DOY']
    del gpi_data['kp']
    del gpi_data['f107d']
    del gpi_data['f107a']
    del gpi_data['year_day']
    del truncate_date, truncate_date3hr

    del Ap, doy, Date_3hrAp,  Ap_dailyavg, f107d_earth, f107a_earth, i
    del Kp_to_Ap, avg_ap
        
        
        
        
    Dnorm         = np.zeros(np.size(sat_data[datename]))

    for it,time in enumerate(sat_data[datename]):

        year  = time.year
        day   = time.dayofyear
        hours = time.hour

        ####---------------------------------------------
        #### Gather the Necessary Flux and Ap Information
        index_date = np.logical_and(gpi_data['Date'].year==year , gpi_data['Date'].dayofyear==day )
        f107a = [float(np.squeeze(np.asarray(gpi_data['f107a_earth'])[index_date]))]
        f107d = [float(np.squeeze(np.asarray(gpi_data['f107d_earth'])[index_date]))]
        Ap_daily_avg = float(np.squeeze(np.asarray(gpi_data['Ap_dailyavg'])[index_date]))

        ### Construct the necessary 3hr Ap Array to go into MSIS
        index_date3hr = np.logical_and(gpi_data['Date_3hrAp'].year==year , gpi_data['Date_3hrAp'].dayofyear==day )
        indexvals =  [i for i, x in enumerate(index_date3hr) if x]
        Ap_doy_windows = gpi_data['Date_3hrAp'][indexvals]

        #### Find the Current 3hr Kp window:A
        Ap_windw_hrs = [i.hour for i in Ap_doy_windows]
        Ap_windw_hrs = np.append(np.array(Ap_windw_hrs),24)  ## add the final window edge
        index_current_Ap = int(np.digitize([time.hour],Ap_windw_hrs))
        if index_current_Ap==8:
            index_current_Ap += -1
        indexglobal_currentAp = indexvals[index_current_Ap]
        Ap_3HR_current        = gpi_data['Ap'][indexglobal_currentAp]
        Ap_3HR_prior          = gpi_data['Ap'][indexglobal_currentAp-1]
        Ap_6HR_prior          = gpi_data['Ap'][indexglobal_currentAp-2]
        Ap_9HR_prior          = gpi_data['Ap'][indexglobal_currentAp-3]
        Ap_12hr_33hr_priorAVG = np.mean(gpi_data['Ap'][indexglobal_currentAp-11 :indexglobal_currentAp-3 ] ) ### 33hrs to 12 hours
        Ap_36hr_57hr_priorAVG = np.mean(gpi_data['Ap'][indexglobal_currentAp-19 :indexglobal_currentAp-11 ])  ### 36hrs to 57 hours

        apsin = [[Ap_daily_avg,          # (1) DAILY AP
                  Ap_3HR_current,        # (2) 3 HR AP INDEX FOR CURRENT TIME
                  Ap_3HR_prior,          # (3) 3 HR AP INDEX FOR 3 HRS BEFORE CURRENT TIME
                  Ap_6HR_prior,          # (4) 3 HR AP INDEX FOR 6 HRS BEFORE CURRENT TIME
                  Ap_9HR_prior,          # (5) 3 HR AP INDEX FOR 9 HRS BEFORE CURRENT TIME
                  Ap_12hr_33hr_priorAVG, # (6) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 12 TO 33 HRS PRIOR
                  Ap_36hr_57hr_priorAVG]]# (7) AVERAGE OF EIGHT 3 HR AP INDICIES FROM 36 TO 57 HRS PRIOR

        output2_sat = msis.run(time, sat_data['lon'][it], sat_data['lat'][it], sat_data['alt'][it]/1000 , f107d, f107a, apsin , version=2, options=SWI_option)
        output2_norm = msis.run(time, sat_data['lon'][it], sat_data['lat'][it], alt_norm                , f107d, f107a, apsin , version=2, options=SWI_option)


        # print(sat_data[rhoname][it])
        # print(output2_norm[0,0,0,0][0] / output2_sat[0,0,0,0][0])
        # print(sat_data[rhoname][it])
        ### Add the values to the growing lists
        Dnorm[it]   = sat_data[rhoname][it] * (output2_norm[0,0,0,0][0] / output2_sat[0,0,0,0][0])   # normalized density to norm altitude with MSIS2

    return(Dnorm)




















# def calc_rho_ScaledEnsembleWgtAvg(OBJECT, model_dict, model_fill, scale_cadence):
    
#     """REDO THIS USING THE SCALED DENSITY DICT FROM THE ABOVE"""


#     dates   = []
#     rho_scaledEnsembleAvg = []
#     std_scaledEnsembleAvg = []
#     list_lat = []

#     # i_countfactor = 0

#     # for ii,arc in enumerate(OBJECT[model_fill]['global_params']['arc_input']):

#     #     epochstart = OBJECT[model_fill]['global_params']['prms']['epoch_start'][ii]
#     #     epochstop  = OBJECT[model_fill]['global_params']['prms']['epoch_stop'][ii]
#     #     hrs        = pd.to_datetime(epochstart, format='%Y-%m-%d %H:%M:%S').hour
#     #     frachours  =(hrs/24)
#     #     #
#     #     arc_name =arc+('%.3f'%frachours).lstrip('0')

#     for ii,arc in enumerate(OBJECT[model_fill]['global_params']['arc_input']):
#         epochstart = OBJECT[model_fill]['global_params']['prms']['epoch_start'][ii]
#         hrs = pd.to_datetime(epochstart, format='%Y-%m-%d %H:%M:%S').hour
#         frachours =(hrs/24)
#         #
#         if len(arc) == 9:
#             maneuv_indicator = arc[8]
#         else:
#             maneuv_indicator = ''
#         arc_type = OBJECT[model_fill]['global_params']['prms']['arc_type']
#         if arc_type == "Nominal30hr_and_AB":
#             arc_name =arc[:8]+ maneuv_indicator
#         else:
#             arc_name =arc[:8]+('%.3f'%frachours).lstrip('0')+ maneuv_indicator

#         start_arc = OBJECT[model_fill]['run_parameters'+arc_name]['prms_arc']['scaleparameter_times'][0]
#         end_arc   = OBJECT[model_fill]['run_parameters'+arc_name]['prms_arc']['scaleparameter_times'][-1]

#         #
#         # start_arc  = pd.to_datetime(epochstart, format='%Y-%m-%d %H:%M:%S') + pd.to_timedelta(scale_cadence,'h')
#         # end_arc   = pd.to_datetime(epochstop, format='%Y-%m-%d %H:%M:%S') - pd.to_timedelta(scale_cadence,'h')

        
# #         epochstart = OBJECT[model_fill]['global_params']['prms']['epoch_start'][ii]
# #         hrs = pd.to_datetime(epochstart, format='%Y-%m-%d %H:%M:%S').hour
# #         frachours =(hrs/24)
        
# #         arc_name =arc+('%.3f'%frachours).lstrip('0')
# #         start_arc = pd.to_datetime(arc, format='%Y.%j')
# #         end_arc = pd.to_datetime(arc, format='%Y.%j')+ pd.to_timedelta(24,'h')
# #         print('arc',arc)
# #         print('start_arc', start_arc)
# #         print('end_arc', end_arc)
# #         print()
#         ### SELECT ONLY THE MIDDLE 24-HOURS
#         A = OBJECT[model_fill]['Density'][arc_name].query( \
#                 f"{start_arc.year}"         \
#                +f"{start_arc.month:02d}"    \
#                +f"{start_arc.day:02d}"      \
#                +f"{start_arc.hour:02d}"     \
#                +f"{start_arc.minute:02d}"   \
#                +f"{start_arc.second:02d}"   \
#                +f" <= Date < "                     \
#                +f"{end_arc.year}"       \
#                +f"{end_arc.month:02d}"  \
#                +f"{end_arc.day:02d}"    \
#                +f"{end_arc.hour:02d}"   \
#                +f"{end_arc.minute:02d}" \
#                +f"{end_arc.second:02d}" \
#             )
#         len_dates = np.shape(A['rho (kg/m**3)'])[0]

#         ### Loop thru all dates of interest in this arc
#         ###   for each date we will make a list of rho, lats, wgts for each model
#         for it in np.arange(0,len_dates):
#             list_it_models = []
#             list_it_lat    = []
#             wgts           = []


#             for model in model_dict.keys():
#                 # print(A)
#                 A = OBJECT[model]['Density'][arc_name].query( \
#                     f"{start_arc.year}"         \
#                    +f"{start_arc.month:02d}"    \
#                    +f"{start_arc.day:02d}"      \
#                    +f"{start_arc.hour:02d}"     \
#                    +f"{start_arc.minute:02d}"   \
#                    +f"{start_arc.second:02d}"   \
#                    +f" <= Date < "                     \
#                    +f"{end_arc.year}"       \
#                    +f"{end_arc.month:02d}"  \
#                    +f"{end_arc.day:02d}"    \
#                    +f"{end_arc.hour:02d}"   \
#                    +f"{end_arc.minute:02d}" \
#                    +f"{end_arc.second:02d}" \
#                 )

#                 ### SCALED ENSEMBLE AVERAGE
#                 ###   THIS ONLY WORKS FOR 24-hour scaling

#                 if scale_cadence ==24:
#                     try:
#                         list_it_models.append(A['rho (kg/m**3)'].iloc[it] \
#                                             *model_dict[model]['ScalingFactors'][ii])
#                         list_it_lat.append(  A['Lat'].iloc[it])
#                         wgts.append(      model_dict[model]['Weight'][arc_name]    )
#                     except:
#         #                 print(f"oops {model}, {arc} may not exist")
#                         continue
#             if scale_cadence ==24: 
#                 dates.append(A['Date'].iloc[it])
#                 list_lat.append(np.average(list_it_lat))
#             else:
                
#             # i_countfactor  = i_countfactor + 1

#             # print(f"  {arc} ")

#             # print("list_it_models",list_it_models)
#             # print("wgts",wgts)

#             # weights are done on an arc-basis and do not depend on Scale Cadence
#             x_bar_wgt  = np.average(list_it_models, weights=wgts)
#             M = np.count_nonzero(wgts)
#             rho_scaledEnsembleAvg.append(  x_bar_wgt   )    
# #             std_scaledEnsembleAvg.append(  np.std(list_it_models)       )
#             std_numerator  = sum([ wgts[i]*((list_it_models[i] - x_bar_wgt)**2) \
#                                                  for i in range(len(wgts)) ])
#             std_denominator= ((M-1)/M)*sum(wgts)
#             std_scaledEnsembleAvg.append(  np.sqrt( std_numerator/std_denominator) )

#     return(dates, list_lat, rho_scaledEnsembleAvg, std_scaledEnsembleAvg)












################################################################################

##### KP STUFF:
# from netCDF4 import Dataset
# def read_nc_file( filename, variables):
#     ''' This function reads the TIEGCM .nc files and saves the given input variables to a dictionary.
#         The breakloop feature is here so that if the file doesn't exist the code can still continue.  '''
#     status = os.path.exists(filename)
    
#     if status == True:
#         data = {}
#         for i, var_names in enumerate(variables):
#             ncid =  Dataset(filename,"r+", format="NETCDF4")# filename must be a string
#             varData = ncid.variables
#             data[var_names] = np.array(varData[var_names])  
#     elif status == False:
#         print('No File Found', filename )
#         breakloop = True
#         data = 0
#         return( data , breakloop)
#     breakloop = False
#     return(data,breakloop )


# arc_list_nc = []

# arc_list_18 = np.arange(292,366)
# for i in arc_list_18:
#     val = '2018'+str(i)
#     arc_list_nc.append(int(val))
    
#     #     print(val)
    
# arc_list_19 = np.arange(1,10)
# for i in arc_list_19:
#     val = '201900'+str(i)
#     arc_list_nc.append(int(val))

# path =  "/data/SatDragModelValidation/data/inputs/atmos_models/geo_phys_indicies/"
# path_to_f107 = path+ 'gpi_1960001-2021243_f107aDaily.nc'
# variables = ['year_day', 'f107d', 'f107a', 'kp']
# f107_data = read_nc_file(path_to_f107, variables)

# date = []
# kp_list = []
# f107d_list = []
# f107a_list  = []
# date_3hr = []
# doy_list    = []



# for i,val in enumerate(arc_list_nc):
    
#     index = f107_data[0]['year_day']==val
#     kp_list.append(f107_data[0]['kp'][index][0])
#     f107d_list.append(f107_data[0]['f107d'][index][0])
#     f107a_list.append(f107_data[0]['f107a'][index][0])
#     doy_list.append(str(f107_data[0]['year_day'][index][0])[-3:])

#     date.append(pd.to_datetime( str(val), format='%Y%j'))

#     date_3hr.append(pd.to_datetime( str(val), format='%Y%j') +pd.Timedelta(hours=0))
#     date_3hr.append(pd.to_datetime( str(val), format='%Y%j') +pd.Timedelta(hours=3))
#     date_3hr.append(pd.to_datetime( str(val), format='%Y%j') +pd.Timedelta(hours=6))
#     date_3hr.append(pd.to_datetime( str(val), format='%Y%j') +pd.Timedelta(hours=9))
#     date_3hr.append(pd.to_datetime( str(val), format='%Y%j') +pd.Timedelta(hours=12))
#     date_3hr.append(pd.to_datetime( str(val), format='%Y%j') +pd.Timedelta(hours=15))
#     date_3hr.append(pd.to_datetime( str(val), format='%Y%j') +pd.Timedelta(hours=18))
#     date_3hr.append(pd.to_datetime( str(val), format='%Y%j') +pd.Timedelta(hours=21))
# #     date_3hr.append(pd.to_datetime( str(val), format='%Y%j') +pd.Timedelta(hours=24))
    
# kp_expand = []
# for i in kp_list:
#     for ii in i:
#         kp_expand.append(ii)
        
        
        
# solar_fluxes = {}
# solar_fluxes['f107d_list'] = f107d_list
# solar_fluxes['f107a_list'] = f107a_list
# solar_fluxes['date']       = date
# solar_fluxes['date_3hr']   = date_3hr
# solar_fluxes['kp_expand']  = kp_expand

# f107d_earth = []
# f107a_earth = []
# ##### Account for the F10.7 at earth (instead of referenced at 1AU) #####

# for i_doy,val_doy in enumerate(doy_list):
#     iday = int(val_doy)
#     theta0 = 2 * np.pi * (iday)/365.
#     sfeps = 1.000110 + 0.034221*np.cos(theta0)+0.001280* np.sin(theta0) +0.000719*np.cos(2.*theta0)+0.000077*np.sin(2.*theta0)

#     f107d_earth.append(sfeps * solar_fluxes['f107d_list'][i_doy])
#     f107a_earth.append(sfeps * solar_fluxes['f107a_list'][i_doy])

# solar_fluxes['f107d_earth'] = f107d_earth
# solar_fluxes['f107a_earth'] = f107a_earth



# ### Prepare RMS total Plot arrays

# arc_listlist=[  ['2018.292', '2018.293', '2018.294', '2018.295', '2018.296', 
#                  '2018.297', '2018.298', '2018.299' ],                  
#                 #
#                 ['2018.304', '2018.305', '2018.306', '2018.307', '2018.308' ],  
#                 #
#                 ['2018.313', '2018.314', '2018.315', '2018.316', '2018.317',
#                  '2018.318', '2018.319', '2018.320', '2018.321', '2018.322',
#                  '2018.323', '2018.324', '2018.325', '2018.326', '2018.327' ],  
#                 #
#                 ['2018.335', '2018.336', '2018.337' ],  
#                 #
#                 ['2018.349', '2018.350', '2018.351', '2018.352' ],  
#                 #
#                 ['2018.356', '2018.357', '2018.358' ],  
#                 #
#                 ['2018.365', '2019.001', '2019.002', '2019.003', '2019.004', 
#                  '2019.005', '2019.006', '2019.007', '2019.008',
#                 '2019.009'],  
#                 ]
################################################################################


# index3h_1 = 168
# index3h_2 = 289-9

# ## -Calculate correlation coefficients
# #####################################################
# run_dict[model]['CorrCoeff']=[]

# # dailyKp = []
# # for i in kp_expand[index3h_1:index3h_2]:
# #     dailyKp.append(np.mean(i))

# corrcoeffs = {}
# for model in run_dict.keys():
#     sf_kplen = []
#     for sf in run_dict[model]['ScalingFactors']:
#         sf_kplen.append( sf )

#     print(model,'----------------')
#     corr = np.corrcoef(sf_kplen, kp_expand[index3h_1:index3h_2] ) 
#     print(f"   R={corr[0,1]}")
#     run_dict[model]['CorrCoeff'] = corr[0,1]
# ######################################################


# # Simplify Plotting Schemes:
# col_msis2     =  "#2ca02c"  # 'tab:green'
# col_jb2008    =  "#ff7f0e"  # 'tab:orange'
# col_dtm2020   =  "#d62728"  # 'tab:red'
# col_tiegcm_oc =  "#17becf"  # 'tab:cyan'
# col_hasdm_oc  =  "#1f77b4"  # 'tab:blue'
# col_ctipe_oc  =  "#9467bd"  # 'tab:purple'
# col_gitm      =  '#e377c2'  # 'tab:pink'

# all_cols = [col_jb2008,
#             col_dtm2020,
#             col_ctipe_oc,
#             col_tiegcm_oc,
#             col_msis2,
# #             col_gitm,
#            ]

# dailyKp = []

# # for i in kp_list[21:37]:
# #     dailyKp.append(np.mean(i))

# fig = make_subplots(rows=2, cols=2,
#                     subplot_titles=([None, "Correlation with Kp",
#                                      None,  None ]),
#                     vertical_spacing = 0.03,
#                     horizontal_spacing = 0.02,
#                         specs=[[{}, {}],
#                                [{},  {}         ]],
#                     column_widths=[0.8, 0.2],
#                     shared_xaxes=True)

# for model in run_dict.keys():
#     fig = plot__ScalingFactor(fig, obj[model],  run_dict[model] )

    
    
# fig.add_trace(go.Scatter(x=date_3hr[index3h_1:index3h_2],
#                            y=kp_expand[index3h_1:index3h_2] ,
# #                            name= 'F107d_1AU',
#                            mode='lines',
#                            opacity=1,
# #                                marker=dict(color='cornflowerblue', size=2 ),
#                            line = dict(shape = 'hv',dash='solid', color = 'black', width=2),
#                            showlegend=False),
#                            secondary_y=False,row=2, col=1)


# fig.add_hline(y=0, line_width=1, line_dash="dash", line_color="black", row=1, col=1)

# #######################################################
# font_dict=dict(family='Arial',size=16,color='black')
# #######################################################
   

# for i in [1,2]:
#     if i ==1:
#         xlabel=False
#     else:
#         xlabel=True
    
#     fig.update_xaxes(### LINE at axis border
#                       showline=True,showticklabels=xlabel,
#                       linecolor='black',linewidth=1,
#                      ### Major ticks
#                       ticks='inside',tickfont=font_dict,mirror=True,
#                       tickwidth=2,ticklen=9,
#                       tickcolor='grey',tick0="2018-11-9" ,
#                       dtick=86400000.0*1,    # milliseconds in a day, every 7 days
# #                       tickformat='%m/%d',
#                     ticklabelstep=2,
#                       #### Minor Ticks
#                        minor=dict(dtick=86400000.0, # milliseconds in a day
#                          tickwidth=1,ticklen=4,
#                          tickcolor='grey',ticks='inside'),
#                       ### GRID
#                        gridcolor='gainsboro',gridwidth=1,
#                        layer='above traces',tickangle=0,
#                        row=i, col=1)
#     fig.update_yaxes(showline=True,      # add line at x=0
#                      showticklabels=True,linecolor='black',  
#                      linewidth=1,ticks='inside',tickfont=font_dict, 
#                      mirror='allticks',tickwidth=1,tickcolor='black',
#                      gridcolor='gainsboro',gridwidth=1,layer='above traces',
#                      row=i, col=1)

# fig.update_yaxes( title="Kp Index",
#                  range=[-0.5,5],
#                  exponentformat= 'power',row=2, col=1)



# fig.add_trace(go.Bar(y=["JB2008",
#                         "DTM2020",
#                         "CTIPe",
#                         "TIEGCM",
#                         "MSIS2",
# #                         "GITM",
#                         ],    
#                      x=[run_dict["jb2008"]['CorrCoeff'],
#                         run_dict["dtm2020_o"]['CorrCoeff'],
#                         run_dict["ctipe_oc"]['CorrCoeff'],
#                         run_dict["tiegcm_oc"]['CorrCoeff'],
#                         run_dict["msis2"]['CorrCoeff'],
# #                         run_dict["gitm"]['CorrCoeff'],
#                        ] ,
#                      text=[np.round(run_dict["jb2008"]['CorrCoeff'],2)     ,
#                            np.round(run_dict["dtm2020_o"]['CorrCoeff'],2)  ,
#                            np.round(run_dict["ctipe_oc"]['CorrCoeff'],2)   ,
#                            np.round(run_dict["tiegcm_oc"]['CorrCoeff'],2)  ,
#                            np.round(run_dict["msis2"]['CorrCoeff'],2)      ,
# #                            np.round(run_dict["gitm"]['CorrCoeff'],2)       ,
#                        ] ,
#                     textposition='outside',
#                      textfont=dict(family='Arial',size=13,color='black'),
#                 marker_color=all_cols,
#                 showlegend=False),row=1, col=2)
# fig.update_traces(orientation="h", selector=dict(type='bar'))

# fig.add_annotation(x=-0.5,y='MSIS2',
#         xref="x",yref="y",
#         showarrow=False,
#         text='MSIS2',
#         font=dict(size=17,color="black"),
#         align="right",
#         row=1, col=2,
#         )
# fig.add_annotation(x=-0.5,y='TIEGCM',
#         xref="x",yref="y",
#         showarrow=False,
#         text='TIEGCM',
#         font=dict(size=17,color="black"),
#         align="right",
#         row=1, col=2,
#         )
# fig.add_annotation(x=-0.5,y='CTIPe',
#         xref="x",yref="y",
#         showarrow=False,
#         text='CTIPe',
#         font=dict(size=17,color="black"),
#         align="right",
#         row=1, col=2,
#         )
# fig.add_annotation(x=0.5,y='DTM2020',
#         xref="x",yref="y",
#         showarrow=False,
#         text='DTM2020',
#         font=dict(size=17,color="black"),
#         align="left",
#         row=1, col=2,
#         )
# fig.add_annotation(x=-0.5,y='JB2008',
#         xref="x",yref="y",
#         showarrow=False,
#         text='JB2008',
#         font=dict(size=17,color="black"),
#         align="right",
#         row=1, col=2,
#         )




# fig.update_xaxes(### LINE at axis border
#                   showline=True,showticklabels=True,
#                   linecolor='gainsboro',linewidth=1,
#                  ### Major ticks
#                   ticks='inside',tickfont=dict(family='Arial',size=14,color='black'),mirror=True,
#                   tickwidth=2,ticklen=9,
#                   tickcolor='white',
#                   ### GRID
#                    gridcolor='white',gridwidth=1,
#                    layer='above traces',
#                     row=1, col=2)
# fig.update_yaxes(showline=True,      # add line at x=0
#                  showticklabels=False,linecolor='gainsboro',  
#                  linewidth=1,ticks='inside',tickfont=dict(family='Arial',size=14,color='black'), 
#                  mirror='allticks',tickwidth=1,tickcolor='gainsboro',
#                  gridcolor='gainsboro',gridwidth=1,layer='above traces',
#                                     row=1, col=2)

# fig.update_xaxes(title="R, Pearson Corr. Coeff.",
#                  range=[-1,1.05],
#                  titlefont=dict(family='Arial',size=14,color='black'),
#                  row=1, col=2)

# fig.update_layout(autosize=False,    width=1000,    height=600,
#                   legend= {'itemsizing': 'trace'},
#                   font=font_dict, plot_bgcolor='white', 
#                  )
# annot = fig['layout']['annotations'][0]
# annot['yanchor']='bottom'
# annot['y']=1.015
# annot['yref']='paper'

# fig.update_xaxes(range=[pd.to_datetime( "181109-000000", format='%y%m%d-%H%M%S'),
#                         pd.to_datetime( "181123-000000", format='%y%m%d-%H%M%S')],
#                  row=1, col=1)


# fig.update_annotations(font_size=17)  # Increase size of subplot title
# fig.show(config=config)

# ### pio.write_image(fig, plots_dir+'twoweek_fullresult.pdf')
# # pio.write_image(fig, plots_dir+'f107_kp_twoweek.jpg', scale=3)

# # pio.write_image(fig, plot_dir+'24hourScalingFactors_w_R.jpg', scale=5)
