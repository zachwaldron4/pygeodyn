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
col_msis2     =  "#2ca02c"  # 'tab:green'
col_jb2008    =  "#ff7f0e"  # 'tab:orange'
col_dtm2020   =  "#d62728"  # 'tab:red'
col_tiegcm_oc =  "#17becf"  # 'tab:cyan'
col_hasdm_oc  =  "#1f77b4"  # 'tab:blue'
col_ctipe_oc  =  "#9467bd"  # 'tab:purple'
col_gitm      =  '#e377c2'  # 'tab:pink'

x_annot_val = 1.1
font_dict=dict(family='Arial',size=14,color='black')
font_annot=dict(family='Arial',size=11,color='black')


def get_plot_params(model_name_string):
    '''
    INPUT:   
        Plot number, model_name string, x_annot_val
    
    RETURN:
        col, x_annot, y_annot1, y_annot2, m_size,   
    '''
        
    if model_name_string == 'msis2':
        col=col_msis2
    if model_name_string == 'manual_msis2':
        col=col_msis2
    elif model_name_string == 'dtm2020_o':
        col=col_dtm2020
    elif model_name_string == 'manual_dtm2020_o':
        col=col_dtm2020
    elif model_name_string == 'jb2008':
        col=col_jb2008
    elif model_name_string == 'manual_jb2008':
        col=col_jb2008
    elif model_name_string == 'tiegcm_oc':
        col=col_tiegcm_oc
    elif model_name_string == 'manual_tiegcm_oc':
        col=col_tiegcm_oc
    elif model_name_string == 'hasdm_oc':
        col=col_hasdm_oc
    elif model_name_string == 'ctipe_oc':
        col=col_ctipe_oc
    elif model_name_string == 'manual_ctipe_oc':
        col=col_ctipe_oc
    elif model_name_string == 'gitm':
        col=col_gitm
    elif model_name_string == 'manual_gitm':
        col=col_gitm
    elif model_name_string == 'manual_ensembleAvg':
        col='black'
    elif model_name_string == 'manual_ensembleWgtAvg':
        col='black'
    elif model_name_string == 'manual_ensembleMed':
        col='blue'

    ### Old Models
    elif model_name_string == 'dtm87':
        col='grey'
    elif model_name_string == 'jaachia71':
        col='grey'
    elif model_name_string == 'msis86':
        col='grey'
    elif model_name_string == 'msis00':
        col='tan'
        
    return(col)


    

###############################################################################

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

        arc_type = OBJECT[choose_model]['global_params']['prms']['arc_type']
        if arc_type == "Nominal30hr_and_AB":
            arc_name =arc[:8]+ maneuv_indicator
        else:
            arc_name =arc[:8]+('%.3f'%frachours).lstrip('0')+ maneuv_indicator

           
        # Truncate the ends of the arc for regular days
#         print(pd.to_datetime(arc, format = '%Y.%j'))
#         print(pd.to_datetime(arc, format = '%Y.%j')+pd.to_timedelta(24, 'h')-pd.to_timedelta(1, 'm'))
#         start_arc = pd.to_datetime(epochstart, format='%Y-%m-%d %H:%M:%S') + pd.to_timedelta(scale_cadence,'h')
#         end_arc   = pd.to_datetime(epochstop , format='%Y-%m-%d %H:%M:%S') - pd.to_timedelta(scale_cadence,'h')
        start_arc = pd.to_datetime(arc[:8], format='%Y.%j')
        end_arc = pd.to_datetime(arc[:8], format = '%Y.%j')+pd.to_timedelta(24, 'h')-pd.to_timedelta(1, 'm')
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
    den_dict['lat'      ]   =   []
#     den_dict['denscaled']   =   []
    den_dict['dens']   =   []
    
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
        start_arc  = pd.to_datetime(epochstart, format='%Y-%m-%d %H:%M:%S') + pd.to_timedelta(scale_cadence,'h')
        end_arc   = pd.to_datetime(epochstop, format='%Y-%m-%d %H:%M:%S') - pd.to_timedelta(scale_cadence,'h')

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

        #24hour period
        for it in np.arange(0,len_dates):
#             den_dict['denscaled'].append(\
#                     OBJECT[choose_model]['Density'][arc]['rho (kg/m**3)'].iloc[it]\
#                     *model_dict[choose_model]['ScalingFactors'][ii])
            den_dict['dens'].append(\
                    A['rho (kg/m**3)'].iloc[it])
            den_dict['lat'].append(\
                    A['Lat'].iloc[it])
            den_dict['dates'].append(A['Date'].iloc[it])

            
            
    sfdate_list = model_dict[choose_model]['ScalingFactor_times']
    sf_list     = model_dict[choose_model]['ScalingFactors']
    df          = pd.DataFrame.from_dict(den_dict)


    scaled_densities = []
    for isf, valsf in enumerate(sfdate_list):
    #     print(isf)
        if isf == 0:
            A=df.query(f" dates <= "              \
                 +f"{sfdate_list[isf].year}"       \
                 +f"{sfdate_list[isf].month:02d}"  \
                 +f"{sfdate_list[isf].day:02d}"    \
                 +f"{sfdate_list[isf].hour:02d}"   \
                 +f"{sfdate_list[isf].minute:02d}" \
                 +f"{sfdate_list[isf].second:02d}" \
                )
            # print('First index:', A)

        elif isf == len(sfdate_list)-1:
            A=df.query( f"{sfdate_list[isf-1].year}"       \
                       +f"{sfdate_list[isf-1].month:02d}"  \
                       +f"{sfdate_list[isf-1].day:02d}"    \
                       +f"{sfdate_list[isf-1].hour:02d}"   \
                       +f"{sfdate_list[isf-1].minute:02d}" \
                       +f"{sfdate_list[isf-1].second:02d}" \
                       +f" <= dates "                     \
                )
            # print('Last index:', A)
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
           
    den_dict['denscaled'] = scaled_densities
            
    return(den_dict)




def calc_rho_ScaledEnsembleWgtAvg(OBJECT, model_dict, model_fill, scale_cadence):
    
    """REDO THIS USING THE SCALED DENSITY DICT FROM THE ABOVE"""


    dates   = []
    rho_scaledEnsembleAvg = []
    std_scaledEnsembleAvg = []
    list_lat = []

    i_countfactor = 0

    for ii,arc in enumerate(OBJECT[model_fill]['global_params']['arc_input']):

        epochstart = OBJECT[model_fill]['global_params']['prms']['epoch_start'][ii]
        epochstop  = OBJECT[model_fill]['global_params']['prms']['epoch_stop'][ii]
        hrs        = pd.to_datetime(epochstart, format='%Y-%m-%d %H:%M:%S').hour
        frachours  =(hrs/24)
        #
        arc_name =arc+('%.3f'%frachours).lstrip('0')
        #
        start_arc  = pd.to_datetime(epochstart, format='%Y-%m-%d %H:%M:%S') + pd.to_timedelta(scale_cadence,'h')
        end_arc   = pd.to_datetime(epochstop, format='%Y-%m-%d %H:%M:%S') - pd.to_timedelta(scale_cadence,'h')

        
#         epochstart = OBJECT[model_fill]['global_params']['prms']['epoch_start'][ii]
#         hrs = pd.to_datetime(epochstart, format='%Y-%m-%d %H:%M:%S').hour
#         frachours =(hrs/24)
        
#         arc_name =arc+('%.3f'%frachours).lstrip('0')
#         start_arc = pd.to_datetime(arc, format='%Y.%j')
#         end_arc = pd.to_datetime(arc, format='%Y.%j')+ pd.to_timedelta(24,'h')
#         print('arc',arc)
#         print('start_arc', start_arc)
#         print('end_arc', end_arc)
#         print()
        ### SELECT ONLY THE MIDDLE 24-HOURS
        A = OBJECT[model_fill]['Density'][arc_name].query( \
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

        ### Loop thru all dates of interest in this arc
        ###   for each date we will make a list of rho, lats, wgts for each model
        for it in np.arange(0,len_dates):
            list_it_models = []
            list_it_lat    = []
            wgts           = []


            for model in model_dict.keys():
                print(A)
                A = OBJECT[model]['Density'][arc_name].query( \
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

                ### SCALED ENSEMBLE AVERAGE
                try:
                    list_it_models.append(A['rho (kg/m**3)'].iloc[it] \
                                          *model_dict[model]['ScalingFactors'][ii])
                    list_it_lat.append(  A['Lat'].iloc[it])
                    wgts.append(      model_dict[model]['Weight'][arc_name]    )
                except:
    #                 print(f"oops {model}, {arc} may not exist")
                    continue
            dates.append(A['Date'].iloc[it])
            list_lat.append(np.average(list_it_lat))

            i_countfactor  = i_countfactor + 1

            print(f"  {arc} ")

            print("list_it_models",list_it_models)
            print("wgts",wgts)
            x_bar_wgt  = np.average(list_it_models, weights=wgts)
            M = np.count_nonzero(wgts)
            rho_scaledEnsembleAvg.append(  x_bar_wgt   )    
#             std_scaledEnsembleAvg.append(  np.std(list_it_models)       )
            std_numerator  = sum([ wgts[i]*((list_it_models[i] - x_bar_wgt)**2) \
                                                 for i in range(len(wgts)) ])
            std_denominator= ((M-1)/M)*sum(wgts)
            std_scaledEnsembleAvg.append(  np.sqrt( std_numerator/std_denominator) )

    return(dates, list_lat, rho_scaledEnsembleAvg, std_scaledEnsembleAvg)






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
