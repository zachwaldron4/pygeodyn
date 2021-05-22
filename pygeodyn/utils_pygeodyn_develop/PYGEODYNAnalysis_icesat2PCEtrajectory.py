#### ----------------------------------------
#### Import modules:
#### -----------------
import numpy as np
import pandas as pd
    #### Computer function
import os
import os.path
import sys
import subprocess
import shutil
import time
    #### modules for reading and converting data
import linecache
from datetime import datetime,timedelta
import copy


#### ----------------------------------------
#### Plotting modules:
#### -----------------
import plotly.graph_objects as go
from plotly.offline import plot, iplot
from plotly.subplots import make_subplots
import plotly.express as px
#### ----------------------------------------
#### ----------------------------------------
#### ----------------------------------------

# Simplify Plotting Schemes:
col1 = px.colors.qualitative.Plotly[0]
col2 = px.colors.qualitative.Plotly[1]
col3 = px.colors.qualitative.Plotly[2]





def add_arc_background_w_text(fig, y_vals,  arc_date_1, arc_date_2, iarc, arc_text = False):
    '''
    Define the arc parameters for this run to be plotted as background 
    
    This function plots background panels to distinguish the arcs and optionally plots text.
    
    '''
    import datetime

    if iarc % 2 == 0:
        color_bg = "LightSkyBlue"
    else:
        color_bg = "LightSlateGrey" 



    #### Arc Background + Label ####
    fig.add_vrect(
        x0=arc_date_1, x1=arc_date_2,
        fillcolor=color_bg, opacity=0.3,
        layer="below", line_width=0,
    )

    if arc_text == True:
        # Create scatter trace of text labels
        fig.add_trace(go.Scatter(
            x=[arc_date_3],
            y=[y_vals],
            text=["Arc "+ str(i_arc+1) ,
                 ],
            mode="text",
            showlegend=False,
            ))

    return(fig)




def legend_as_annotation(fig, den_model_string, color_it, x_annot, y_annot):
    fig.add_annotation(
            x=x_annot,
            y=y_annot,
            xref="x domain",
            yref="y domain",
            showarrow=False,
            text=den_model_string,
            font=dict(
                size=16,
                color="#ffffff"
                ),
            align="center",
            bordercolor="#c7c7c7",
            borderwidth=2,
            borderpad=4,
            bgcolor=color_it,
            opacity=0.9
            )

    return(fig)



def plot_residuals_observed(fig, obj_m1, plot_num):
            # obj_m1 = Obj_Geodyn
            # fig = make_subplots(rows=3, cols=1, 
            #         subplot_titles=(['PCE X', 'PCE Y', 'PCE Z']),
            #         vertical_spacing = 0.1,
            #                     )
#     arc_list = [obj_m1.__dict__['run_parameters']['ARC']]
    model_m1 = obj_m1.__dict__['global_params']['den_model']
#     iter_list = [ obj_m1.__dict__['run_parameters']['str_iteration']]

    if plot_num == 0:
        col = col1
        x_annot = 1.05
        y_annot = .97
        m_size = 1.5
    elif plot_num == 1:
        x_annot = 1.05
        y_annot = .8
        col = col2
        m_size = 1.5
    elif plot_num == 2:
        x_annot = 1.05
        y_annot = .63 
        col = col3    
        m_size = 1.5
        
#     print(arc_list)

    for i,  arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):
        i_arc = i+1
        
        str_run_param = 'run_parameters'+ arc
        final_iter = obj_m1.__dict__[str_run_param]['str_iteration']

        index_pce_x = obj_m1.Residuals_obs[arc]['StatSatConfig'] == 'PCE X    '
        index_pce_y = obj_m1.Residuals_obs[arc]['StatSatConfig'] == 'PCE Y    '
        index_pce_z = obj_m1.Residuals_obs[arc]['StatSatConfig'] == 'PCE Z    '

        fig.add_trace(go.Scattergl(x=obj_m1.Residuals_obs[arc]['Date'][index_pce_x][::10],
                                 y=obj_m1.Residuals_obs[arc]['Residual'][index_pce_x][::10]*1e2,
                                 name= 'PCE X',
                                 mode='markers',
                                 marker=dict(color=col,
                                 size=m_size,),
                                 showlegend=False,
                                 ),
                                 row=1, col=1,
                                 )
        fig.add_trace(go.Scattergl(x=obj_m1.Residuals_obs[arc]['Date'][index_pce_y][::10],
                                 y=obj_m1.Residuals_obs[arc]['Residual'][index_pce_y][::10]*1e2,
                                 name= 'PCE Y',
                                 mode='markers',
                                 marker=dict(color=col,
                                 size=m_size,),
                                 showlegend=False,
                                 ),
                                 row=2, col=1,
                                 )
        fig.add_trace(go.Scattergl(x=obj_m1.Residuals_obs[arc]['Date'][index_pce_z][::10],
                                 y=obj_m1.Residuals_obs[arc]['Residual'][index_pce_z][::10]*1e2,
                                 name= 'PCE Z',
                                 mode='markers',
                                 marker=dict(color=col,
                                 size=m_size,),
                                 showlegend=False,
                                 ),
                                 row=3, col=1,
                                 )
        arc_date_1 = obj_m1.Residuals_obs[arc]['Date'].iloc[0]
        arc_date_2 = obj_m1.Residuals_obs[arc]['Date'].iloc[-1]

        fig = add_arc_background_w_text(fig, 1.1*np.max(obj_m1.Residuals_obs[arc]['Residual'] ),arc_date_1, arc_date_2,i_arc, False)

        fig = legend_as_annotation(fig, obj_m1.__dict__['global_params']['den_model'], col, x_annot, y_annot)

    fig.update_layout(title="Observation Residuals")
    fig.update_layout(
                    autosize=False,
                    width=900,
                    height=900,
                    font=dict(size=14),
                    legend= {'itemsizing': 'constant'})

    fig.update_yaxes( title="Residuals (cm)",exponentformat= 'power',row=1, col=1)
    fig.update_yaxes( title="Residuals (cm)",exponentformat= 'power',row=2, col=1)
    fig.update_yaxes( title="Residuals (cm)",exponentformat= 'power',row=3, col=1)
    fig.update_xaxes( title="Date", row=3, col=1)
    return(fig)




# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## # 
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #



def Calc_Cd_percent_diff_apriori(obj_m1):
    '''
    CALCULATE:  
    '''

    SAT_ID = int(obj_m1.__dict__['global_params']['SATID'])
    which_stat = 'CURRENT_VALUE'
    model_m1 = obj_m1.__dict__['global_params']['den_model']

    all_cd_m1    = []
    all_dates_m1 = []

    for ii,arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):
        i_arc = ii+1
#         print(obj_m1.AdjustedParams[arc])
        last_iter = list(obj_m1.AdjustedParams[arc].keys())[-1]
        labels = list(obj_m1.AdjustedParams[arc][last_iter][SAT_ID]['0CD'].keys())

        for i,val in enumerate(obj_m1.AdjustedParams[arc][last_iter][SAT_ID]['0CD'].keys()):
            all_cd_m1.append(obj_m1.AdjustedParams[arc][last_iter][SAT_ID]['0CD'][val][which_stat])
            all_dates_m1.append(labels[i])

    # take % difference from a priori
    cd_apriori= 2.2
    percdiff_cd_m1 = ((np.array(all_cd_m1) - cd_apriori)/ cd_apriori)*100

    obj_m1_stats = {}
    obj_m1_stats['cd_apriori'] = cd_apriori
    obj_m1_stats['cd_percdiff_from_apriori'] = percdiff_cd_m1
    obj_m1_stats['all_cd'] = all_cd_m1
    obj_m1_stats['all_dates'] = all_dates_m1

    return(obj_m1_stats)



def plot_cd_and_percdiff_from_apriori(fig, obj_m1, plot_num):

    obj_m1_stats = Calc_Cd_percent_diff_apriori(obj_m1)

    
    if plot_num == 0:
        col = col1
        x_annot = 1.05
        y_annot = .95
        m_size = 2
        
    elif plot_num == 1:
        x_annot = 1.05
        y_annot = .8
        col = col2
        m_size = 2
        
    elif plot_num == 2:
        x_annot = 1.05
        y_annot = .65 
        col = col3    
        m_size = 2
        
    SAT_ID = int(obj_m1.__dict__['global_params']['SATID'])
    which_stat = 'CURRENT_VALUE'


    model_m1 = obj_m1.__dict__['global_params']['den_model']
######     make plot:
#     fig = make_subplots(
#         rows=2, cols=1,
#         subplot_titles=(["Timeseries of Cd", "Percent difference from a priori (Cd=2.2)"]),
#         vertical_spacing = 0.1,
#         )

    for ii,arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):
        
        str_run_param = 'run_parameters'+ arc
        final_iter = obj_m1.__dict__[str_run_param]['str_iteration']

        i_arc = ii+1
        last_iter = list(obj_m1.AdjustedParams[arc].keys())[-1]
        labels = list(obj_m1.AdjustedParams[arc][last_iter][SAT_ID]['0CD'].keys())

        val_list_1 = []
        for i in obj_m1.AdjustedParams[arc][last_iter][SAT_ID]['0CD'].keys():
            val_list_1.append(obj_m1.AdjustedParams[arc][last_iter][SAT_ID]['0CD'][i][which_stat])
        
        #### FIRST PLOT (CD TIMESERIES and APRIORI CD)
        fig.add_trace(go.Scattergl(x=labels,
                                   y=val_list_1,
                                   name= model_m1 ,#+ ' |  Arc ' +str(i_arc) +' | Iters: '+ str(last_iter),
                                   mode='markers',
                                   marker=dict(
                                   color=col,
                                   size=6,),
                                   showlegend=False,
                                   ),
                                   row=1, col=1,
                                   )
        fig.add_trace(go.Scattergl(x=labels,
                                   y=obj_m1_stats['cd_apriori']*np.ones(np.size(labels)),
                                   name= 'A priori Cd',
                                   mode='lines',
                                   marker=dict(
                                   color='black',
                                   size=1,),
                                   showlegend=False,
                                   ),
                                   row=1, col=1,
                                   )
        arc_date_1 = obj_m1.Residuals_obs[arc]['Date'].iloc[0]
        arc_date_2 = obj_m1.Residuals_obs[arc]['Date'].iloc[-1]

        fig = add_arc_background_w_text(fig, 2.2, arc_date_1, arc_date_2, i_arc, False)

    #### SECOND PLOT (PERC diff b/w apriori and Cd)
    fig = legend_as_annotation(fig, obj_m1.__dict__['global_params']['den_model'], col, x_annot, y_annot)

    fig.add_trace(go.Scattergl(x=obj_m1_stats['all_dates'],
                             y=obj_m1_stats['cd_percdiff_from_apriori'],
                            name= model_m1,
                            mode='markers',
                            marker=dict(
                            color=col,
                            size=6,
                            ),
                            showlegend=False,
                            ),
                            row=2, col=1,)


    # fix layout info:
    fig.update_yaxes( title="Cd ",exponentformat= 'power',row=1, col=1)
    fig.update_yaxes( title="% difference",exponentformat= 'power',row=2, col=1)
    fig.update_xaxes( title="Date", row=2, col=1)

    fig.update_layout(title="Time Dependent Drag Coefficient ")
    fig.update_layout(
                    autosize=False,
                    width=900,
                    height=900,
                    font=dict(size=14))

#     iplot(fig)
    return(fig)



#  load the saved F107 and AP for these arcs from a saved file.
# def get_geomag_and_solarflux_data(obj_m1):

    
#     arc_list = obj_m1.__dict__['run_parameters']['arc_input']
#     f107a =[]
#     f107d =[]
#     date = []
#     ap = []

#     import pandas as pd
#     for i,arc in enumerate(arc_list):
#     #     print(i,arc)
#         msisin_den_file = '/data/data_geodyn/results/st/msis00/msis00_acceloff/' + "DENSITY/st"+arc + ".goco05s_msisin"
#         DEN1_csv = pd.read_csv(msisin_den_file, 
#                             skiprows = 1, 
#                             names = ['IYYDDD',
#                                      'IYR',
#                                       'DAY',
#                                      'UTSEC',
#                                      'ALTKM',
#                                      'GLAT',
#                                      'GLON',
#                                      'STLOC', 
#                                      'AVGFLX',
#                                      'FLUX',
#                                      'AP1',
#                                      'AP2',
#                                      'AP3',
#                                      'AP4',
#                                      'AP5',
#                                      'AP6',
#                                      'AP7',
#                                     ],
#                             sep = '\s+',
#                             )
#         DEN1_csv['Date'] = (pd.to_datetime('0'+ ((DEN1_csv['IYR'].astype(int).astype(str))),  format='%y') 
#                         +  pd.to_timedelta(DEN1_csv['DAY'], unit='days'))

#         f107a.append(DEN1_csv['AVGFLX'].values)
#         f107d.append(DEN1_csv['FLUX'].values)
#         date.append(DEN1_csv['Date'].values)
#         ap.append(DEN1_csv['AP7'].values)
#     return(f107a, f107d, date, ap)








# def orb_avg(den_df, arc):
#     lat = np.asarray(den_df[arc]['Lat'])
#     time_pd = pd.to_datetime(den_df[arc]['Date'])
#     i = np.nonzero( lat[1:]*lat[0:-1]  <  np.logical_and(0 , lat[1:] > lat[0:-1] )  )
#     i = i[0]

#     d_avg = np.zeros(np.size(i))
#     height_avg = np.zeros(np.size(i))
#     time_avg = []

#     d_avg_rolling = []
    
#     roll_avg_count = 0
#     for j in range(np.size(i)-1):
#     #     print(j+1)
#         d_avg[j] = np.mean(  den_df[arc]['rho (kg/m**3)'][   i[j] : i[j+1]-1  ]  )
#         height_avg[j] = np.mean(  den_df[arc]['Height (meters)'][   i[j] : i[j+1]-1  ]  )
#         mean_time=time_pd[   i[j] : i[j+1]-1  ].mean() 
#         time_avg.append(  mean_time)

#         if roll_avg_count ==2:
#             d_avg_rolling.append(np.mean([ d_avg[j],  d_avg[j-1]]))
            
#             roll_avg_count =0
            
#         roll_avg_count+=1 
        
#     return(time_avg, d_avg, d_avg_rolling )
    






# def plot_density_decimated(fig, obj_m1, plot_num, decimation):

#     model_m1 = obj_m1.__dict__['run_parameters']['den_model']
#     if plot_num == 0:
#         col = col1
#         x_annot = 1.05
#         y_annot = .9
#     elif plot_num == 1:
#         x_annot = 1.05
#         y_annot = .8
#         col = col2
#     elif plot_num == 2:
#         x_annot = 1.05
#         y_annot = .7 
#         col = col3
    
#     data_nums_1 = 1000
#     data_nums_2 = decimation

# #     fig = make_subplots(rows=1, cols=1, 
# #             subplot_titles=(["Every 200th point"]),
# #                            )

#     for arc in obj_m1.__dict__['run_parameters']['arc']:

#         fig.add_trace(go.Scattergl(x=obj_m1.Density[arc]['Date'][::data_nums_2],
#                                  y=obj_m1.Density[arc]['rho (kg/m**3)'][::data_nums_2],
#         #                          name = geodyn_run_params_m1['den_model'],
#                                  mode='markers',
#                                     marker=dict(color=col,
#                                     size=4,
#                                     ),
#                                   showlegend=False,
#                                    ),
#                                    row=1, col=1,
#                                    )

#     fig.add_annotation(
#                 x = x_annot,
#                 y = y_annot,
#                 xref="x domain",
#                 yref="y domain",
#                 showarrow=False,
#                 text=model_m1,
#                 font=dict(
#                     size=16,
#                     color="#ffffff"
#                     ),
#                 align="center",
#                 bordercolor="#c7c7c7",
#                 borderwidth=2,
#                 borderpad=4,
#                 bgcolor=col,
#                 opacity=0.9
#                 )
#     fig.update_layout(
#         title="Density along Starlette Orbit",
#         )
#     fig.update_yaxes(type="log", exponentformat= 'power',)
#     fig.update_layout(legend= {'itemsizing': 'constant'})
#     fig.update_yaxes(title_text="kg/m^3", row=1, col=1)
#     fig.update_xaxes(title_text="Date", row=1, col=1)
#     fig.update_layout(legend= {'itemsizing': 'constant'})


#     # fig = add_arc_background_w_text(9.8e-14)
#     # fig = legend_as_annotation()
#     fig = add_arc_background_w_text(fig, 9.8e-14,  obj_m1.__dict__['run_parameters']['arc'], True)
# #     fig = legend_as_annotation(fig)

#     fig.update_layout(
#         autosize=False,
#         width=900,
#         height=700,
#     )
#     fig.update_layout(
#         font=dict(
#             size=18,
#                  ),)

#     return(fig)




# def plot_density_orbit_avg_with_fluxes(fig, obj_m1, plot_num ):
    
#     arc_list = obj_m1.__dict__['run_parameters']['arc']
#     model_m1 = obj_m1.__dict__['run_parameters']['den_model']
#     if plot_num == 0:
#         col = col1
#         x_annot = 1.05
#         y_annot = .9
#     elif plot_num == 1:
#         x_annot = 1.05
#         y_annot = .8
#         col = col2
#     elif plot_num == 2:
#         x_annot = 1.05
#         y_annot = .7 
#         col = col3

#     (f107a,
#     f107d,
#     date,
#     ap) = get_geomag_and_solarflux_data(obj_m1)

# #     fig = make_subplots(rows=3, cols=1, 
# #             subplot_titles=(['Orbit Averaged Density', 'Ap', 'F10.7']),
# #              horizontal_spacing = 0.05,
# #     #          vertical_spacing = 0.12,
# #              vertical_spacing = 0.1,
# #                        )

#     for i_arc,arc in enumerate(arc_list):
#         time_avg,d_avg, d_avg_rolling = orb_avg(obj_m1.Density, arc)
#         fig.add_trace(go.Scattergl(x=time_avg[::2],
#                                  y=d_avg_rolling,
#                                 name= ' Arc ' +str(i_arc) ,
#                                  mode='markers',
#                                  marker=dict(
#                                  color=col,
#                                  size=4,),
#                                  showlegend=False,
#                                    ),
#                                    row=1, col=1,
#                                    )


#         fig.add_trace(go.Scattergl(x=pd.to_datetime(date[i_arc][::500]),
#                                  y=ap[i_arc][::500],
#                                  name= 'Arc :'+str(i_arc+1),
#                                  mode='markers',
#                                  marker=dict(color = 'Grey',
#                                  size=4,),
#                                  showlegend=False,
#                                     ),
#                                    row=2, col=1,
#                                    )



#         fig.add_trace(go.Scattergl(x=pd.to_datetime(date[i_arc][::1000]),
#                              y=f107d[i_arc][::1000],
#     #                          name= 'Ap',
#                              mode='markers',
#                              marker=dict(color = 'Grey',
#                              size=4,),
#                               showlegend=False,
#                                    ),
#                                    row=3, col=1,
#                                    )

#     fig.add_annotation(
#                 x = x_annot,
#                 y = y_annot,
#                 xref="x domain",
#                 yref="y domain",
#                 showarrow=False,
#                 text=model_m1,
#                 font=dict(
#                     size=16,
#                     color="#ffffff"
#                     ),
#                 align="center",
#                 bordercolor="#c7c7c7",
#                 borderwidth=2,
#                 borderpad=4,
#                 bgcolor=col,
#                 opacity=0.9
#                 )
#     fig = add_arc_background_w_text(fig, 1.1*np.max(d_avg_rolling ), arc_list, False)
#     # fig = legend_as_annotation(fig)

#     fig.update_layout(    title="Density along Starlette Orbit"       )
#     fig.update_yaxes(type="log", exponentformat= 'power',row=1, col=1)
#     fig.update_xaxes(title_text="Date", row=3, col=1)
#     fig.update_yaxes(title_text="kg/m^3", row=1, col=1)
#     fig.update_yaxes(title_text="nT", row=2, col=1)
#     fig.update_yaxes(title_text="sfu", row=3, col=1)
#     fig.update_layout(legend= {'itemsizing': 'constant'})
#     fig.update_layout(
#         font=dict(          size=18, ),
#         autosize=False,
#         width=900,
#         height=1000,)
    
#     return(fig)

















# def plot_composite_density_cd_fluxes(fig, obj_m1, plot_num):
# #     fig = make_subplots(rows=3, cols=1, 
# #             subplot_titles=(['Orbit Averaged Density','Adjusted Cd', 'Geomagnetic and Solar Activity']),
# #              horizontal_spacing = 0.05,
# #     #          vertical_spacing = 0.12,
# #              vertical_spacing = 0.1,
# #               specs=[[{"secondary_y": False}],
# #                     [{"secondary_y": False}],
# #                     [{"secondary_y": True}], ])        



#     arc_list = obj_m1.__dict__['run_parameters']['arc']
#     model_m1 = obj_m1.__dict__['run_parameters']['den_model']
#     if plot_num == 0:
#         col = col1
#         x_annot = 1.05
#         y_annot = .9
#     elif plot_num == 1:
#         x_annot = 1.05
#         y_annot = .75
#         col = col2
#     elif plot_num == 2:
#         x_annot = 1.05
#         y_annot = .6 
#         col = col3

#     (f107a,
#     f107d,
#     date,
#     ap) = get_geomag_and_solarflux_data(obj_m1)


#     for i_arc,arc in enumerate(arc_list):
#         time_avg,d_avg, d_avg_rolling = orb_avg(obj_m1.Density, arc)
#         fig.add_trace(go.Scattergl(x=time_avg[::2],
#                                  y=d_avg_rolling,
#                                 name= ' Arc ' +str(i_arc) ,
#                                  mode='markers',
#                                  marker=dict(
#                                  color=col,
#                                  size=3,),
#                                  showlegend=False,
#                                    ),
#                                    row=1, col=1,
#                                    )



#         fig.add_trace(go.Scattergl(x=pd.to_datetime(date[i_arc][::500]),
#                                  y=ap[i_arc][::500],
#                                  name= 'Ap',
#                                  mode='markers',
#                                  marker=dict(color = 'blue',
#                                  size=3,),
#                                  showlegend=False,
#                                     ),
#                                  secondary_y=False,
#                                    row=3, col=1,
#                                    )



#         fig.add_trace(go.Scattergl(x=pd.to_datetime(date[i_arc][::1000]),
#                              y=f107d[i_arc][::1000],
#                              name= 'F10.7',
#                              mode='markers',
#                              marker=dict(color = 'red',
#                              size=3,),
#                               showlegend=False,
#                                    ),
#                                 secondary_y=True,
#                                    row=3, col=1,
#                                    )


#     fig = plot_cd_timeseries(fig, obj_m1,  plot_num, row_choose = 2)
    
#     fig = add_arc_background_w_text(fig, 1.1*np.max(d_avg_rolling ), arc_list, False)
#     fig.add_annotation(
#                 x = x_annot,
#                 y = y_annot,
#                 xref="x domain",
#                 yref="y domain",
#                 showarrow=False,
#                 text=model_m1,
#                 font=dict(
#                     size=16,
#                     color="#ffffff"
#                     ),
#                 align="center",
#                 bordercolor="#c7c7c7",
#                 borderwidth=2,
#                 borderpad=4,
#                 bgcolor=col,
#                 opacity=0.9
#                 )


#     # fix layout info:
#     fig.update_yaxes( title="Cd ",exponentformat= 'power',row=2, col=1)
#     fig.update_layout(
#         title="Density, Cd, and Activity Indicies",
#         )

#     fig.update_yaxes(type="log", exponentformat= 'power',row=1, col=1)

#     fig.update_xaxes(title_text="Date", row=3, col=1)
#     fig.update_yaxes(title_text="kg/m^3", row=1, col=1)
#     fig.update_yaxes(title_text="Ap [nT]", row=3, col=1, secondary_y=False, color='blue')
#     fig.update_yaxes(title_text="F10.7 [sfu]", row=3, col=1, secondary_y=True, color='red')

#     fig.update_layout(legend= {'itemsizing': 'constant'})
#     fig.update_layout(
#         font=dict(size=18,),
#         autosize=False,
#         width=850,
#         height=950,)


#     return(fig)












# def plot_residual_meas_summary(fig, obj_m1, plot_num):
    
#     import datetime
#     mark_size = 10
    
#     arc_list = obj_m1.__dict__['run_parameters']['arc']
#     model_m1 = obj_m1.__dict__['run_parameters']['den_model']
#     if plot_num == 0:
#         col = col1
#         x_annot = 1.05
#         y_annot = .9
#     elif plot_num == 1:
#         x_annot = 1.05
#         y_annot = .8
#         col = col2
#     elif plot_num == 2:
#         x_annot = 1.05
#         y_annot = .7 
#         col = col3    #     fig = make_subplots(rows=2, cols=1, 
# #          subplot_titles=(["Mean Residuals", 'RMS of Fit']),
# #          vertical_spacing = 0.1,
# #            )

#     for i,arc in enumerate(arc_list):
#         i_arc = i+1
#         arc_val = arc[:6]
#         arc_date = np.datetime64(datetime.datetime.strptime(arc_val, '%y%m%d'))
#         iter_index =  int(obj_m1.Residuals_summary[arc]['Iter'].values[-1])-1

#         mean_arc = obj_m1.Residuals_summary[arc]['MEAN'].values.astype(float)[iter_index]*1e2
#         rms_arc = obj_m1.Residuals_summary[arc]['RMS'].values.astype(float)[iter_index]

#         fig.add_trace(go.Scattergl(x=[arc_date],
#                                  y=[mean_arc],
#                                  name= 'Arc: '+str(i_arc),
#                                  mode='markers',
#                                  marker=dict(color=col,
#                                  size=10,),
#                                  showlegend=False,
#                                  ),
#                                   row=1, col=1,
#                                  )
#         fig.add_trace(go.Scattergl(x=[arc_date],
#                              y=[rms_arc],
#                              name= 'Arc: '+str(i_arc),
#                              mode='markers',
#                              marker=dict(color=col,
#                              size=10,),
#                              showlegend=False,
#                              ),
#                               row=2, col=1,
#                              )
    

#     fig = add_arc_background_w_text(fig, .002, arc_list, False)
# #     fig = legend_as_annotation(fig)
   
#     fig.add_annotation(
#                 x = x_annot,
#                 y = y_annot,
#                 xref="x domain",
#                 yref="y domain",
#                 showarrow=False,
#                 text=model_m1,
#                 font=dict(
#                     size=16,
#                     color="#ffffff"
#                     ),
#                 align="center",
#                 bordercolor="#c7c7c7",
#                 borderwidth=2,
#                 borderpad=4,
#                 bgcolor=col,
#                 opacity=0.9
#                 )
#     fig.update_layout(
#         title="Residual Measurement Summary",
#         )
#     fig.update_yaxes(title_text="Residual [cm]", row=1, col=1)
#     fig.update_yaxes(title_text="RMS", row=2, col=1)
#     fig.update_xaxes(title_text="Arc Dates", row=2, col=1)
#     #     fig.update_xaxes(title_text="Arc", row=2, col=1)
#     fig.update_layout(legend= {'itemsizing': 'constant'})
#     fig.update_xaxes(tickangle=0)

#     fig.update_layout(
#         font=dict(size=15),
#         autosize=False,
#         width=  750,
#         height= 750,
#             )
# #     fig.show()

#     avg_rms_m1 = []
#     avg_resid_m1 = []
#     for i,arc in enumerate(arc_list):
#         iter_index =  int(obj_m1.Residuals_summary[arc]['Iter'].values[-1])-1
#         mean_arc = obj_m1.Residuals_summary[arc]['MEAN'].values.astype(float)[iter_index]
#         rms_arc = obj_m1.Residuals_summary[arc]['RMS'].values.astype(float)[iter_index]
#         avg_rms_m1.append(rms_arc)
#         avg_resid_m1.append(mean_arc)

    

#     avg_rms_m1 =   "{:.5e}".format(np.mean(avg_rms_m1))  
#     avg_resid_m1 = "{:.5e}".format(np.mean(avg_resid_m1)*1e2)


#     print(model_m1, ' RMS ' ,avg_rms_m1)
#     print(model_m1, ' Resid ' ,avg_resid_m1)
#     print()
#     return(fig)








# def plot_drhodz(fig, obj_m1, obj_m2, plot_num,  decimate, textm1, textm2):
    
#     arc_list = obj_m1.__dict__['run_parameters']['arc']
#     model_m1 = obj_m1.__dict__['run_parameters']['den_model']
        
#     for i,arc in enumerate(arc_list):
#         i_arc = i+1

#         fig.add_trace(go.Scattergl(x=obj_m1.__dict__['Density'][arc]['Date'][::decimate],
#                                  y=np.abs(obj_m1.__dict__['Density'][arc]['drhodz (kg/m**3/m)'][::decimate]),
#                                  name= 'Arc: '+str(i_arc),
#                                  mode='markers',
#                                  marker=dict(color=col1,
#                                  size=3,),
#                                  showlegend=False,
#                                  ),
#                                   row=1, col=1,
#                                  )
#         fig.add_trace(go.Scattergl(x=obj_m2.__dict__['Density'][arc]['Date'][::decimate],
#                                  y=np.abs(obj_m2.__dict__['Density'][arc]['drhodz (kg/m**3/m)'][::decimate]),
#                                  name= 'Arc: '+str(i_arc),
#                                  mode='markers',
#                                  opacity=0.8,
#                                  marker=dict(color=col2,
#                                  size=3,),
#                                  showlegend=False,
#                                  ),
#                                   row=1, col=1,
#                                  )

        
#         A = obj_m1.__dict__['Density'][arc]['drhodz (kg/m**3/m)'][::decimate]
#         B = obj_m2.__dict__['Density'][arc]['drhodz (kg/m**3/m)'][::decimate]
        
#         percdiff = ((A - B)/ B)*100

#         fig.add_trace(go.Scattergl(x=obj_m1.__dict__['Density'][arc]['Date'][::decimate],
#                          y=percdiff,
#                          name= 'Arc: '+str(i_arc),
#                          mode='markers',
#                          marker=dict(color=col3,
#                          size=3,),
#                          showlegend=False,
#                          ),
#                           row=2, col=1,
#                          )
        
#     fig = add_arc_background_w_text(fig, 1.1*np.max(A ), arc_list, False)

#     fig.add_annotation(
#                 x = 1.15,
#                 y = .95,
#                 xref="x domain",
#                 yref="y domain",
#                 showarrow=False,
#                 text= textm1 ,
#                 font=dict(
#                     size=16,
#                     color="#ffffff"
#                     ),
#                 align="center",
#                 bordercolor="#c7c7c7",
#                 borderwidth=2,
#                 borderpad=4,
#                 bgcolor=col1,
#                 opacity=0.9,
#                 row=1, col=1,
#                 )
#     fig.add_annotation(
#                 x = 1.15,
#                 y = .7,
#                 xref="x domain",
#                 yref="y domain",
#                 showarrow=False,
#                 text= textm2 ,
#                 font=dict(
#                     size=16,
#                     color="#ffffff"
#                     ),
#                 align="center",
#                 bordercolor="#c7c7c7",
#                 borderwidth=2,
#                 borderpad=4,
#                 bgcolor=col2,
#                 opacity=0.9,
#                 row=1, col=1,
#                 )
    
#     fig.update_layout(  title='Compare DRHODZ Versions for '+model_m1 )
#     fig.update_yaxes(type="log", exponentformat= 'power', row=1, col=1)
#     fig.update_yaxes(title = '|kg/m^4|', row=1, col=1)
#     fig.update_yaxes(title = '% Difference', row=2, col=1)
#     fig.update_xaxes(title = 'Date', row=2, col=1)


#     fig.update_layout(
#         font=dict(size=17))

#     return(fig)









# def plot_simple_choose(fig, obj_m1, plot_num, choose_dataset, choose_variable, decimate):
    
#     arc_list = obj_m1.__dict__['run_parameters']['arc']
#     model_m1 = obj_m1.__dict__['run_parameters']['den_model']
#     if plot_num == 0:
#         col = col1
#         x_annot = 1.05
#         y_annot = .9
#     elif plot_num == 1:
#         x_annot = 1.05
#         y_annot = .8
#         col = col2
#     elif plot_num == 2:
#         x_annot = 1.05
#         y_annot = .7 
#         col = col3    
        
#     for i,arc in enumerate(arc_list):
#         i_arc = i+1

#         fig.add_trace(go.Scattergl(x=obj_m1.__dict__[choose_dataset][arc]['Date'][::decimate],
#                                  y=obj_m1.__dict__[choose_dataset][arc][choose_variable][::decimate],
#                                  name= 'Arc: '+str(i_arc),
#                                  mode='markers',
#                                  marker=dict(color=col,
#                                  size=3,),
#                                  showlegend=False,
#                                  ),
#                                  )
# #     fig = add_arc_background_w_text(fig, 1.1*np.max(obj_m1.choose_dataset[arc]['Residual'] ), arc_list, False)

#     fig.add_annotation(
#                 x = x_annot,
#                 y = y_annot,
#                 xref="x domain",
#                 yref="y domain",
#                 showarrow=False,
#                 text=model_m1,
#                 font=dict(
#                     size=16,
#                     color="#ffffff"
#                     ),
#                 align="center",
#                 bordercolor="#c7c7c7",
#                 borderwidth=2,
#                 borderpad=4,
#                 bgcolor=col,
#                 opacity=0.9
#                 )
    
# #     fig.update_layout(
# #         title='Observation Residuals',
# #         yaxis_title="Residuals",
# #         xaxis_title="Date",
# #         )
# #     fig.update_layout(legend= {'itemsizing': 'constant'})
#     fig.update_layout(
#         font=dict(size=17))
#     # iplot(fig) 
#     return(fig)

def scale_density_with_cdadjustment(obj_m1):
    obj_m1_stats = Calc_Cd_percent_diff_apriori(obj_m1)
    cd_windows = obj_m1_stats['all_dates']
    cd_scaling = obj_m1_stats['cd_percdiff_from_apriori']/100
    
    save_dens  = []
    save_dates = []
#     for arc in obj_m1.__dict__['run_parameters']['arc']:
    for ii,arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):
        
#         str_run_param = 'run_parameters'+ arc
#         final_iter = obj_m1.__dict__[str_run_param]['str_iteration']

#         i_arc = ii+1
        dates = obj_m1.Density[arc]['Date']
        dens  = obj_m1.Density[arc]['rho (kg/m**3)']

        for i,val in enumerate(cd_windows):
            window = np.logical_and(dates>cd_windows[i], dates < cd_windows[i] + timedelta(hours=8))
            density_scaled = np.add( dens[window], np.multiply(dens[window],cd_scaling[i]))
    #         obj_m1.Density[arc]['dens_scaled'] = scale_loop #dens_scaled

            save_dates.append( dates[window])
            save_dens.append( density_scaled)
    return(save_dates, save_dens)



def plot_ScaleDensity_with_CdScaleFactor(fig, obj_m1, plot_num, decimation):


    model_m1 = obj_m1.__dict__['global_params']['den_model']
    if plot_num == 0:
        col = col1
        x_annot = 1.05
        y_annot = .9
    elif plot_num == 1:
        x_annot = 1.05
        y_annot = .8
        col = col2
    elif plot_num == 2:
        x_annot = 1.05
        y_annot = .7 
        col = col3

    data_nums_1 = 1000
    data_nums_2 = decimation
    
    dates_scal, dens_scal = scale_density_with_cdadjustment(obj_m1)
    obj_m1_stats = Calc_Cd_percent_diff_apriori(obj_m1)

#         fig = make_subplots(rows=2, cols=1, 
#                             specs=[
#                             [{"secondary_y": False}],
#                             [{"secondary_y": True}], ])                  



#     for arc in obj_m1.__dict__['run_parameters']['arc']:
    for ii,arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):
        str_run_param = 'run_parameters'+ arc
        final_iter = obj_m1.__dict__[str_run_param]['str_iteration']
        i_arc = ii+1
        
        for i_plot, dens in enumerate(dens_scal):
            fig.add_trace(go.Scattergl(x=dates_scal[i_plot][::data_nums_2],
                                 y=dens_scal[i_plot][::data_nums_2],
        #                          name = geodyn_run_params_m1['den_model'],
                                 mode='markers',
                                    marker=dict(color='limegreen',size=5),
                                  showlegend=False,
                                   ),
                                   secondary_y=False,
                                   row=1, col=1,
                                   )
        fig.add_trace(go.Scattergl(x=obj_m1.Density[arc]['Date'][::data_nums_2],
                                     y=obj_m1.Density[arc]['rho (kg/m**3)'][::data_nums_2],
            #                          name = geodyn_run_params_m1['den_model'],
                                     mode='markers',
                                        marker=dict(color='blue',
                                        size=3,
                                        opacity = .6,
                                        ),
                                      showlegend=False,
                                       ),
                                      secondary_y=False,
                                       row=1, col=1,
                                       )
        arc_date_1 = obj_m1.Residuals_obs[arc]['Date'].iloc[0]
        arc_date_2 = obj_m1.Residuals_obs[arc]['Date'].iloc[-1]
        fig = add_arc_background_w_text(fig, 1.1*np.max(obj_m1.Residuals_obs[arc]['Residual'] ),
                                            arc_date_1, arc_date_2, i_arc, False)

    fig.add_trace(go.Scattergl(x=obj_m1_stats['all_dates'],
                  y=obj_m1_stats['cd_percdiff_from_apriori']/100,
#                     mode='lines+markers',
                    mode='lines+markers',
                            marker=dict(color=col3,
                            size=4,
                            ),
                          showlegend=False,
                           ),
                          secondary_y=False,
                           row=2, col=1,
                           )


    fig.update_layout(
        title="Density along ICESat2's Orbit",
        )
    fig.update_yaxes(type="log", exponentformat= 'power', row=1, col=1)
    fig.update_yaxes(type="linear", exponentformat= 'power', row=2, col=1)
    fig.update_yaxes(title_text="kg/m^3", row=1, col=1)
    fig.update_yaxes(title_text="unitless", row=2, col=1)
    fig.update_xaxes(title_text="Date", row=2, col=1)

    fig.update_layout(
        autosize=False,
        width=900,
        height=700,
    )
    fig.update_layout(
        font=dict(
            size=14,
                 ),)
    return(fig)








# def plot_cd_timeseries(fig, obj_m1, plot_num, row_choose):

#     obj_m1_stats = Calc_Cd_percent_diff_apriori(obj_m1)
    
#     if plot_num == 0:
#         col = col1
#     elif plot_num == 1:
#         col = col2
#     elif plot_num == 2:
#         col = col3
        
#     SAT_ID = int(obj_m1.__dict__['global_params']['SATID'])
#     which_stat = 'CURRENT_VALUE'


#     model_m1 = obj_m1.__dict__['global_params']['den_model']
#     # make plot:
# #     fig = make_subplots(
# #         rows=2, cols=1,
# #         subplot_titles=(["Timeseries of Cd", "Percent difference from a priori (Cd=2.2)"]),
# #         vertical_spacing = 0.1,
# #         )

#     for ii,arc in enumerate(obj_m1.__dict__['global_params']['arc_input']):
# #         print(arc)
#         i_arc = ii+1
#         last_iter = list(obj_m1.AdjustedParams[arc].keys())[-1]
#         labels = list(obj_m1.AdjustedParams[arc][last_iter][SAT_ID]['0CD'].keys())

#         val_list_1 = []
#         for i in obj_m1.AdjustedParams[arc][last_iter][SAT_ID]['0CD'].keys():
#             val_list_1.append(obj_m1.AdjustedParams[arc][last_iter][SAT_ID]['0CD'][i][which_stat])
#         fig.add_trace(go.Scattergl(x=labels,
#                                          y=val_list_1,
#                                         name= model_m1 + ' |  Arc ' +str(i_arc) +' | Iters: '+ str(last_iter),
#                                          mode='markers',
#                                          marker=dict(
#                                           color=col,
#                                          size=6,),
#                                          showlegend=False,
#                                          ),
#                                          row=row_choose, col=1,
#                                          )



# #     fig = add_arc_background_w_text(fig, 9.5, obj_m1.__dict__['run_parameters']['arc'], True)
# #     fig = legend_as_annotation(fig)

#     # fix layout info:
#     fig.update_yaxes( title="Cd ",exponentformat= 'power',row=row_choose, col=1)
# #     fig.update_xaxes( title="Date", row=row_choose, col=1)

# #     fig.update_layout(title="Time Dependent Drag Coefficient ")
# #     fig.update_layout(
# #                     autosize=True,
# # #                     width=900,
# # #                     height=900,
# #                     font=dict(size=14))

# #     iplot(fig)
#     return(fig)



