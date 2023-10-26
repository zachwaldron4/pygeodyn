def clean_nb_readdata(m1, m2, m3):


    ##################################
    # INPUT PARAMETERS:
    ##################################
    sat = 'st'
    arc = '030914_2wk'
    grav_id ='goco05s' 
    local_path = '/data/geodyn_proj/analysis/starlette_analysis/'
    Accel_Status = 'acceloff'
    SAT_ID = 7501001

    ###################################
                                      #
#     # CHOOSE Models to compare:       #
#     m1 = 'msis86'                     #
#     m2 = 'msis00'                     #
#     m3 = 'msis2'                      #
    ###################################



    ##################################
    # PATH TO DENSITY MODEL RUNS:
    ##################################

    msis86_model = 'msis86'
    path_to_msis86 = '/data/geodyn_proj/runs_geodyn/results/'+sat+'/'+msis86_model+'/'+  msis86_model+'_'+ Accel_Status +'/'

    msis2_model = 'msis2'
    path_to_msis2 = '/data/geodyn_proj/runs_geodyn/results/'+sat+'/'+msis2_model+'/'+  msis2_model+'_'+ Accel_Status +'/'

    msis00_model = 'msis00'
    path_to_msis00 = '/data/geodyn_proj/runs_geodyn/results/'+sat+'/'+msis00_model+'/'+  msis00_model+'_'+ Accel_Status +'/'


    if m1 == 'msis86':
        path_to_m1 = path_to_msis86
    elif m1 == 'msis00':
        path_to_m1 = path_to_msis00
    elif m1 == 'msis2':
        path_to_m1 = path_to_msis2


    if m2 == 'msis86':
        path_to_m2 = path_to_msis86
    elif m2 == 'msis00':
        path_to_m2 = path_to_msis00
    elif m2 == 'msis2':
        path_to_m2 = path_to_msis2

    if m3 == 'msis86':
        path_to_m3 = path_to_msis86
    elif m3 == 'msis00':
        path_to_m3 = path_to_msis00
    elif m3 == 'msis2':
        path_to_m3 = path_to_msis2



    import sys  
    sys.path.insert(0, '/data/geodyn_proj/analysis/util_funcs/py_read_geodyn_output/')
    from a_ReadGEODYN import Read_GEODYN_func
    

    
    
    

    (AdjustedParams_m1, 
     Trajectory_m1, 
     Density_m1, 
     ResidsObs_m1,
     ResidsSummStation_m1,
     ResidsMeasSumm_m1,
     Stats_m1)  = Read_GEODYN_func(arc, 
                                            sat,
                                            SAT_ID,
                                            grav_id, 
                                            m1,
                                            local_path, 
                                            path_to_m1,
                                            False,
                                            2003,
                                            'SLR',
                                           verbose_loading=False,
                                    Verbose_Stats=True)    

    (AdjustedParams_m2, 
     Trajectory_m2, 
     Density_m2, 
     ResidsObs_m2,
     ResidsSummStation_m2,
     ResidsMeasSumm_m2,
     Stats_m2)  = Read_GEODYN_func(arc, 
                                            sat,
                                            SAT_ID,
                                            grav_id,
                                            m2,
                                            local_path, 
                                            path_to_m2,
                                            False,
                                            2003,
                                            'SLR',
                                           verbose_loading=False,
                                    Verbose_Stats=True)    


    (AdjustedParams_m3, 
     Trajectory_m3, 
     Density_m3, 
     ResidsObs_m3,
     ResidsSummStation_m3,
     ResidsMeasSumm_m3,
     Stats_m3)  = Read_GEODYN_func(arc, 
                                            sat,
                                            SAT_ID,
                                            grav_id, 
                                            m3,
                                            local_path, 
                                            path_to_m3,
                                            False,
                                            2003,
                                            'SLR',
                                           verbose_loading=False,
                                    Verbose_Stats=True)    

    
    return(AdjustedParams_m1,
            Trajectory_m1,
            Density_m1,
            ResidsObs_m1,
            ResidsSummStation_m1,
            ResidsMeasSumm_m1,
           Stats_m1,
            AdjustedParams_m2,
            Trajectory_m2,
            Density_m2,
            ResidsObs_m2,
            ResidsSummStation_m2,
            ResidsMeasSumm_m2,
           Stats_m2,
            AdjustedParams_m3,
            Trajectory_m3,
            Density_m3,
            ResidsObs_m3,
            ResidsSummStation_m3,
            ResidsMeasSumm_m3,
          Stats_m3)
    
    
    
    
    
def plot_cd_timeseries(m1, m2, m3, SAT_ID, AdjustedParams_m1, AdjustedParams_m2, AdjustedParams_m3):
    
    labels = list(AdjustedParams_m1[5][SAT_ID]['0CD'].keys())
    val_list = []
    for i in AdjustedParams_m1[5][SAT_ID]['0CD'].keys():
        val_list.append(AdjustedParams_m1[5][SAT_ID]['0CD'][i]['CURRENT_VALUE'])



    import plotly.graph_objects as go
    import numpy as np
    from plotly.offline import plot, iplot
#     %matplotlib inline
    from plotly.subplots import make_subplots


    last_iter = 5
    which_stat = 'CURRENT_VALUE' # 2 is the current val

    labels = list(AdjustedParams_m1[5][SAT_ID]['0CD'].keys())


    fig = make_subplots(
        rows=1, cols=1,
#         subplot_titles=("Time Dependent Drag Coefficient ",
                        )
    
    val_list = []
    for i in AdjustedParams_m1[last_iter][SAT_ID]['0CD'].keys():
        val_list.append(AdjustedParams_m1[last_iter][SAT_ID]['0CD'][i][which_stat])
    fig.add_trace(go.Scatter(x=labels,
                                     y=val_list,
                                      name= m1,
                                   mode='markers',
                                        marker=dict(
                                        size=10,),
                                    ), row=1, col=1,
                                       )


    val_list = []
    for i in AdjustedParams_m2[last_iter][SAT_ID]['0CD'].keys():
        val_list.append(AdjustedParams_m2[last_iter][SAT_ID]['0CD'][i][which_stat])
    fig.add_trace(go.Scatter(x=labels,
                                     y=val_list,
                                      name= m2,
                                   mode='markers',
                                        marker=dict(
                                        size=8,),
                                    ), row=1, col=1,
                                       )

    val_list = []
    for i in AdjustedParams_m3[last_iter][SAT_ID]['0CD'].keys():
        val_list.append(AdjustedParams_m3[last_iter][SAT_ID]['0CD'][i][which_stat])
    fig.add_trace(go.Scatter(x=labels,
                                     y=val_list,
                                      name= m3,
                                   mode='markers',
                                        marker=dict(
                                        size=8,),
                                    ), row=1, col=1,
                                       )

    fig.update_yaxes( title="Cd (Unitless)",exponentformat= 'power',row=1, col=1)
    fig.update_xaxes( title="Date", row=1, col=1)

    fig.update_layout(title="Time Dependent Drag Coefficient ")
    fig.update_layout(
        font=dict(
#             family="Courier New, monospace",
            size=14,
                 ),)
    fig.update_layout(
        autosize=False,
        width=600,
        height=500,
    )
    iplot(fig)
    return(fig)
    
    
    
    
def plot_residual_measurement_summary(m1, m2,m3, ResidsMeasSumm_m1,ResidsMeasSumm_m2,ResidsMeasSumm_m3):
    
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots
    import plotly.express as px

    col1 = px.colors.qualitative.Plotly[0]
    col2 = px.colors.qualitative.Plotly[1]
    col3 = px.colors.qualitative.Plotly[2]

    mark_size = 10
    fig = make_subplots(rows=2, cols=1, 
        subplot_titles=("Mean","RMS", ),
                       )
    fig.add_trace(go.Scatter(x=ResidsMeasSumm_m1['Iter'][1:].astype(int),
                             y=ResidsMeasSumm_m1['MEAN'][1:].values.astype(float),
                             name= m1,
                             mode='markers',
                             marker=dict(color = col1,
                             size=mark_size,),
                             ),
                              row=1, col=1,
                             )
    fig.add_trace(go.Scatter(x=ResidsMeasSumm_m2['Iter'][1:].astype(int),
                             y=ResidsMeasSumm_m2['MEAN'][1:].values.astype(float),
                             name= m2,
                             mode='markers',
                             marker=dict(color = col2,
                             size=mark_size,),
                             ),
                             row=1, col=1,
                             )
    fig.add_trace(go.Scatter(x=ResidsMeasSumm_m3['Iter'][1:].astype(int),
                             y=ResidsMeasSumm_m3['MEAN'][1:].values.astype(float),
                             name= m3,
                             mode='markers',
                             marker=dict(color = col3,
                             size=mark_size,),
                             ),
                             row=1, col=1,
                             )

    fig.add_trace(go.Scatter(x=ResidsMeasSumm_m1['Iter'][1:].astype(int),
                             y=ResidsMeasSumm_m1['RMS'][1:].values.astype(float),
                             name= m1,
                             mode='markers',
                             marker=dict(color = col1,
                             size=mark_size,),
                             showlegend=False,
                             ),
                              row=2, col=1,
                             )
    fig.add_trace(go.Scatter(x=ResidsMeasSumm_m2['Iter'][1:].astype(int),
                             y=ResidsMeasSumm_m2['RMS'][1:].values.astype(float),
                             name= m2,
                             mode='markers',
                             marker=dict(color = col2,
                             size=mark_size,),
                             showlegend=False,
                             ),
                             row=2, col=1,
                             )
    fig.add_trace(go.Scatter(x=ResidsMeasSumm_m3['Iter'][1:].astype(int),
                             y=ResidsMeasSumm_m3['RMS'][1:].values.astype(float),
                             name= m3,
                             mode='markers',
                             marker=dict(color = col3,
                             size=mark_size,),
                             showlegend=False,
                             ),
                             row=2, col=1,
                             )
    fig.update_layout(
        title="Residual Measurement Summary",
    #     yaxis_title="Root mean Square",
    #     xaxis_title="Iteration",
        )
    fig.update_yaxes(title_text="Residual", row=1, col=1)
    fig.update_yaxes(title_text="RMS", row=2, col=1)

    fig.update_xaxes(title_text="Iterations", row=1, col=1)
    fig.update_xaxes(title_text="Iterations", row=2, col=1)

    fig.update_layout(legend= {'itemsizing': 'constant'})
    fig.update_xaxes(tickangle=0)

#     fig.update_layout(
#         autosize=False,
#         width=650,
#         height=700,
#     )
    fig.update_layout(
        font=dict(
#             family="Courier New, monospace",
            size=15,
                 ),)

    # fig.update_layout(showlegend=False)

    fig.show()
    return(fig)

    
def plot_resid_summary_by_station(m1, m2, m3, ResidsSummStation_m1, ResidsSummStation_m2, ResidsSummStation_m3):
    import plotly.graph_objects as go
    import numpy as np
    from plotly.offline import plot, iplot
    from plotly.subplots import make_subplots
    import pandas as pd
    import plotly.express as px


    col1 = px.colors.qualitative.Plotly[0]
    col2 = px.colors.qualitative.Plotly[1]
    col3 = px.colors.qualitative.Plotly[2]

    # mark_size = 10
    fig = make_subplots(rows=2, cols=1, 
        subplot_titles=("Mean","RMS", ),
                      vertical_spacing=0.29 , )

    select_iter = ResidsSummStation_m3.Iter == 5
    # fig = go.Figure()
    mark_size = 10


    fig.add_trace(go.Scatter(x=ResidsSummStation_m1['STATION'][select_iter],
                             y=ResidsSummStation_m1['MEAN'][select_iter].values.astype(float),
                             name= m1,
                             mode='markers',
                             marker=dict(color =col1,
                             size=mark_size,),
                             ),
                            row=1, col=1,
                             )
    fig.add_trace(go.Scatter(x=ResidsSummStation_m2['STATION'][select_iter],
                             y=ResidsSummStation_m2['MEAN'][select_iter].values.astype(float),
                             name= m2,
                             mode='markers',
                             marker=dict(color =col2,
                             size=mark_size,),
                             ),
                            row=1, col=1,
                             )
    fig.add_trace(go.Scatter(x=ResidsSummStation_m3['STATION'][select_iter],
                             y=ResidsSummStation_m3['MEAN'][select_iter].values.astype(float),
                             name= m3,
                             mode='markers',
                             marker=dict(color =col3,
                             size=mark_size,),
                             ),
                            row=1, col=1,
                             )


    fig.add_trace(go.Scatter(x=ResidsSummStation_m1['STATION'][select_iter],
                             y=ResidsSummStation_m1['RMS'][select_iter].values.astype(float),
                             name= m1,
                             mode='markers',
                             marker=dict(color=col1,
                             size=mark_size,),
                             showlegend=False,
                             ),
                            row=2, col=1,
                             )
    fig.add_trace(go.Scatter(x=ResidsSummStation_m2['STATION'][select_iter],
                             y=ResidsSummStation_m2['RMS'][select_iter].values.astype(float),
                             name= m2,
                             mode='markers',
                             marker=dict(color=col2,
                             size=mark_size,),
                             showlegend=False,
                             ),
                            row=2, col=1,
                             )
    fig.add_trace(go.Scatter(x=ResidsSummStation_m3['STATION'][select_iter],
                             y=ResidsSummStation_m3['RMS'][select_iter].values.astype(float),
                             name= m3,
                             mode='markers',
                             marker=dict(color=col3,
                             size=mark_size,),
                             showlegend=False,
                             ),
                            row=2, col=1,
                             )

    fig.update_layout(
        title="Stats -- Residual Summary by Station",
    #     yaxis_title="Root mean Square",
    #     xaxis_title="Stations",
        )

    fig.update_yaxes(title_text="mean residuals", row=1, col=1)
    fig.update_yaxes(title_text="rms", row=2, col=1)

    fig.update_xaxes(title_text="Stations", row=1, col=1)
    fig.update_xaxes(title_text="Stations", row=2, col=1)

    fig.update_layout(legend= {'itemsizing': 'constant'})
    fig.update_xaxes(tickangle=45)

    fig.update_yaxes(type="log", exponentformat= 'power')
    fig.update_layout(
        autosize=False,
        width=900,
        height=700,
    )
    fig.update_layout(
        font=dict(
#             family="Courier New, monospace",
            size=17,
                 ),)

    iplot(fig) 
    return(fig)
    
    
    
def plot_weighted_residual_summary_station(m1, m2, m3, ResidsSummStation_m1, ResidsSummStation_m2, ResidsSummStation_m3):

    import plotly.graph_objects as go
    import numpy as np
    from plotly.offline import plot, iplot
    # %matplotlib inline
    import pandas as pd
    from plotly.subplots import make_subplots
    import plotly.express as px


    col1 = px.colors.qualitative.Plotly[0]
    col2 = px.colors.qualitative.Plotly[1]
    col3 = px.colors.qualitative.Plotly[2]

    # mark_size = 10
    fig = make_subplots(rows=2, cols=1, 
        subplot_titles=("Weighted Mean","Weighted RMS"),
                      vertical_spacing=0.29 ,
                       )

    select_iter = ResidsSummStation_m3.Iter == 5
    # fig = go.Figure()
    mark_size = 10


    fig.add_trace(go.Scatter(x=ResidsSummStation_m1['STATION'][select_iter],
                             y=ResidsSummStation_m1['WTD_MEAN'][select_iter].values.astype(float),
                             name= m1,
                             mode='markers',
                             marker=dict(color =col1,
                             size=mark_size,),
                             ),
                            row=1, col=1,
                             )
    fig.add_trace(go.Scatter(x=ResidsSummStation_m2['STATION'][select_iter],
                             y=ResidsSummStation_m2['WTD_MEAN'][select_iter].values.astype(float),
                             name= m2,
                             mode='markers',
                             marker=dict(color =col2,
                             size=mark_size,),
                             ),
                            row=1, col=1,
                             )
    fig.add_trace(go.Scatter(x=ResidsSummStation_m3['STATION'][select_iter],
                             y=ResidsSummStation_m3['WTD_MEAN'][select_iter].values.astype(float),
                             name= m3,
                             mode='markers',
                             marker=dict(color =col3,
                             size=mark_size,),
                             ),
                            row=1, col=1,
                             )


    fig.add_trace(go.Scatter(x=ResidsSummStation_m1['STATION'][select_iter],
                                 y=ResidsSummStation_m1['WTD_RMS'][select_iter].values.astype(float),
                             name= m1,
                             mode='markers',
                             marker=dict(color=col1,
                             size=mark_size,),
                             showlegend=False,
                             ),
                            row=2, col=1,
                             )
    fig.add_trace(go.Scatter(x=ResidsSummStation_m2['STATION'][select_iter],
                             y=ResidsSummStation_m2['WTD_RMS'][select_iter].values.astype(float),
                             name= m2,
                             mode='markers',
                             marker=dict(color=col2,
                             size=mark_size,),
                             showlegend=False,
                             ),
                            row=2, col=1,
                             )
    fig.add_trace(go.Scatter(x=ResidsSummStation_m3['STATION'][select_iter],
                             y=ResidsSummStation_m3['WTD_RMS'][select_iter].values.astype(float),
                             name= m3,
                             mode='markers',
                             marker=dict(color=col3,
                             size=mark_size,),
                             showlegend=False,
                             ),
                            row=2, col=1,
                             )


    fig.update_layout(
        title="Weighted Stats -- Residual Summary by Station",
    #     yaxis_title="Root mean Square",
    #     xaxis_title="Stations",
        )

    fig.update_yaxes(title_text="mean residuals", row=1, col=1)
    fig.update_yaxes(title_text="rms", row=2, col=1)

    fig.update_xaxes(title_text="Stations", row=1, col=1)
    fig.update_xaxes(title_text="Stations", row=2, col=1)

    fig.update_layout(legend= {'itemsizing': 'constant'})
    fig.update_xaxes(tickangle=45)

    fig.update_yaxes(type="log", exponentformat= 'power')
    fig.update_layout(
        autosize=False,
        width=900,
        height=700,
    )
    fig.update_layout(
        font=dict(
#             family="Courier New, monospace",
            size=17,
                 ),)
    iplot(fig) 
    return(fig)
    
    
    
    
    
def plot_observed_residuals(m1,m2,m3, ResidsObs_m1, ResidsObs_m2, ResidsObs_m3):
    
    import plotly.graph_objects as go
    import numpy as np
    from plotly.offline import plot, iplot
#     %matplotlib inline
    import pandas as pd

    # stations_unique = []
    # for station in ResidsObs_m3['track_1']:
    #     if station not in stations_unique:
    #         stations_unique.append(station)

    # elev_angle = 40

    fig = go.Figure()
    # for station in stations_unique:
    #     index_station = ResidsObs_m3.track_1==station
    #     index_elev = ResidsObs_m3.Elev1>=elev_angle

    fig.add_trace(go.Scatter(x=ResidsObs_m1['Date'],
                             y=ResidsObs_m1['Residual'].values.astype(float),
                             name= m1,
                             mode='markers',
                             marker=dict(
                             size=3,),
                             ),
                             )
    fig.add_trace(go.Scatter(x=ResidsObs_m2['Date'],
                             y=ResidsObs_m2['Residual'].values.astype(float),
                             name= m2,
                             mode='markers',
                             marker=dict(
                             size=3,),
                             ),
                             )
    fig.add_trace(go.Scatter(x=ResidsObs_m3['Date'],
                             y=ResidsObs_m3['Residual'].values.astype(float),
                             name= m3,
                             mode='markers',
                             marker=dict(
                             size=3,),
                             ),
                             )

    fig.update_layout(
        title='Observation Residuals',
        yaxis_title="Residuals",
        xaxis_title="Date",
        )
    fig.update_layout(legend= {'itemsizing': 'constant'})

    # fig.update_yaxes(type="log", exponentformat= 'power')
    fig.update_layout(
        font=dict(
#             family="Courier New, monospace",
            size=17,
                 ),)

    iplot(fig) 
    return(fig)
    
    
    
    
    
    
def plot_density_along_orbit(m1, m2, m3, Density_m1, Density_m2, Density_m3):

    import plotly.graph_objects as go
    import numpy as np
    from plotly.offline import plot, iplot
#     %matplotlib inline
    from plotly.subplots import make_subplots
    import plotly.express as px

    col1 = px.colors.qualitative.Plotly[0]
    col2 = px.colors.qualitative.Plotly[1]
    col3 = px.colors.qualitative.Plotly[2]

    data_nums_1 = 1000
    data_nums_2 = 100

    fig = make_subplots(rows=2, cols=1, 
        subplot_titles=("First 4 Minutes","Every 100th point"),
                       )

    fig.add_trace(go.Scatter(x=Density_m1['Date'][:data_nums_1],
                                     y=Density_m1['rho (kg/m**3)'][:data_nums_1],
                                        name=m1,
                                         mode='markers',
                                        marker=dict(
                                        size=4,
                                        ),
                                       ),
                                        row=1, col=1,
                                       )


    fig.add_trace(go.Scatter(x=Density_m2['Date'][:data_nums_1],
                             y=Density_m2['rho (kg/m**3)'][:data_nums_1],
                             name= m2,
                             mode='markers',
                             marker=dict(
                             size=2,),
                             ),
                            row=1, col=1,
                             )

    fig.add_trace(go.Scatter(x=Density_m3['Date'][:data_nums_1],
                             y=Density_m3['rho (kg/m**3)'][:data_nums_1],
                             name= m3,
                             mode='markers',
                             marker=dict(
                             size=2,),
                             ),
                              row=1, col=1,
                             )
    fig.add_trace(go.Scatter(x=Density_m1['Date'][::data_nums_2],
                                     y=Density_m1['rho (kg/m**3)'][::data_nums_2],
                                     name = m1,
                                     mode='markers',
                                        marker=dict(color=col1,
                                        size=3,
                                        ),
                                      showlegend=False,

                                       ),
                                       row=2, col=1,
                                       )

    fig.add_trace(go.Scatter(x=Density_m2['Date'][::data_nums_2],
                             y=Density_m2['rho (kg/m**3)'][::data_nums_2],
                             name= m2,
                             mode='markers',
                             marker=dict(color=col2,
                             size=2,),
                                      showlegend=False,

                             ),
                            row=2, col=1,
                             )

    fig.add_trace(go.Scatter(x=Density_m3['Date'][::data_nums_2],
                             y=Density_m3['rho (kg/m**3)'][::data_nums_2],
                             name= m3,
                             mode='markers',
                             marker=dict(color=col3,
                             size=2,),
                                      showlegend=False,

                             ),
                              row=2, col=1,
                             )

    fig.update_layout(
        title="Density along Starlette Orbit",
        )

    fig.update_yaxes(type="log", exponentformat= 'power',)
    fig.update_layout(legend= {'itemsizing': 'constant'})

    fig.update_yaxes(title_text="kg/m^3", row=1, col=1)
    fig.update_yaxes(title_text="kg/m^3", row=2, col=1)

    fig.update_xaxes(title_text="Date", row=1, col=1)
    fig.update_xaxes(title_text="Date", row=2, col=1)

    fig.update_layout(legend= {'itemsizing': 'constant'})
#     fig.update_xaxes(tickangle=45)


    fig.update_layout(
        autosize=False,
        width=900,
        height=700,
    )
    fig.update_layout(
        font=dict(
#             family="Courier New, monospace",
            size=18,
                 ),)

    iplot(fig)

    return(fig)


    
    
    
    
def clean_verify_msis2_externally():
    sat = 'st'
    arc = '030914_2wk'
    grav_id ='goco05s' 
    local_path = '/data/analysis/starlette_analysis/'
    Accel_Status = 'acceloff'
    SAT_ID = 7501001

    msis2_model = 'msis2'
    path_to_m3 = '/data/runs_geodyn/'+sat+'/results/'+msis2_model+'/'+  msis2_model+'_'+ Accel_Status +'/'

    import pandas as pd
    msisin_den_file = path_to_m3 + "DENSITY/st030914_2wk.goco05s_msisin"
    msisout_den_file = path_to_m3 + "DENSITY/st030914_2wk.goco05s_msisout"
    msisSWI_den_file = path_to_m3 + "DENSITY/st030914_2wk.goco05s_msisSWI"

    DEN1_csv = pd.read_csv(msisin_den_file, 
                        skiprows = 1, 
                        names = ['IYYDDD',
                                 'IYR',
                                  'DAY',
                                 'UTSEC',
                                 'ALTKM',
                                 'GLAT',
                                 'GLON',
                                 'STLOC', 
                                 'AVGFLX',
                                 'FLUX',
                                 'AP1',
                                 'AP2',
                                 'AP3',
                                 'AP4',
                                 'AP5',
                                 'AP6',
                                 'AP7',
                                ],
                        sep = '\s+',
                        )

    DEN2_csv = pd.read_csv(msisout_den_file, 
                        skiprows = 1, 
                        names = ['n_HE',
                                 'n_O1',
                                 'n_N2',
                                 'n_O2',
                                 'n_Ar',
                                 'TotalMassDen',
                                 'n_H',
                                 'n_N',
                                 'n_AnomO',
                                 'T_Exo',
                                 'T_Alt',
                                 'Rho',
                                 'dRhodZ',
                                      ],


                        sep = '\s+',
                        )


    DEN1_csv['Date'] = (pd.to_datetime('0'+ ((DEN1_csv['IYR'].astype(int).astype(str))),  format='%y') 
                        +  pd.to_timedelta(DEN1_csv['DAY'], unit='days'))


    DEN2_csv['Date'] = (pd.to_datetime('0'+ ((DEN1_csv['IYR'].astype(int).astype(str))),  format='%y') 
                        +  pd.to_timedelta(DEN1_csv['DAY'], unit='days'))





#     with open(msisSWI_den_file, 'r') as f:
#         for line_no, line_text in enumerate(f):
#             if float(line_text) <= 0:
#                 print()

    SWI_option = [1.0]*25
    SWI_option[8] = -1.0


    import matplotlib.pyplot as plt
    import numpy as np
    from pymsis import msis
    import pandas as pd



    msis2_df= pd.DataFrame(data={'Rho'   :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'N2'    :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'O2'    :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'O'     :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'He'    :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'H'     :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'Ar'    :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'N'     :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'AnomO' :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'NO'    :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'Temp'  :np.ones(np.shape(DEN1_csv['Date']))*np.nan})


    for index, row in DEN1_csv[:5000].iterrows():

        lon = row['GLON']
        lat = row['GLAT']
        alts = row['ALTKM']
        f107 = row['FLUX']
        f107a = row['AVGFLX']

        aps = [[row['AP1'],row['AP2'],row['AP3'],row['AP4'],row['AP5'],row['AP6'],row['AP7']]]
        date = row['Date']

        msis_data2 =  msis.run(date, lon, lat, alts, f107, f107a, aps, version=2,  options = SWI_option)
        msis_data2  = np.squeeze(msis_data2)

        msis2_df.loc[index, 'Rho'] = msis_data2[0]
        msis2_df.loc[index, 'N2'] = msis_data2[1]
        msis2_df.loc[index, 'O2'] = msis_data2[2]
        msis2_df.loc[index, 'O'] = msis_data2[3]
        msis2_df.loc[index, 'He'] = msis_data2[4]
        msis2_df.loc[index, 'H'] = msis_data2[5]
        msis2_df.loc[index, 'Ar'] = msis_data2[6]
        msis2_df.loc[index, 'N'] = msis_data2[7]
        msis2_df.loc[index, 'AnomO'] = msis_data2[8]
        msis2_df.loc[index, 'NO'] = msis_data2[9]
        msis2_df.loc[index, 'Temp'] = msis_data2[10]

        msis2_df.loc[index, 'Date'] = row['Date']

    return(msis2_df, DEN2_csv)
    
    
def clean_verify_msis00_externally():
    sat = 'st'
    arc = '030914_2wk'
    grav_id ='goco05s' 
    local_path = '/data/analysis/starlette_analysis/'
    Accel_Status = 'acceloff'
    SAT_ID = 7501001

    msis00_model = 'msis00'
    path_to_m3 = '/data/runs_geodyn/'+sat+'/results/'+msis00_model+'/'+  msis00_model+'_'+ Accel_Status +'/'

    import pandas as pd
    msisin_den_file = path_to_m3 + "DENSITY/st030914_2wk.goco05s_msisin"
    msisout_den_file = path_to_m3 + "DENSITY/st030914_2wk.goco05s_msisout"
    msisSWI_den_file = path_to_m3 + "DENSITY/st030914_2wk.goco05s_msisSWI"

    DEN1_csv = pd.read_csv(msisin_den_file, 
                        skiprows = 1, 
                        names = ['IYYDDD',
                                 'IYR',
                                  'DAY',
                                 'UTSEC',
                                 'ALTKM',
                                 'GLAT',
                                 'GLON',
                                 'STLOC', 
                                 'AVGFLX',
                                 'FLUX',
                                 'AP1',
                                 'AP2',
                                 'AP3',
                                 'AP4',
                                 'AP5',
                                 'AP6',
                                 'AP7',
                                ],
                        sep = '\s+',
                        )

    DEN2_csv = pd.read_csv(msisout_den_file, 
                        skiprows = 1, 
                        names = ['n_HE',
                                 'n_O1',
                                 'n_N2',
                                 'n_O2',
                                 'n_Ar',
                                 'TotalMassDen',
                                 'n_H',
                                 'n_N',
                                 'n_AnomO',
                                 'T_Exo',
                                 'T_Alt',
                                 'Rho',
                                 'dRhodZ',
                                      ],


                        sep = '\s+',
                        )


    DEN1_csv['Date'] = (pd.to_datetime('0'+ ((DEN1_csv['IYR'].astype(int).astype(str))),  format='%y') 
                        +  pd.to_timedelta(DEN1_csv['DAY'], unit='days'))


    DEN2_csv['Date'] = (pd.to_datetime('0'+ ((DEN1_csv['IYR'].astype(int).astype(str))),  format='%y') 
                        +  pd.to_timedelta(DEN1_csv['DAY'], unit='days'))





#     with open(msisSWI_den_file, 'r') as f:
#         for line_no, line_text in enumerate(f):
#             if float(line_text) <= 0:
#                 print()

    SWI_option = [1.0]*25
    SWI_option[8] = -1.0


    import matplotlib.pyplot as plt
    import numpy as np
    from pymsis import msis
    import pandas as pd


    msis00_df= pd.DataFrame(data={'Rho'   :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'N2'    :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'O2'    :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'O'     :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'He'    :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'H'     :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'Ar'    :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'N'     :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'AnomO' :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'NO'    :np.ones(np.shape(DEN1_csv['Date']))*np.nan,
                                  'Temp'  :np.ones(np.shape(DEN1_csv['Date']))*np.nan})



    for index, row in DEN1_csv[:5000].iterrows():

        lon = row['GLON']
        lat = row['GLAT']
        alts = row['ALTKM']
        f107 = row['FLUX']
        f107a = row['AVGFLX']

        aps = [[row['AP1'],row['AP2'],row['AP3'],row['AP4'],row['AP5'],row['AP6'],row['AP7']]]
        date = row['Date']

        msis_data00 =  msis.run(date, lon, lat, alts, f107, f107a, aps, version=0, options = SWI_option)

        msis_data00 = np.squeeze(msis_data00)

        msis00_df.loc[index, 'Rho'] = msis_data00[0]
        msis00_df.loc[index, 'N2'] = msis_data00[1]
        msis00_df.loc[index, 'O2'] = msis_data00[2]
        msis00_df.loc[index, 'O'] = msis_data00[3]
        msis00_df.loc[index, 'He'] = msis_data00[4]
        msis00_df.loc[index, 'H'] = msis_data00[5]
        msis00_df.loc[index, 'Ar'] = msis_data00[6]
        msis00_df.loc[index, 'N'] = msis_data00[7]
        msis00_df.loc[index, 'AnomO'] = msis_data00[8]
        msis00_df.loc[index, 'NO'] = msis_data00[9]
        msis00_df.loc[index, 'Temp'] = msis_data00[10]
        msis00_df.loc[index, 'Date'] = row['Date']

    return(msis00_df, DEN2_csv)   


def plot_verify_msis2_externally(msis2_df, Density_m3, DEN2_csv):
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots
    import plotly.express as px

    col1 = px.colors.qualitative.Plotly[0]
    col2 = px.colors.qualitative.Plotly[1]
    col3 = px.colors.qualitative.Plotly[2]

    mark_size = 3
    fig = make_subplots(rows=2, cols=1, 
        subplot_titles=("Density Verification","Percent Difference" ),
                       )

    data_num1 = 5000
    data_num2 = 2
 
    fig.add_trace(go.Scatter(x=msis2_df['Date'][:data_num1][::data_num2],
                             y=msis2_df['Rho'][:data_num1][::data_num2],
                             name= "Pymsis2",
                             mode='markers',
                             marker=dict(
                             size=3,),
                             ),
                              row=1, col=1,
                             )

    fig.add_trace(go.Scatter(x=DEN2_csv['Date'][:data_num1][::data_num2],
                             y=DEN2_csv['Rho'][:data_num1][::data_num2],
                             name= "MSIS.f90",
                             mode='markers',
                             marker=dict(
                             size=2),
                             ),
                              row=1, col=1,
                             )

    A = msis2_df['Rho'][:data_num1][::data_num2]
    B = DEN2_csv['Rho'][:data_num1][::data_num2]
    
    diff = ((A-B)/B)*100
    
    fig.add_trace(go.Scatter(x=msis2_df['Date'][:data_num1][::data_num2],
                         y=diff,
                         name= "% diff",
                         mode='markers',
                         marker=dict(
                         size=4,),
                         ),
                          row=2, col=1,
                         )
    
    
    fig.update_layout(
            title="MSIS 2.0 Verification",
#         yaxis_title="Density [kg/m^3]",
#         xaxis_title="Date",
        )

    fig.update_yaxes(type="log", exponentformat= 'power', row=1, col=1)
    fig.update_layout(legend= {'itemsizing': 'constant'})

    fig.update_yaxes(title_text="kg/m^3", row=1, col=1)
    fig.update_yaxes(title_text="% difference (Out of 100%)", row=2, col=1)

    fig.update_xaxes(title_text="Date", row=1, col=1)
    fig.update_xaxes(title_text="Date", row=2, col=1)

    fig.update_layout(legend= {'itemsizing': 'constant'})
    fig.update_layout(
        autosize=False,
        width=900,
        height=700,
    )
    fig.update_xaxes(tickangle=0)

    fig.update_layout(
    font=dict(
#             family="Courier New, monospace",
        size=18,
             ),)

    fig.show()
    return(fig)


def plot_verify_msis00_externally(msis00_df, Density_m3, DEN2_csv):
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots
    import plotly.express as px

    col1 = px.colors.qualitative.Plotly[0]
    col2 = px.colors.qualitative.Plotly[1]
    col3 = px.colors.qualitative.Plotly[2]

    mark_size = 3
    fig = make_subplots(rows=2, cols=1, 
        subplot_titles=("Density Verification","Percent Difference" ),
                       )

    data_num1 = 5000
    data_num2 = 2
 
    fig.add_trace(go.Scatter(x=msis00_df['Date'][:data_num1][::data_num2],
                             y=msis00_df['Rho'][:data_num1][::data_num2],
                             name= "Pymsis00",
                             mode='markers',
                             marker=dict(
                             size=3,),
                             ),
                              row=1, col=1,
                             )

    fig.add_trace(go.Scatter(x=DEN2_csv['Date'][:data_num1][::data_num2],
                             y=DEN2_csv['Rho'][:data_num1][::data_num2],
                             name= "MSIS.f90",
                             mode='markers',
                             marker=dict(
                             size=2),
                             ),
                              row=1, col=1,
                             )

    A = msis00_df['Rho'][:data_num1][::data_num2]
    B = DEN2_csv['Rho'][:data_num1][::data_num2]
    
    diff = ((A-B)/B)*100
    
    fig.add_trace(go.Scatter(x=msis00_df['Date'][:data_num1][::data_num2],
                         y=diff,
                         name= "% diff",
                         mode='markers',
                         marker=dict(
                         size=4,),
                         ),
                          row=2, col=1,
                         )
    
    
    fig.update_layout(
            title="MSISe00 Verification",
#         yaxis_title="Density [kg/m^3]",
#         xaxis_title="Date",
        )

    fig.update_yaxes(type="log", exponentformat= 'power', row=1, col=1)
    fig.update_layout(legend= {'itemsizing': 'constant'})

    fig.update_yaxes(title_text="kg/m^3", row=1, col=1)
    fig.update_yaxes(title_text="% difference (Out of 100%)", row=2, col=1)

    fig.update_xaxes(title_text="Date", row=1, col=1)
    fig.update_xaxes(title_text="Date", row=2, col=1)

    fig.update_layout(legend= {'itemsizing': 'constant'})
    fig.update_layout(
        autosize=False,
        width=900,
        height=700,
    )
    fig.update_xaxes(tickangle=0)

    fig.update_layout(
        font=dict(
            size=18,
                 ),)
    fig.show()
    return(fig)

    