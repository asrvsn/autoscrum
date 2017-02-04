import plotly.plotly as py
import plotly.graph_objs as go

import json


with open("schedule_vis.cache", 'r') as f_vis: 
    vis = json.loads(f_vis.read())

    traces = []
    for dev, timeline in vis.iteritems():
        for entry in timeline:
            if len(entry) == 0:
                name = 'Blocked'
                marker = dict(
                        color = 'rgba(246, 78, 139, 0.6)',
                        line = dict(
                            color = 'rgba(246, 78, 139, 1.0)',
                            width = 3)
                    )
            else:
                name = 'Working'
                marker = dict(
                        color = 'rgba(58, 71, 80, 0.6)',
                        line = dict(
                            color = 'rgba(58, 71, 80, 1.0)',
                            width = 3)
                    )

            trace = go.Bar(
                y=[dev],
                x=[entry[0]],
                name='Blocked',
                orientation='h',
                marker = marker
                )
            traces.append(trace)

    layout = go.Layout(
        barmode='stack'
    )

    fig = go.Figure(data=traces, layout=layout)
    url = py.plot(fig, filename='marker-h-bar')

    with open("schedule_vis.url", 'w') as f_url:
        f_url.write(url)        
