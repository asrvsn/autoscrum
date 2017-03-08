import plotly.plotly as py
import plotly.graph_objs as go

import json
import sys

from shortid import ShortId

idgen = ShortId()

f_vis_name = sys.argv[1]

with open(f_vis_name + ".cache", 'r') as f_vis: 
    vis = json.loads(f_vis.read())

    traces = []
    for dev, timeline in vis.iteritems():
        for entry in timeline:
            if entry[1] == None:
                name = 'Blocked'
                marker = dict(
                        color = 'rgba(58, 71, 80, 0.6)',
                        line = dict(
                            color = 'rgba(58, 71, 80, 1.0)',
                            width = 3)
                    )
            else:
                name = entry[1]
                marker = dict(
                        color = 'rgba(246, 78, 139, 0.6)',
                        line = dict(
                            color = 'rgba(246, 78, 139, 1.0)',
                            width = 3)
                    )

            trace = go.Bar(
                y=[dev],
                x=[entry[0]],
                name=name,
                orientation='h',
                marker = marker
                )
            traces.append(trace)

    title = 'AlphaSheets dev timeline (' + idgen.generate() + ')'

    layout = go.Layout(
        barmode='stack',
        showlegend=False,
        title=title
    )

    fig = go.Figure(data=traces, layout=layout)
    url = py.plot(fig, filename='marker-h-bar', auto_open=False)

    with open(f_vis_name + ".url", 'w') as f_url:
        f_url.write(url)        
