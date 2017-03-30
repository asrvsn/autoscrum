import plotly.plotly as py
import plotly.graph_objs as go

import json
import sys

from shortid import ShortId

idgen = ShortId()

f_vis_name = sys.argv[1]

colors = [
    'ef5350',
    'EC407A',
    'AB47BC',
    '7E57C2',
    '5C6BC0',
    '42A5F5',
    '29B6F6',
    '26C6DA',
    '26A69A',
    '66BB6A',
    '9CCC65',
    'D4E157',
    'FFEE58',
    'FFCA28',
    'FFA726',
    'FF7043',
    '8D6E63',
    'BDBDBD',
    '78909C'
    ]

with open(f_vis_name + ".cache", 'r') as f_vis: 
    my_id = idgen.generate()

    vis = json.loads(f_vis.read())

    traces = []

    i = 0

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
                        color = colors[i],
                        line = dict(
                            color = 'rgba(58, 71, 80, 1.0)',
                            width = 3)
                    )

            trace = go.Bar(
                y=[dev],
                x=[entry[0]],
                name=name,
                orientation='h',
                marker = marker,
                legendgroup=dev
                )
            traces.append(trace)

        i = (i + 1) % len(colors)

    title = 'AlphaSheets dev timeline (' + my_id + ')'

    layout = go.Layout(
        autosize=False,
        width=1500,
        height=1000,
        barmode='stack',
        title=title,
        legend=dict(xanchor='left',yanchor='bottom',x=-2)
    )

    fig = go.Figure(data=traces, layout=layout)
    url = py.plot(fig, filename='marker-h-bar-'+my_id, auto_open=False)

    with open(f_vis_name + ".url", 'w') as f_url:
        f_url.write(url)        
