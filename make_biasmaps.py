import bokeh
from bokeh.plotting import figure, save, show
from bokeh.models import (
    ColumnDataSource,
    HoverTool,
    ColorMapper,
    LinearColorMapper
)
from bokeh.palettes import Viridis10 as palette
import numpy as np
import pandas as pd
import sys

def filter_lower48(counties):
	counties = {
		code: county for code, county in counties.items() if 
			county["state"] in ["ca", "az", "nv", "or", "wa", "nm", "tx", "id", 
								"co", "ut", "mt", "wy", "nd", "sd", "ok", "ne", 
								"ks", "mo", "al", "la", "wi", "ms", "ar", "wi", 
								"il", "fl", "in", "ia", "mn", "ky", "oh", "tn", 
								"ga", "sc", "nc", "de", "dc", "md", "pa", "va", 
								"wv", "nj",	"ct", "me", "ma", "nh", "ny", "ri", 
								"vt", "mi"]
	}
	return(counties)

def make_mapdat(counties, plotting_dat, metric):
	map_dat = pd.DataFrame({'county_number':[str(county_id[1]) for county_id in counties],
                       'state_name':[county['state'] for county in counties.values()],
                       'x':[county["lons"] for county in counties.values()],
                       'y':[county["lats"] for county in counties.values()],
                       'county_name':[county['detailed name'] for county in counties.values()]})

	map_dat['county_number'] = map_dat['county_number'].apply(lambda x: x.zfill(3))
	map_dat["county_id"] = map_dat["state_name"] + '-' + map_dat["county_number"]
	map_dat = plotting_dat.merge(map_dat, on='county_id')
	map_dat = map_dat[~map_dat[metric].isnull()]
	return(map_dat)


def main(metric):
	from bokeh.sampledata.us_counties import data as counties
	counties = filter_lower48(counties)
	df_comeans = pd.read_csv('/Users/travis/Documents/gits/educational_disparities/output/map_data.csv')
	df_comeans['county_id'] = df_comeans.county_id.str.lower()

	map_dat = make_mapdat(counties, df_comeans, metric=metric)

	color_mapper = LinearColorMapper(palette=palette)
	source = ColumnDataSource(data=dict(
	    x=map_dat.x.tolist(),
	    y=map_dat.y.tolist(),
	    name=map_dat.county_name.tolist(),
	    rate=map_dat[metric].tolist(),
	    obs=map_dat[metric+'_obs'].tolist(),
	))
	TOOLS = "pan,wheel_zoom,reset,hover,save"

	plot_title = {'implicit':"Project Implicit estimates for implicit racial bias (2002-2014)",
		'explicit':"Project Implicit estimates for explicit racial bias (2002-2014)",
		'implicit_sex':"Project Implicit estimates for implicit sexuality bias (2002-2014)",
		'explicit_sex':"Project Implicit estimates for explicit sexuality bias (2002-2014)"}

	p = figure(
	    title=plot_title[metric], tools=TOOLS,
	    x_axis_location=None, y_axis_location=None, plot_width=800, plot_height=500
	)
	p.grid.grid_line_color = None

	p.patches('x', 'y', source=source,
	          fill_color={'field': 'rate', 'transform': color_mapper},
	          fill_alpha=0.7, line_color="white", line_width=0.5)

	hover = p.select_one(HoverTool)
	hover.point_policy = "follow_mouse"
	hover.tooltips = [
	    ("Name", "@name"),
	    (metric, "@rate"),
	    ("observations:", "@obs")
	]

	bokeh.plotting.output_file('/Users/travis/Documents/gits/educational_disparities/figs/maps/'+metric+'.html')
	save(p)

if __name__=="__main__":
	main(sys.argv[1])
