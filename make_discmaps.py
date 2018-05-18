import bokeh
from bokeh.plotting import figure, save, show
from bokeh.models import (
    ColumnDataSource,
    HoverTool,
)
from bokeh.palettes import Viridis10 as palette
from bokeh.transform import factor_cmap
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
	map_dat[metric+'_cat'] = pd.cut(map_dat[metric], bins=[0, .99, 1, 2, 3, 4, 5, 6, 7, 8, np.inf], include_lowest=True)
	return(map_dat)


def main(metric):
	from bokeh.sampledata.us_counties import data as counties
	counties = filter_lower48(counties)
	df_comeans = pd.read_csv('/Users/travis/Documents/gits/educational_disparities/output/map_data.csv')
	df_comeans['county_id'] = df_comeans.county_id.str.lower()

	map_dat = make_mapdat(counties, df_comeans, metric=metric)
	cmap = {}
	cats = list(set(map_dat[metric+'_cat']))
	cats = [cat[1:] for cat in cats]
	cats.sort()
	for i, color in enumerate(palette):
	    cmap[cats[i]] = color

	map_dat[metric+'_cat'] = map_dat[metric+'_cat'].str[1:]

	source = ColumnDataSource(data=dict(
	    x=map_dat.x.tolist(),
	    y=map_dat.y.tolist(),
	    name=map_dat.county_name.tolist(),
	    raw_rate=map_dat[metric].astype(str).tolist(),
	    rate=map_dat[metric+'_cat'].tolist(),
	    black_students_action=map_dat[metric+'_black'].tolist(),
	    white_students_action=map_dat[metric+'_white'].tolist(),
	    black_students_total=map_dat['black'].tolist(),
	    white_students_total=map_dat['white'].tolist()
	))
	TOOLS = "pan,wheel_zoom,reset,hover,save"

	plot_title = {'expulsion_combined':"Relative Risk rate (black/white) for expulsions",
		'in_school_arrest':"Relative Risk rate (black/white) for in-school arrests",
		'inschool_susp':"Relative Risk rate (black/white) for in-school suspensions",
		'law_enforcement':"Relative Risk rate (black/white) for law-enforcement referrals",
		'oos_susp':"Relative Risk rate (black/white) for out-of-school suspensions"}

	p = figure(
	    title=plot_title[metric], tools=TOOLS,
	    x_axis_location=None, y_axis_location=None, plot_width=800, plot_height=500
	)
	p.grid.grid_line_color = None

	p.patches('x', 'y', source=source,
	          fill_color=factor_cmap('rate', palette=list(cmap.values()), factors=list(cmap.keys())),
	          fill_alpha=0.7, line_color="white", line_width=0.5)

	hover = p.select_one(HoverTool)
	hover.point_policy = "follow_mouse"
	hover.tooltips = [
	    ("Name", "@name"),
	    ("Relative Risk", "@raw_rate"),
	    ("black students disciplined", "@black_students_action"),
	    ("white students disciplined", "@white_students_action"),
	    ("black students enrolled", "@black_students_total"),
	    ("white students enrolled", "@white_students_total")
	]

	bokeh.plotting.output_file('/Users/travis/Documents/gits/educational_disparities/figs/maps/'+metric+'.html')
	save(p)

if __name__=="__main__":
	main(sys.argv[1])
