import os
import pandas as pd
import geopandas as geopd
from matplotlib import pyplot as plt
import geoplot
import mapclassify
import glob
from PIL import Image
import numpy as np
from mpl_toolkits.axes_grid1 import make_axes_locatable

#load data
state_dict =  pd.read_csv("heat_map/data/state_gadm_dict_cleaned.csv", encoding="utf-8", sep=";")
state_dict =  dict(zip(state_dict.state, state_dict.sub_region_gadm))

country_to_state_dict = pd.read_csv("heat_map/data/gadm_country_to_state_dict.csv", encoding="utf-8")
country_to_state_dict =  dict(zip(country_to_state_dict.state, country_to_state_dict.sub_region_gadm))

state_map = geopd.read_file("heat_map/data/gadm36_levels.gpkg", layer='level1', encoding="utf-8")
state_map["NAME_1"] = state_map["NAME_1"].str.lower()

data = pd.read_csv("heat_map/data/all_data_over_time.csv" ,encoding="utf-8", sep=",")
data = data.replace({"sub_region_1": country_to_state_dict})
data = data.replace({"sub_region_1": state_dict})
data["sub_region_1"] = data["sub_region_1"].str.split("; ")
data = data.explode("sub_region_1").reset_index()


aggregation_parameter = 'StringencyIndex'
aggregation_parameter_name = 'Stringency Index'

data = data[data["parameter"] == "co"]

days = data["Date"].unique()
vmin = data[aggregation_parameter].min()
vmax = data[aggregation_parameter].max()


for day in days:
    date_data = data[data["Date"] == day]
    date_data = date_data.groupby(by=["CountryCode", "sub_region_1"], as_index=False).agg({aggregation_parameter : 'mean'})
    plot_map = state_map.merge(date_data, how="left", left_on="NAME_1", right_on="sub_region_1")
    #check if match is correct
    if len(date_data["sub_region_1"].unique()) != sum(~np.isnan(plot_map[aggregation_parameter])):
        print(set(date_data["sub_region_1"].unique()).difference(set(plot_map[~np.isnan(plot_map[aggregation_parameter])]["NAME_1"].unique())))
    #create plot
    fig, ax = plt.subplots(1, 1, figsize=(23, 10))
    divider = make_axes_locatable(ax)
    cax = divider.append_axes("right", size="5%", pad=0.1)

    ax = plot_map.plot(color="white", linewidth=0.8, edgecolor='0.8',  figsize=(23, 10), ax=ax)
    fig = plot_map.dropna(axis=0, subset=[aggregation_parameter]).plot(column=aggregation_parameter, cmap='RdYlGn_r', figsize=(23, 10), vmin=vmin,
                       vmax=vmax, legend=True, norm=plt.Normalize(vmin=vmin, vmax=vmax), ax=ax, cax=cax)

    fig.axis('off')
    fig.set_title(aggregation_parameter_name + " world map", fontdict={'fontsize': '40', 'fontweight': '6'})
    fig.annotate(day, xy=(0.1, .225), xycoords='figure fraction', horizontalalignment='left', verticalalignment='top', fontsize=35)
    chart = fig.get_figure()
    chart.savefig("worldplot_" + day + '_' + aggregation_parameter + '.png', dpi=300, transparent=True)
    plt.close("all")

img, *imgs = [Image.open(f) for f in sorted(glob.glob("worldplot_*" + aggregation_parameter + ".png"))]
img.save(fp="worldplot_"+ aggregation_parameter + "_timeseries.gif", format='GIF', append_images=imgs,
         save_all=True, duration=500, loop=0)
