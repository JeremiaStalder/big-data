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

region_state_dict = pd.read_csv("heat_map/data/state_gadm_dict_cleaned_with_regions.csv",encoding="latin-1", sep=",")


#data = pd.read_csv("heat_map/data/all_data_over_time.csv" ,encoding="utf-8", sep=",")
data = pd.read_csv("heat_map/data/region_effects.csv" ,encoding="utf-8", sep=",")
data.columns = ['region', 'parameter', 'value_coefficient_10', 'value_p_value',
       'error_prediction_coefficient_10', 'error_prediction_p_value',
       'value_difference_coefficient_10', 'value_difference_p_value',
       'CountryCode', 'sub_region_1']
#data = data.replace({"sub_region_gadm": country_to_state_dict})
#data = data.replace({"sub_region_gadm": state_dict})

data = data.merge(region_state_dict, how="left", left_on="region", right_on="Region")
data["sub_region_gadm"] = data["sub_region_gadm"].str.split("; ")
data = data.explode("sub_region_gadm").reset_index()

aggregation_parameter = 'value_difference_coefficient_10'
aggregation_parameter_name = 'co'

data = data[data["parameter"] == "co"]

vmin = data[aggregation_parameter].min()
vmax = data[aggregation_parameter].max()



date_data = data.groupby(by=["sub_region_gadm"], as_index=False).agg({aggregation_parameter : 'mean'})
plot_map = state_map.merge(date_data, how="left", left_on="NAME_1", right_on="sub_region_gadm")
#check if match is correct
if len(date_data["sub_region_gadm"].unique()) != sum(~np.isnan(plot_map[aggregation_parameter])):
    print(set(date_data["sub_region_gadm"].unique()).difference(set(plot_map[~np.isnan(plot_map[aggregation_parameter])]["NAME_1"].unique())))
#create plot
fig, ax = plt.subplots(1, 1, figsize=(23, 10))
divider = make_axes_locatable(ax)
cax = divider.append_axes("right", size="5%", pad=0.1)

ax = plot_map.plot(color="white", linewidth=0.8, edgecolor='0.8',  figsize=(23, 10), ax=ax)
fig = plot_map.dropna(axis=0, subset=[aggregation_parameter]).plot(column=aggregation_parameter, cmap='Greens_r', figsize=(23, 10), vmin=vmin,
                   vmax=vmax, legend=True, norm=plt.Normalize(vmin=vmin, vmax=vmax), ax=ax, cax=cax)

fig.axis('off')
fig.set_title(aggregation_parameter_name + " world map", fontdict={'fontsize': '40', 'fontweight': '6'})
chart = fig.get_figure()
chart.savefig("worldplot_" + '_' + aggregation_parameter + '.png', dpi=300, transparent=True)
plt.close("all")
