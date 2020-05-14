# Big Data Project: Do the lockdown measures related to the corona-virus pandemic impact groud level air pollution? 
# Contributors: Jeremia Stalder, Fabian Karst, Erik Senn

# Data Sources

## Main Sources
airpollution data openaq: https://openaq.org/
weather data from World Meterological Organisation: https://www.ncei.noaa.gov/data/global-summary-of-the-day/archive/ 
government response ticker data - Oxford Covid-19 Government Response Ticker: https://covidtracker.bsg.ox.ac.uk/
google mobility data - : https://www.google.com/covid19/mobility/
map data: https://gadm.org/data.html

## Supplementary Sources
countries of the world: https://www.kaggle.com/fernandol/countries-of-the-world
iso country codes: https://www.kaggle.com/juanumusic/countries-iso-codes/data
unit-conversion pm to microg/m^3: https://www.ccohs.ca/oshanswers/chemicals/convert.html


# Short Description of Main Scripts IN ORDER OF EXECUTION

	  
	## SQL AIRPolution 
	Input: openaq API, ,cleaned weather data from WMO (only for unit conversion)
	Output: cleaned daily open aq data on database
	Description:
	  Clean missing values
	  Convert to values of one parameter same unit using weather data (conversion depends on temperature and air pressiure
		(not required for weatherData.R, required for openaqMergeWithStringencyAndPredictions.R)
	  Add subregion name via request to google maps using coordinates
	  Chunk Files and load them in database
	

	## "weatherData.R"
	Input: raw weather data from WMO, openaq from database
	Output: ./data/predictionAirpollutionFromWeatherData
	Description:
	  Import and clean weather data
	  Match each weather station to closest airquality measure station
	  Predict particles based on weather
	  Short descriptive plots


	## "nationwideDataIncludingOxfordCovidGovernmentTickerAndCountriesOfTheWorld.R" 
	Input: covid_government_response_tracker.csv, countries of the world, wikipedia-iso-country-codes
	Output: nationwide_data_clean.csv
	Description:
		Merging of Oxford Stringency Data and Countries of the world
		Short Descriptive Plots

	## "mobilityData.R"
	Input: Global_Mobility_Report.csv
	Output: global_mobility_report_clean.csv
	Description:
		Clean Data
		Aggregate Daily per subregion (state, kanton, however you call it):
		
	## "modelStringencyIndexWithMobilityData.R"
	Input: nationwide_data_clean.csv, global_mobility_report_clean.csv
	Output: global_mobility_report_clean_with_predictions_stringency_index.csv
	Description:
		Merge nationwide data and mobility trends
		Descriptives for association between stringency index and mobility trends
		Model subregion-mobility index - train random forest on countrywide observed data, predict subregion stringency index

	## "openaqMergeWithStringencyAndPredictions.R"
	Input: openaq from database / openaq_state_original.csv, global_mobility_report_clean_with_predictions_stringency_index.csv, "particle"_prediction.csv ("particle" = co, pm25,pm10, so2, no2, o3), unit_conversion.csv 
	Output: open_state_clean.csv and other aggregated variants (_ma, country_difference_data (_standardized), subregion_difference_data (_standardized),), global_mobility_report_clean_to_merge_with_openaq.csv, all_data_over_time, countryListOpenaqStringencyMerged
	Description:
		Short Cleaning Airpullution Data
		Merge Airpollution Data, Predictions from Weather Model and Stringency Data
		NAs: Only keep observations with Stringency Index and Airpollution value, prediction not required
		Moving Averages for value, value_last_year, value_difference, prediciton, error_prediciton for descriptives and effect estimation
		Save combined and seperate datasets (to analyse the correct set observations that will be matched in the effect estimation whichout unnecesessary columns)
		
	## "openaqDescriptives.R"
	Input: open_state_clean.csv and other aggregated variants (_ma, country_difference_data (_standardized), subregion_difference_data (_standardized),)
	Output: descriptive plots
	Description:
		Descriptives for different aspects
			World, Region Country, Subregion
			Particle
			Analysis variable (value, value_last_year, value_difference, prediciton, error_prediciton including MAs)
		Findings 
			strong seasonal patterns. value_diff and error_prediction remove seasonalities
			indication that value diff world wide does not follow the same trend before covid, for regions it partly does -> supports common trend assumption in regions
		
	## "effectEstimation.R"
	Input: global_mobility_report_clean_stringency_index.csv, open_state_clean.csv and other aggregated variants (_ma, country_difference_data (_standardized), subregion_difference_data (_standardized),)
	Output: region_effects.csv, subregion_effects.csv (and for each 3 subfiles only containing the effect of one of the 3 analysis variables, 2 files without subregion for easier): scatter plots with linear model
	Description: 
		Flexible Difference in Difference Estimation with Choices
			Dataset
			Timeframe for "before Covid" and "in Covid"
			Analysis variable (value, error_prediciton, value_difference)
			Selection on Regions / Countries (default: world)
			Handling of Outliers / low number of observations 
			Estimation Method: First Differences (pooled OLS implemented, but depreciated)
		Findings
			Value_difference
				for particle: CO overall negative effects
				for regions:
					North America: negative significant effects for: co, o3, pm25, so2
					Western Europe: positive significant effect for: pm25, pm10
					Eastern Europe: positive significant effect for: o3
					Asia(excl near East):  negative significant effect for: so2
	
	
	## Shiny ui and server 
	Input: all_data_over_time.csv, weather data and predictions
	Output: Plots for presenation

	## " PYTHON script for plotting map
	Input:  all_data_over_time.csv, region_effects.csv, subregion_effects.csv
	Output: map plots and gif map plots	for presentation







