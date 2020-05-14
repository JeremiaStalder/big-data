# Big Data Project
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


# Project Structure

## Research Question: Do the lockdown measures related to the corona-virus pandemic impact groud level air pollution? 


# Order of Execution of Files

# Short Description of Scripts in order of execution

	## Airpullution Data SQL: @Fabian

	## Weather Scripts @JERE

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
		Timeseries data "all_data_over_time" for result presentation
		
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
	Output: region_effects.csv, subregion_effects.csv (and 3 subfiles only containing the effect of one of the 3 analysis variables)
	Description: 
		Flexible Difference in Difference Estimation with Choices
			Timeframe
			Analysis variable (value, error_prediciton, value_difference)
			Selection on Regions / Countries (default: world)
			







