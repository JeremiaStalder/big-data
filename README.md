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

# Short Description of Scripts

	## Airpullution Data SQL: @Fabian

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
		Descriptives for 

	## "openaqMergeWithStringencyAndPredictions.R"
	Input: openaq from database / openaq_state_original.csv, ,,unit_conversion.csv 
	Output: global_mobility_report_clean_with_predictions_stringency_index.csv
	Description:
		Clean Data
		Aggregate Daily on subregion level (states, kantons, however you call it)







