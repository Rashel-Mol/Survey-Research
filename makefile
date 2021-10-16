all: data/rawdata.csv gen/temp/clean_data.csv

data/rawdata.csv: src/data-preperation/download_explore_data.R
	R --vanilla < src/data-preperation/download_explore_data.R

gen/temp/clean_data.csv: data/rawdata.csv src/data-preperation/download_explore_data.R src/data-preperation/clean_data.R
	R --vanilla < src/data-preperation/clean_data.R

