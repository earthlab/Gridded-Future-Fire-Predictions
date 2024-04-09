###  Deloitte model procedure
### Processing procedure

# Download and Create yearly tiffs from GridMet past climate data
# data description and download details @ https://www.climatologylab.org/gridmet.html
# need URLS in CSV file pointing to NetCDF files
GridMET-climate-data.R

# Summarizes Housing data at L4 ecoregions, monthly time steps
ICLUS_L4_Population.R

### From Here below needs to be run for each MACA model
# Download and Create yearly tiffs from MACA future climate data
# data description and download details @ https://climate.northwestknowledge.net/MACA/index.php
# need URLS in CSV file pointing to NetCDF files
MACA_yearly.R

# Create Ecoregion_summaries of climate data at monthly step
Level4_Ecoregion_climate.R

# Merge data to create Count Data frame
L4_Eco_Count_DF.R

# Make Stan_rds other rds files to run & process model output
## this file parses the US into 12 subset to be analyzed because it is far
## too large
make-stan-V6.R

### From Here on you absolutely need HPC capabilities 
## I recommend at least 16 CPU and 128GB of RAM  per subset
## after setting up an instance I use the docker below found at 
## https://github.com/mbjoseph/wildfire-extremes/tree/2.0
## docker run -e PASSWORD=yourpassword -d -p 8787:8787 mbjoseph/wildfire-extremes

#Run models
fit-count-zinb-nuts.R # need to point to correct zi_d file on line 3
fit-burn-area-lognormal.R # need to point to correct stan_d file on line 3
# make sure name the output correct file name line 15

#Summarizing model output
Model_output_processing.R 
Combine_to_CONUS.R
