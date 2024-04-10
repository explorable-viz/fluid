# Artefact notes

## Regenerating data for energy linked-inputs

 - Download energy data from [here](https://ember-climate.org/data-catalogue/yearly-electricity-data/) (choose yearly full release long format (CSV))
 - Download gdp data from [here](https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?most_recent_value_desc=true), download button on the right, choose csv, and extract the contents into your downloads
 - Leave both files in `~/Downloads/`, then run `energy_data.py`, if you want to change countries or years of interest, you may need to do some fiddling, since some countries have different names in both datasets.

## Running the web app

- `yarn serve-icfp2024` to build and serve ICFP 2024 content via web app
- open a browser at the served URL (usually `127.0.0.1:8080`).
