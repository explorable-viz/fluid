import pandas as pd
import os

energy_data = pd.read_csv("~/Downloads/yearly_full_release_long_format.csv")
energy_data.fillna(0, inplace=True)
gdp_data = pd.read_csv("~/Downloads/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_6298251.csv", header=2)
gdp_data.fillna(0, inplace=True)
years =  [2013, 2014, 2015, 2016, 2017, 2018]
country_codes = ["USA"] # ["BRA", "CHN", "DEU", "FRA", "EGY", "IND", "JPN", "MEX", "NGA", "USA"]
mini = False
prefix = os.path.expanduser("~/Documents/fluid/fluid/example/linked-inputs/")
if mini:
    prefix += "mini-"
non_renewables_str = "["

def loc_cond(code, category, year, variable, unit):
    return (energy_data['Country code'] == code) & (energy_data['Category'] == category) & (energy_data['Year'] == year) & (energy_data['Variable'] == variable) & (energy_data['Unit'] == unit)

print("Non-Renewables energy_data")
for year in years:
    non_renewables_str += "\n"
    for country in country_codes:
        nuclearGen = energy_data.loc[loc_cond(country, 'Electricity generation', year, 'Nuclear', 'TWh')]['Value'].values[0]
        nuclearCap = '%.2f'%(energy_data.loc[loc_cond(country, 'Capacity', year, 'Nuclear', 'GW')]['Value'].values[0] * 8.76)
        gasGen = energy_data.loc[loc_cond(country, 'Electricity generation', year, 'Gas', 'TWh')]['Value'].values[0]
        gasCap = '%.2f'%(energy_data.loc[loc_cond(country, 'Capacity', year, 'Gas', 'GW')]['Value'].values[0] * 8.76)
        coalGen = energy_data.loc[loc_cond(country, 'Electricity generation', year, 'Coal', 'TWh')]['Value'].values[0]
        coalCap = '%.2f'%(energy_data.loc[loc_cond(country, 'Capacity', year, 'Coal', 'GW')]['Value'].values[0] * 8.76)
        petrolGen = energy_data.loc[loc_cond(country, 'Electricity generation', year, 'Other Fossil', 'TWh')]['Value'].values[0]
        petrolCap = '%.2f'%(energy_data.loc[loc_cond(country, 'Capacity', year, 'Other Fossil', 'GW')]['Value'].values[0] * 8.76)
        # carbInt = energy_data.loc[(energy_data['Country code'] == country) & (energy_data['Subcategory'] == 'CO2 intensity') & (energy_data['Year'] == year)]['Value'].values[0]

        # Horrible hack but we have to do it:
        row = ('   { country: \"' + country + '\", year: ' + str(year)+''
               ', nuclearGen: '+ str(nuclearGen) + ', nuclearCap: ' + str(nuclearCap)+''
               ', gasGen: ' + str(gasGen) + ', gasCap: ' + str(gasCap)+''
               ', coalGen: ' + str(coalGen) + ', coalCap: ' + str(coalCap)+''
               ', petrolGen: ' + str(petrolGen) + ', petrolCap: ' + str(petrolCap)+'}')
        if not (country == country_codes[-1] and year == years[-1]):
            row += ","
        non_renewables_str += row
        non_renewables_str += "\n"
non_renewables_str += "]"
print(non_renewables_str)

with open(prefix + 'non-renewables.fld', 'w') as f:
    f.write(non_renewables_str)

print("Renewables energy_energy_data")
renewables_str = "["
for year in years:
    for country in country_codes:
        renewables_str += "\n"
        bioGen = energy_data.loc[loc_cond(country, 'Electricity generation', year, 'Bioenergy', 'TWh')]['Value'].values[0]
        bioCap = '%.2f'%(energy_data.loc[loc_cond(country, 'Capacity', year, 'Bioenergy', 'GW')]['Value'].values[0] * 8.76)
        hydroGen = energy_data.loc[loc_cond(country, 'Electricity generation', year, 'Hydro', 'TWh')]['Value'].values[0]
        hydroCap = '%.2f'%(energy_data.loc[loc_cond(country, 'Capacity', year, 'Hydro', 'GW')]['Value'].values[0] * 8.76)
        solarGen = energy_data.loc[loc_cond(country, 'Electricity generation', year, 'Solar', 'TWh')]['Value'].values[0]
        solarCap = '%.2f'%(energy_data.loc[loc_cond(country, 'Capacity', year, 'Solar', 'GW')]['Value'].values[0] * 8.76)
        windGen = energy_data.loc[loc_cond(country, 'Electricity generation', year, 'Wind', 'TWh')]['Value'].values[0]
        windCap = '%.2f'%(energy_data.loc[loc_cond(country, 'Capacity', year, 'Wind', 'GW')]['Value'].values[0] * 8.76)
        biorow = '   { year: ' + str(year) + ', country: \"' + country +'\", energyType: \"Bio\", output: ' + str(bioGen) + ', capacity: ' + str(bioCap) + '},\n'
        hydrorow = '   { year: ' + str(year) + ', country: \"' + country +'\", energyType: \"Hydro\", output: ' + str(hydroGen) + ', capacity: ' + str(hydroCap) + '},\n'
        solarrow = '   { year: ' + str(year) + ', country: \"' + country +'\", energyType: \"Solar\", output: ' + str(solarGen) + ', capacity: ' + str(solarCap) + '},\n'
        windrow = '   { year: ' + str(year) + ', country: \"' + country +'\", energyType: \"Wind\", output: ' + str(windGen) + ', capacity: ' + str(windCap) + '}'
        if not (country == country_codes[-1] and year == years[-1]):
            windrow += ",\n"
        else:
            windrow += "\n"
        
        renewables_str += biorow
        renewables_str += hydrorow
        renewables_str += solarrow
        renewables_str += windrow
renewables_str += "]"

print(renewables_str)
with open(prefix + 'renewables.fld', 'w') as f:
    f.write(renewables_str)