import pandas as pd
import os

energy_data = pd.read_csv("~/Downloads/yearly_full_release_long_format.csv")
energy_data.fillna(0, inplace=True)
gdp_data = pd.read_csv("~/Downloads/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_5994720.csv", header=2)
gdp_data.fillna(0, inplace=True)
years = [2013, 2014, 2015, 2016, 2017, 2018]
countries = ['Brazil', 'China', 'Germany', 'Egypt', 'United States of America']
prefix = os.path.expanduser("~/Documents/fluid/fluid/example/linked-inputs/")
non_renewables_str = "["

print("Non-Renewables energy_data")
for year in years:
    non_renewables_str += "\n"
    for country in countries:
        nuclear = energy_data.loc[(energy_data['Area'] == country) & (energy_data['Category'] == 'Capacity') & (energy_data['Year'] == year) & (energy_data['Variable'] == 'Nuclear')]['Value'].values[0]
        gas = energy_data.loc[(energy_data['Area'] == country) & (energy_data['Category'] == 'Capacity') & (energy_data['Year'] == year) & (energy_data['Variable'] == 'Gas')]['Value'].values[0]
        coal = energy_data.loc[(energy_data['Area'] == country) & (energy_data['Category'] == 'Capacity') & (energy_data['Year'] == year) & (energy_data['Variable'] == 'Coal')]['Value'].values[0]
        petrol = energy_data.loc[(energy_data['Area'] == country) & (energy_data['Category'] == 'Capacity') & (energy_data['Year'] == year) & (energy_data['Variable'] == 'Other Fossil')]['Value'].values[0]
        carbInt = energy_data.loc[(energy_data['Area'] == country) & (energy_data['Subcategory'] == 'CO2 intensity') & (energy_data['Year'] == year)]['Value'].values[0]

        # Horrible hack but we have to do it:
        gdp_country = country
        gdp_country = gdp_country.replace("United States of America", "United States")
        gdp_country = gdp_country.replace("Egypt", "Egypt, Arab Rep.")
        gdp = '%.3f'%(gdp_data.loc[(gdp_data['Country Name'] == gdp_country)][str(year)].values[0] / 1000)
        row = '   { country: \"' + country + '\", year: ' + str(year) +', nuclear: '+ str(nuclear) + ', gas: ' + str(gas) +', coal: ' + str(coal) + ', petrol: ' + str(petrol) + ', carbonInt: ' + str(carbInt) + ', gdpPerCap: ' + gdp + '}'
        if not (country == countries[-1] and year == years[-1]):
            row += ","
        row = row.replace("United States of America", "USA")
        non_renewables_str += row
        non_renewables_str += "\n"
non_renewables_str += "]"
print(non_renewables_str)

with open(prefix + 'non-renewables.fld', 'w') as f:
    f.write(non_renewables_str)

print("Renewables energy_energy_data")
renewables_str = "["
for year in years:
    for country in countries:
        renewables_str += "\n"
        bio = energy_data.loc[(energy_data['Area'] == country) & (energy_data['Category'] == 'Capacity') & (energy_data['Year'] == year) & (energy_data['Variable'] == 'Bioenergy')]['Value'].values[0]
        hydro = energy_data.loc[(energy_data['Area'] == country) & (energy_data['Category'] == 'Capacity') & (energy_data['Year'] == year) & (energy_data['Variable'] == 'Hydro')]['Value'].values[0]
        solar = energy_data.loc[(energy_data['Area'] == country) & (energy_data['Category'] == 'Capacity') & (energy_data['Year'] == year) & (energy_data['Variable'] == 'Solar')]['Value'].values[0]
        wind = energy_data.loc[(energy_data['Area'] == country) & (energy_data['Category'] == 'Capacity') & (energy_data['Year'] == year) & (energy_data['Variable'] == 'Wind')]['Value'].values[0]
        biorow = '   { year: ' + str(year) + ', country: \"' + country +'\", energyType: \"Bio\", output: ' + str(bio) + '},\n'
        biorow = biorow.replace("United States of America", "USA")
        hydrorow = '   { year: ' + str(year) + ', country: \"' + country +'\", energyType: \"Hydro\", output: ' + str(hydro) + '},\n'
        hydrorow = hydrorow.replace("United States of America", "USA")
        solarrow = '   { year: ' + str(year) + ', country: \"' + country +'\", energyType: \"Solar\", output: ' + str(solar) + '},\n'
        solarrow = solarrow.replace("United States of America", "USA")
        windrow = '   { year: ' + str(year) + ', country: \"' + country +'\", energyType: \"Wind\", output: ' + str(wind) + '}'
        if not (country == countries[-1] and year == years[-1]):
            windrow += ",\n"
        else:
            windrow += "\n"
        windrow = windrow.replace("United States of America", "USA")
        
        renewables_str += biorow
        renewables_str += hydrorow
        renewables_str += solarrow
        renewables_str += windrow
renewables_str += "]"

print(renewables_str)
with open(prefix + 'renewables.fld', 'w') as f:
    f.write(renewables_str)