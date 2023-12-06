import pandas as pd

data = pd.read_csv("~/Downloads/yearly_full_release_long_format.csv")
data.fillna(0, inplace=True)
years = [2013, 2014, 2015, 2016, 2017, 2018]
countries = ['Brazil', 'China', 'Germany', 'Nigeria', 'United States of America']

print("Non-Renewables Data")
for country in countries:
    for year in years:
        nuclear = data.loc[(data['Area'] == country) & (data['Category'] == 'Capacity') & (data['Year'] == year) & (data['Variable'] == 'Nuclear')]['Value'].values[0]
        gas = data.loc[(data['Area'] == country) & (data['Category'] == 'Capacity') & (data['Year'] == year) & (data['Variable'] == 'Gas')]['Value'].values[0]
        coal = data.loc[(data['Area'] == country) & (data['Category'] == 'Capacity') & (data['Year'] == year) & (data['Variable'] == 'Coal')]['Value'].values[0]
        petrol = data.loc[(data['Area'] == country) & (data['Category'] == 'Capacity') & (data['Year'] == year) & (data['Variable'] == 'Other Fossil')]['Value'].values[0]
        carbInt = data.loc[(data['Area'] == country) & (data['Subcategory'] == 'CO2 intensity') & (data['Year'] == year)]['Value'].values[0]
        row = '{ country: \"' + country + '\", year: ' + str(year) +', nuclear: '+ str(nuclear) + ', gas: ' + str(gas) +', coal: ' + str(coal) + ', petrol: ' + str(petrol) + ', carbonInt: ' + str(carbInt) + '},'
        row = row.replace("United States of America", "USA")
        print(row)

print("Renewables Data")

for year in years:
    for country in countries:
        bio = data.loc[(data['Area'] == country) & (data['Category'] == 'Capacity') & (data['Year'] == year) & (data['Variable'] == 'Bioenergy')]['Value'].values[0]
        hydro = data.loc[(data['Area'] == country) & (data['Category'] == 'Capacity') & (data['Year'] == year) & (data['Variable'] == 'Hydro')]['Value'].values[0]
        solar = data.loc[(data['Area'] == country) & (data['Category'] == 'Capacity') & (data['Year'] == year) & (data['Variable'] == 'Solar')]['Value'].values[0]
        wind = data.loc[(data['Area'] == country) & (data['Category'] == 'Capacity') & (data['Year'] == year) & (data['Variable'] == 'Wind')]['Value'].values[0]
        biorow = '{ year: ' + str(year) + ', country: \"' + country +'\", energyType: \"Bio\", output: ' + str(bio) + '},'
        biorow = biorow.replace("United States of America", "USA")
        hydrorow = '{ year: ' + str(year) + ', country: \"' + country +'\", energyType: \"Hydro\", output: ' + str(hydro) + '},'
        hydrorow = hydrorow.replace("United States of America", "USA")
        solarrow = '{ year: ' + str(year) + ', country: \"' + country +'\", energyType: \"Solar\", output: ' + str(solar) + '},'
        solarrow = solarrow.replace("United States of America", "USA")
        windrow = '{ year: ' + str(year) + ', country: \"' + country +'\", energyType: \"Wind\", output: ' + str(wind) + '},'
        windrow = windrow.replace("United States of America", "USA")
        
        print(biorow)
        print(hydrorow)
        print(solarrow)
        print(windrow)
        print()