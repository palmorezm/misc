
# Query to ACS for age and sex data

# Packages 
import requests
import json

# Set Credentials
api_key = '2853f879994081a000267a9c87662b8b5111ceab'
endpoint = 'https://api.census.gov/data/2021/acs/acs5'

# Specify Parameters
params = {
    'get': 'NAME,B01001_001E,B01001_002E,B01001_026E',
    'for': 'tract:*',
    'in': 'state:55'
}

# Check for response
response = requests.get(endpoint, params=params)

# Parse from JSON
data = json.loads(response.text)

# Validate 
print(data)
