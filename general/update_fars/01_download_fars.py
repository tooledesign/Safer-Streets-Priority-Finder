from urllib import request
import os

# place to save data locally
data_folder = "/mnt/c/Users/tputta/OneDrive - Toole Design/Desktop/SSPF/FARS"

start_yr = 2015
end_yr = 2021

for yr in range(start_yr, end_yr+1):
    print(f"Downloading {yr} FARS data")
    if not os.path.exists(f"{data_folder}/{yr}"):
        os.mkdir(f"{data_folder}/{yr}")
    request.urlretrieve(f"https://static.nhtsa.gov/nhtsa/downloads/FARS/{yr}/National/FARS{yr}NationalCSV.zip", f"{data_folder}/{yr}/FARS{yr}NationalCSV.zip")
    yr += 1