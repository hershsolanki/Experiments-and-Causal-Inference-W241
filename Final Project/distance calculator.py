from geopy.geocoders import Nominatim
from geopy.distance import geodesic, great_circle
import pandas as pd
geolocator = Nominatim(user_agent='myapplication', timeout=100)
location = geolocator.geocode("New York, NY")
import warnings
warnings.filterwarnings("ignore")
# print(location.latitude)
# print(location.longitude)

df = pd.read_csv("/Users/h/Documents/1H/Dropbox/MIDS/241-Final-Project/data/raw/data.csv")

# Vectorization doesn't seem to be working...
# # print(df["SchoolLocation"])
# # df["University Lat"] = geolocator.geocode(df["School Location"]).latitude
# # print(df["University Lat"])

df["University Lat"] = 0
df["University Long"] = 0
df["City Lat"] = 0
df["City Long"] = 0
df["Distance"] = 0

for i in range (0, len(df)):
    # print(df["School Location"][i])
    uni_location = geolocator.geocode(df["School Location"][i])
    print(df["School Location"][i])
    uni_lat = uni_location.latitude
    uni_lon = uni_location.longitude
    print(uni_lat)
    print(uni_lon)
    df["University Lat"][i] = uni_lat
    df["University Long"][i] = uni_lon
    print(i)

for i in range(0, len(df)):
    city_location = geolocator.geocode(df["Location"][i])
    print(df["Location"][i])
    lat_city = city_location.latitude
    lon_city = city_location.longitude
    print(lat_city)
    print(lon_city)
    df["City Lat"][i] = lat_city
    df["City Long"][i] = lon_city
    print(i)

for i in range(0, len(df)):
    uni = (df["University Lat"][i], df["University Long"][i])
    city = (df["City Lat"][i], df["City Long"][i])
    df['Distance'][i] = geodesic(uni, city).miles
    print(df["Location"][i] + " - " + df["School Location"][i])
    print(df['Distance'][i])


df.to_csv("Data With Distance.csv")