"""
Author: Colin Broderick

Usage: time python get_trip_counts.py --c 'nyc' -f 'path/to/tripdata/csv-files' -s 'path/to/station_latlons.txt' 
Output: The script will out put two count csv files, one from and one to.
        /total_from.csv
        /total_to.csv
"""

import pandas as pd
import numpy as np
import scipy as sp
import scipy.stats
import argparse
import glob
import os

# allow input to be specified for the input files
parser = argparse.ArgumentParser(description='This is a script to count number of trips between stations in bikeshare system of New York or London')
parser.add_argument('-c','--city', help='Specify the city to perform analysis: nyc or london (or boris)',required=True)
parser.add_argument('-d','--directory', help='Input folder where trip data is. Must include trailing /.',required=True)
parser.add_argument('-s','--stations',help='Stations csv file location', required=True)

args = parser.parse_args()
 
## show values ##
print ("City: %s" % args.city )
print ("Trips file is at: %s" % args.directory )
print ("Stations csv file at: %s" % args.stations )

#set up variables to handle files
city = args.city
trips_folder = args.directory
stations_file = args.stations

"""
city = "london"
trips_folder = "/Users/colinbroderick/repos/bike-correlations/data/barclayscyclehireusagestats/"
stations_file = "/Users/colinbroderick/repos/temp/bike-correlations/data/station_latlons_london.txt"

city = "nyc"
trips_folder = "/Users/colinbroderick/repos/bike-correlations/data/nyc/citi_bike_usage_stats/"
stations_file = "/Users/colinbroderick/repos/temp/bike-correlations/data/station_latlons_nyc.txt"

"""
# trips and stations
class Lonstations(object):
    def __init__(self):
        self.stations = 0
        self.trips = 0
        self.trip_counts = 0
    def get_stations(self, data_path):
        parsed = pd.read_csv(data_path,
            usecols=["id","lat","long", "name"])
        self.stations = pd.DataFrame({
            "id": parsed["id"],
            "name": parsed["name"],
            "lat": parsed["lat"],
            "lng": parsed["long"]
        }).groupby("id").first().reset_index()
        return self
    def count_stations(self):
        count = self.stations["id"].count()
        print "The number of bike share stations is %s." % count
        return count
    # Rental Id,Duration,Bike Id,End Date,EndStation Id,EndStation Name,Start Date,StartStation Id,StartStation Name
    def get_trips(self, data_path):
        parsed = pd.read_csv(data_path,
            usecols=[  
                      "StartStation Id",
                      "EndStation Id"])
        self.trips = pd.DataFrame({
            "start_id": parsed["StartStation Id"],
            "end_id": parsed["EndStation Id"]
        })
        return self
    def get_counts(self, data_path):
        parsed = pd.read_csv(data_path,
            usecols=['start_id', 'end_id', 'count'])
        self.trip_counts = pd.DataFrame({
            "start_id": parsed["start_id"],
            "end_id": parsed["end_id"],
            "trip_count": parsed["count"]
        }) 
    def count_total_trips(self):
        #total_trips = len(self.trips)
        trips = self.trips
        total_trips = trips[(trips["start_id"] != trips["end_id"]) & (trips["trip_duration"] > 0)]
        #print "There were a total %s trips in this period." % total_trips.size()
        return total_trips

class NYCstations(Lonstations):
    def __init__(self):
        self.stations = 0
        self.trips = 0
        self.trip_counts = 0
    def get_trips(self, data_path):
        parsed = pd.read_csv(data_path,
            usecols=["end station id","start station id"])
        self.trips = pd.DataFrame({
            "start_id": parsed["start station id"],
            "end_id": parsed["end station id"]
        })
        return self
    def get_counts(self, data_path):
        parsed = pd.read_csv(data_path,
            usecols=['start_id', 'end_id','count'])
        self.trip_counts = pd.DataFrame({
            "start_id": parsed["start_id"],
            "end_id": parsed["end_id"],
            "trip_num": parsed["count"]
        })
        return self
        



    
 #####################################
 # THE MONEY QUERY                   #
 #####################################
 #####################################
 # THE MONEY QUERY                   #
 #####################################        
 

if city == "nyc":
    london = NYCstations()
    london.get_stations(stations_file)
elif city == "london":
    london = Lonstations()
    london.get_stations(stations_file)
elif city == "boris":
    london = Lonstations()
    london.get_stations(stations_file)
    

# get the directory path of the trips files
path = os.path.abspath(os.path.dirname(trips_folder))
# get all the filenames ending in .csv
filenames = glob.glob(path + "/*.csv")
if city == "london" or city == "boris":
    filenames.remove (filenames [32]) # temp avoid problem file

all_trips = []
print "%s trips files \n" % len(filenames)
# if only one file in folder possibly a string.
# check to make sure that the filenames is either a list or a string.
if isinstance(filenames, list):
    for idx, file in enumerate (filenames):
        print "Reading File %s / %s = %s ...." % (idx, len (filenames), file)
        london.get_trips(file)
        all_trips.append(london.trips)
elif isinstance(filenames, basestring):
    print "Reading File %s...." % filenames
    london.get_trips(file)
    all_trips.append(london.trips)
else:
    print "There is a problem with your directory or file."
    sys.exit(1)

# combine all the trips into one file
df = pd.concat(all_trips)

#####################################
# THE MONEY QUERY                   #
#####################################

# Real total number of stations
station_count = london.count_stations()
# Create and identity matrix that is (n x n) for NYC (332 x 332)
#i_matrix = np.matrix(np.identity(station_count), copy=False)
london_stations = london.stations[["id"]]
# count for number of stations missing from list
count_no_station = 0
trips_c_list= []
trips_t_list= []
# loop to run through trips for every station
# sum up the trip counts for each station
counts_from_each = df.query('start_id != end_id and end_id != start_id').groupby(['start_id',"end_id"]).size()
counts_to_each = df.query('end_id != start_id and start_id != end_id').groupby(['end_id',"start_id"]).size()

counts_from_each = pd.DataFrame(counts_from_each)

print "Trips From: %s" % counts_from_each.sum()
print "Trips To: %s" % counts_to_each.sum()
print "Total: %s" % (counts_to_each.sum() + counts_from_each.sum())

# render as a flat square list
unstack_counts_f = counts_from_each.unstack()
unstack_counts_t = counts_to_each.unstack()

# There are some stations missing from the london stations file present in the trips OD data.
if city == "london" or city == "boris":
    #remove station 241 first
    unstack_counts_f = unstack_counts_f.drop(unstack_counts_f.columns[238], axis=1)
    #remove station 0 col
    unstack_counts_f = unstack_counts_f.drop(unstack_counts_f.columns[0], axis=1)
    #remove station 0 and 241 rows
    unstack_counts_t = unstack_counts_t.drop(unstack_counts_t.index[238])
    unstack_counts_t = unstack_counts_t.drop(unstack_counts_t.index[0])

# Unstack and totals save to csv files"""
unstack_counts_f.to_csv("total_from.csv")
unstack_counts_t.to_csv("total_to.csv")

# Move to next step to calculate the linear regressions between all rows and all columns in file reg1.py.
