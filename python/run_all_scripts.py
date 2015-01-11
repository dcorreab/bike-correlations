import os
x = "weekday"
if x == "weekday":
    """os.system("time python get_trip_counts.py -c nyc -d '/Users/colinbroderick/repos/bike-correlations/data/nyc/citi_bike_usage_stats/' -s '/Users/colinbroderick/repos/temp/bike-correlations/data/station_latlons_nyc.txt' -p weekday")
    #run the regressions for each day of the week
    days = ("mon", "tue","wed","thu","fri","sat","sun")
    #days = ("sat", "sun")
    print("\nNOW RUNNING REGRESSIONS!\n")
    for day in days:
        print("\n%s\n" % day.upper())
        os.system("time python reg1.py -c nyc -day %s" % day)
    
    print("\nNOW CALCULATING KVALS!\n")
    for day in days:
        os.system('Rscript nyc1.R "%s"' % day)"""
    print("\nMAKING PLOTS!\n")
    os.system('Rscript get_bubbles.R "sun"')
    
else:
    """os.system("time python get_trip_counts.py -c nyc -d '/Users/colinbroderick/repos/bike-correlations/data/nyc/citi_bike_usage_stats/' -s '/Users/colinbroderick/repos/temp/bike-correlations/data/station_latlons_nyc.txt' -p total")
    print("\nNOW RUNNING REGRESSIONS!\n")
    os.system("time python reg1.py -c nyc -day total")
    
    print("\nNOW CALCULATING KVALS!\n")
    os.system('Rscript nyc1.R "total"')"""
    print("\nMAKING PLOTS!\n")
    os.system('Rscript get_bubbles.R "total"')