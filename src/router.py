# Calculates distance between two lat-lon coordinates as routed using
# routino.org. The main work is preparing the appropriate OSM network and
# extracting all nodes for routing. The desired lat-lon coordinates then need to
# be matched onto existing nodes within the osm network
#
# At the moment, it's just a proof of principle using a pair of dummy
# coordinates. Now it just needs to be extended to calculate all distances
# between all pairs in station_latlons ...
import os, time, subprocess
from bs4 import BeautifulSoup  

def checkPlanetSplitter (city="nyc"):
    # Run planetsplitter if .mem files don't exist for city. Also unzips OSM
    # file if still in .bz2 format
    files = os.listdir (".") # from /src
    if city.lower ()[0] == "l":
        city = "london"
        prfx = "lo"
    else:
        city = "nyc"
        prfx = "ny"
    # First unzip
    datadir = "../data/"
    dfiles = os.listdir (datadir)
    fcheck = any (f.find (city) > -1 and f.endswith(".osm") for f in dfiles)
    if not any (f.find(city) > -1 and f.endswith (".osm") for f in dfiles):
        bf = [f for f in dfiles if f.find (city) > -1 and f.endswith (".bz2")]
        if not bf:
            print "ERROR: %s.bz2 file does not exist to unzip" % bf 
            # TODO: exception handler
        else:
            bf = datadir + bf [0]
            args = ["bunzip2", bf]
            print "Unzipping planet-%s.osm ... " % city
            subprocess.Popen (args)
    if not any (f.startswith(prfx) and f.endswith(".mem") for f in files):
        planetfile = datadir + "planet-" + city + ".osm"
        args = ["/Users/colinbroderick/Downloads/routino-2.7.2/src/planetsplitter", "--prefix=" + prfx,\
                "--tagging=/Users/colinbroderick/Downloads/routino-2.7.2/xml/routino-tagging.xml",\
                planetfile]
        print "planet-%s.osm not yet split. Running planetsplitter..." % city
        subprocess.Popen (args)
    else:
        print "%s already split" % city

def getBounds (city="nyc"):
    wd = '../data/'
    if city == "london": fname = wd + 'planet-london.osm'
    else: fname = wd + 'planet-nyc.osm'
    with open (fname) as f:
        for line in f:
            if line.find ("bounds") > -1:
                lineout = line
                break
    soup = BeautifulSoup (lineout)
    bounds = soup.find ("bounds")
    lats = [bounds.attrs['minlat'], bounds.attrs['maxlat']]
    lons = [bounds.attrs['minlon'], bounds.attrs['maxlon']]
    return zip (lats, lons)

def getAllNodes (city="london"):
    # OSM files are potentially huge, so the short and easy way of parsing
    # either crashes or takes an enormously long time. This much faster way
    # directly scans the osm file and extracts only the nodes.
    start = time.time ()
    ids = []
    lats = []
    lons = []
    count = 0
    wd = '../data/'
    if city == "london": fname = wd + 'planet-london.osm'
    else: fname = wd + 'planet-nyc.osm'
    with open (fname) as f:
        for line in f:
            if line.find ('node') > -1 and line.find ('lat=') > 1:
                # Using Soup is easier but is orders of magnitude slower!
                #soup = BeautifulSoup (line)
                #node = soup.find ("node")
                #ids.append (int (node.attrs ["id"]))
                #lats.append (float (node.attrs ["lat"]))
                #lons.append (float (node.attrs ["lon"]))
                # So values are directly stripped instead. Note that this
                # requires lines to be ordered (id,lat,lon,version).
                id = line.split ("id=")[1].split ("lat=")[0]
                ids.append (int (id.split('"')[1].strip("' ").strip('" ')))
                lat = line.split ("lat=")[1].split ("lon=")[0]
                lats.append (float (lat.strip("' ").strip('" ')))
                lon = line.split ("lon=")[1].split ("version=")[0]
                lons.append (float (lon.split('"')[1].strip("' ").strip('" ')))
    end = time.time ()
    print len (ids), " nodes extracted in ", end - start, "s."
    return zip (ids, lats, lons)

def matchNodes (lon0, lat0, nodes):
    ni = -1
    mindiff = float("inf")
    count = 0
    for (id, lat, lon) in nodes:
        diff = abs (lat - lat0) + abs (lon - lon0)
        if diff < mindiff:
            ni = count
            mindiff = diff
        count = count + 1
    return nodes [ni]

# This routine produces "quickest.html" describing the route. This file is then
# analysed with getDist.py
def doRoute (lat1, lon1, lat2, lon2, nodes, city="london"):
    node1 = matchNodes (lon1, lat1, nodes)
    node2 = matchNodes (lon2, lat2, nodes)
    # profiles copied from  ../xml/routino-profiles.xml, same for translations.
    # Directory in first arg has to be changed to appropriate routine dir
    args = ["/Users/colinbroderick/Downloads/routino-2.7.2/src/router", "--prefix="+city[:2],\
            "--transport=bicycle", "--quickest",\
            "--profiles=../data/routino-profiles.xml",\
            "--translations=../data/routino-translations.xml",\
            "--lon1="+str (node1[2]), "--lat1="+str (node1[1]),\
            "--lon2="+str (node2[2]), "--lat2="+str (node2[1])]
    # Popen.wait comes with a warning that should *not* affect router output.
    subprocess.Popen (args).wait()
