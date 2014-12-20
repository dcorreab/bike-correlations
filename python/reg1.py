"""
Author: Colin Broderick
DESC:
Script to get linear regressions of station trips counts in NYC/London.
"""
import pandas as pd
import scipy as sp
import scipy.stats
import argparse
from progressbar import ProgressBar

# allow input to be specified for the input files
parser = argparse.ArgumentParser(description='Get R2 correlations given total trips matrix for bikeshare system of New York or London. Loads london_total_from.csv and london_total_to.csv')
parser.add_argument('-c','--city', help='Specify the city to perform regressions: nyc or london (or boris)',required=True)
args = parser.parse_args()

## show values ##
print ("City: %s" % args.city )
#set up variables to handle files
city = args.city
if city == 'boris':
    city == 'london'
# Open the trips from file
print "Loading Trips..."
print "Loading Trips From...\t total_from.csv"
print "Loading Trips TO...\t total_to.csv"
#trips_from = pd.read_csv('../results/%s_total_from.csv' % city, header=1, skiprows=[2]).set_index('end_id').fillna(0)
trips_from = pd.read_csv('../results/%s_total_from.csv' % city, header=0, index_col=0).fillna(0)
# no need to skip rows
trips_to = pd.read_csv('../results/%s_total_to.csv' % city, header=0, index_col=0).fillna(0)

"""
NOTES:
xi = boom1.irow(1)
y = boom1.irow(2)
mask = ~np.isnan(xi) & ~np.isnan(y)
slope, intercept, r_value, p_value, std_err = sp.stats.linregress(xi[mask],y[mask])
 
#access the columns
boom1.iloc[:,]
 
#access the row
boom1.iloc[1]

if len(trips_to) > len(trips_to.columns):
    loop_len = len(trips_to)
elif len(trips_to.columns) > len(trips_to.boom1):
    loop_len = len(trips_to.columns)
"""


# fixes to square off the matrix for London
if city == 'london' or city == 'boris':
    # trips_to
    # remove station 198 from cols
    trips_to = trips_to.drop(trips_to.columns[195], axis = 1)
    # remove 434 from rows 
    trips_to = trips_to.drop(trips_to.index[420])
    # remove 346 from rows
    trips_to = trips_to.drop(trips_to.index[340])
    
    # trips_from
    # remove station 198 from rows,
    trips_from = trips_from.drop(trips_from.index[195])
    # remove station 434 from cols.
    trips_from = trips_from.drop(trips_from.columns[420], axis=1)
    # remove station 346 from cols.
    trips_from = trips_from.drop(trips_from.columns[340], axis=1)

def get_r2_cor(boom1):
    print "\nCalculating correlations...\n"
    pbar = ProgressBar(maxval=len(boom1)).start()
    row_r2vals = []
    col_r2vals = []
    # number of rows and columns diff in both files. not square.
    # get r2 values of each row with the next row & each col with the next col
    # get r2 values for columns with each column and the next column.    
    col_names = list(boom1.columns)
    row_r2vals.append(col_names)
    col_r2vals.append(col_names)
    for row in range(0, len(boom1)):
        #print "\nI am ROW: %s" % row
        # rows
        xi = boom1.iloc[row]
        r2_vals = []
        #r2_vals.append(boom1.index[row])
        #cols
        xic = boom1.iloc[:,row]
        cr2_vals = []
        #cr2_vals.append(boom1.columns[row])
        for i in range(0, len(boom1)):
            # rows
            y = boom1.iloc[i]
            slope, intercept, r_value, p_value, std_err = sp.stats.linregress(xi,y)
            r2_vals.append(r_value**2)
            # cols
            yc = boom1.iloc[:,i]
            slope, intercept, r_value, p_value, std_err = sp.stats.linregress(xic,yc)
            cr2_vals.append(r_value**2)
        row_r2vals.append(r2_vals)
        col_r2vals.append(cr2_vals)
        pbar.update(row+1)
    pbar.finish()
    print "No of R2values: %s x %s" % (len(row_r2vals), len(col_r2vals))
    rows = boom1.index
    cols =  boom1.columns
    r2_cols = pd.DataFrame(col_r2vals)
    r2_cols = r2_cols.drop(r2_cols.index[0])
    r2_cols.index = rows
    r2_cols.columns = cols
    
    r2_rows = pd.DataFrame(row_r2vals)
    r2_rows = r2_rows.drop(r2_rows.index[0])
    r2_rows.index = rows
    r2_rows.columns = cols
    return r2_rows, r2_cols



"""
def get_r2_cor(boom1):
    print "Calculating correlations"
    row_r2vals = []
    col_r2vals = []
    # number of rows and columns diff in both files. not square.
    # get r2 values of each row with the next row & each col with the next col
    # get r2 values for columns with each column and the next column.
    for col in range(0, len(boom1.columns)-1):
        xic = boom1.iloc[:,col]
        cr2_vals = []   
        for i in range(0, len(boom1.columns)-1):
            yc = boom1.iloc[:,i+1]
            slope, intercept, r_value, p_value, std_err = sp.stats.linregress(xic,yc)
            cr2_vals.append(r_value**2)
        col_r2vals.append(cr2_vals)
        
    for row in range(0, len(boom1)-1):
        #print "\nI am ROW: %s" % row
        xi = boom1.iloc[row]
        r2_vals = []
        for i in range(0, len(boom1)-1):
            #print i
            y = boom1.iloc[i+1]
            slope, intercept, r_value, p_value, std_err = sp.stats.linregress(xi,y)
            r2_vals.append(r_value**2)
        row_r2vals.append(r2_vals)
    print "No of R2values: %s x %s" % (len(row_r2vals), len(col_r2vals))
    return row_r2vals, col_r2vals
"""

#row vals are [0]. col vals are [1]
get_r2_from = get_r2_cor(trips_from)
get_r2_to = get_r2_cor(trips_to)

cols_from = get_r2_from[1]
rows_from = get_r2_from[0]

cols_to = get_r2_to[1]
rows_to = get_r2_to[0]

cols_from.to_csv("../results/%s_from_cols.csv" % city)
rows_from.to_csv("../results/%s_from_rows.csv" % city)

cols_to.to_csv("../results/%s_to_cols.csv" % city)
rows_to.to_csv("../results/%s_to_rows.csv" % city)

 
