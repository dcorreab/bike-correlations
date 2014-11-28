import pandas as pd
lon_d = pd.read_csv("../data/station_dists_london.txt", names = ['start_id', 'end_id','dist'],index_col=[0,1], header =None)

print "ROWS FROM: \n"
n_fr = pd.read_csv("../results/nyc_from_rows.csv").set_index('end_id')
l_fr = pd.read_csv("../results/london_from_rows.csv").set_index('end_id')
print n_fr.shape, l_fr.shape

print "ROWS TO: \n"
n_tr = pd.read_csv("../results/nyc_to_rows.csv").set_index('end_id')
l_tr = pd.read_csv("../results/london_to_rows.csv").set_index('end_id')
print n_tr.shape, l_tr.shape

print "COlUMNS FROM:\n"
n_fc = pd.read_csv("../results/nyc_from_cols.csv").set_index('end_id')
l_fc = pd.read_csv("../results/london_from_cols.csv").set_index('end_id')
print n_fc.shape, l_fc.shape

print "COlUMNS TO:\n"
n_tc = pd.read_csv("../results/nyc_to_cols.csv").set_index('end_id')
l_tc = pd.read_csv("../results/london_to_cols.csv").set_index('end_id')
print n_fr.shape, l_fr.shape

x = []
for i, item in enumerate(l_fc.index):
    if len(lon_d.query('end_id == %s' % item)) == 0:
        x.extend([(i, item)])
        print i, item
    elif len(lon_d.query('start_id == %s' % item)) == 0:
        x.extend([(i, item)])
        print i, item

# missing and need to be removed from all correlations
"""x.extend([
(35, 37.0),
(77, 79.0),
(79, 81.0),
(180, 183.0),
(225, 229.0),
(278, 283.0),
(280, 285.0),
(297, 302.0),
(362, 369.0),
(405, 413.0),
(549, 567.0)])"""
x.reverse()

to_remove = []
for i, item in enumerate(x):
    if l_tc.iloc[item[0]].name == item[1]:
        to_remove.append(item[0])
    else:
        print "There's something wrong with %s %s" % (i, item)

l_tc.drop(l_tc.columns[to_remove], axis=1, inplace=True)
l_tc.drop(l_tc.index[to_remove], axis=0, inplace=True)
l_fc.drop(l_fc.columns[to_remove], axis=1, inplace=True)
l_fc.drop(l_fc.index[to_remove], axis=0, inplace=True)

l_fr.drop(l_fr.columns[to_remove], axis=1, inplace=True)
l_fr.drop(l_fr.index[to_remove], axis=0, inplace=True)
l_tr.drop(l_tr.columns[to_remove], axis=1, inplace=True)
l_tr.drop(l_tr.index[to_remove], axis=0, inplace=True)

l_tc.to_csv("../results/london_to_cols.csv")
l_tr.to_csv("../results/london_to_rows.csv")
l_fc.to_csv("../results/london_from_cols.csv")
l_fr.to_csv("../results/london_from_rows.csv")

# 768 lon_matrix.index[736]
# 772 lon_matrix.index[740]
# 776 lon_matrix.index[744]
# 777 lon_matrix.index[745]
#delete = [736, 740, 744, 745]
#lon_matrix.drop(lon_matrix.index[delete], inplace=True)
#lon_matrix.drop(lon_matrix.columns[delete], inplace=True, axis=1)