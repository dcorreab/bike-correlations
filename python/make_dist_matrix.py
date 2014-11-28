import pandas as pd

lon_d = pd.read_csv("../data/station_dists_london_orig.txt", names = ['start_id', 'end_id','dist'],index_col=[0,1], header =None)
lon_matrix = lon_d.unstack()
# remove various stations not present in the trips data
# 768 lon_matrix.index[736]
# 772 lon_matrix.index[740]
# 776 lon_matrix.index[744]
# 777 lon_matrix.index[745]
delete = [736, 740, 744, 745]
lon_matrix.drop(lon_matrix.index[delete], inplace=True)
lon_matrix.drop(lon_matrix.columns[delete], inplace=True, axis=1)
lon_matrix.to_csv("../data/london_d_matrix.csv")



nyc_d = pd.read_csv("station_dists_nyc.txt", names = ['start_id', 'end_id','dist'],index_col=[0,1], header =None)
#nyc_d1 = pd.read_csv("station_dists_nyc.txt", names = ['start_id', 'end_id','dist'],, header =None)
nyc_matrix = nyc_d.unstack()
nyc_matrix.to_csv("../data/nyc_d_matrix.csv")