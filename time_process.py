# read libraries
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import datetime
import statsmodels.api as sm

class timeProcess:
    'process time elements'

    def __init__(self,df):
        'initialize parameters'
        self.data = df

    def getTf(self):
        'return the time series dataframe'
        return self.data

    def str2dtime(self,col):
        'transform a string column into a data time column'
        self.data[col] = pd.to_datetime(self.data[col])

    def capMaxDate(self,target_col,max_y,max_m,max_d,new_col_name):
        'set a max date cap to a column'
        self.data[new_col_name] = self.data.apply( lambda x: pd.Timestamp(max_y,max_m,max_d) if (x[target_col]>=pd.Timestamp(max_y,max_m,max_d)) else x[target_col], axis=1)

    def diffDays(self,col_A,col_B,new_col_name):
        'diff days equal to col_A - col_B'
        self.data[new_col_name] = (self.data[col_A]-self.data[col_B].values).astype('timedelta64[D]')

    def filterDate(self,target_col,y,m,d):
        'compare a column with a specific date and filter the data'
        self.data = self.data[self.data[target_col]<=pd.Timestamp(y,m,d)]

    def extractDate(self,time_col,time_extract = '%Y-%m-%d'):
        'extract date string from a string'
        self.data[time_col] = self.data[time_col].apply(lambda x: datetime.datetime.strptime(x,time_extract))

    def resample(self,timeCol,period):
        'resample the time series dataframe'
        new_data = self.data.set_index(timeCol)
        return new_data.resample(period)

    def acf_pacf(self,timeCol,targetCol,lagNum,figSizeTuple = (8,8)):
        tf = self.data.set_index(timeCol)
        tf_series = pd.Series(tf[targetCol],dtype = 'float')

        'vizualize the pcf and pacf graphs'
        fig = plt.figure(figsize=figSizeTuple)

        # 自己相関(ACF)のグラフ
        ax1 = fig.add_subplot(211)
        sm.graphics.tsa.plot_acf(tf_series, lags=lagNum, ax=ax1) #ACF計算とグラフ自動作成

        # 偏自己相関(PACF)のグラフ
        ax2 = fig.add_subplot(212)
        sm.graphics.tsa.plot_pacf(tf_series, lags=lagNum, ax=ax2) #PACF計算とグラフ自動作成

        plt.tight_layout() # グラフ間スキマ調整

if __name__ == '__main__':
    import matplotlib.pyplot as plt
    import numpy as np
    import pandas as pd
    import datetime
    import statsmodels.api as sm

## sample dataset for debugging --------
# from pydataset import data
# dataset = data('BJsales')
# numdays = dataset.shape[0]
# base = datetime.datetime.today()
# date_list = pd.date_range(datetime.datetime.today(), periods=numdays).date.tolist()
# dataset['date'] = date_list
# dataset['date_str'] = dataset['date'].apply(lambda x : str(x)[:10])
# dataset.head()