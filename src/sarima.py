# read libraries
import time_process as tp
# from statsmodels.tsa.api import VAR, DynamicVAR
# from statsmodels.tsa.base.datetools import dates_from_str

class SARIMA(tp.timeProcess):
    'execute Sarima model'

    def __init__(self,df,timeCol,targetCol,frequency):
        'create a SARIMA model'
        self.data = df.set_index(timeCol)
        self.original = pd.Series(self.data.loc[:,(targetCol)],dtype = 'float')
        self.data.timeIdx = pd.to_datetime(self.data.index)
        self.res = sm.tsa.seasonal_decompose(self.original,period = frequency)
        self.trend = self.res.trend # trend data
        self.seasonal = self.res.seasonal # seasonal data
        self.residual = self.res.resid # residual

    def getSARIMA(self):
        '''
        get SARIMA model
        The return order is (data,res,trend,seasonal,residual)
        '''
        return self,data,self.res,self.trend,self.seasonal,self.residual

    def vizSARIMA(self,figSizeTuple = (8,8)):
        'visualize the SARIMA model'    
        plt.figure(figsize=figSizeTuple) # グラフ描画枠作成、サイズ指定

        # オリジナルデータのプロット
        plt.subplot(411) # グラフ4行1列の1番目の位置（一番上）
        plt.plot(self.original)
        plt.ylabel('Original')

        # trend データのプロット
        plt.subplot(412) # グラフ4行1列の2番目の位置
        plt.plot(self.trend)
        plt.ylabel('Trend')

        # seasonalデータ のプロット
        plt.subplot(413) # グラフ4行1列の3番目の位置
        plt.plot(self.seasonal)
        plt.ylabel('Seasonality')

        # residual データのプロット
        plt.subplot(414) # グラフ4行1列の4番目の位置（一番下）
        plt.plot(self.residual)
        plt.ylabel('Residuals')

        plt.tight_layout() # グラフの間隔を自動調整


## sample dataset for debugging --------
# from pydataset import data
# dataset = data('BJsales')
# numdays = dataset.shape[0]
# base = datetime.datetime.today()
# date_list = pd.date_range(datetime.datetime.today(), periods=numdays).date.tolist()
# dataset['date'] = date_list
# dataset['date_str'] = dataset['date'].apply(lambda x : str(x)[:10])
# dataset.head()

# s = SARIMA(dataset,'date','BJsales',4)
# s.vizSARIMA()


if __name__ == '__main__':
    import time_process as tp
