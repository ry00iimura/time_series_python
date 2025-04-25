# read library
# install.packages('vars')
library(vars)

#read sample
# Canada primary quoter macr economic index 1980-200
# prod PRODUCTIVITY e employment u turnover rw actial feeｷ
data(Canada)

# select jisu how long before we need consider in model?
VAR?elect(Canada,lag.max=4) -> varsel #automatically culculate jisu AIU(n) n is jisu
 
# SELECT JISUU of auc
varsel$selection[1] -> P

# create VAR model
VAR(Canada,p=P) -> Cnd.var

summary(Cnd.var)

plot(Cnd.var)


# predict 3years later n.aded is the number ?f seasonality , ci is trusted interval.
predict(Cnd.var, n.ahead=12, ci=0.95)


# glender inga
causality(Cnd.var, cause="e")
causality(Cnd.var, cause="U")
causality(Cnd.var, ca?se="prod")
causality(Cnd.var, cause="rw")


# impalus response function(irf)
ir?(Cnd.var,n.ahead=12,ci=0.95) -> Cnd.irf
 
plot(Cnd.irf)


# bunsan bunkai
fevd(Cnd.var, n.ahead=14) -> Cnd.fevd

plot(Cnd.fevd) # レイアウトが潰れていま?ｷが…上から(e, prod, rw, U)



read.csv("stores.csv")
