load(file = 'data_4.RData')

# library("writexl")
# 
# df <- data.frame(dt)
# write_xlsx(df, "C:/Winston Files/2023 Duke/Duke/2022 Fall/Fall 2/Track 527Q-Intermediate Finance/Case Assignment/Case#5.xlsx")
# write.csv(dt, file="dt_Sherry.csv")


CPI = dt$Inflation #Inflation 
CPI_ind = cumprod(CPI+1)*100
ISMNO = dt$ISMNO.Index
my = dt$MY
par(mfrow = c(3,1))
plot(CPI_ind, type = 'l', main = 'CPI_U Inflation Index(1973 = 100)'
)
plot(CPI, type = 'l', main = 'CPI_U Inflation')
plot(ISMNO, type= 'l', main = 'ISM New orders Index(Mean = 50)')

aver = 6.5

#1. Use elements you have already calculated to calculate the portfolio weights, expected return, expected 
# standard deviation of returns, and expected Sharpe Ratio of the maximal Sharpe Ratio portfolio
# Repeat for the optimal portfolio allocations for mean variance investors with risk aversion
# (A in the equation below0 of 6.5

comp = function(dt, sub = 'all'){
  if(!(sub %in% c('all', 'INF1GRW1', 'INF2GRW1',
                  'INF1GRW2', 'INF2GRW2'))){
    stop('Invalid subset!')
  }
  
  if(sub != 'all'){
    dt = dt[dt[sub] == 1,]
  }
  A = dt[,6:10] #assets
  R = A[, 1:4] #risk assets
  RF = A[, 5] #Risk free
  a = colMeans(A) #mean
  s = apply(R, MARGIN = 2, sd) #sd
  r = colMeans(R)
  rf = mean(RF)
  sr = (r-rf)/s
  corr = cor(R)
  VCV = cov(R)
  w = t(solve(VCV) %*% (r-rf)) / as.numeric(t(rep(1,4)) %*% solve(VCV) %*% (r-rf))
  w = c(w, 1-sum(w))
  w.a = t(solve(VCV) %*% (r-rf)) / aver
  w.a = c(w.a, 1-sum(w.a))
  result = list(mean = a, Std = s, 'SharpeRatio' = sr,
                'W.MSR' = w, "W.aversion6.5" = w.a, VCV = VCV)
  return(result)
}
  
## unconditional 
(res1 = comp(dt))

## INF1GRW1
(res2 = comp(dt, sub = 'INF1GRW1'))

## INF2GRW1
(res3 = comp(dt, sub = 'INF2GRW1'))


## INF1GRW2
(res4 = comp(dt, sub = 'INF1GRW2'))

## INF2GRW2
(res5 = comp(dt, sub = 'INF2GRW2'))


# 2. Create a summary table on the “Summary” sheet which contains: the mean, standard 
# deviation, and Sharpe Ratio of each asset return calculated on each sheet and the weights 
# of the portfolio optimal for a mean-variance investor with a risk aversion of 6.5
# calculated on each sheet (the “6.5-Optimal Portfolio”)


tab_mean = rbind(res1$mean, res2$mean, res3$mean, res4$mean, res5$mean) #Mean
tab_mean
tab_sd = rbind(res1$Std, res2$Std, res3$Std, res4$Std, res5$Std) #SD
tab_sd
tab_sr = rbind(res1$SharpeRatio, res2$SharpeRatio, res3$SharpeRatio, res4$SharpeRatio, res5$SharpeRatio) #Sharpe Ratio
tab_sr
tab_MSR_w = rbind(res1$W.MSR, res2$W.MSR, res3$W.MSR, res4$W.MSR, res5$W.MSR) # MSR Weight
tab_MSR_w
tab_Aver_w = rbind(res1$W.aversion6.5, res2$W.aversion6.5, res3$W.aversion6.5, res4$W.aversion6.5, res5$W.aversion6.5) #Risk Aversion 
# res1$W.aversion6.5
tab_Aver_w
colnames(tab_MSR_w) = colnames(tab_Aver_w) = colnames(tab_mean)
rownames(tab_mean) = 
  rownames(tab_sd) = rownames(tab_sr) = 
  row.names(tab_MSR_w) = row.names(tab_Aver_w)
  c('Unconditional', 'INF1GRW1', 'INF2GRW1', 'INF1GRW2', 'INF2GRW2')

tab_VCV <- list(res1$VCV, res2$VCV, res3$VCV, res4$VCV, res5$VCV)
tab_VCV
  
A = dt[,6:10]
R = A[, 1:4]
RF = A[, 5]

riskfree <- tab_mean[,5]
riskfree

# MSR - Unconditional
MSR_mean_U <- tab_MSR_w[1,] %*% res1$mean
MSR_mean_U
MSR_sd_U <- (tab_MSR_w[1,1:4] %*% res1$VCV %*% tab_MSR_w[1,1:4])^0.5
MSR_sd_U
MSR_sr_U <- (MSR_mean_U-riskfree[1])/MSR_sd_U
MSR_sr_U

# MSR - INF1GRW1
MSR_mean_INF1GRW1 <- tab_MSR_w[2,] %*% res2$mean
MSR_mean_INF1GRW1
MSR_sd_INF1GRW1 <- (tab_MSR_w[2,1:4] %*% res2$VCV %*% tab_MSR_w[2,1:4])^0.5
MSR_sd_INF1GRW1
MSR_sr_INF1GRW1 <- (MSR_mean_INF1GRW1-riskfree[2])/MSR_sd_INF1GRW1
MSR_sr_INF1GRW1

# MSR - INF2GRW1
MSR_mean_INF2GRW1 <- tab_MSR_w[3,] %*% res3$mean
MSR_mean_INF2GRW1
MSR_sd_INF2GRW1 <- (tab_MSR_w[3,1:4] %*% res3$VCV %*% tab_MSR_w[3,1:4])^0.5
MSR_sd_INF2GRW1
MSR_sr_INF2GRW1 <- (MSR_mean_INF2GRW1-riskfree[3])/MSR_sd_INF2GRW1
MSR_sr_INF2GRW1

# MSR - INF1GRW2
MSR_mean_INF1GRW2 <- tab_MSR_w[4,] %*% res4$mean
MSR_mean_INF1GRW2
MSR_sd_INF1GRW2 <- (tab_MSR_w[4,1:4] %*% res4$VCV %*% tab_MSR_w[4,1:4])^0.5
MSR_sd_INF1GRW2
MSR_sr_INF1GRW2 <- (MSR_mean_INF1GRW2-riskfree[4])/MSR_sd_INF1GRW2
MSR_sr_INF1GRW2

# MSR - INF2GRW2
MSR_mean_INF2GRW2 <- tab_MSR_w[5,] %*% res5$mean
MSR_mean_INF2GRW2
MSR_sd_INF2GRW2 <- (tab_MSR_w[5,1:4] %*% res5$VCV %*% tab_MSR_w[5,1:4])^0.5
MSR_sd_INF2GRW2
MSR_sr_INF2GRW2 <- (MSR_mean_INF2GRW2-riskfree[5])/MSR_sd_INF2GRW2
MSR_sr_INF2GRW2

# 3. Create a set of portfolio weights by equally weighting the portfolio weights of the 6.5-
# Optimal Portfolio calculated for each regime (not the Unconditional data set, so the 
# weights will each be .25). Call this the Static – EW Portfolio. Now create 4 more sets of 
# portfolio weights, each by weighting one regime 1/2 and the other three at 1/6, naming 
# each Tilt – “Regime” (e.g “Tilt – INF1GRW1”) based on the regime it overweights.


prob0= c(1/4, 1/4, 1/4, 1/4)
prob0
prob1= c(1/2, 1/6, 1/6, 1/6)
prob2=c(1/6, 1/2, 1/6, 1/6)
prob3=c(1/6, 1/6, 1/2, 1/6)
prob4=c(1/6, 1/6, 1/6, 1/2)

# tab_Aver_w
# 
# Static = prob0 %*% tab_Aver_w[2:5,]
# Static
# Tilt_INF1GRW1 = prob1 %*% tab_Aver_w[2:5,]
# Tilt_INF1GRW1
# Tilt_INF1GRW2 = prob2 %*% tab_Aver_w[2:5,]
# Tilt_INF2GRW1 = prob3 %*% tab_Aver_w[2:5,]
# Tilt_INF2GRW2 = prob4 %*% tab_Aver_w[2:5,]
# 
# tab_weight = rbind(Static, Tilt_INF1GRW1, Tilt_INF1GRW2, Tilt_INF2GRW1, Tilt_INF2GRW2)
# tab_weight

# MSR
tab_MSR_w
Static_MSR = prob0 %*% tab_MSR_w[2:5,]
Static_MSR
Tilt_INF1GRW1_MSR = prob1 %*% tab_MSR_w[2:5,]
Tilt_INF1GRW1_MSR
Tilt_INF1GRW2_MSR = prob2 %*% tab_MSR_w[2:5,]
Tilt_INF1GRW2_MSR
Tilt_INF2GRW1_MSR = prob3 %*% tab_MSR_w[2:5,]
Tilt_INF2GRW1_MSR
Tilt_INF2GRW2_MSR = prob4 %*% tab_MSR_w[2:5,]
Tilt_INF2GRW2_MSR

New_tab_weight_MSR <- rbind(Static_MSR,Tilt_INF1GRW1_MSR,Tilt_INF1GRW2_MSR,Tilt_INF2GRW1_MSR,Tilt_INF2GRW2_MSR)
New_tab_weight_MSR

# 4. Calculate Sharpe Ratios

riskfree <- tab_mean[,5]
riskfree

# Static EW NewMSR - Unconditional
Static_NewMSR_mean_U <- New_tab_weight_MSR[1,] %*% tab_mean[1,]
Static_NewMSR_mean_U

Static_NewMSR_sd_U <- (New_tab_weight_MSR[1,1:4] %*% res1$VCV %*% New_tab_weight_MSR[1,1:4])^0.5
Static_NewMSR_sd_U
Static_NewMSR_sr_U <- (Static_NewMSR_mean_U-riskfree[1])/Static_NewMSR_sd_U
Static_NewMSR_sr_U

# Static EW NewMSR - INF1GRW1
Static_NewMSR_mean_INF1GRW1 <- New_tab_weight_MSR[1,] %*% res2$mean
Static_NewMSR_mean_INF1GRW1
Static_NewMSR_sd_INF1GRW1 <- (New_tab_weight_MSR[1,1:4] %*% res2$VCV %*% New_tab_weight_MSR[1,1:4])^0.5
Static_NewMSR_sd_INF1GRW1
Static_NewMSR_sr_INF1GRW1 <- (Static_NewMSR_mean_INF1GRW1-riskfree[2])/Static_NewMSR_sd_INF1GRW1
Static_NewMSR_sr_INF1GRW1

# Static EW NewMSR - INF2GRW1
Static_NewMSR_mean_INF2GRW1 <- New_tab_weight_MSR[1,] %*% res3$mean
Static_NewMSR_mean_INF2GRW1
Static_NewMSR_sd_INF2GRW1 <- (New_tab_weight_MSR[1,1:4] %*% res3$VCV %*% New_tab_weight_MSR[1,1:4])^0.5
Static_NewMSR_sd_INF2GRW1
Static_NewMSR_sr_INF2GRW1 <- (Static_NewMSR_mean_INF2GRW1-riskfree[3])/Static_NewMSR_sd_INF2GRW1
Static_NewMSR_sr_INF2GRW1

# Static EW NewMSR - INF1GRW2
Static_NewMSR_mean_INF1GRW2 <- New_tab_weight_MSR[1,] %*% res4$mean
Static_NewMSR_mean_INF1GRW2
Static_NewMSR_sd_INF1GRW2 <- (New_tab_weight_MSR[1,1:4] %*% res4$VCV %*% New_tab_weight_MSR[1,1:4])^0.5
Static_NewMSR_sd_INF1GRW2
Static_NewMSR_sr_INF1GRW2 <- (Static_NewMSR_mean_INF1GRW2-riskfree[4])/Static_NewMSR_sd_INF1GRW2
Static_NewMSR_sr_INF1GRW2

# Static EW NewMSR - INF2GRW2
Static_NewMSR_mean_INF2GRW2 <- New_tab_weight_MSR[1,] %*% res5$mean
Static_NewMSR_mean_INF2GRW2
Static_NewMSR_sd_INF2GRW2 <- (New_tab_weight_MSR[1,1:4] %*% res5$VCV %*% New_tab_weight_MSR[1,1:4])^0.5
Static_NewMSR_sd_INF2GRW2
Static_NewMSR_sr_INF2GRW2 <- (Static_NewMSR_mean_INF2GRW2-riskfree[5])/Static_NewMSR_sd_INF2GRW2
Static_NewMSR_sr_INF2GRW2

# Tilt11 NewMSR - Unconditional
Tilt11_NewMSR_mean_U <- New_tab_weight_MSR[2,] %*% res1$mean
Tilt11_NewMSR_mean_U
Tilt11_NewMSR_sd_U <- (New_tab_weight_MSR[2,1:4] %*% res1$VCV %*% New_tab_weight_MSR[2,1:4])^0.5
Tilt11_NewMSR_sd_U
Tilt11_NewMSR_sr_U <- (Tilt11_NewMSR_mean_U-riskfree[1])/Tilt11_NewMSR_sd_U
Tilt11_NewMSR_sr_U

# Tilt11 NewMSR - INF1GRW1
Tilt11_NewMSR_mean_INF1GRW1 <- New_tab_weight_MSR[2,] %*% res2$mean
Tilt11_NewMSR_mean_INF1GRW1
Tilt11_NewMSR_sd_INF1GRW1 <- (New_tab_weight_MSR[2,1:4] %*% res2$VCV %*% New_tab_weight_MSR[2,1:4])^0.5
Tilt11_NewMSR_sd_INF1GRW1
Tilt11_NewMSR_sr_INF1GRW1 <- (Tilt11_NewMSR_mean_INF1GRW1-riskfree[2])/Tilt11_NewMSR_sd_INF1GRW1
Tilt11_NewMSR_sr_INF1GRW1

# Tilt11 NewMSR - INF2GRW1
Tilt11_NewMSR_mean_INF2GRW1 <- New_tab_weight_MSR[2,] %*% res3$mean
Tilt11_NewMSR_mean_INF2GRW1
Tilt11_NewMSR_sd_INF2GRW1 <- (New_tab_weight_MSR[2,1:4] %*% res3$VCV %*% New_tab_weight_MSR[2,1:4])^0.5
Tilt11_NewMSR_sd_INF2GRW1
Tilt11_NewMSR_sr_INF2GRW1 <- (Tilt11_NewMSR_mean_INF2GRW1-riskfree[3])/Tilt11_NewMSR_sd_INF2GRW1
Tilt11_NewMSR_sr_INF2GRW1

# Tilt11 NewMSR - INF1GRW2
Tilt11_NewMSR_mean_INF1GRW2 <- New_tab_weight_MSR[2,] %*% res4$mean
Tilt11_NewMSR_mean_INF1GRW2
Tilt11_NewMSR_sd_INF1GRW2 <- (New_tab_weight_MSR[2,1:4] %*% res4$VCV %*% New_tab_weight_MSR[2,1:4])^0.5
Tilt11_NewMSR_sd_INF1GRW2
Tilt11_NewMSR_sr_INF1GRW2 <- (Tilt11_NewMSR_mean_INF1GRW2-riskfree[4])/Tilt11_NewMSR_sd_INF1GRW2
Tilt11_NewMSR_sr_INF1GRW2

# Tilt11 NewMSR - INF2GRW2
Tilt11_NewMSR_mean_INF2GRW2 <- New_tab_weight_MSR[2,] %*% res5$mean
Tilt11_NewMSR_mean_INF2GRW2
Tilt11_NewMSR_sd_INF2GRW2 <- (New_tab_weight_MSR[2,1:4] %*% res5$VCV %*% New_tab_weight_MSR[2,1:4])^0.5
Tilt11_NewMSR_sd_INF2GRW2
Tilt11_NewMSR_sr_INF2GRW2 <- (Tilt11_NewMSR_mean_INF2GRW2-riskfree[5])/Tilt11_NewMSR_sd_INF2GRW2
Tilt11_NewMSR_sr_INF2GRW2

# Tilt21 NewMSR - Unconditional
Tilt21_NewMSR_mean_U <- New_tab_weight_MSR[3,] %*% res1$mean
Tilt21_NewMSR_mean_U
Tilt21_NewMSR_sd_U <- (New_tab_weight_MSR[3,1:4] %*% res1$VCV %*% New_tab_weight_MSR[3,1:4])^0.5
Tilt21_NewMSR_sd_U
Tilt21_NewMSR_sr_U <- (Tilt21_NewMSR_mean_U-riskfree[1])/Tilt21_NewMSR_sd_U
Tilt21_NewMSR_sr_U

# Tilt21 NewMSR - INF1GRW1
Tilt21_NewMSR_mean_INF1GRW1 <- New_tab_weight_MSR[3,] %*% res2$mean
Tilt21_NewMSR_mean_INF1GRW1
Tilt21_NewMSR_sd_INF1GRW1 <- (New_tab_weight_MSR[3,1:4] %*% res2$VCV %*% New_tab_weight_MSR[3,1:4])^0.5
Tilt21_NewMSR_sd_INF1GRW1
Tilt21_NewMSR_sr_INF1GRW1 <- (Tilt21_NewMSR_mean_INF1GRW1-riskfree[2])/Tilt21_NewMSR_sd_INF1GRW1
Tilt21_NewMSR_sr_INF1GRW1

# Tilt21 NewMSR - INF2GRW1
Tilt21_NewMSR_mean_INF2GRW1 <- New_tab_weight_MSR[3,] %*% res3$mean
Tilt21_NewMSR_mean_INF2GRW1
Tilt21_NewMSR_sd_INF2GRW1 <- (New_tab_weight_MSR[3,1:4] %*% res3$VCV %*% New_tab_weight_MSR[3,1:4])^0.5
Tilt21_NewMSR_sd_INF2GRW1
Tilt21_NewMSR_sr_INF2GRW1 <- (Tilt21_NewMSR_mean_INF2GRW1-riskfree[3])/Tilt21_NewMSR_sd_INF2GRW1
Tilt21_NewMSR_sr_INF2GRW1

# Tilt21 NewMSR - INF1GRW2
Tilt21_NewMSR_mean_INF1GRW2 <- New_tab_weight_MSR[3,] %*% res4$mean
Tilt21_NewMSR_mean_INF1GRW2
Tilt21_NewMSR_sd_INF1GRW2 <- (New_tab_weight_MSR[3,1:4] %*% res4$VCV %*% New_tab_weight_MSR[3,1:4])^0.5
Tilt21_NewMSR_sd_INF1GRW2
Tilt21_NewMSR_sr_INF1GRW2 <- (Tilt21_NewMSR_mean_INF1GRW2-riskfree[4])/Tilt21_NewMSR_sd_INF1GRW2
Tilt21_NewMSR_sr_INF1GRW2

# Tilt21 NewMSR - INF2GRW2
Tilt21_NewMSR_mean_INF2GRW2 <- New_tab_weight_MSR[3,] %*% res5$mean
Tilt21_NewMSR_mean_INF2GRW2
Tilt21_NewMSR_sd_INF2GRW2 <- (New_tab_weight_MSR[3,1:4] %*% res5$VCV %*% New_tab_weight_MSR[3,1:4])^0.5
Tilt21_NewMSR_sd_INF2GRW2
Tilt21_NewMSR_sr_INF2GRW2 <- (Tilt21_NewMSR_mean_INF2GRW2-riskfree[5])/Tilt21_NewMSR_sd_INF2GRW2
Tilt21_NewMSR_sr_INF2GRW2

# Tilt12 NewMSR - Unconditional
Tilt12_NewMSR_mean_U <- New_tab_weight_MSR[4,] %*% res1$mean
Tilt12_NewMSR_mean_U
Tilt12_NewMSR_sd_U <- (New_tab_weight_MSR[4,1:4] %*% res1$VCV %*% New_tab_weight_MSR[4,1:4])^0.5
Tilt12_NewMSR_sd_U
Tilt12_NewMSR_sr_U <- (Tilt12_NewMSR_mean_U-riskfree[1])/Tilt12_NewMSR_sd_U
Tilt12_NewMSR_sr_U

# Tilt12 NewMSR - INF1GRW1
Tilt12_NewMSR_mean_INF1GRW1 <- New_tab_weight_MSR[4,] %*% res2$mean
Tilt12_NewMSR_mean_INF1GRW1
Tilt12_NewMSR_sd_INF1GRW1 <- (New_tab_weight_MSR[4,1:4] %*% res2$VCV %*% New_tab_weight_MSR[4,1:4])^0.5
Tilt12_NewMSR_sd_INF1GRW1
Tilt12_NewMSR_sr_INF1GRW1 <- (Tilt12_NewMSR_mean_INF1GRW1-riskfree[2])/Tilt12_NewMSR_sd_INF1GRW1
Tilt12_NewMSR_sr_INF1GRW1

# Tilt12 NewMSR - INF2GRW1
Tilt12_NewMSR_mean_INF2GRW1 <- New_tab_weight_MSR[4,] %*% res3$mean
Tilt12_NewMSR_mean_INF2GRW1
Tilt12_NewMSR_sd_INF2GRW1 <- (New_tab_weight_MSR[4,1:4] %*% res3$VCV %*% New_tab_weight_MSR[4,1:4])^0.5
Tilt12_NewMSR_sd_INF2GRW1
Tilt12_NewMSR_sr_INF2GRW1 <- (Tilt12_NewMSR_mean_INF2GRW1-riskfree[3])/Tilt12_NewMSR_sd_INF2GRW1
Tilt12_NewMSR_sr_INF2GRW1

# Tilt12 NewMSR - INF1GRW2
Tilt12_NewMSR_mean_INF1GRW2 <- New_tab_weight_MSR[4,] %*% res4$mean
Tilt12_NewMSR_mean_INF1GRW2
Tilt12_NewMSR_sd_INF1GRW2 <- (New_tab_weight_MSR[4,1:4] %*% res4$VCV %*% New_tab_weight_MSR[4,1:4])^0.5
Tilt12_NewMSR_sd_INF1GRW2
Tilt12_NewMSR_sr_INF1GRW2 <- (Tilt12_NewMSR_mean_INF1GRW2-riskfree[4])/Tilt12_NewMSR_sd_INF1GRW2
Tilt12_NewMSR_sr_INF1GRW2

# Tilt12 NewMSR - INF2GRW2
Tilt12_NewMSR_mean_INF2GRW2 <- New_tab_weight_MSR[4,] %*% res5$mean
Tilt12_NewMSR_mean_INF2GRW2
Tilt12_NewMSR_sd_INF2GRW2 <- (New_tab_weight_MSR[4,1:4] %*% res5$VCV %*% New_tab_weight_MSR[4,1:4])^0.5
Tilt12_NewMSR_sd_INF2GRW2
Tilt12_NewMSR_sr_INF2GRW2 <- (Tilt12_NewMSR_mean_INF2GRW2-riskfree[5])/Tilt12_NewMSR_sd_INF2GRW2
Tilt12_NewMSR_sr_INF2GRW2

# Tilt22 NewMSR - Unconditional
Tilt22_NewMSR_mean_U <- New_tab_weight_MSR[5,] %*% res1$mean
Tilt22_NewMSR_mean_U
Tilt22_NewMSR_sd_U <- (New_tab_weight_MSR[5,1:4] %*% res1$VCV %*% New_tab_weight_MSR[5,1:4])^0.5
Tilt22_NewMSR_sd_U
Tilt22_NewMSR_sr_U <- (Tilt22_NewMSR_mean_U-riskfree[1])/Tilt22_NewMSR_sd_U
Tilt22_NewMSR_sr_U

# Tilt22 NewMSR - INF1GRW1
Tilt22_NewMSR_mean_INF1GRW1 <- New_tab_weight_MSR[5,] %*% res2$mean
Tilt22_NewMSR_mean_INF1GRW1
Tilt22_NewMSR_sd_INF1GRW1 <- (New_tab_weight_MSR[5,1:4] %*% res2$VCV %*% New_tab_weight_MSR[5,1:4])^0.5
Tilt22_NewMSR_sd_INF1GRW1
Tilt22_NewMSR_sr_INF1GRW1 <- (Tilt22_NewMSR_mean_INF1GRW1-riskfree[2])/Tilt22_NewMSR_sd_INF1GRW1
Tilt22_NewMSR_sr_INF1GRW1

# Tilt22 NewMSR - INF2GRW1
Tilt22_NewMSR_mean_INF2GRW1 <- New_tab_weight_MSR[5,] %*% res3$mean
Tilt22_NewMSR_mean_INF2GRW1
Tilt22_NewMSR_sd_INF2GRW1 <- (New_tab_weight_MSR[5,1:4] %*% res3$VCV %*% New_tab_weight_MSR[5,1:4])^0.5
Tilt22_NewMSR_sd_INF2GRW1
Tilt22_NewMSR_sr_INF2GRW1 <- (Tilt22_NewMSR_mean_INF2GRW1-riskfree[3])/Tilt22_NewMSR_sd_INF2GRW1
Tilt22_NewMSR_sr_INF2GRW1

# Tilt22 NewMSR - INF1GRW2
Tilt22_NewMSR_mean_INF1GRW2 <- New_tab_weight_MSR[5,] %*% res4$mean
Tilt22_NewMSR_mean_INF1GRW2
Tilt22_NewMSR_sd_INF1GRW2 <- (New_tab_weight_MSR[5,1:4] %*% res4$VCV %*% New_tab_weight_MSR[5,1:4])^0.5
Tilt22_NewMSR_sd_INF1GRW2
Tilt22_NewMSR_sr_INF1GRW2 <- (Tilt22_NewMSR_mean_INF1GRW2-riskfree[4])/Tilt22_NewMSR_sd_INF1GRW2
Tilt22_NewMSR_sr_INF1GRW2

# Tilt22 NewMSR - INF2GRW2
Tilt22_NewMSR_mean_INF2GRW2 <- New_tab_weight_MSR[5,] %*% res5$mean
Tilt22_NewMSR_mean_INF2GRW2
Tilt22_NewMSR_sd_INF2GRW2 <- (New_tab_weight_MSR[5,1:4] %*% res5$VCV %*% New_tab_weight_MSR[5,1:4])^0.5
Tilt22_NewMSR_sd_INF2GRW2
Tilt22_NewMSR_sr_INF2GRW2 <- (Tilt22_NewMSR_mean_INF2GRW2-riskfree[5])/Tilt22_NewMSR_sd_INF2GRW2
Tilt22_NewMSR_sr_INF2GRW2


# Trying to code it more elegantly
# New MSR Means
AllNewMSRmeans <- data.frame()
for (i in 1:5) {
    NewMSRmeans <- c()
    for (j in 1:5){
        NewMSRmean <- New_tab_weight_MSR[i,] %*% tab_mean[j,]
        NewMSRmeans <- cbind(NewMSRmeans,NewMSRmean)
    }
    # print(NewMSRmeans)
    AllNewMSRmeans <- rbind(AllNewMSRmeans,NewMSRmeans)
}
AllNewMSRmeans
colnames(AllNewMSRmeans) <- c('Unconditional','INF1GRW1','INF2GRW1','INF1GRW2','INF2GRW2')
rownames(AllNewMSRmeans) <- c('Static - EW','Tilt - INF1GRW1','Tilt - INF2GRW1','Tilt - INF1GRW2','Tilt - INF2GRW2')
AllNewMSRmeans

# New MSR Sd
AllNewMSRsds <- data.frame()
for (i in 1:5) {
    NewMSRsds <- c()
    for (j in 1:5){
        NewMSRsd <- (New_tab_weight_MSR[i,1:4] %*% tab_VCV[[j]] %*% New_tab_weight_MSR[i,1:4])^0.5
        NewMSRsds <- cbind(NewMSRsds,NewMSRsd)
    }
    AllNewMSRsds <- rbind(AllNewMSRsds,NewMSRsds)
}
AllNewMSRsds
colnames(AllNewMSRsds) <- c('Unconditional','INF1GRW1','INF2GRW1','INF1GRW2','INF2GRW2')
rownames(AllNewMSRsds) <- c('Static - EW','Tilt - INF1GRW1','Tilt - INF2GRW1','Tilt - INF1GRW2','Tilt - INF2GRW2')
AllNewMSRsds

# New MSR Sharpe Ratios
riskfree <- tab_mean[,5]
riskfree

AllNewMSRExcessReturns <- data.frame()
for (i in 1:5){
    ExcessReturns <- AllNewMSRmeans[i,]-riskfree
    AllNewMSRExcessReturns <- rbind(AllNewMSRExcessReturns,ExcessReturns)
}
AllNewMSRExcessReturns

AllNewMSRSharpeRatios <- round(AllNewMSRExcessReturns/AllNewMSRsds,3)
AllNewMSRSharpeRatios
colnames(AllNewMSRSharpeRatios) <- c('Unconditional','INF1GRW1','INF2GRW1','INF1GRW2','INF2GRW2')
rownames(AllNewMSRSharpeRatios) <- c('Static - EW','Tilt - INF1GRW1','Tilt - INF2GRW1','Tilt - INF1GRW2','Tilt - INF2GRW2')
AllNewMSRSharpeRatios

# Testing on 6.5 Optimal

# Finding the weights for Static and Tilts
tab_Aver_w
Static_6.5 = prob0 %*% tab_Aver_w[2:5,]
Static_6.5
Tilt_INF1GRW1_6.5 = prob1 %*% tab_Aver_w[2:5,]
Tilt_INF1GRW1_6.5
Tilt_INF1GRW2_6.5 = prob2 %*% tab_Aver_w[2:5,]
Tilt_INF1GRW2_6.5
Tilt_INF2GRW1_6.5 = prob3 %*% tab_Aver_w[2:5,]
Tilt_INF2GRW1_6.5
Tilt_INF2GRW2_6.5 = prob4 %*% tab_Aver_w[2:5,]
Tilt_INF2GRW2_6.5

New_tab_weight_6.5 <- rbind(Static_6.5,Tilt_INF1GRW1_6.5,Tilt_INF1GRW2_6.5,Tilt_INF2GRW1_6.5,Tilt_INF2GRW2_6.5)
New_tab_weight_6.5

# New 6.5 Means
# tab_mean
AllNew6.5means <- data.frame()
for (i in 1:5) {
    New6.5means <- c()
    for (j in 1:5){
        New6.5mean <- New_tab_weight_6.5[i,] %*% tab_mean[j,]
        New6.5means <- cbind(New6.5means,New6.5mean)
    }
    AllNew6.5means <- rbind(AllNew6.5means,New6.5means)
}
AllNew6.5means
colnames(AllNew6.5means) <- c('Unconditional','INF1GRW1','INF2GRW1','INF1GRW2','INF2GRW2')
rownames(AllNew6.5means) <- c('Static - EW','Tilt - INF1GRW1','Tilt - INF2GRW1','Tilt - INF1GRW2','Tilt - INF2GRW2')
AllNew6.5means

# New 6.5 Sd
AllNew6.5sds <- data.frame()
for (i in 1:5) {
    New6.5sds <- c()
    for (j in 1:5){
        New6.5sd <- (New_tab_weight_6.5[i,1:4] %*% tab_VCV[[j]] %*% New_tab_weight_6.5[i,1:4])^0.5
        New6.5sds <- cbind(New6.5sds,New6.5sd)
    }
    AllNew6.5sds <- rbind(AllNew6.5sds,New6.5sds)
}
AllNew6.5sds
colnames(AllNew6.5sds) <- c('Unconditional','INF1GRW1','INF2GRW1','INF1GRW2','INF2GRW2')
rownames(AllNew6.5sds) <- c('Static - EW','Tilt - INF1GRW1','Tilt - INF2GRW1','Tilt - INF1GRW2','Tilt - INF2GRW2')
AllNew6.5sds

# New 6.5 Sharpe Ratios
riskfree <- tab_mean[,5]
riskfree

AllNew6.5ExcessReturns <- data.frame()
for (i in 1:5){
    ExcessReturns <- AllNew6.5means[i,]-riskfree
    AllNew6.5ExcessReturns <- rbind(AllNew6.5ExcessReturns,ExcessReturns)
}
AllNew6.5ExcessReturns

AllNew6.5SharpeRatios <- round(AllNew6.5ExcessReturns/AllNew6.5sds,3)
AllNew6.5SharpeRatios
colnames(AllNew6.5SharpeRatios) <- c('Unconditional','INF1GRW1','INF2GRW1','INF1GRW2','INF2GRW2')
rownames(AllNew6.5SharpeRatios) <- c('Static - EW','Tilt - INF1GRW1','Tilt - INF2GRW1','Tilt - INF1GRW2','Tilt - INF2GRW2')
AllNew6.5SharpeRatios

#4. Estimate the Sharpe ratio of each 6.5-Optimal Portfolio and the Static and Tilted
# Portfolios in each regime and in the unconditional (full) data set. Thus you will have a 
# 10x5 table of Sharpe Ratios, Unconditional in Unconditional, Unconditional in 
# INF1GRW1, etc. You may use the VCV and mean returns calculated in each regime to 
# create these estimates.


comp = function(dt, sub = 'all'){
  if(!(sub %in% c('all', 'INF1GRW1', 'INF2GRW1',
                  'INF1GRW2', 'INF2GRW2'))){
    stop('Invalid subset!')
  }
  
  if(sub != 'all'){
    dt = dt[dt[sub] == 1,]
  }
  A = dt[,6:10] #assets
  R = A[, 1:4] #risk assets
  A_for_SD = A[,1:5]
  RF = A[, 5] #Risk free
  a = colMeans(A) #mean
  s = apply(R, MARGIN = 2, sd) #sd
  s_w_rf = apply(A_for_SD, MARGIN = 2, sd)
  r = colMeans(R)
  rf = mean(RF)
  sr = (r-rf)/s
  corr = cor(R)
  VCV = cov(R)
  w = t(solve(VCV) %*% (r-rf)) / as.numeric(t(rep(1,4)) %*% solve(VCV) %*% (r-rf))
  w = c(w, 1-sum(w))
  w.a = t(solve(VCV) %*% (r-rf)) / aver
  w.a = c(w.a, 1-sum(w.a))
  result = list(mean = a, Std = s, 'SharpeRatio' = sr,
                'W.MSR' = w, "W.aversion6.5" = w.a, Std_w_rf = s_w_rf)
  return(result)
}

## unconditional 
(res1_3 = comp(dt))

## INF1GRW1
(res2_3 = comp(dt, sub = 'INF1GRW1'))

## INF2GRW1
(res3_3 = comp(dt, sub = 'INF2GRW1'))


## INF1GRW2
(res4_3 = comp(dt, sub = 'INF1GRW2'))

## INF2GRW2
(res5_3 = comp(dt, sub = 'INF2GRW2'))



### means

Static_Tilt_Mean = tab_weight %*% tab_mean
Static_Tilt_Mean

Static_Tilt_Mean1 = rowSums(Static_Tilt_Mean[,1:5])
Static_Tilt_Mean1

### standard deviation 


tab_sd1 = rbind(res1_3$Std_w_rf, res2_3$Std_w_rf, res3_3$Std_w_rf, res4_3$Std_w_rf, res5_3$Std_w_rf) #SD with Rf
tab_sd1

Static_Tilt_SD = tab_weight %*% tab_sd1

Static_Tilt_SD


Static_Tilt_SD1 = rowSums(Static_Tilt_SD[,1:5])
Static_Tilt_SD1

###Sharpe Ratio




# suppose you already got the weight 

INF1GRW1.w = mean(w*dt[dt['INF1GRW1']==1,6:9])


Return = tab_mean*tab_MSR_w
Return

#####
A2=c(A1$mean, A1$rf)
A3= A2[1:5]

Uncon.r = sum(A1*res1$W.aversion6.5)

INF1GRW1.r = sum(A1*res2$W.aversion6.5)

INF1GRW2.r = sum(A1*res3$W.aversion6.5)

INF2GRW1.r = sum(A1*res4$W.aversion6.5)

INF2GRW2.r = sum(A1*res5$W.aversion6.5)

A4 = c(A1$sd, sd(dt$Tbill))

Uncon.sd = sum(A4*res1$W.aversion6.5)

INF1GRW1.sd = sum(A4*res2$W.aversion6.5)

INF1GRW2.sd = sum(A4*res3$W.aversion6.5)

INF2GRW1.sd = sum(A4*res4$W.aversion6.5)

INF2GRW2.sd = sum(A4*res5$W.aversion6.5)

rf = A2[5]

Uncon.sr = (Uncon.r-rf)/Uncon.sd/100

INF1GRW1.sr = (INF1GRW1.r-rf)/INF1GRW1.sd/100

INF2GRW1.sr = (INF2GRW1.r-rf)/INF2GRW1.sd/100

INF1GRW2.sr = (INF1GRW2.r-rf)/INF1GRW2.sd/100

INF2GRW2.sr = (INF2GRW2.r-rf)/INF2GRW2.sd/100


rbind(Uncon.sr,INF1GRW1.sr, INF2GRW1.sr, INF1GRW2.sr, INF2GRW2.sr)







