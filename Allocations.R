load("~/Desktop/Duke MQM/Intermediate Finance/A.RData")

cnames <- colnames(nominal)
assets <- nominal[, cnames[2:6]]
assets.r <- assets[,-ncol(assets)]
assets.nr <- assets[,ncol(assets)]

### 1
assets.means <- colMeans(assets)
assets.r.sd <- apply(assets.r, MARGIN = 2, FUN = sd)


### 2
assets.r.cor <- cor(assets.r)

### 3
VCV <- assets.r.sd %*% t(assets.r.sd) * assets.r.cor
VCV

assets.r.mean <- colMeans(assets.r)
assets.nr.mean <- mean(assets.nr)

### 4
vec1 <- rep(1,length=length(assets.r.mean))
alloc.r.MSR <- t(solve(VCV) %*% (assets.r.mean-assets.nr.mean)) / 
  as.numeric(t(vec1) %*% solve(VCV) %*% (assets.r.mean-assets.nr.mean))
alloc.r.MSR

(alloc.MSR = transform(alloc.r.MSR, rf = 1-sum(alloc.r.MSR)))

alloc.r.GMV = t(solve(VCV) %*% vec1) / as.numeric(t(vec1) %*% solve(VCV) %*% vec1)
alloc.r.GMV

(alloc.GMV = transform(alloc.r.GMV, rf = 1-(sum(alloc.r.GMV))))


ra = c(1.3, 2.8, 6.5, 10.5, 16.9)

alloc.r.ra = t(sapply(ra, FUN = function(x) {
  t(solve(VCV) %*% (assets.r.mean-assets.nr.mean)) /x
}
))
alloc.r.ra

(alloc.ra = transform(alloc.r.ra, rf=1-(rowSums(alloc.r.ra))))
colnames(alloc.ra) = colnames(alloc.GMV)

alloc.ra

alloc = as.matrix(rbind(alloc.MSR, alloc.GMV, alloc.ra))
rownames(alloc) = c('MSR', 'GMV', as.character(ra))
alloc

### 5
ER = alloc %*% assets.means
alloc.r = alloc[,-ncol(alloc)]

stdev = sapply(X=1:nrow(alloc.r),
               FUN = function(x) {
                 (alloc.r[x, ] %*% VCV %*% (alloc.r[x,]))^0.5
               }
)

SPratio = (ER - assets.nr.mean) / stdev
SPratio


#Plot
M1 = as.numeric(t(vec1) %*% solve(VCV) %*% assets.r.mean)
M2 = as.numeric(t(assets.r.mean) %*% solve(VCV) %*% assets.r.mean)
M3 = as.numeric(t(vec1) %*% solve(VCV) %*% vec1)
M4 = M2 * M3 - M1 ^2
M5 = M2 -2 * M1 * assets.nr.mean + M3 * assets.nr.mean ^ 2
N1 = 1 / M4 * (M2 * solve(VCV) %*% vec1 - M1 * solve(VCV) %*% assets.r.mean)

N2 = 1 / M4 * (M3 * solve(VCV) %*% assets.r.mean - M1 * solve(VCV) %*% vec1)

TargetER = seq(0,0.024,by=0.0001)
#without rf
alloc2 = t(sapply(
  TargetER,
  FUN = function(x) {
    N1 + N2 * x
  }
))
ER2 = alloc2 %*% assets.r.mean
Std2 = sapply(
  X = 1:nrow(alloc2),
  FUN = function(x) {
    (alloc2[x,] %*% VCV %*% (alloc2[x,])) ^ 0.5
  }
)

#draw Frontier

plot(stdev, ER,type = 'l', col = 'blue', lwd = 2
     , main = 'Portfolio Frontier with Rf: Portfolio Choice with Risk Aversion'
     , xlab = 'Standard Deviation', ylab = 'Expected Return'
     , xlim = c(-0.01, 0.15), ylim = c(0, 0.025)
     , xaxt = 'n', yaxt = 'n'
)

par(new=T)
lines(Std2, ER2, col = 'brown', lwd = 2)
par(new=T)
points(stdev, ER, pch = 19)
