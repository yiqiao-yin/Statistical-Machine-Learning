# download data
DJ30_smbl_list = c("AAPL", "AXP", "BA", "CAT", "CSCO", "CVX", "DD", "DIS", "GE",
                   "GS", "HD", "IBM", "INTC", "JNJ", "JPM", "KO", "MCD", "MMM",
                   "MRK", "MSFT", "NKE", "PFE", "PG", "TRV", "UNH", "UTX", "V",
                   "VZ", "WMT", "XOM")
baseLink = paste("http://chart.finance.yahoo.com/table.csv?s=", DJ30_smbl_list, "&a=0&b=1&c=2010&d=0&e=1&f=2011&g=d&ignore=.csv", sep="")
baseLoc = "/Users/linxiliu/Dropbox/Teaching/Statistical_Machine_Learning/Assignments/Assignment_1/"
fileLoc = paste(baseLoc, "DJ30Data/10-11", DJ30_smbl_list, ".csv", sep="")

for (i in 1:length(baseLink)){
  download.file(baseLink[i], fileLoc[i], method="internal", quiet = F)
}

# construct the initial data
X = c()
for (i in 1:length(fileLoc)){
  X = cbind(X, read.csv(fileLoc[i])$Close)
}
colnames(X) = DJ30_smbl_list
n = dim(X)[1]
p = dim(X)[2]

## cor = F
pcaX = princomp(X, cor=F)
V = pcaX$loadings
center = pcaX$center
scale = pcaX$scale

# Plot
# biplot
pdf(width=5, height=5, file=paste(baseLoc, "P4figure_corF_biplot.pdf", sep=""))
biplot(pcaX, cex=0.6)
dev.off()

# screeplot
pdf(width=5, height=5, file=paste(baseLoc, "P4figure_corF_screeplot.pdf", sep=""))
screeplot(pcaX)
dev.off()

## cor = T
pcaX = princomp(X, cor=T)
V = pcaX$loadings
center = pcaX$center
scale = pcaX$scale

# Plot
# biplot
pdf(width=5, height=5, file=paste(baseLoc, "P4figure_corT_biplot.pdf", sep=""))
biplot(pcaX, cex=0.6)
dev.off()

# screeplot
pdf(width=5, height=5, file=paste(baseLoc, "P4figure_corT_screeplot.pdf", sep=""))
screeplot(pcaX)
dev.off()

## calculate
StockReturn = (X[2 : n, ] - X[1 : (n - 1), ]) / X[1 : (n - 1), ]
pcaReturn = princomp(StockReturn, cor=T)
V = pcaReturn$loadings
center = pcaReturn$center
scale = pcaReturn$scale

# Plot
# biplot
pdf(width=5, height=5, file=paste(baseLoc, "P4figure_corT_biplotReturn.pdf", sep=""))
biplot(pcaReturn, cex=0.6)
dev.off()

# screeplot
pdf(width=5, height=5, file=paste(baseLoc, "P4figure_corT_screeplotReturn.pdf", sep=""))
screeplot(pcaReturn)
dev.off()

