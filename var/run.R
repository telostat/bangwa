library(microbenchmark)
load_all(".")
a <- matrix(rnorm(100000), ncol=100)
print(microbenchmark(mvReturns(a), diff(a)/a[-NROW(a),], times=1000))
