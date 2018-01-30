# myqaptest.R
# Simplified version of sna::qaptest
# Note that only single graph QAP hypothesis testing is supported

library(sna)
library(igraph)

myqaptest <- function(gr, FUN, reps=1000, ...) {
  out <- list()
  fun <- match.fun(FUN)
  out$testval <- fun(gr, ...)
  out$dist <- vector(mode="numeric", length=reps)
  for (i in 1:reps) {
    grp <- permute.vertices(gr, sample(1:vcount(gr)))
    out$dist[i] <- fun(grp, ...)
  }
  out$pgreq <- mean(as.numeric(out$dist >= out$testval))
  out$pleeq <- mean(as.numeric(out$dist <= out$testval))
  class(out) <- c("sna::qaptest", "sna::qap")
  out
}

summary.qaptest <- function (qt) { sna::summary.qaptest(qt) }
plot.qaptest <- function(qt) { sna::plot.qaptest(qt) }

# Example using assortativity
# Can't use the example from sna::qaptest because multiple graphs
# are not supported.
#g <- erdos.renyi.game(100, 0.1, type=c("gnp"))
#cl <- sample(rep(c(1,2,3,4), 25))
#qap <- myqaptest(g, assortativity.nominal, reps=1000, 
#                 types=cl, directed=FALSE)

