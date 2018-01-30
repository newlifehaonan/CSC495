# mycugtest.R
# A simplified version of sna::cug.test
# Takes only a single igraph
# Does not allow self-loops
# Does not support dyad-census conditioning
# Does not include edge values
# Function arguments are not a separate list but are treated as optional
# argument, similar to qaptest.

library(sna)
library(igraph)

mycugtest <- function (gr, FUN, directed=FALSE, 
                      cmode=c("size", "edges"), reps=1000, ...) {

  n <- vcount(gr)
  cmode <- match.arg(cmode)
  if (cmode == "size") {
      m <- NULL
  }
  else if (cmode == "edges") {
      m <- ecount(gr)
  }

  drawrep <- switch(cmode, size = function(n, ...) {
      erdos.renyi.game(n, 0.5, type="gnp", directed = directed)
    }, edges = function(n, m, ...) {
      erdos.renyi.game(n, m, type="gnm", directed=directed)
    })

  fun <- match.fun(FUN)
  modename = switch(directed, "directed", "undirected")

  obs <- fun(gr, ...)
  repstats <- vector()
  for (i in 1:reps) {
    repstats[i] <- fun(drawrep(n = n, m = m), ...)
  }
  out <- list(obs.stat = obs, rep.stat = repstats, mode = modename, 
              diag = FALSE, cmode = cmode, 
              plteobs = mean(repstats <= obs), 
              pgteobs = mean(repstats >= obs), reps = reps)
  class(out) <- "sna::cug.test"
  out
}

# Wrappers for output methods
print.cug.test <- function(ct) { sna::print.cug.test(ct) }
plot.cug.test <- function(ct) { sna::plot.cug.test(ct) }

# From sna::cug.test documentation
#Draw a highly reciprocal network
#g<-sna::rguman(1,15,mut=0.25,asym=0.05,null=0.7)
#mat <- sna::as.sociomatrix.sna(g)
#gr <- graph.adjacency(mat)

#Test transitivity against size and density
#c1 <- mycugtest(gr,transitivity,cmode="size", directed=TRUE, type="global")
#c2 <- mycugtest(gr,transitivity,cmode="edges", directed=TRUE, type="global")


