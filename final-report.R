
library('rstudioapi')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ---- loadLibraries ----
library('ggplot2')
library(plyr)
library(reshape2)
library(igraph)
library('sand')

igraph.degree <- degree
igraph.betweenness <- betweenness

source('multiplot.R')

# ---- setGlobals ----
set.seed(666)
co.purple.0 <- "#9C27B0"
co.purple.1 <- "#E1BEE7"

# ---- entireGraph ----
g.1 <- read_graph('./_save/1-g-cpu.graphml', 'graphml')

summary(g.1)
table(V(g.1)$node_type)
table(E(g.1)$edge_type)
data.frame(
  Attribute=sort(vertex_attr_names(g.1)),
  Description=c(
    "Node in degree.",
    "Node out degree.",
    "Node total degree.",
    "Full public name of user.",
    "Product or user helpfulness ratio mean over all connected reviews.",
    "Product or user helpfulness ratio median over all connected reviews.",
    "Product or user helpfulness total number of votes over all connected reviews.",
    "Product or user helpfulness up vote total over all connected reviews.",
    "Id of node for graphml format.",
    "Product ASIN, user ID, or category name.",
    "Either 'product', 'user', or 'category'.",
    "Price of product.",
    "Product or user mean rating over all connected reviews.",
    "Product or user median rating over all connected reviews.",
    "Title of product.",
    "Top category of product, in this case all values are 'Music'.",
    "All time top ranking within the Music category."
  )
)
data.frame(
  Attribute=sort(edge_attr_names(g.1)),
  Description=c(
    "Either 'd_category_includes_product' (category->product), 'd_category_is_parent_of' (category->category), or 'd_has_reviewed' (user->product).",
    "Review helpfulness up vote total.",
    "Review helpfulness up and down vote total.",
    "Review overall score [0,5]",
    "Unix time of review."
  )
)

# ---- entireGraph.skip ----
write_graph(g.1, './_save/2-g-cpu.graphml', 'graphml')

# ---- ppProjection.save ----
g.2 <- read_graph('./_save/2-g-cpu.graphml', 'graphml')
g.2 <- delete_vertices(g.2, V(g.2)[V(g.2)$node_type == 'category'])
V(g.2)$type <- V(g.2)$node_type == 'product'
table(V(g.2)$node_type)
g.2 <- bipartite_projection(g.2, which='true')
write_graph(g.2, './_save/3-g-pp.graphml', 'graphml')

# ---- ppProjection.load ----
summary(read_graph('./_save/3-g-pp.graphml', 'graphml'))

# ---- filter ----
g.3 <- read_graph('./_save/3-g-pp.graphml', 'graphml')

g.3.strengths <- log(strength(g.3))
strengths.breaks <- seq(from=0, to=max(g.3.strengths) + 1)
strengths.df <- data.frame(table(cut(g.3.strengths, breaks=50)))
strengths.df <- strengths.df[strengths.df$Freq > 0,]
strengths.df$log_bin <- strengths.df$Var1
strengths.df$log_freq <- log(strengths.df$Freq)
names(strengths.df) <- c('bin', 'freq', 'log_bin', 'log_freq')
f.plot.ls <- ggplot(
    strengths.df,
    aes(
      x=bin,
      y=log_freq
    )
  ) +
  geom_point(
    color=co.purple.0
  ) +
  ggtitle('Log-Log Product Strength Dist') +
  xlab('Log Strength') +
  ylab('Log Count') +
  scale_y_continuous() +
  scale_x_discrete(
    breaks=levels(strengths.df$bin)[c(T, rep(F, 10))]
  )
g.3.weights <- log(E(g.3)$weight)
weights.breaks <- seq(from=0, to=max(g.3.weights) + 1)
weights.df <- data.frame(table(cut(g.3.weights, breaks=25)))
weights.df <- weights.df[weights.df$Freq > 0,]
weights.df$log_bin <- weights.df$Var1
weights.df$log_freq <- log(weights.df$Freq)
names(weights.df) <- c('bin', 'freq', 'log_bin', 'log_freq')
f.plot.lw <- ggplot(
    weights.df,
    aes(
      x=bin,
      y=log_freq
    )
  ) +
  geom_point(
    color=co.purple.0
  ) +
  ggtitle('Log-Log Edge Weight Dist') +
  xlab('Log Weight') +
  ylab('Log Count') +
  scale_y_continuous() +
  scale_x_discrete(
    breaks=levels(weights.df$bin)[c(T, rep(F, 3))]
  )
filter.df.1 <- data.frame(edge_weight=E(g.3)$weight)
f.plot.ew <- ggplot(
  filter.df.1,
  aes(
    x=edge_weight
  )
) +
  geom_histogram(
    binwidth=1,
    fill=co.purple.0
  ) +
  ggtitle('Edge Weight Dist') +
  xlab('Weight') +
  ylab('Count')
filter.df.2 <- data.frame(strength=strength(g.3))
f.plot.s <- ggplot(
  filter.df.2,
  aes(
    x=strength
  )
) +
  geom_histogram(
    binwidth=50,
    fill=co.purple.0
  ) +
  ggtitle('Product Strength Dist') +
  xlab('Strength') +
  ylab('Count')

g.3 <- delete_edges(g.3, E(g.3)[E(g.3)$weight < 2])
g.3 <- delete_vertices(g.3, V(g.3)[strength(g.3) <= 50])

filter.df.1 <- data.frame(edge_weight=E(g.3)$weight)
f.plot.ew.a <- ggplot(
  filter.df.1,
  aes(
    x=edge_weight
  )
) +
  geom_histogram(
    binwidth=1,
    fill=co.purple.1
  ) +
  ggtitle('Edge Weight Dist, Filtered') +
  xlab('Weight') +
  ylab(NULL)
filter.df.2 <- data.frame(strength=strength(g.3))
f.plot.s.a <- ggplot(
  filter.df.2,
  aes(
    x=strength
  )
) +
  geom_histogram(
    binwidth=50,
    fill=co.purple.1
  ) +
  ggtitle('Product Strength Dist, Filtered') +
  xlab('Strength') +
  ylab(NULL)

multiplot(f.plot.ls, f.plot.lw, cols=2)
multiplot(f.plot.ew, f.plot.s, f.plot.ew.a, f.plot.s.a, cols=2)

# ---- filter.save ----
le <- cluster_leading_eigen(g.3)
fg <- cluster_fast_greedy(g.3)
# bt <- cluster_edge_betweenness(g.3)
wt <- cluster_walktrap(g.3)
wt10 <- cluster_walktrap(g.3, steps=10)

saveRDS(le, './_save/le.rds')
saveRDS(fg, './_save/fg.rds')
saveRDS(wt, './_save/wt.rds')
saveRDS(wt10, './_save/wt10.rds')

# ---- filter.load -----
le <- readRDS('./_save/le.rds')
fg <- readRDS('./_save/fg.rds')
wt <- readRDS('./_save/wt.rds')
wt10 <- readRDS('./_save/wt10.rds')

clusterings.framed <- data.frame(
  Algorithm=c("Leading Eigenvector", "Fastgreedy", "Walktrap 4 Steps", "Walktrap 10 Steps"),
  Number_of_Clusters=c(length(le), length(fg), length(wt), length(wt10)),
  Modularity=as.numeric(lapply(list(le, fg, wt, wt10), modularity))
)
clusterings.framed[order(clusterings.framed$Modularity, decreasing=TRUE),]

V(g.3)$community_id_le <- membership(le)
V(g.3)$community_id_fg <- membership(fg)
V(g.3)$community_id_wt <- membership(wt)
V(g.3)$community_id_wt10 <- membership(wt10)

# ---- filter.skip ---- 
write_graph(g.3, './_save/4-g-pp.graphml', 'graphml')
# ...intermediate step of adding categories to the graph with python

# ---- comm.start ----
g.5 <- read_graph('./_save/5-g-ppc.graphml', 'graphml')

clustering <- readRDS('./_save/le.rds')

# delete some vertices we don't want (only relevant for le)
g.5 <- delete_vertices(g.5, V(g.5)$community_id_le == "6")
g.5 <- delete_vertices(g.5, V(g.5)$community_id_le == "7")

community_ids <- names(table(V(g.5)$community_id_le))

# remove categories that connect too much
head(sort(igraph.degree(g.5, V(g.5)$node_type=='category'), decreasing=TRUE), 5)
g.5 <- delete_vertices(g.5, V(g.5)$name == "Pop")
g.5 <- delete_vertices(g.5, V(g.5)$name == "Rock")
head(sort(igraph.degree(g.5, V(g.5)$node_type=='category'), decreasing=TRUE), 5)

# ---- comm.top5 ----
top_categories <- data.frame()
pre_edges <- E(g.5)[E(g.5)$edge_type == 'd_category_includes_product']
for (cid in community_ids) {
  unique_categories <- c()
  for (pid in communities(clustering)[cid]) {
    unique_categories <- c(unique_categories, tail_of(g.5, pre_edges[to(V(g.5)[pid])])$name)
  }
  tops <- head(sort(table(unique_categories), decreasing=TRUE), n=5)
  
  if (length(tops) > 1) {
    c.frame <- data.frame(
      tops,
      Community=cid
    )
  } else if (length(tops) == 1) {
    c.frame <- data.frame(
      unique_categories=unique(unique_categories),
      Freq=table(unique_categories)[[1]],
      Community=cid
    )
  }
  top_categories <- rbind(top_categories, c.frame)
}
names(top_categories) <- c("Category", "Count", "Community")
top_categories$Id <- rep(c(1:5),5)
f.topCats <- ggplot(
  top_categories,
  aes(
    x=Community,
    y=Count,
    group=Id,
    label=Category
  )
) +
  geom_bar(
    stat="identity",
    color="black",
    fill=co.purple.0,
    position="dodge"
  ) +
  geom_text(
    angle=90,
    position=position_dodge(width=0.9),
    hjust=-0.05
  ) +
  ylim(0, 650) +
  ggtitle("Number of Products in each Category in Each Community") +
  guides(fill=FALSE)
f.topCats

# ---- comm.basicStats ----
g.5 <- delete_vertices(g.5, V(g.5)[V(g.5)$node_type == 'category'])
c.counts <- vapply(
  community_ids,
  function (i) length(V(g.5)[V(g.5)$community_id_le == i]),
  1
)
c.reviews <- vapply(
  community_ids,
  function (i) sum(V(g.5)[V(g.5)$community_id_le == i]$degree_in),
  1
)
g.5.strengths <- strength(g.5)
c.strengths <- vapply(
  community_ids,
  function (i) mean(g.5.strengths[V(g.5)$community_id_le == i]),
  1
)
helpful.frame.1 <- data.frame(
  Community=community_ids,
  Count=c.counts,
  Reviews=c.reviews,
  Strengths=c.strengths
)
f.prodPerComm <- ggplot(
  helpful.frame.1,
  aes(
    x=Community,
    y=Count
  )
) +
  geom_bar(
    stat="identity",
    color="black",
    fill=co.purple.0
  ) +
  ylab("Number of Products") +
  ggtitle("Products")
f.revPerComm <- ggplot(
  helpful.frame.1,
  aes(
    x=Community,
    y=Reviews
  )
) +
  geom_bar(
    stat="identity",
    color="black",
    fill=co.purple.0
  ) +
  ylab("Number of Reviews") +
  ggtitle("Reviews")
f.strengthPerComm <- ggplot(
  helpful.frame.1,
  aes(
    x=Community,
    y=Strengths
  )
) +
  geom_bar(
    stat="identity",
    color="black",
    fill=co.purple.0
  ) +
  ylab("Average Node Strength") +
  ggtitle("Average Node Strength")
multiplot(f.prodPerComm, f.revPerComm, f.strengthPerComm, cols=3)

# ---- comm.help ----
h.means.up <- vapply(
  community_ids,
  function (i) mean(V(g.5)[V(g.5)$community_id_le == i]$helpful_up_total),
  1
)
h.means.tot <- vapply(
  community_ids,
  function (i) mean(V(g.5)[V(g.5)$community_id_le == i]$helpful_total),
  1
)
helpful.frame.2 <- melt(data.frame(
  Community=community_ids,
  Yes=h.means.up,
  Total=h.means.tot
))
f.helpBars <- ggplot(
  helpful.frame.2,
  aes(
    x=Community,
    y=value,
    group=variable,
    fill=variable
  )
) +
  geom_bar(
    stat="identity",
    color="black",
    position="dodge"
  ) +
  scale_fill_manual(values=c(co.purple.1, co.purple.0)) +
  ylab("Mean Votes") +
  ggtitle(
    "Mean Helpfulness Votes",
    subtitle="per product, per community"
  ) +
  guides(fill=guide_legend(title="Helpful"))
g.5.df.help <- data.frame(
  Community=V(g.5)$community_id_le,
  Yes=V(g.5)$helpful_up_total,
  Total=V(g.5)$helpful_total
)
g.5.df.help.melt <- melt(g.5.df.help, "Community")
f.helpPerComm <- ggplot(
  g.5.df.help.melt,
  aes(
    x=factor(Community),
    y=value,
    fill=factor(variable)
  )
) +
  geom_boxplot(
    outlier.size=0.05,
    outlier.color='red'
  ) +
  xlab("Community") +
  ylab("Mean Votes") +
  ylim(0, 500) +
  scale_fill_manual(values=c(co.purple.1, co.purple.0)) +
  guides(fill=guide_legend(title="Helpful")) +
  ggtitle(
    "Mean Helpfulness Distribution",
    subtitle="per product, per community"
  )
multiplot(f.helpPerComm, f.helpBars, cols=2)

# is graph assortative on helpfulness
assortativity_nominal(g.5, types=cut(V(g.5)$helpful_total, 10), directed=FALSE)
assortativity_nominal(g.5, types=cut(V(g.5)$helpful_up_total, 10), directed=FALSE)

# ---- comm.rating ----
g.5.df <- as.data.frame(
  list(
    RatingMean=V(g.5)$rating_mean,
    Community=V(g.5)$community_id_le
  ),
  stringsAsFactors=FALSE
)
ggplot(
  g.5.df,
  aes(
    factor(Community),
    RatingMean
  )
) +
  geom_boxplot(
    fill=co.purple.0,
    outlier.size=NA
  ) +
  geom_dotplot(
    binwidth=0.02,
    binaxis='y',
    stackdir='center',
    fill='red',
    color=NA
  ) +
  xlab("Community") +
  ylab("Mean Rating") +
  ggtitle(
    "Mean Rating Distribution Per Community",
    subtitle="Each dot is 1 product's mean rating"
  )

# is graph assortative on review means
assortativity_nominal(g.5, types=cut(V(g.5)$rating_mean, 10), directed=FALSE)

# ---- rand.help ----
if(!require(sna)) install.packages('sna', repos='http://cran.us.r-project.org')
source('mycugtest.R')
source('myqaptest.R')

# ---- rand.help.save ----
g.5.cug.assort.help.tot <- mycugtest(
  g.5,
  assortativity_nominal,
  types=cut(V(g.5)$helpful_total, 10),
  directed=FALSE
)
g.5.qap.assort.help.tot <- myqaptest(
  g.5,
  assortativity_nominal,
  types=cut(V(g.5)$helpful_total, 10),
  directed=FALSE
)
g.5.cug.assort.help.up <- mycugtest(
  g.5,
  assortativity_nominal,
  types=cut(V(g.5)$helpful_up_total, 10),
  directed=FALSE
)
g.5.qap.assort.help.up <- myqaptest(
  g.5,
  assortativity_nominal,
  types=cut(V(g.5)$helpful_up_total, 10),
  directed=FALSE
)
saveRDS(g.5.cug.assort.help.tot, './_save/rand.help.cug.rds')
saveRDS(g.5.qap.assort.help.tot, './_save/rand.help.qap.rds')
saveRDS(g.5.cug.assort.help.up, './_save/rand.helpup.cug.rds')
saveRDS(g.5.qap.assort.help.up, './_save/rand.helpup.qap.rds')

# ---- rand.help.load ----
g.5.cug.assort.help.tot <- readRDS('./_save/rand.help.cug.rds')
g.5.qap.assort.help.tot <- readRDS('./_save/rand.help.qap.rds')
g.5.cug.assort.help.up <- readRDS('./_save/rand.helpup.cug.rds')
g.5.qap.assort.help.up <- readRDS('./_save/rand.helpup.qap.rds')
print.cug.test(g.5.cug.assort.help.tot)
plot.cug.test(g.5.cug.assort.help.tot)
summary.qaptest(g.5.qap.assort.help.tot)
plot.qaptest(g.5.qap.assort.help.tot)
print.cug.test(g.5.cug.assort.help.up)
plot.cug.test(g.5.cug.assort.help.up)
summary.qaptest(g.5.qap.assort.help.up)
plot.qaptest(g.5.qap.assort.help.up)

# ---- rand.rate.save ----
g.5.cug.assort.rate <- mycugtest(
  g.5,
  assortativity_nominal,
  types=cut(V(g.5)$rating_mean, 10),
  directed=FALSE
)
g.5.qap.assort.rate <- myqaptest(
  g.5,
  assortativity_nominal,
  types=cut(V(g.5)$rating_mean, 10),
  directed=FALSE
)
saveRDS(g.5.cug.assort.rate, './_save/rand.rate.cug.rds')
saveRDS(g.5.qap.assort.rate, './_save/rand.rate.qap.rds')

# ---- rand.rate.load ----
g.5.cug.assort.rate <- readRDS('./_save/rand.rate.cug.rds')
g.5.qap.assort.rate <- readRDS('./_save/rand.rate.qap.rds')
print.cug.test(g.5.cug.assort.rate)
plot.cug.test(g.5.cug.assort.rate)
summary.qaptest(g.5.qap.assort.rate)
plot.qaptest(g.5.qap.assort.rate)

# ---- nothing ----
# betweenness done in python, R is too slow for some reason





