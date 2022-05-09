require(partykit)
require(rpart)
a = ForwardMarginalEffect$new(makePredictor(forest, Boston, "medv"),
                          feature = c("rm", "tax"),
                          step.size = c(1, 100),
                          ep.method = "envelope",
                          nlm.intervals = 1)$compute()
a$results

data = a$predictor$X[a$results$obs.id,]
data.table::set(data, j = "fme", value = a$results$fme)
pruned.tree = Pruner$new(tree, method = "partitions", value = 1)$prune()
plot(pruned.tree)
tree = ctree(fme ~ ., data = data, control = ctree_control(alpha = 0.81))
tree = as.party(rpart(fme ~ ., data = data, control = rpart.control(minbucket = round(nrow(data)*0.04), cp= 0.0001)))
data_party(tree) # get data
terminal.nodes = nodeids(tree, terminal = TRUE) # get terminal nodes
nodeids(tree) # get nodes
get_paths(tree, i = terminal.nodes)
pruned = prune(tree, method = "partitions", value = 5)
plot(pruned)
