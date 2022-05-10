require(partykit)
require(rpart)
a = FME$new(makePredictor(forest, Boston, "medv"),
                          feature = c("rm", "tax"),
                          step.size = c(1, 100),
                          ep.method = "envelope",
                          nlm.intervals = 1)$compute()
a$results

data = a$predictor$X[a$results$obs.id,]
data.table::set(data, j = "fme", value = a$results$fme)

tree1 = ctree(fme ~ ., data = data, control = ctree_control(alpha = 0.81))
tree2 = as.party(rpart(fme ~ ., data = data, control = rpart.control(minbucket = round(nrow(data)*0.04), cp= 0.0001)))

pruned.tree1 = Pruner$new(tree1, method = "partitions", value = 3)$prune()
plot(pruned.tree1)

pruned.tree2 = Pruner$new(tree2, method = "max.cov", value = 0.25)$prune()
plot(pruned.tree2)

#get_paths(tree, i = terminal.nodes)

