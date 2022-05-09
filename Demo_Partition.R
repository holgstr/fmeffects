require(partykit)
require(rpart)
a = ForwardMarginalEffect$new(model = forest,
                          data = Boston,
                          y = "medv",
                          feature = c("rm", "tax"),
                          step.size = c(1, 100),
                          ep.method = "envelope",
                          nlm.intervals = 1)$compute()
a$results

data = a$predictor$X[a$results$obs.id,]
data.table::set(data, j = "fme", value = a$results$fme)

#data
tree = rpart(fme ~ ., data = data, control = rpart.control(minbucket = round(nrow(data)*0.04), cp= 0.001))
party.tree = as.party(tree)
plot(party.tree)
data_party(party.tree) # in 'fitted' gives id of terminal node
which(party.tree$fitted[,1] == 4)
mean(a$results$fme)
party.tree = nodeprune(party.tree, ids = c(3))
plot(party.tree)
