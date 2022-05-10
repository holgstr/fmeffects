data("birthwt", package = "MASS")
task = as_task_regr(birthwt, id = "birthwt", target = "bwt")
learner = lrn("regr.ranger")$train(task)
a = FME$new(makePredictor(learner, birthwt, "bwt"),
            feature = c("smoke"),
            step.size = 1,
            ep.method = "envelope", # atm envelope is only checked for numerical features
            nlm.intervals = 1)
a$compute()

# tree with exactly 5 partitions
d = PartitioningCtree$new(a, "partitions", 5)$compute()
plot(d$tree)
