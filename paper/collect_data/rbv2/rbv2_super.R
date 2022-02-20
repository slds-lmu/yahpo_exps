library("data.table")
library("mlr3misc")

workdir = paste0(path.expand("~"), "/..", "/LRZ Sync+Share/multifidelity_data/")
base_configs = c("rbv2_aknn", "rbv2_glmnet", "rbv2_ranger", "rbv2_rpart", "rbv2_svm", "rbv2_xgboost")


pars = list(
    "svm" = c("cost", "gamma", "tolerance", "shrinking", "kernel", "degree"),
    "ranger" = c("num.trees", "replace", "sample.fraction", "mtry.power", "respect.unordered.factors", "min.node.size", "splitrule"),
    "glmnet" = c("alpha", "s"),
    "xgboost" = c("booster", "nrounds", "eta", "gamma", "lambda", "alpha", "subsample",  "max_depth", "min_child_weight", "colsample_bytree", "colsample_bylevel",  "rate_drop", "skip_drop"),
    "aknn" = c("k", "M", "ef_construction", "ef", "distance"),
    "rpart" =  c("cp","maxdepth","minbucket","minsplit")
)

read_file = function(cfg, nrows) {
    file = paste0(workdir, cfg, "/data.csv")
    dt = fread(file,  stringsAsFactors = TRUE, nrows = nrows)
    dt[, learner_id := factor(gsub("rbv2_", "", cfg))]
    dt = prefix_colnames(dt, cfg)
    return(dt)
}

prefix_colnames = function(dt, cfg) {
    colnames(dt) = map_chr(colnames(dt), function(x) {
        ifelse(x %in% pars[[gsub("rbv2_", "", cfg)]], paste0(gsub("rbv2_", "", cfg), ".", x), x)
    })
    return(dt)
}

sample_max = function(x, n) {
  n = ceiling(n/2)
  idx = which(x$trainsize == 1)
  idx = sample(idx, min(n, length(idx)))
  idx2 = which(x$trainsize < 1)
  idx2 = sample(idx2, min(n, length(idx2)))
  idx = c(idx2, idx)
  x[idx,]
}

# Create an empty data.table with all the right colnames for rbinding
dt = rbindlist(map(base_configs, read_file, nrows = 1L), fill = TRUE, use.names = TRUE)[0L,]

overview = data.table()
out_file = paste0(path.expand("~"), "/..", "/LRZ Sync+Share/multifidelity_data/rbv2_super/data.csv")
# First: collect statistics about the scenarios
for (cfg in base_configs) {
  newdt =  read_file(cfg, Inf)
  overview = rbindlist(list(overview, newdt[, .N, by = "task_id"][, learner := cfg]))
}

# Only use tasks that have more then 1000 Obs on each algorithm
use_task_ids = overview[N  > 1500, ][,.N, by = c("task_id")][N == 6L,]$task_id

for (cfg in base_configs) {
    newdt =  read_file(cfg, Inf)[task_id %in% use_task_ids, ]
    newdt = newdt[, sample_max(.SD, 1e5), by = "task_id"]
    df = rbindlist(list(dt[0L, ], newdt), fill = TRUE, use.names = TRUE)
    fwrite(df, out_file, append=ifelse(cfg==base_configs[1], FALSE, TRUE))
}


dt = fread(out_file, stringsAsFactors = TRUE)
dt[, .N, by= c("task_id", "learner_id")][order(N)]


