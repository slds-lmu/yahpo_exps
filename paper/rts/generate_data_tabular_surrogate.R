library(mfsurrogates)
library(bbotk)
library(paradox)
library(data.table)
library(mlr3)
library(mlr3misc)
library(mlr3oml)
library(future)
library(future.apply)

plan("multisession", workers = 2L)

# helper to set up grid
get_data = function(cfg, n = 10^6) {
  resolution = floor((n / 10L) ^ (1 / (length(cfg$param_set$params) - 1L)))  # / 10L due to fidelity
  if (resolution == 0) resolution = 1
  design = map(cfg$param_set$params, function(param) {
    tmp = if (param$id == "fidelity") {
      1 / (2 ^ (0:9))  # due to hyperband eta = 2
    } else {
      generate_design_grid(ParamSet$new(list(param)), resolution = resolution)$data[[1L]]
    }
    tmp
  })

  design = as.data.table(expand.grid(design))
  ins = OptimInstanceSingleCrit$new(
    objective = cfg$get_objective(),
    terminator = trm("none")
  )

  batch_size = 1000L
  lower = 1
  upper = lower + batch_size - 1
  while(upper <= nrow(design)) {
    ins$eval_batch(design[lower:upper, ])
    if (upper == nrow(design)) {
      break
    }
    lower = upper + 1
    upper = lower + batch_size - 1
    if (lower > nrow(design)) lower = nrow(design)
    if (upper > nrow(design)) upper = nrow(design)
  }
  ins$archive$data[, c(cfg$param_set$ids(), cfg$codomain$ids())]
}

# get data for tabular and surrogate
future_lapply(c("branin", "currin", "hartmann3d", "hartmann6d", "borehole", "rpartphoneme", "rpartbtc", "glmnetphoneme", "glmnetbtc"), function(cfg_id) {
  set.seed(123)
  cfg = cfgs(cfg_id)
  data = get_data(cfg)
  saveRDS(data, paste0("../multifidelity_data/", paste0(cfg_id, "_surrogate/data.rds")))
}, future.packages = c("mlr3", "mlr3misc", "mlr3oml"))

# fit the surrogate model
lapply(c("branin", "currin", "hartmann3d", "hartmann6d", "borehole", "rpartphoneme", "rpartbtc", "glmnetphoneme", "glmnetbtc"), function(cfg_id) {
  cfg_id = paste0(cfg_id, "_surrogate")
  set.seed(123)
  cfg = cfgs(cfg_id, workdir = "../multifidelity_data")
  cfg$fit_surrogate(overwrite = TRUE, plot = TRUE)
})

# hartmann6d_surrogate_x in 1000 steps
lapply(c(100, 300, 500, 800, 1000, 3000, 5000, 8000, 10000, 50000, 100000), function(size) {
  path = paste0("../multifidelity_data/", "hartmann6d_surrogate_", gsub("\\.", "_", as.character(size / 1000)), "/")
  data = readRDS("../multifidelity_data/hartmann6d_surrogate/data.rds")
  set.seed(123)
  data = data[sample(nrow(data), size = size), ]
  saveRDS(data, paste0(path, "data.rds"))
  set.seed(123)
  cfg = cfgs("hartmann6d_surrogate", workdir = "../multifidelity_data")
  cfg$subdir = path
  cfg$workdir = NULL
  cfg$fit_surrogate(overwrite = TRUE, plot = TRUE)  # used 0.2 train/valid split and early stopping after 50
})

# glmnetphoneme_surrogate_x in 1000 steps
lapply(c(30, 50, 80, 100, 300, 500, 800, 1000, 3000, 5000, 8000, 10000, 50000, 100000), function(size) {
  path = paste0("../multifidelity_data/", "glmnetphoneme_surrogate_", gsub("\\.", "_", as.character(size / 1000)), "/")
  data = readRDS("../multifidelity_data/glmnetphoneme_surrogate/data.rds")
  set.seed(123)
  data = data[sample(nrow(data), size = size), ]
  saveRDS(data, paste0(path, "data.rds"))
  set.seed(123)
  cfg = cfgs("glmnetphoneme_surrogate", workdir = "../multifidelity_data")
  cfg$subdir = path
  cfg$workdir = NULL
  cfg$fit_surrogate(overwrite = TRUE, plot = TRUE)  # used 0.2 train/valid split and early stopping after 50
})

# FIXME: manual for nb301
get_measures = function(cfg_id) {
  set.seed(123)
  cfg = cfgs(cfg_id, workdir = "../multifidelity_data")
  budget_param = cfg$budget_param[1L]
  objective = cfg$get_objective()
  data = cfg$data
  to_retrafo = names(data$trafos)
  test = cbind(data$xtest, data$ytest)
  test[, (to_retrafo) := pmap(list(.SD, data$trafos), function(x, t) {t$retrafo(x)}), .SDcols = to_retrafo]
  budgets = quantile(unique(test[[budget_param]]))[2:5]  # 25, 50, 75, 100
  map_dtr(cfg$codomain$ids(), function(target) {
    #map_dtr(cfg$task_levels, function(task) {
      map_dtr(budgets, function(budget) {
        tmp = test[get(budget_param) <= budget]
        #tmp = test[get(cfg$task_col) == task & get(budget_param) <= budget]
        if (nrow(tmp) == 0L) {
          return(data.table(target = target, budget = budget, rsq = NA, srho = NA, ktau = NA))
          #return(data.table(target = target, task = task, budget = budget, rsq = NA, srho = NA, ktau = NA))
        }          
        set.seed(123)
        tmp = tmp[sample(nrow(tmp), size = min(c(nrow(tmp), 10000L)), replace = FALSE)]
        y = tmp[[target]]
        x = tmp[, cfg$param_set$ids(), with = FALSE]
        y_ = objective$eval_dt(x)[[target]]
        data.table(target = target, budget = budget, rsq = mlr3measures::rsq(truth = y, response = y_), srho = mlr3measures::srho(truth = y, response = y_), ktau = mlr3measures::ktau(truth = y, response = y_))
        #data.table(target = target, task = task, budget = budget, rsq = mlr3measures::rsq(truth = y, response = y_), srho = mlr3measures::srho(truth = y, response = y_), ktau = mlr3measures::ktau(truth = y, response = y_))
      })
    })
  #})
}

get_measures_agg = function(cfg_id) {
  set.seed(123)
  cfg = cfgs(cfg_id, workdir = "../multifidelity_data")
  budget_param = cfg$budget_param[1L]
  objective = cfg$get_objective()
  data = cfg$data
  to_retrafo = names(data$trafos)
  test = cbind(data$xtest, data$ytest)
  test[, (to_retrafo) := pmap(list(.SD, data$trafos), function(x, t) {t$retrafo(x)}), .SDcols = to_retrafo]
  map_dtr(cfg$codomain$ids(), function(target) {
    #map_dtr(cfg$task_levels, function(task) {
      tmp = test
      #tmp = test[get(cfg$task_col) == task]
      if (nrow(tmp) == 0L) {
        return(data.table(target = target, rsq = NA, srho = NA, ktau = NA))
        #return(data.table(target = target, task = task, rsq = NA, srho = NA, ktau = NA))
      }          
      set.seed(123)
      tmp = tmp[sample(nrow(tmp), size = min(c(nrow(tmp), 10000L)), replace = FALSE)]
      y = tmp[[target]]
      x = tmp[, cfg$param_set$ids(), with = FALSE]
      y_ = objective$eval_dt(x)[[target]]
      data.table(target = target, rsq = mlr3measures::rsq(truth = y, response = y_), srho = mlr3measures::srho(truth = y, response = y_), ktau = mlr3measures::ktau(truth = y, response = y_))
      #data.table(target = target, task = task, rsq = mlr3measures::rsq(truth = y, response = y_), srho = mlr3measures::srho(truth = y, response = y_), ktau = mlr3measures::ktau(truth = y, response = y_))
    })
  #})
}

measures = map(c("rbv2_super", "rbv2_svm", "rbv2_rpart", "rbv2_aknn", "rbv2_glmnet", "rbv2_ranger", "rbv2_xgboost", "nb301", "lcbench"), function(cfg_id) {
  get_measures(cfg_id)
}) 

names(measures) = c("rbv2_super", "rbv2_svm", "rbv2_rpart", "rbv2_aknn", "rbv2_glmnet", "rbv2_ranger", "rbv2_xgboost", "nb301", "lcbench")
saveRDS(measures, "/home/user/measures.rds")

measures = map(c("branin_surrogate", "currin_surrogate", "hartmann3d_surrogate", "hartmann6d_surrogate", "borehole_surrogate"), function(cfg_id) {
  get_measures_agg(cfg_id)
}) 

names(measures) = c("branin_surrogate", "currin_surrogate", "hartmann3d_surrogate", "hartmann6d_surrogate", "borehole_surrogate")
saveRDS(measures, "/home/user/measures_synthetic.rds")
