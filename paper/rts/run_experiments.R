library(batchtools)
library(lgr)
library(mlr3misc)
library(data.table)

root = here::here()
source(file.path(root, "real_tabular_surrogate_helpers.R"))

#reg = makeExperimentRegistry(file.dir = "/dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/ru84tad2/registry_rts", source = file.path(root, "real_tabular_surrogate_helpers.R"))
reg = makeExperimentRegistry(file.dir = NA, source = file.path(root, "real_tabular_surrogate_helpers.R"))  # interactive session
saveRegistry(reg)

eval_ = function(job, data, instance, ...) {
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  root = here::here()
  workdir = "multifidelity_data/"

  xs = list(...)
  optimizer = xs$optimizer

  if (optimizer == "bo_full") {
    surrogate = xs$surrogate
    acqopt = xs$acqopt
    if (surrogate == "gp") {
      learner = lrn("regr.km", covtype = "matern3_2", optim.method = "gen", nugget.stability = 10^-8, control = list(trace = FALSE))
      surrogate = SurrogateSingleCritLearner$new(learner)
    } else if (surrogate == "rf") {
      learner = lrn("regr.ranger", se.method = "jack", num.trees = 500L, keep.inbag = TRUE)
      surrogate = SurrogateSingleCritLearner$new(learner)
    } else if (surrogate == "nn") {
      learner = LearnerRegrBananasNN$new()
      surrogate = SurrogateSingleCritLearner$new(learner)
    }

    if (acqopt == "default") {
      if (instance$method != "tabular") {
        acqopt = AcqOptimizer$new(opt("nloptr", algorithm = "NLOPT_LN_NELDERMEAD"), trm("none"))  # FIXME: or "NLOPT_LN_BOBYQA"
      }
    } else if (acqopt == "alt") {
      if (instance$method != "tabular") {
        acqopt = AcqOptimizer$new(opt("random_search", batch_size = 1000L), trm("evals", n_evals = 10000L))
      }
    }
  } else {
    surrogate = acqopt = NULL
  }

  start_t = Sys.time()
  res = do_optimize(id = as.character(instance$id), method = as.character(instance$method), optimizer = optimizer, surrogate = surrogate, acqopt = acqopt)
  end_t = Sys.time()

  list(archive = res, runtime = end_t - start_t)
}
addAlgorithm("eval_", fun = eval_)

instances_plan = setDT(expand.grid(id = c("branin", "currin", "hartmann3d", "hartmann6d", "borehole", "rpartphoneme", "glmnetphoneme", "rpartbtc", "glmnetbtc"), method = c("real", "tabular", "surrogate")))
instances_plan[, id_plan := 1:.N]

prob_designs = imap(split(instances_plan, instances_plan$id_plan), function(instancex, name) {
  prob_id = paste0(instancex$id, "_", instancex$method)
  addProblem(prob_id, fun = function(...) list(...), seed = 123)
  set_names(list(instancex), prob_id)
})
nn = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = nn

ids = addExperiments(
  prob.designs = prob_designs,
  algo.designs = list(eval_ = data.table(optimizer = "rs_full")),
  repls = 30L
)
addJobTags(ids, "rs_full")

ids = addExperiments(
  prob.designs = prob_designs,
  algo.designs = list(eval_ = data.table(optimizer = "bo_full", surrogate = "gp", acqopt = "default")),
  repls = 30L
)
addJobTags(ids, c("bo_full", "gp", "default"))

ids = addExperiments(
  prob.designs = prob_designs,
  algo.designs = list(eval_ = data.table(optimizer = "bo_full", surrogate = "gp", acqopt = "alt")),
  repls = 30L
)
addJobTags(ids, c("bo_full", "gp", "alt"))

ids = addExperiments(
  prob.designs = prob_designs,
  algo.designs = list(eval_ = data.table(optimizer = "bo_full", surrogate = "rf", acqopt = "default")),
  repls = 30L
)
addJobTags(ids, c("bo_full", "rf", "default"))

ids = addExperiments(
  prob.designs = prob_designs,
  algo.designs = list(eval_ = data.table(optimizer = "bo_full", surrogate = "rf", acqopt = "alt")),
  repls = 30L
)
addJobTags(ids, c("bo_full", "rf", "alt"))

ids = addExperiments(
  prob.designs = prob_designs,
  algo.designs = list(eval_ = data.table(optimizer = "bo_full", surrogate = "nn", acqopt = "default")),
  repls = 30L
)
addJobTags(ids, c("bo_full", "nn", "default"))

ids = addExperiments(
  prob.designs = prob_designs,
  algo.designs = list(eval_ = data.table(optimizer = "bo_full", surrogate = "nn", acqopt = "alt")),
  repls = 30L
)
addJobTags(ids, c("bo_full", "nn", "alt"))

ids = addExperiments(
  prob.designs = prob_designs,
  algo.designs = list(eval_ = data.table(optimizer = "hb")),
  repls = 30L
)
addJobTags(ids, "hb")

# standard resources used to submit jobs to cluster
resources.serial.default = list(
  walltime = 3600L * 24L * 1L, memory = 1024L * 2L, clusters = "serial", max.concurrent.jobs = 100L
)

all_jobs = findJobs()
all_jobs[, chunk := batchtools::chunk(job.id, chunk.size = 100L)]  # ceiling(NROW(all_jobs) / 100L)
submitJobs(all_jobs, resources = resources.serial.default)


plan(multicore, workers = 4)
tags = batchtools::getUsedJobTags()
tab = getJobTable()

instances = map_chr(tab$prob.pars, function(x) as.character(x$id))
methods = map_chr(tab$prob.pars, function(x) as.character(x$method))

not_done = findNotDone()
error_info = map(findErrors()$job.id, function(j) {
  tmp = makeJob(j)
  list(id = tmp$instance$id, method = tmp$instance$method, optimizer = tmp$algo.pars$optimizer, surrogate = tmp$algo.pars$surrogate, acqopt = tmp$algo.pars$acqopt)
})

synthetics = c("branin", "currin", "hartmann3d", "hartmann6d", "borehole")

# traces
map(synthetics, function(instance) {
  results = map_dtr(unique(methods), function(method) {
    jobs = tab[which(instances == instance & methods == method)]
    tmp = rbindlist(reduceResultsDataTable(fun = function(x, job) {
      archive = x$archive
      optimizer = job$algo.pars$optimizer
      if (optimizer == "bo_full") {
        optimizer = paste0(job$algo.pars$optimizer, "_", job$algo.pars$surrogate, "_", job$algo.pars$acqopt)
      }
      archive$optimizer = optimizer
      archive[, repl := job$repl]
      archive[, id := job$id]
      archive
    }, ids = jobs)$result, fill = TRUE)
    tmp[, normalized_regret := (best - min(best)) / diff(range(best)), by = .(repl)]
    agg = setNames(tmp[, mean(normalized_regret), by = .(cumbudget, iteration, method, optimizer, config)], c("cumbudget", "iteration", "method", "optimizer", "config", "mean_normalized_regret"))
    agg$sd_normalized_regret = tmp[, sd(normalized_regret), by = .(cumbudget, iteration, method, optimizer, config)]$V1
    agg$n_repl = tmp[, length(normalized_regret), by = .(cumbudget, iteration, method, optimizer, config)]$V1
    agg$mean_best = tmp[, mean(best), by = .(cumbudget, iteration, method, optimizer, config)]$V1
    agg$sd_best = tmp[, sd(best), by = .(cumbudget, iteration, method, optimizer, config)]$V1
    agg$se_normalized_regret = agg$sd_normalized_regret / sqrt(agg$n_repl)
    agg$se_best= agg$sd_best / sqrt(agg$n_repl)
    agg
  }, .fill = TRUE)
  saveRDS(results, paste0("results/rts_results_", instance, ".rds"))
})

# ranks
map(synthetics, function(instance) {
  results = map_dtr(unique(methods), function(method) {
    jobs = tab[which(instances == instance & methods == method)]
    tmp = rbindlist(reduceResultsDataTable(fun = function(x, job) {
      archive = x$archive
      optimizer = job$algo.pars$optimizer
      if (optimizer == "bo_full") {
        optimizer = paste0(job$algo.pars$optimizer, "_", job$algo.pars$surrogate, "_", job$algo.pars$acqopt)
      }
      archive$optimizer = optimizer
      archive[, repl := job$repl]
      archive[, id := job$id]
      archive
    }, ids = jobs)$result, fill = TRUE)
    ranks = map_dtr(unique(tmp$repl), function(r) {
       map_dtr(1:100, function(cb) {  # FIXME: 1:100 cumbudget fixed here
         archive = tmp[repl == r & cumbudget <= cb]
         archive[, row_id := seq_len(.N)]
         hb_to_drop = archive[optimizer == "hb" & cumbudget != max(archive[optimizer == "hb", cumbudget]), row_id]
         others_to_drop = archive[optimizer %nin% "hb" & cumbudget <= (cb - 1), row_id]
         archive = archive[- c(hb_to_drop, others_to_drop)]
         archive$rank = match(archive$best, sort(archive$best))
         archive$cbx = cb
         archive
       })
    })
    agg = setNames(ranks[, mean(rank), by = .(cbx, method, optimizer, config)], c("cbx", "method", "optimizer", "config", "mean_rank"))
    agg$sd_rank = ranks[, sd(rank), by = .(cbx, method, optimizer, config)]$V1
    agg$n_repl = ranks[, length(rank), by = .(cbx, method, optimizer, config)]$V1
    agg$se_rank = agg$sd_rank / sqrt(agg$n_repl)
    agg
  }, .fill = TRUE)
  saveRDS(results, paste0("results/rts_results_rank_", instance, ".rds"))
})

# consensus
consensus = map(unique(methods), function(method) {
  all_ranks = map(synthetics, function(instance) {
    jobs = tab[which(instances == instance & methods == method)]
    tmp = rbindlist(reduceResultsDataTable(fun = function(x, job) {
      archive = x$archive[iteration == max(iteration)]
      optimizer = job$algo.pars$optimizer
      if (optimizer == "bo_full") {
        optimizer = paste0(job$algo.pars$optimizer, "_", job$algo.pars$surrogate, "_", job$algo.pars$acqopt)
      }
      archive$optimizer = optimizer
      archive[, repl := job$repl]
      archive[, id := job$id]
      archive
    }, ids = jobs)$result, fill = TRUE)
    ranks = map_dtr(unique(tmp$repl), function(r) {
       archive = tmp[repl == r]
       archive$rank = match(archive$best, sort(archive$best))
       rel = archive[, c("optimizer", "rank", "repl")]
       setkeyv(rel, "rank")
    })
    agg = setNames(ranks[, mean(rank), by = .(optimizer)], c("optimizer", "mean_rank"))
    ranking(agg$optimizer, domain = agg$optimizer, decreasing = TRUE, complete = TRUE)
  })
  all_ranks = relation_ensemble(list = all_ranks)
  consensus = relation_consensus(all_ranks, method = "SD/L")
  ids = tryCatch(relation_class_ids(consensus), error = function(ec) NA_integer_)
})
names(consensus) = unique(methods)
saveRDS(consensus, "results/rts_results_consensus.rds")

tmp = map(consensus, function(x) data.table(rank = x, optimizer = names(x)))
setkeyv(tmp$real, "rank")
optimizers = tmp$real$optimizer
setkeyv(tmp$tabular, "rank")
setkeyv(tmp$surrogate, "rank")
ranking = map_chr(tmp, function(x) paste0(x$rank[match(x$optimizer, optimizers)], collapse = ""))

