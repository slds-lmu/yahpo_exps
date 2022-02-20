################################################################################
# This script ingests data from rbv2 and turns it into a single table for use 
# with yahpo gym. 
# - Preprocessing
# - Merge memory data from log files
# - Returns a flat table
################################################################################

library(data.table)
library(mlr3misc)
library(future.apply)
future_map = future_lapply

basepath = paste0(path.expand("~"), "/results")
mempath = paste0(path.expand("~"), "/results/memory_rbv2")
metrics = c("f1", "mmce", "auc", "logloss", "timepredict", "timetrain")
fcts = c("num.impute.selected.cpo", "learner", "task")

memfiles = list.files(mempath, full.names = TRUE)

# Map over chunks of the file
map_chunked = function(all, fct, chunks = 10L, ...) {
  ixs = chunk_vector(seq_along(all), n_chunks = chunks)
  imap(ixs, function(x, i) {
    catf(paste0("On chunk: ", i, "/", chunks, "\n"))
    fct(all, x, i, ...)
  })
  gc()
}


save_rds = function(all, x, i, filename) {
  lstx = future_map(x, function(xs) {
    lst = all[[xs]]
    x = data.table(lst$performances)
    
    # Get rid of failed runs
    nas = apply(x[, intersect(metrics, colnames(x)), with = FALSE], 1, function(x) mean(is.na(x)))
    x = x[nas < 0.5,]
    if (all(is.na(x$trainsize))) return(NULL)
    
    # Convert columns
    x[, trainsize := round(trainsize / max(trainsize, na.rm = TRUE),2)]
    x[, repl := as.factor(seq_along(iter)), by = trainsize]
    x[, predictsize := NULL]
    x[, iter := NULL]
    x[, (names(lst$METADATA)) := lst$METADATA]
    x = cbind(x, rbindlist(map(x$point, function(xp) eval(parse(text = xp)))))
    x[, point := NULL]
    x[, SUPEREVAL := NULL]
    fcts = intersect(fcts, colnames(x))
    x[, (fcts) := map(.SD, as.factor), .SDcols = fcts]
    x[,c("task_id", "dataset") := split_task_col(task)]
    x[,task := NULL]
    return(x)
  })
  dt = rbindlist(lstx, fill = TRUE)
  fileext = paste0("_", i, "_prep.rds")
  file = gsub(".rds", fileext, filename)
  catf(paste0("Saving to ", file, "/n"))
  saveRDS(dt, file)
}

split_task_col = function(x) {
  splits = strsplit(levels(x), split = ".", fixed = TRUE)
  dt = rbindlist(map(splits, function(x) {
    
    data.table(factor(x[length(x)]), factor(paste0(x[-length(x)], collapse=".")))
  }))
  dt[as.integer(x),]
}

sample_max = function(x, n) {
  idx = which(x$trainsize == 1)
  idx = sample(idx, min(n, length(idx)))
  idx = sort(c(which(x$trainsize < 1), idx))
  x[idx,]
}


##########################################################################################################
# Step 1: Convert to chunked .rds files
# Chunking is required since memory overhead is massive.
if (FALSE) {
  # First we iterate over all files to save the prepared data in chunks
  # (This is done to avoid memory problems)
  files = list.files(basepath, full.names = TRUE)
  files = files[endsWith(files, ".rds") & !grepl("_prep", files) &!grepl("_full", files)]
  files = files[grepl("normal", files)]
  for (j in c(1:10)) {
    j = 10
    filename = files[[j]]
    # lsti = readRDS(filename)
    if (j == 10) {
      xgb_files = c()
      xs = chunk_vector(seq_len(length(lsti)), 3, shuffle = FALSE)
      for (i in seq_len(3)) {
        file = gsub(".rds", paste0("_",i, ".rds"), filename)
        xgb_files = c(xgb_files, file)
        saveRDS(lsti[xs[[i]]], file)
      }
      # plan("multisession", workers = 3L)
      for (i in seq_len(3)) {
        file = gsub(".rds", paste0("_",i, ".rds"), filename)
        lsti = readRDS(file)
        map_chunked(lsti, save_rds, chunks = ifelse(j != 10, 2, 4), file)
        rm(lsti)
        gc()
      }
    } else {
      map_chunked(lsti, save_rds, chunks = ifelse(j != 10, 2, 16))
    }

  }
}

##########################################################################################################
# Step 2: Export collected data.
# Saves the full data (for future use) as well as a .csv that is used to fit surrogates

files = list.files(basepath, full.names = TRUE)
files = files[endsWith(files, "_prep.rds")]

# Minimum number of observations per task
min_n = 1000L

# What do we keep for csv export?
metrics = c("timetrain", "timepredict", "acc", "bac", "auc", "multiclass.aunp", "brier", "multiclass.brier", "f1", "logloss", "M")
cols = c("dataset", "task_id", "trainsize","repl", "seed")
csv_path = "~/../LRZ Sync+Share/multifidelity_data/"


##########################################################################################################
# glmnet
learner = 'glmnet'
this_file = files[grepl(paste0(learner, "_\\d*_prep"), files)]
memfile = memfiles[grepl(learner, memfiles)]
dt = rbindlist(map(this_file, readRDS), use.names=TRUE, fill = TRUE)
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)]
dt = merge(dt, mems, by.x =c("seed", "dataset", "task_id", "learner"), by.y = c("seed", "task", "data_id", "learner_id"), all.x = TRUE)
# Save full data
saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", "_\\1_full", this_file[1]))

# Save csv for learning
pars = c("alpha", "s", "num.impute.selected.cpo")
dt[dt[, .N, by = "task_id"][N > min_n,], on = "task_id"][, N := NULL]
dt = dt = dt[, sample_max(.SD, 10^5), by=task_id]
dt = dt[as.integer(repl) %in% 1:10, c(cols, pars, metrics), with=FALSE]
dt[, memory := M/1024][, M := NULL]
dt[, brier := ifelse(is.na(brier), multiclass.brier, brier)][, multiclass.brier := NULL]
dt[, auc := ifelse(is.na(auc ), multiclass.aunp, auc)][, multiclass.aunp := NULL]
dt[, task_id := as.character(task_id)]
fwrite(dt, paste0(csv_path, "rbv2_", learner, "/data.csv"))


##########################################################################################################
# ranger
learner = "ranger"
this_file = files[grepl("ranger.pow_\\d*_prep", files)]
for (i in 1:2) {
  dt = rbindlist(map(chunk_vector(this_file, 2)[[i]], readRDS), use.names = TRUE)[, learner := "classif.ranger"]
  memfile = memfiles[grepl("ranger", memfiles)]
  mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)]
  dt = merge(dt, mems, by.x =c("seed", "dataset", "task_id", "learner"), by.y = c("seed", "task", "data_id", "learner_id"), all.x = TRUE)
  # saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", paste0("_\\1_full_", i), this_file[1]))

  # Save csv
  pars = c("num.trees", "replace", "sample.fraction", "mtry.power", "num.random.splits", "respect.unordered.factors", "min.node.size", "splitrule", "num.impute.selected.cpo")
  dt = dt[as.integer(repl) %in% 1:10, c(cols, pars, metrics), with=FALSE]
  dt[, memory := M/1024][, M := NULL]
  dt[, brier := ifelse(is.na(brier), multiclass.brier, brier)][, multiclass.brier := NULL]
  dt[, auc := ifelse(is.na(auc ), multiclass.aunp, auc)][, multiclass.aunp := NULL]
  dt[, num.trees := as.integer(num.trees)][, min.node.size := as.integer(min.node.size)]
  out = paste0(csv_path, "rbv2_", learner, "/data.csv")
  fwrite(dt, out, append = ifelse(i > 1, TRUE, FALSE))
}

dt = fread(out)
dt = dt[dt[, .N, by = "task_id"][N > min_n,], on = "task_id"][, N := NULL]
dt = dt[, sample_max(.SD, 1.5*10^5), by=task_id]
fwrite(dt, out)

##########################################################################################################
# svm
learner = 'svm'
n_chunks = 4
this_file = files[grepl("svm.*_\\d*_prep", files)]
memfile = memfiles[grepl("svm", memfiles)]
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)][, learner_id := factor(ifelse(setting == "all", "classif.svm", "classif.svm.radial"))]
out_tmp = paste0(csv_path, "rbv2_", learner, "/data_tmp.csv")

for (i in seq_len(n_chunks)) {
  dt = rbindlist(map(chunk_vector(this_file, n_chunks)[[i]], readRDS), use.names = TRUE, fill = TRUE)# [, learner := "classif.svm"]
  dt[, fitted := NULL]
  dt[, kernel := ifelse(is.na(kernel), "radial", kernel)]
  dt[, shrinking := as.logical(shrinking)]
  dt[, kernel := as.factor(kernel)]
  dt = merge(dt, unique(mems), by.x =c("seed", "dataset", "task_id", "learner"), by.y = c("seed", "task", "data_id", "learner_id"), all.x = TRUE, all.y = FALSE)
  saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", paste0("_\\1_full_", i), this_file[1]))
  
  # Save csv
  pars = c("cost", "gamma", "tolerance", "shrinking", "kernel", "degree", "num.impute.selected.cpo")
  dt = dt[as.integer(repl) %in% 1:10, c(cols, pars, metrics), with=FALSE]
  dt[, memory := M/1024][, M := NULL]
  dt[, dataset := factor(dataset)]
  dt[, task_id := factor(task_id)]
  dt[, kernel := factor(kernel)]
  dt[, num.impute.selected.cpo := factor(num.impute.selected.cpo)]
  dt[, brier := ifelse(is.na(brier), multiclass.brier, brier)][, multiclass.brier := NULL]
  dt[, auc := ifelse(is.na(auc ), multiclass.aunp, auc)][, multiclass.aunp := NULL]
  dt[, degree := as.integer(degree)]
  fwrite(dt, out_tmp, append = ifelse(i > 1, TRUE, FALSE))
  gc()
}


# We go over it in chunks to avoid out-of-memory problems
dt = fread(out_tmp, stringsAsFactors = TRUE, select = "task_id")
n_chunks = 2
chks = chunk_vector(unique(dt$task_id), n_chunks)
out = paste0(csv_path, "rbv2_", learner, "/data.csv")
for (i in seq_len(n_chunks)) {
  dt = fread(out_tmp,  stringsAsFactors = TRUE, index = "task_id")
  dt = dt[task_id %in% chks[[i]],]
  dt = dt[, sample_max(.SD, 1.5*10^5), by=task_id]
  dt = dt[dt[, .N, by = "task_id"][N > min_n,], on = "task_id"][, N := NULL]
  fwrite(dt, out, append = ifelse(i > 1, TRUE, FALSE))
}
unlink(out_tmp)


  

##########################################################################################################
# xgboost
learner = "xgboost"
n_chunks = 12
this_file = files[grepl("xgboost.*_\\d*_prep", files)]
memfile = memfiles[grepl("xgboost", memfiles)]
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)]
mems[setting == "gbtree", learner_id := "classif.xgboost.gbtree"]
mems[setting == "gblinear", learner_id := "classif.xgboost.gblinear"]
mems[setting == "dart", learner_id := "classif.xgboost.dart"]
mems[, learner_id := factor(learner_id)]
out_tmp = paste0(csv_path, "rbv2_", learner, "/data.csv")

for (i in seq_len(n_chunks)) {
  dt = rbindlist(map(chunk_vector(this_file, n_chunks, shuffle = FALSE)[[i]], readRDS), use.names = TRUE, fill = TRUE)
  dt[, nthread := NULL]
  dt = merge(dt, unique(mems), 
             by.x =c("seed", "dataset", "task_id", "learner"), 
             by.y = c("seed", "task", "data_id", "learner_id"),
             all.x = TRUE, all.y = FALSE
  )
  dt[, learner := "classif.xgboost"][, setting := NULL]
  dt[, booster := as.factor(booster)]
  saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", paste0("_\\1_full_", i), this_file[1]))
}

header = fread(out_tmp, nrows = 0, stringsAsFactors = TRUE)
for (i in seq_len(n_chunks)) {
  dt = readRDS(gsub("_all_classif.(.*)_\\d_prep", paste0("_\\1_full_", i), this_file[1]))
  # Save csv
  pars = c("booster", "nrounds", "eta", "gamma", "lambda", "alpha", "subsample",  "max_depth", "min_child_weight", "colsample_bytree", "colsample_bylevel",  "rate_drop", "skip_drop", "num.impute.selected.cpo")
  pars = intersect(pars, colnames(dt))
  dt = dt[as.integer(repl) %in% 1:10, c(cols, pars, metrics), with=FALSE]
  dt[, memory := M/1024][, M := NULL]
  dt[, brier := ifelse(is.na(brier), multiclass.brier, brier)][, multiclass.brier := NULL]
  dt[, auc := ifelse(is.na(auc ), multiclass.aunp, auc)][, multiclass.aunp := NULL]
  # First filtering
  dt = dt[, sample_max(.SD, 2*10^5), by=task_id]
  dt = rbindlist(list(header, dt), use.names = TRUE, fill = TRUE)
  fwrite(dt, out_tmp, append = ifelse(i > 1, TRUE, FALSE))
  gc()
}

# We go over it in chunks to avoid out-of-memory problems
dt = fread(out_tmp, stringsAsFactors = TRUE, select = "task_id", skip = Inf)
n_chunks = 2
chks = chunk_vector(unique(dt$task_id), n_chunks)
out = paste0(csv_path, "rbv2_", learner, "/data.csv")
for (i in seq_len(n_chunks)) {
  dt = fread(out_tmp,  stringsAsFactors = TRUE, index = "task_id")
  dt = dt[task_id %in% chks[[i]],]
  dt = dt[, sample_max(.SD, 1.5*10^5), by=task_id]
  dt = dt[dt[, .N, by = "task_id"][N > min_n,], on = "task_id"][, N := NULL]
  fwrite(dt, out, append = ifelse(i > 1, TRUE, FALSE))
}

unlink(out_tmp)

dt = fread(out)
dt = dt[, -c(1,2)]
fwrite(dt, out)

##########################################################################################################
# aknn
learner = 'aknn'
this_file = files[grepl("HNSW.*_\\d*_prep", files)]
dt = rbindlist(map(this_file, readRDS), use.names = TRUE, fill = TRUE)
dt[, distance := as.factor(distance)][, RcppHNSW.M := M][, M:= NULL]
dt[, learner := as.factor(learner)]
memfile = memfiles[grepl("Rcpp", memfiles)]
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)]
dt = merge(dt, unique(mems), by.x =c("seed", "dataset", "task_id", "learner"), by.y = c("seed", "task", "data_id", "learner_id"), all.x = TRUE, all.y = FALSE)
saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", "_\\1_full", this_file[1]))

# Save csv
pars = c("k", "RcppHNSW.M", "ef_construction", "ef", "distance", "num.impute.selected.cpo")
dt = dt[as.integer(repl) %in% 1:10, c(cols, pars, metrics), with=FALSE]
dt = dt[dt[, .N, by = "task_id"][N > min_n,], on = "task_id"][, N := NULL]
dt = dt[, sample_max(.SD, 1.5*10^5), by=task_id]
dt[, memory := M/1024][, M := NULL]
dt[, M := RcppHNSW.M][, RcppHNSW.M := NULL]
dt[, brier := ifelse(is.na(brier), multiclass.brier, brier)][, multiclass.brier := NULL]
dt[, auc := ifelse(is.na(auc ), multiclass.aunp, auc)][, multiclass.aunp := NULL]
dt[, ef := as.integer(ef)][, ef_construction := as.integer(ef_construction)][, M := as.integer(M)]
fwrite(dt, paste0(csv_path, "rbv2_", learner, "/data.csv"))


##########################################################################################################
# rpart
learner = "rpart"
this_file = files[grepl("rpart.*_\\d*_prep", files)]
dt = rbindlist(map(this_file, readRDS), use.names = TRUE, fill = TRUE)
memfile = memfiles[grepl("rpart", memfiles)]
mems = fread(memfile)[, V1 := NULL][, data_id := as.factor(data_id)]
dt = merge(dt, unique(mems), by.x =c("seed", "dataset", "task_id", "learner"), by.y = c("seed", "task", "data_id", "learner_id"), all.x = TRUE, all.y = FALSE)
# Save full data
saveRDS(dt, gsub("_all_classif.(.*)_\\d_prep", "_\\1_full", this_file[1]))

# Save csv for learning
pars = c("cp","maxdepth","minbucket","minsplit","num.impute.selected.cpo")
dt = dt[as.integer(repl) %in% 1:10, c(cols, pars, metrics), with=FALSE]
dt = dt[dt[, .N, by = "task_id"][N > min_n,], on = "task_id"][, N := NULL]
dt = dt[, sample_max(.SD, 1.5*10^5), by=task_id]
dt[, memory := M/1024][, M := NULL]
dt[, brier := ifelse(is.na(brier), multiclass.brier, brier)][, multiclass.brier := NULL]
dt[, auc := ifelse(is.na(auc ), multiclass.aunp, auc)][, multiclass.aunp := NULL]
dt[, maxdepth  := as.integer(maxdepth)][, minbucket := as.integer(minbucket)][, minsplit := as.integer(minsplit)]
fwrite(dt, paste0(csv_path, "rbv2_", learner, "/data.csv"))

