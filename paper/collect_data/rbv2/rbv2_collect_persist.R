##########################################################################################################
# Step 3: Collect for re-upload

library(data.table)
library(mlr3misc)
library(future.apply)

basepath = paste0(path.expand("~"), "/results")
mempath = paste0(path.expand("~"), "/results/memory_rbv2")

files = list.files(basepath, full.names = TRUE)
files = files[endsWith(files, ".rds") & !grepl("_prep", files) & grepl("_full", files)]

learner = c("glmnet", "rpart", "ranger", "xgboost", "aknn", "svm")

lrn = learner[5]
infiles = files[grepl(lrn, files)]
cnter = ifelse(length(infiles) > 1, list(paste0("_", seq_along(infiles))), list(""))[[1]]
outfiles = paste0(basepath, "/processed/all_evals_", lrn, cnter, ".rds")


map(seq_along(infiles), function(i) {
  file.copy(infiles[i], outfiles[i])
  unlink(infiles[i])
})
