This directory contains all code to replicate all results, tables and figures of the single-objective and multi-objective benchmarks.

* `plots/` contains all plots presented in the paper
* `results/` contains raw benchmark results saved as `.rds` files
* `run_mf.R` and `run_mo.R` are the main files
* `analyze_mf.R` and `analyze_mo.R` contain code needed to analyze the results and create plots and tables
* `dehb_wrapper.py`, `hb_bohb_wrapper.py`, `helpers.R`, `optuna_wrapper.py`, `random_wrapper.py`, and `smac_wrapper.py`are helper files that contain code needed by the main files
* `registry_yahpo_mf_clean.zip` and `registry_yahpo_mo_clean.zip` are the original batchtools registries as used on the cluster (stripped of the individual results which are collected in `results/` and stripped of the logs to save some space); see `run_mf.R` or `run_mo.R` for an example on how to inspect these registries
* `renv.lock` lists the exact R packages that were used on the cluster 
* `requirements.txt` lists the exact python modules that were used on the cluster

R 4.0.5 and python 3.8.7 were used.

To replicate our results, install R and do the following steps (we assume that you work in a terminal and are in the current directory):

Then, within R (we assume that you managed to install R 4.0.5 or use a docker environment):

```r
install.packages("renv")  # 0.15.2
renv::restore(lockfile = "renv.lock")
```

Close the R session, followed by setting up python and a virtualenv (we use pyenv and pipenv here):

```bash
CONFIGURE_OPTS=--enable-shared pyenv install 3.8.7
pipenv --python 3.8.7
pipenv shell
pip install -r requirements.txt
```

Obtain data required for YAHPO GYM:

```bash
git clone https://github.com/slds-lmu/yahpo_data
```

And setup YAHPO GYM (within the virtualenv):

```python
from yahpo_gym import local_config
local_config.init_config()
local_config.set_data_path("yahpo_data")
```

To obtain DEHB:

```bash
git clone https://github.com/automl/DEHB
cd DEHB
git checkout 67ac239f8cbede87a0db9951cf0d4aa18ef806da
```

Note that DEHB must be put on path. See the comment in `run_mf.R` and adapt the `"FIXME_DEHB_path"` placeholder.

Finally, if you want to rerun experiments, make sure to adapt some paths to point at your newly created virtualenv, i.e., in `run_mf.R` and `run_mo.R`.
We indicate this via `"FIXME_python_path_of_your_virtualenv"` as a placeholder.

