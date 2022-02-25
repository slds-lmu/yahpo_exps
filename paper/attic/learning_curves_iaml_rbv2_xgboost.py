from yahpo_gym.configuration import config_dict, cfg
from yahpo_gym.benchmark_set import BenchmarkSet
from yahpo_gym import local_config
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

bench = BenchmarkSet("rbv2_rpart", check=False)
df = pd.read_csv(bench.config.get_path("dataset"))
df = df[df.task_id.astype('str').isin(bench.instances)]


metric = 'bac'

def plot_true_lc(ccc, color):
    # Subset data to obtain learning curve for config
    config = pd.DataFrame(data = ccc, index = [0])
    config.drop(bench.config.fidelity_params, axis = 1, inplace=True)
    pdf = df.merge(config, on=list(config.columns.values))
    pdf = pdf.sort_values(bench.config.fidelity_params[0])
    plt.plot(pdf[bench.config.fidelity_params[0]], pdf[metric], marker = ".", color=color, linestyle='dashed', alpha =.7)

def plot_lc_iaml(ccc, color, deterministic = True):
    ccc["task_id"] = str(ccc["task_id"])
    out = []
    for i in range(45):
        val = ccc.copy()
        val.update({"trainsize" : i/50 + 0.01})
        out += [val]

    if deterministic:
        res = bench.objective_function(out, seed=0)
        res = [x[metric] for x in res]
        plt.plot([x/50 + 0.05 for x in range(45)], res, color=color)
    else:
        colors = ['red', 'blue', 'orange', 'purple', 'green', 'gold', 'magenta', 'darkviolet', 'cyan', 'olive']
        for i in range(10):
            res = bench.objective_function(out, seed=i)
            res = [x[metric] for x in res]
            plt.plot([x/50 + 0.05 for x in range(45)], res, color=colors[i])


# Draw a sample configuration
colors = ['red', 'blue', 'orange', 'purple', 'green', 'gold', 'magenta', 'darkviolet', 'cyan', 'olive']
for i in range(5):
    ccs = df.sample(1, random_state=np.random.RandomState(100*i)).to_dict()
    ccc = {k:list(ccs[k].values())[0] for k in bench.config_space.get_hyperparameter_names()}
    print(ccc)
    col = colors[i]
    plot_true_lc(ccc, color = col)
    plot_lc_iaml(ccc, color = col)
    plt.xlabel("Dataset fraction")
    plt.ylabel("Accuracy")

plt.savefig(f"learning_curves_iaml_xgb.png")