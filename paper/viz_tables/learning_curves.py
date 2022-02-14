from yahpo_gym.configuration import config_dict, cfg
from yahpo_gym.benchmark_set import BenchmarkSet
from yahpo_gym import local_config
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

np.random.RandomState(0)

bench = BenchmarkSet("lcbench", download = False, check=False)
bench.config.config['model'] = "new_model_resnet.onnx"

df = pd.read_csv(bench.config.get_path("test_dataset"))

def plot_true_lc(ccc, color):
    # Subset data to obtain learning curve for config
    config = pd.DataFrame(data = ccc, index = [0])
    config.drop(bench.config.fidelity_params, axis = 1, inplace=True)
    pdf = df.merge(config, on=list(config.columns.values))
    pdf = pdf.sort_values(bench.config.fidelity_params[0])
    plt.plot(pdf[bench.config.fidelity_params[0]], pdf[bench.config.y_names[1]], marker = ".", color=color, linestyle='dashed', alpha =.7)

def plot_lc_lcbench(ccc, color, deterministic = True):
    ccc["OpenML_task_id"] = str(ccc["OpenML_task_id"])
    out = []
    for i in range(51):
        val = ccc.copy()
        val.update({"epoch" : i+1})
        out += [val]

    if deterministic:
        res = bench.objective_function(out, seed=0)
        res = [x['val_accuracy'] for x in res]
        plt.plot(range(51), res, color=color)
    else:
        colors = ['red', 'blue', 'orange', 'purple', 'green', 'gold', 'magenta', 'darkviolet', 'cyan', 'olive']
        for i in range(10):
            res = bench.objective_function(out, seed=i)
            res = [x['val_accuracy'] for x in res]
            plt.plot(range(51), res, color=colors[i])


# Draw a sample configuration
colors = ['red', 'blue', 'orange', 'purple', 'green', 'gold', 'magenta', 'darkviolet', 'cyan', 'olive']
for i in range(5):
    ccs = df.sample(1, random_state=np.random.RandomState(1000*i)).to_dict()
    ccc = {k:list(ccs[k].values())[0] for k in bench.config_space.get_hyperparameter_names()}
    print(ccc)
    col = colors[i]
    plot_true_lc(ccc, color = col)
    plot_lc_lcbench(ccc, color = col)
    plt.xlabel("Epoch")
    plt.ylabel("Accuracy")

plt.savefig(f"learning_curves_lcbench.png")
