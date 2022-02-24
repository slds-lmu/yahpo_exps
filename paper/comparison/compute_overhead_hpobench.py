# Run with python -m memory_profiler compute_overhead_hpobench.py
from hpobench.container.benchmarks.surrogates.svm_benchmark import SurrogateSVMBenchmark
from memory_profiler import profile
import numpy as np
import time
import os, psutil

def eval_time(objfun, repls = 10):
    t = [None for _ in range(repls)]
    for i in range(repls):
        t0 = time.time(); 
        objfun()
        t1 = time.time()
        t[i] = t1 - t0
    
    t = np.array([t]).mean()
    print(f'Time taken: {np.round(t, 3)} s')


@profile
def objfun():
    n_trials = 300
    b = SurrogateSVMBenchmark(rng=1)
    xs = [None for _ in range(n_trials)]
    # Random search
    for i in range(n_trials):
        config = b.get_configuration_space(seed=1).sample_configuration()
        result_dict = b.objective_function(configuration=config, fidelity={"dataset_fraction": 1.}, rng=1)
    return xs


if __name__ == '__main__':
    eval_time(objfun)
