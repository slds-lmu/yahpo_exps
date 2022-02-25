# Run with python -m memory_profiler compute_overhead_yahpo.py
import yahpo_gym
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
def objfun(batch = True):
    n_trials = 300
    bench = yahpo_gym.BenchmarkSet("rbv2_svm", instance = '4134', active_session=True)
    
    # Random search
    if batch:
        value = bench.config_space.sample_configuration(n_trials)
        xs = bench.objective_function(value)
    else:
        xs = [None for _ in range(n_trials)]
        for i in range(n_trials):
            # Sample a point from the configspace and evaluate
            value = bench.config_space.sample_configuration(1)
            xs[i] = bench.objective_function(value)
    return xs


if __name__ == '__main__':
    eval_time(objfun)