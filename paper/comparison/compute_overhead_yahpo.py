import yahpo_gym
from memory_profiler import profile
import numpy as np
import time
import os, psutil
def eval_time(objfun, repls = 1):
    t = [None for _ in range(repls)]
    for i in range(repls):
        t0 = time.time(); 
        objfun()
        t1 = time.time()
        t[i] = t1 - t0
    
    t = np.array([t]).mean()
    print(f'Time taken: {np.round(t, 3)} s')
    print(f'Memory: {psutil.Process(os.getpid()).memory_info().rss / 1024 ** 2} MB')

@profile
def objfun():
    n_trials = 50
    bench = yahpo_gym.BenchmarkSet("lcbench", instance = '3945')
    bench.set_instance("3945")
    xs = [None for _ in range(n_trials)]
    # Random search
    for i in range(n_trials):
        # Sample a point from the configspace and evaluate
        value = bench.config_space.sample_configuration(1).get_dictionary()
        xs[i] = bench.objective_function(value)
    return xs


if __name__ == '__main__':
    eval_time(objfun)