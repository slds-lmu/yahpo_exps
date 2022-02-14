import yahpo_gym
import os, psutil
import time
import numpy as np

if __name__ == '__main__':
    bench = yahpo_gym.BenchmarkSet("lcbench")
    bench.config.config['model']
    # Set an instance
    bench.set_instance("3945")
    # Sample a point from the configspace (containing parameters for the instance and budget)
    value = bench.config_space.sample_configuration(1).get_dictionary()

    def eval_time():
        t0 = time.time(); bench.objective_function(value); t1 = time.time(); return t1 - t0

    tt = np.array([eval_time() for _ in range(200)]).mean() * 1000
    print(f'Time taken: {np.round(tt, 3)} ms')
    print(f'Memory: {psutil.Process(os.getpid()).memory_info().rss / 1024 ** 2} MB')