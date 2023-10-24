import gan_shadish
import pandas as pd
import numpy as np
import torch

np.random.seed(123)
torch.manual_seed(123)
# first redo with the original data
output_path = "data/"
data_path = "data/shadish.csv"
#
file = data_path
df = pd.read_csv(file)
gan_shadish.do_all(df, "shadish", batch_size=128, max_epochs=1000, path=output_path)
