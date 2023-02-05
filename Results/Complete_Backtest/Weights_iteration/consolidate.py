import pandas as pd
import os

files = os.listdir(os.getcwd())
csv_files = list(filter(lambda f: f.endswith('.csv'), files))

weights = pd.concat([pd.read_csv(i) for i in csv_files])

weights.to_excel("all_weights.xlsx")