import pandas as pd
df = pd.read_csv("BacktestKPIs.csv",index_col=[0])
df.to_excel("BacktestKPIs.xlsx",index=False)
