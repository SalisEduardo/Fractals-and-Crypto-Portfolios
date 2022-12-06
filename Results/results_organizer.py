from nbformat import write
import pandas as pd
import re
import os

#Returns serie
series_all_files = os.listdir("Results/Series_returns")
cum_rets_files = list(filter(lambda f: f.startswith('cumulative'), series_all_files))
daily_rets_files  = list(filter(lambda f: f.startswith('daily'), series_all_files))  


def change_column_names(df):
    cols = df.columns
    cols_formated = [x.replace("first_","") for x in cols]
    cols_formated = [x.replace("second_","") for x in cols_formated]
    cols_formated = [x.replace("third_","") for x in cols_formated]
    df.columns = cols_formated
    return(df)


def consolidate_series(export_name, dir_path,files_list):
    with pd.ExcelWriter(export_name) as writer:  
        for i in files_list:
            file_path = dir_path + i
            df = pd.read_csv(file_path,index_col=[0])
            df = change_column_names(df)
            name_sheet = re.findall(r'\d+',i)[0]
            df.to_excel(writer,sheet_name=name_sheet)


consolidate_series('CumulativeReturns.xlsx', "Results/Series_returns/",cum_rets_files)
consolidate_series('DailyReturns.xlsx', "Results/Series_returns/",daily_rets_files)

#weights
files_weights_portfolio4 = series_all_files = os.listdir("Results/Weights/4assets")
files_weights_portfolio8 = series_all_files = os.listdir("Results/Weights/8assets")

with pd.ExcelWriter("Weights.xlsx") as writer:  
    for i in files_weights_portfolio4:
        file_path = "Results/Weights/4assets/" + i
        df = pd.read_csv(file_path,index_col=[0])
        df = change_column_names(df)
        name_sheet = re.search('%s(.*)%s' % ("_", ".csv"), i).group(1)
        df.to_excel(writer,sheet_name=name_sheet)

    for i in files_weights_portfolio8:
        file_path = "Results/Weights/8assets/" + i
        df = pd.read_csv(file_path,index_col=[0])
        df = change_column_names(df)
        name_sheet = re.search('%s(.*)%s' % ("_", ".csv"), i).group(1)
        df.to_excel(writer,sheet_name=name_sheet)
    


