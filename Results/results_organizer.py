from nbformat import write
import pandas as pd
import re
import os

#Returns serie
series_all_files = os.listdir("Results/Series_returns")
cum_rets_files = list(filter(lambda f: f.startswith('cumulative'), series_all_files))
index_cum_rets = "Results/Crypto_Indexes/Series/index_cumulative.csv"
daily_rets_files  = list(filter(lambda f: f.startswith('daily'), series_all_files))  
index_daily = "Results/Crypto_Indexes/Series/index_daily.csv"

def change_column_names(df):
    cols = df.columns
    cols_formated = [x.replace("first_","") for x in cols]
    cols_formated = [x.replace("second_","") for x in cols_formated]
    cols_formated = [x.replace("third_","") for x in cols_formated]
    df.columns = cols_formated
    return(df)


def consolidate_series(export_name, dir_path,files_list,index_path):
    with pd.ExcelWriter(export_name) as writer:  
        for i in files_list:
            file_path = dir_path + i
            df = pd.read_csv(file_path,index_col=[0])
            df = change_column_names(df)
            name_sheet = re.findall(r'\d+',i)[0]
            df.to_excel(writer,sheet_name=name_sheet)
        index = pd.read_csv(index_path,index_col=[0])
        index.to_excel(writer,sheet_name='Index')


consolidate_series('CumulativeReturns.xlsx', "Results/Series_returns/",cum_rets_files,index_cum_rets)
consolidate_series('DailyReturns.xlsx', "Results/Series_returns/",daily_rets_files,index_daily)

#Weights
files_weights_portfolio4 = os.listdir("Results/Weights/4assets")
files_weights_portfolio8 = os.listdir("Results/Weights/8assets")
files_weights_index = os.listdir("Results/Crypto_Indexes/Weights")

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
    
    for i in files_weights_index:
        file_path = "Results/Crypto_Indexes/Weights/" + i
        df = pd.read_csv(file_path,index_col=[0])
        df.to_excel(writer,sheet_name=i.replace(".csv",""))


all_files_4c = os.listdir("Results/KPIs/4assets")
all_files_4c = sorted(all_files_4c)    
csv_files_2020_4c = list(filter(lambda f: f.endswith('2020.csv'), all_files_4c))
csv_files_2020_4c = sorted(csv_files_2020_4c)

csv_files_2021_4c = list(filter(lambda f: f.endswith('2021.csv'), all_files_4c))
csv_files_2021_4c = sorted(csv_files_2021_4c)

csv_files_2022_4c = list(filter(lambda f: f.endswith('2022.csv'), all_files_4c))
csv_files_2022_4c = sorted(csv_files_2022_4c)

all_files_8c = os.listdir("Results/KPIs/8assets")  
all_files_8c = sorted(all_files_8c)  
csv_files_2020_8c = list(filter(lambda f: f.endswith('2020.csv'), all_files_8c))
csv_files_2020_8c = sorted(csv_files_2020_8c)

csv_files_2021_8c = list(filter(lambda f: f.endswith('2021.csv'), all_files_8c))
csv_files_2021_8c = sorted(csv_files_2021_8c)

csv_files_2022_8c = list(filter(lambda f: f.endswith('2022.csv'), all_files_8c))
csv_files_2022_8c = sorted(csv_files_2022_8c)


all_files_EW = os.listdir("Results/Crypto_Indexes/KPIs/EW")
all_files_EW = sorted(all_files_EW)    
csv_files_2020_EW = sorted(list(filter(lambda f: f.endswith('2020.csv'), all_files_EW)))
csv_files_2021_EW = sorted(list(filter(lambda f: f.endswith('2021.csv'), all_files_EW)))
csv_files_2022_EW = sorted(list(filter(lambda f: f.endswith('2022.csv'), all_files_EW)))

all_files_InvInef = os.listdir("Results/Crypto_Indexes/KPIs/InvInef")  
all_files_InvInef = sorted(all_files_InvInef)    
csv_files_2020_InvInef = sorted(list(filter(lambda f: f.endswith('2020.csv'), all_files_InvInef)))
csv_files_2021_InvInef = sorted(list(filter(lambda f: f.endswith('2021.csv'), all_files_InvInef)))
csv_files_2022_InvInef = sorted(list(filter(lambda f: f.endswith('2022.csv'), all_files_InvInef)))



with pd.ExcelWriter("ConjuntoKPIs.xlsx") as writer:  
    for i,j in zip(csv_files_2020_4c,csv_files_2020_8c):
        if not("Drawdowns2020" in i): 

            file_path_i = "Results/KPIs/4assets/" + i
            dfi = pd.read_csv(file_path_i,index_col=[0])
            dfi = change_column_names(dfi)
            dfi = dfi.transpose()
            file_path_j = "Results/KPIs/8assets/" + j
            dfj = pd.read_csv(file_path_j,index_col=[0])
            dfj = change_column_names(dfj)
            dfj = dfj.transpose()
            df = pd.concat([dfi,dfj])
            df.to_excel(writer,sheet_name=i.replace(".csv",""))
        else:
            file_path_i = "Results/KPIs/4assets/" + i
            dfi = pd.read_csv(file_path_i,index_col=[0])
            file_path_j = "Results/KPIs/8assets/" + j
            dfj = pd.read_csv(file_path_j,index_col=[0])
            df = pd.concat([dfi,dfj])
            df.to_excel(writer,sheet_name=i.replace(".csv",""))

    for i,j in zip(csv_files_2021_4c,csv_files_2020_8c):
            if not("Drawdowns2021" in i): 

                file_path_i = "Results/KPIs/4assets/" + i
                dfi = pd.read_csv(file_path_i,index_col=[0])
                dfi = change_column_names(dfi)
                dfi = dfi.transpose()
                file_path_j = "Results/KPIs/8assets/" + j
                dfj = pd.read_csv(file_path_j,index_col=[0])
                dfj = change_column_names(dfj)
                dfj = dfj.transpose()
                df = pd.concat([dfi,dfj])
                df.to_excel(writer,sheet_name=i.replace(".csv",""))
            else:
                file_path_i = "Results/KPIs/4assets/" + i
                dfi = pd.read_csv(file_path_i,index_col=[0])
                file_path_j = "Results/KPIs/8assets/" + j
                dfj = pd.read_csv(file_path_j,index_col=[0])
                df = pd.concat([dfi,dfj])
                df.to_excel(writer,sheet_name=i.replace(".csv",""))

    for i,j in zip(csv_files_2022_4c,csv_files_2022_8c):
                if not("Drawdowns2022" in i): 

                    file_path_i = "Results/KPIs/4assets/" + i
                    dfi = pd.read_csv(file_path_i,index_col=[0])
                    dfi = change_column_names(dfi)
                    dfi = dfi.transpose()
                    file_path_j = "Results/KPIs/8assets/" + j
                    dfj = pd.read_csv(file_path_j,index_col=[0])
                    dfj = change_column_names(dfj)
                    dfj = dfj.transpose()
                    df = pd.concat([dfi,dfj])
                    df.to_excel(writer,sheet_name=i.replace(".csv",""))
                else:
                    file_path_i = "Results/KPIs/4assets/" + i
                    dfi = pd.read_csv(file_path_i,index_col=[0])
                    file_path_j = "Results/KPIs/8assets/" + j
                    dfj = pd.read_csv(file_path_j,index_col=[0])
                    df = pd.concat([dfi,dfj])
                    df.to_excel(writer,sheet_name=i.replace(".csv",""))


with pd.ExcelWriter("Index_ConjuntoKPIs.xlsx") as writer:  
    for i,j in zip(csv_files_2020_EW,csv_files_2020_InvInef):
        if not("Drawdowns2020" in i): 

            file_path_i = "Results/Crypto_Indexes/KPIs/EW/" + i
            dfi = pd.read_csv(file_path_i,index_col=[0])
            dfi = change_column_names(dfi)
            dfi = dfi.transpose()
            file_path_j = "Results/Crypto_Indexes/KPIs/InvInef/" + j
            dfj = pd.read_csv(file_path_j,index_col=[0])
            dfj = change_column_names(dfj)
            dfj = dfj.transpose()
            df = pd.concat([dfi,dfj])
            df.to_excel(writer,sheet_name=i.replace(".csv",""))
        else:
            file_path_i = "Results/Crypto_Indexes/KPIs/EW/" + i
            dfi = pd.read_csv(file_path_i,index_col=[0])
            file_path_j = "Results/Crypto_Indexes/KPIs/InvInef/" + j
            dfj = pd.read_csv(file_path_j,index_col=[0])
            df = pd.concat([dfi,dfj])
            df.to_excel(writer,sheet_name=i.replace(".csv",""))

    for i,j in zip(csv_files_2021_EW,csv_files_2021_InvInef):
            if not("Drawdowns2021" in i): 

                file_path_i = "Results/Crypto_Indexes/KPIs/EW/" + i
                dfi = pd.read_csv(file_path_i,index_col=[0])
                dfi = change_column_names(dfi)
                dfi = dfi.transpose()
                file_path_j = "Results/Crypto_Indexes/KPIs/InvInef/" + j
                dfj = pd.read_csv(file_path_j,index_col=[0])
                dfj = change_column_names(dfj)
                dfj = dfj.transpose()
                df = pd.concat([dfi,dfj])
                df.to_excel(writer,sheet_name=i.replace(".csv",""))
            else:
                file_path_i = "Results/Crypto_Indexes/KPIs/EW/" + i
                dfi = pd.read_csv(file_path_i,index_col=[0])
                file_path_j = "Results/Crypto_Indexes/KPIs/InvInef/" + j
                dfj = pd.read_csv(file_path_j,index_col=[0])
                df = pd.concat([dfi,dfj])
                df.to_excel(writer,sheet_name=i.replace(".csv",""))

    for i,j in zip(csv_files_2022_EW,csv_files_2022_InvInef):
                if not("Drawdowns2022" in i): 

                    file_path_i = "Results/Crypto_Indexes/KPIs/EW/" + i
                    dfi = pd.read_csv(file_path_i,index_col=[0])
                    dfi = change_column_names(dfi)
                    dfi = dfi.transpose()
                    file_path_j = "Results/Crypto_Indexes/KPIs/InvInef/" + j
                    dfj = pd.read_csv(file_path_j,index_col=[0])
                    dfj = change_column_names(dfj)
                    dfj = dfj.transpose()
                    df = pd.concat([dfi,dfj])
                    df.to_excel(writer,sheet_name=i.replace(".csv",""))
                else:
                    file_path_i = "Results/Crypto_Indexes/KPIs/EW/" + i
                    dfi = pd.read_csv(file_path_i,index_col=[0])
                    file_path_j = "Results/Crypto_Indexes/KPIs/InvInef/" + j
                    dfj = pd.read_csv(file_path_j,index_col=[0])
                    df = pd.concat([dfi,dfj])
                    df.to_excel(writer,sheet_name=i.replace(".csv",""))