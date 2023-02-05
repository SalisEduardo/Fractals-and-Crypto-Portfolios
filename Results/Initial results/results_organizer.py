from nbformat import write
import pandas as pd
import re
import os

#  Robust Opt

files_optm_maxSR_Weights = os.listdir("Results/RobustTest_optm/Weights")
index = list(filter(lambda f: "index" in f, files_optm_maxSR_Weights))

more4_2020  = list(filter(lambda f: ("2020" in f) & ("more4" in f), files_optm_maxSR_Weights))
more4_2021  = list(filter(lambda f: ("2021" in f) & ("more4" in f), files_optm_maxSR_Weights))
more4_2022  = list(filter(lambda f: ("2022" in f) & ("more4" in f), files_optm_maxSR_Weights))

more8_2020  = list(filter(lambda f: ("2020" in f) & ("more8" in f), files_optm_maxSR_Weights))
more8_2021  = list(filter(lambda f: ("2021" in f) & ("more8" in f), files_optm_maxSR_Weights))
more8_2022  = list(filter(lambda f: ("2022" in f) & ("more8" in f), files_optm_maxSR_Weights))

less4_2020  = list(filter(lambda f: ("2020" in f) & ("less4" in f), files_optm_maxSR_Weights))
less4_2021  = list(filter(lambda f: ("2021" in f) & ("less4" in f), files_optm_maxSR_Weights))
less4_2022  = list(filter(lambda f: ("2022" in f) & ("less4" in f), files_optm_maxSR_Weights))

less8_2020  = list(filter(lambda f: ("2020" in f) & ("less8" in f), files_optm_maxSR_Weights))
less8_2021  = list(filter(lambda f: ("2021" in f) & ("less8" in f), files_optm_maxSR_Weights))
less8_2022  = list(filter(lambda f: ("2022" in f) & ("less8" in f), files_optm_maxSR_Weights))


with pd.ExcelWriter("Optm_weights.xlsx") as writer:
      for i in index:
            pd.read_csv(i).to_excel(writer,sheet_name="index")
      for i in more4_2020:
            pd.read_csv(i).to_excel(writer,sheet_name="more4_2020")
      for i in more4_2021:
            pd.read_csv(i).to_excel(writer,sheet_name="more4_2021")
      for i in more4_2022:
            pd.read_csv(i).to_excel(writer,sheet_name="more4_2022")

      for i in less4_2020:
            pd.read_csv(i).to_excel(writer,sheet_name="less4_2020")
      for i in less4_2021:
            pd.read_csv(i).to_excel(writer,sheet_name="less4_2021")
      for i in less4_2022:
            pd.read_csv(i).to_excel(writer,sheet_name="less4_2022")

      for i in more8_2020:
            pd.read_csv(i).to_excel(writer,sheet_name="more8_2020")
      for i in more8_2021:
            pd.read_csv(i).to_excel(writer,sheet_name="more8_2021")
      for i in more8_2022:
            pd.read_csv(i).to_excel(writer,sheet_name="more8_2022")

      for i in less8_2020:
            i.to_excel(writer,sheet_name="less8_2020")
      for i in less8_2021:
            i.to_excel(writer,sheet_name="less8_2021")
      for i in less8_2022:
            i.to_excel(writer,sheet_name="less8_2022")




# optm_backtest2020 = pd.read_csv("Results/RobustTest_optm/optm_backtest2020.csv")
# optm_backtest2021 = pd.read_csv("Results/RobustTest_optm/optm_backtest2021.csv")
# optm_backtest2022 = pd.read_csv("Results/RobustTest_optm/optm_backtest2022.csv")

# with pd.ExcelWriter("Optm_backtest.xlsx") as writer:
#       optm_backtest2020.to_excel(writer,sheet_name="2020")
#       optm_backtest2021.to_excel(writer,sheet_name="2021")
#       optm_backtest2022.to_excel(writer,sheet_name="2022")









# Robust test max box

# box_MAX_backtest2020 = pd.read_csv("Results/RobustTest_minaloc/Max_aloc/box_MAX_backtest2020.csv")
# box_MAX_backtest2021 = pd.read_csv("Results/RobustTest_minaloc/Max_aloc/box_MAX_backtest2021.csv")
# box_MAX_backtest2022 = pd.read_csv("Results/RobustTest_minaloc/Max_aloc/box_MAX_backtest2022.csv")

# with pd.ExcelWriter("Box_constrained_MAX.xlsx") as writer:
#       box_MAX_backtest2020.to_excel(writer,sheet_name="2020")
#       box_MAX_backtest2021.to_excel(writer,sheet_name="2021")
#       box_MAX_backtest2022.to_excel(writer,sheet_name="2022")



# Robust min box

# box_backtest2020 = pd.read_csv("Results/RobustTest_minaloc/Min_aloc/box_backtest2020.csv")

# box_backtest2021 = pd.read_csv("Results/RobustTest_minaloc/Min_aloc/box_backtest2021.csv")
# box_backtest2022 = pd.read_csv("Results/RobustTest_minaloc/Min_aloc/box_backtest2022.csv")

# with pd.ExcelWriter("Box_constrained_test.xlsx") as writer:
#       box_backtest2020.to_excel(writer,sheet_name="2020")
#       box_backtest2021.to_excel(writer,sheet_name="2021")
#       box_backtest2022.to_excel(writer,sheet_name="2022")




#Returns serie


# series_all_files = os.listdir("Results/Series_returns")
# cum_rets_files = list(filter(lambda f: f.startswith('cumulative'), series_all_files))
# index_cum_rets = "Results/Crypto_Indexes/Series/index_cumulative.csv"
# daily_rets_files  = list(filter(lambda f: f.startswith('daily'), series_all_files))  
# index_daily = "Results/Crypto_Indexes/Series/index_daily.csv"

# def change_column_names(df):
#     cols = df.columns
#     cols_formated = [x.replace("first_","") for x in cols]
#     cols_formated = [x.replace("second_","") for x in cols_formated]
#     cols_formated = [x.replace("third_","") for x in cols_formated]
#     df.columns = cols_formated
#     return(df)


# def consolidate_series(export_name, dir_path,files_list,index_path):
#     with pd.ExcelWriter(export_name) as writer:  
#         for i in files_list:
#             file_path = dir_path + i
#             df = pd.read_csv(file_path,index_col=[0])
#             df = change_column_names(df)
#             name_sheet = re.findall(r'\d+',i)[0]
#             df.to_excel(writer,sheet_name=name_sheet)
#         index = pd.read_csv(index_path,index_col=[0])
#         index.to_excel(writer,sheet_name='Index')


# consolidate_series('CumulativeReturns.xlsx', "Results/Series_returns/",cum_rets_files,index_cum_rets)
# consolidate_series('DailyReturns.xlsx', "Results/Series_returns/",daily_rets_files,index_daily)

# #Weights
# files_weights_portfolio4 = os.listdir("Results/Weights/4assets")
# files_weights_portfolio8 = os.listdir("Results/Weights/8assets")
# files_weights_index = os.listdir("Results/Crypto_Indexes/Weights")

# with pd.ExcelWriter("Weights.xlsx") as writer:  
#     for i in files_weights_portfolio4:
#         file_path = "Results/Weights/4assets/" + i
#         df = pd.read_csv(file_path,index_col=[0])
#         df = change_column_names(df)
#         #name_sheet = re.search('%s(.*)%s' % ("_", ".csv"), i).group(1)
#         name_sheet = i.replace(".csv","")
#         name_sheet = name_sheet.replace("first","2020")
#         name_sheet = name_sheet.replace("second","2021")
#         name_sheet = name_sheet.replace("third","2022")
#         df.to_excel(writer,sheet_name=name_sheet)

#     for i in files_weights_portfolio8:
#         file_path = "Results/Weights/8assets/" + i
#         df = pd.read_csv(file_path,index_col=[0])
#         df = change_column_names(df)
#         #name_sheet = re.search('%s(.*)%s' % ("_", ".csv"), i).group(1)
#         name_sheet = i.replace(".csv","")
#         name_sheet = name_sheet.replace("first","2020")
#         name_sheet = name_sheet.replace("second","2021")
#         name_sheet = name_sheet.replace("third","2022")
#         df.to_excel(writer,sheet_name=name_sheet)
    
#     for i in files_weights_index:
#         file_path = "Results/Crypto_Indexes/Weights/" + i
#         df = pd.read_csv(file_path,index_col=[0])
#         df.to_excel(writer,sheet_name=i.replace(".csv",""))


# all_files_4c = os.listdir("Results/KPIs/4assets")
# all_files_4c = sorted(all_files_4c)    
# csv_files_KPI_4c = list(filter(lambda f: f.startswith('KPIs'), all_files_4c))
# csv_files_KPI_4c= ["Results/KPIs/4assets/" + i for i in csv_files_KPI_4c]

# all_files_8c = os.listdir("Results/KPIs/8assets")  
# all_files_8c = sorted(all_files_8c)  
# csv_files_KPI_8c = list(filter(lambda f: f.startswith('KPIs'), all_files_8c))
# csv_files_KPI_8c= ["Results/KPIs/8assets/" + i for i in csv_files_KPI_8c]

# all_KPIs_portfolios = csv_files_KPI_4c + csv_files_KPI_8c


# all_files_EW = os.listdir("Results/Crypto_Indexes/KPIs/EW")
# all_files_EW = sorted(all_files_EW)    
# csv_files_KPI_EW = sorted(list(filter(lambda f: f.startswith('KPIs'), all_files_EW)))
# csv_files_KPI_EW= ["Results/Crypto_Indexes/KPIs/EW/" + i for i in csv_files_KPI_EW]

# all_files_InvInef = os.listdir("Results/Crypto_Indexes/KPIs/InvInef")  
# all_files_InvInef = sorted(all_files_InvInef)    
# csv_files_KPI_InvInef = sorted(list(filter(lambda f: f.startswith('KPIs'), all_files_InvInef)))
# csv_files_KPI_InvInef= ["Results/Crypto_Indexes/KPIs/InvInef/" + i for i in csv_files_KPI_InvInef]

# all_files_MVP = os.listdir("Results/Crypto_Indexes/KPIs/MVP")  
# all_files_MVP = sorted(all_files_MVP)    
# csv_files_KPI_MVP = sorted(list(filter(lambda f: f.startswith('KPIs'), all_files_MVP)))
# csv_files_KPI_MVP= ["Results/Crypto_Indexes/KPIs/MVP/" + i for i in csv_files_KPI_MVP]

# all_files_maxSR = os.listdir("Results/Crypto_Indexes/KPIs/maxSR")  
# all_files_maxSR = sorted(all_files_maxSR)    
# csv_files_KPI_maxSR = sorted(list(filter(lambda f: f.startswith('KPIs'), all_files_maxSR)))
# csv_files_KPI_maxSR= ["Results/Crypto_Indexes/KPIs/maxSR/" + i for i in csv_files_KPI_maxSR]



# all_KPIs_index = csv_files_KPI_EW + csv_files_KPI_InvInef + csv_files_KPI_MVP + csv_files_KPI_maxSR


# with pd.ExcelWriter("ConjuntoKPIs.xlsx") as writer:  
#     list_df_kpis = []
#     for i in all_KPIs_portfolios:
#         df = pd.read_csv(i)
#         list_df_kpis.append(df)
#     kpis = pd.concat(list_df_kpis,axis=0)
#     kpis['Strategy_name'] = kpis['Strategy_name'].apply(lambda x: x.replace("first_","")) 
#     kpis['Strategy_name'] = kpis['Strategy_name'].apply(lambda x: x.replace("second_","")) 
#     kpis['Strategy_name'] = kpis['Strategy_name'].apply(lambda x: x.replace("third_","")) 
#     kpis.to_excel(writer,sheet_name="Portfolios")
    
#     list_df_index_kpis = []
#     for i in all_KPIs_index:
#         df = pd.read_csv(i)
#         list_df_index_kpis.append(df)
#     kpis_index = pd.concat(list_df_index_kpis,axis=0)
#     kpis_index['Strategy_name'] = kpis_index['Strategy_name'].apply(lambda x: x.replace("first_","")) 
#     kpis_index['Strategy_name'] = kpis_index['Strategy_name'].apply(lambda x: x.replace("second_","")) 
#     kpis_index['Strategy_name'] = kpis_index['Strategy_name'].apply(lambda x: x.replace("third_","")) 
#     kpis_index.to_excel(writer,sheet_name="Index")
