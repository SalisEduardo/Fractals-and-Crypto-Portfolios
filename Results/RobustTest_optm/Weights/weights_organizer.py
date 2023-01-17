from nbformat import write
import pandas as pd
import re
import os

#  Robust Opt

files_optm_maxSR_Weights = os.listdir("/home/eduardo/Documentos/Fractals and crypto/Results/RobustTest_optm/Weights")
index = pd.concat([pd.read_csv(i,index_col=[0]) for i in list(filter(lambda f: "index" in f, files_optm_maxSR_Weights))])

more4_2020  = pd.concat([pd.read_csv(i,index_col=[0]) for i in list(filter(lambda f: ("2020" in f) & ("more4" in f), files_optm_maxSR_Weights))]) 
more4_2021  = pd.concat([pd.read_csv(i,index_col=[0]) for i in list(filter(lambda f: ("2021" in f) & ("more4" in f), files_optm_maxSR_Weights))])
more4_2022  = pd.concat([pd.read_csv(i,index_col=[0]) for i in list(filter(lambda f: ("2022" in f) & ("more4" in f), files_optm_maxSR_Weights))])

less4_2020  = pd.concat([pd.read_csv(i,index_col=[0]) for i in list(filter(lambda f: ("2020" in f) & ("less4" in f), files_optm_maxSR_Weights))]) 
less4_2021  = pd.concat([pd.read_csv(i,index_col=[0]) for i in list(filter(lambda f: ("2021" in f) & ("less4" in f), files_optm_maxSR_Weights))])
less4_2022  = pd.concat([pd.read_csv(i,index_col=[0]) for i in list(filter(lambda f: ("2022" in f) & ("less4" in f), files_optm_maxSR_Weights))])

more8_2020  = pd.concat([pd.read_csv(i,index_col=[0]) for i in list(filter(lambda f: ("2020" in f) & ("more8" in f), files_optm_maxSR_Weights))]) 
more8_2021  = pd.concat([pd.read_csv(i,index_col=[0]) for i in list(filter(lambda f: ("2021" in f) & ("more8" in f), files_optm_maxSR_Weights))])
more8_2022  = pd.concat([pd.read_csv(i,index_col=[0]) for i in list(filter(lambda f: ("2022" in f) & ("more8" in f), files_optm_maxSR_Weights))])

less8_2020  = pd.concat([pd.read_csv(i,index_col=[0]) for i in list(filter(lambda f: ("2020" in f) & ("less8" in f), files_optm_maxSR_Weights))]) 
less8_2021  = pd.concat([pd.read_csv(i,index_col=[0]) for i in list(filter(lambda f: ("2021" in f) & ("less8" in f), files_optm_maxSR_Weights))])
less8_2022  = pd.concat([pd.read_csv(i,index_col=[0]) for i in list(filter(lambda f: ("2022" in f) & ("less8" in f), files_optm_maxSR_Weights))])

with pd.ExcelWriter("Optm_weights.xlsx") as writer:

      index.to_excel(writer,sheet_name="index")
      more4_2020.to_excel(writer,sheet_name="more4_2020")
      more4_2021.to_excel(writer,sheet_name="more4_2021")
      more4_2022.to_excel(writer,sheet_name="more4_2022")

      less4_2020.to_excel(writer,sheet_name="less4_2020")
      less4_2021.to_excel(writer,sheet_name="less4_2021")
      less4_2022.to_excel(writer,sheet_name="less4_2022")

      more8_2020.to_excel(writer,sheet_name="more8_2020")
      more8_2021.to_excel(writer,sheet_name="more8_2021")
      more8_2022.to_excel(writer,sheet_name="more8_2022")

      less8_2020.to_excel(writer,sheet_name="less8_2020")
      less8_2021.to_excel(writer,sheet_name="less8_2021")
      less8_2022.to_excel(writer,sheet_name="less8_2022")
 