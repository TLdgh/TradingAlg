from finvizfinance.screener.overview import Overview
import pandas as pd
from datetime import date
import os

def SectorScreener():
  foverview = Overview()
  
  Sectors=["Basic Materials","Communication Services","Consumer Cyclical","Consumer Defensive",
           "Energy","Financial","Healthcare", "Industrials","Real Estate","Technology","Utilities"]
  
  df_list=[]
  for s in Sectors:
    filters_dict = {'Sector':s, 'Country': "USA", 'Price':'Over $5','Average Volume':'Over 100K'}
    foverview.set_filter(filters_dict=filters_dict)
    df = foverview.screener_view()
    df_list.append(df)
  
  res=pd.concat(df_list, axis=0, ignore_index=True)
  
  current_directory = os.getcwd()
  current_date = date.today()
  current_date = str(current_date).replace("-", "")
  res.to_csv(current_directory+"/Data/OriginalStockData/US/SectorDistribution/distribution_"+current_date+".csv", index=False)

if __name__=="__main__":
  SectorScreener()
