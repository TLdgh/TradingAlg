from finvizfinance.screener.overview import Overview
from finvizfinance.quote import finvizfinance
import pandas as pd
from datetime import date
import os, time

def SectorScreener():
  foverview = Overview()
  
  Sectors=["Basic Materials","Communication Services","Consumer Cyclical","Consumer Defensive",
           "Energy","Financial","Healthcare", "Industrials","Real Estate","Technology","Utilities"]
  AdditionalTickers=["ACN","NXPI","TEL","LIN","LYB","SW","AMCR","ETN","TT","JCI","PNR","ALLE",
                     "MDT","STE","CB","AON","ACGL","WTW","EG","LULU","NVR","GRMN","APTV"]
  
  df_list=[]
  for s in Sectors:
    filters_dict = {'Sector':s, 'Country': "USA", 'Price':'Over $5','Average Volume':'Over 100K'}
    foverview.set_filter(filters_dict=filters_dict)
    df = foverview.screener_view()
    df = df.dropna(subset=["Market Cap"])
    df_list.append(df)
  
  for t in AdditionalTickers:
    stock=finvizfinance(t)
    stock_fundament=stock.ticker_fundament()
    stock_fundament['Ticker']=t    
    stock_fundament["Market Cap"]=float(stock_fundament['Market Cap'].strip("B"))*1000000000
    stock_fundament["P/E"]=float(stock_fundament['P/E'])
    stock_fundament["Price"]=float(stock_fundament['Price'])
    stock_fundament["Change"]=float(stock_fundament['Change'].strip("%"))/100
    stock_fundament["Volume"]=int(stock_fundament['Volume'].replace(",",""))
    stock_fundament=pd.DataFrame(stock_fundament, index=[0])
    df = stock_fundament[['Ticker','Company','Sector','Industry','Country','Market Cap','P/E', 'Price','Change','Volume']]
    df_list.append(df)
    time.sleep(5)
    
  res=pd.concat(df_list, axis=0, ignore_index=True)
  
  current_directory = os.getcwd()
  current_date = date.today()
  current_date = str(current_date).replace("-", "")
  res.to_csv(current_directory+"/Data/OriginalStockData/US/SectorDistribution/distribution_"+current_date+".csv", index=False)

if __name__=="__main__":
  SectorScreener()
