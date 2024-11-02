from finvizfinance.screener.overview import Overview
from finvizfinance.quote import finvizfinance
import pandas as pd
from datetime import date
import os, time, requests, logging, warnings

def SectorScreener(root_directory):
  try:
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
        stock_fundament["P/E"]=float(stock_fundament['P/E'].replace("-","0"))
        stock_fundament["Price"]=float(stock_fundament['Price'])
        stock_fundament["Change"]=float(stock_fundament['Change'].strip("%"))/100
        stock_fundament["Volume"]=int(stock_fundament['Volume'].replace(",",""))
        stock_fundament=pd.DataFrame(stock_fundament, index=[0])
        df = stock_fundament[['Ticker','Company','Sector','Industry','Country','Market Cap','P/E', 'Price','Change','Volume']]
        df_list.append(df)
        time.sleep(5)
        
      res=pd.concat(df_list, axis=0, ignore_index=True)
      
      current_date = date.today()
      current_date = str(current_date).replace("-", "")
      full_path=root_directory+"/Data/OriginalStockData/US/SectorDistribution/distribution_"+current_date+".csv"
      res.to_csv(full_path, index=False)

      logging.info(f"File downloaded successfully and saved to: {full_path}")
    
  except TypeError as e:
      logging.error(f"An error occurred: {e}")

def downloadVIX(root_directory):
  vix_url="https://cdn.cboe.com/api/global/us_indices/daily_prices/VIX_History.csv"
  vxn_url="https://cdn.cboe.com/api/global/us_indices/daily_prices/VXN_History.csv"

  urls={"VIX_History": vix_url, "VXN_History":vxn_url}
  save_directory = root_directory+'/Data/OriginalStockData/US'  # Change this to your desired directory

  for key, url in urls.items():
      file_name = key+'.csv'  # Name of the file to be saved
      full_path = os.path.join(save_directory, file_name)  # Create the full file path

      try:

          # Send a GET request to download the file with a timeout
          response = requests.get(url, timeout=3)

          # Check if the request was successful (status code 200 means success)
          if response.status_code == 200:
              # Save the .xlsx file to the specified directory
              with open(full_path, 'wb') as file:
                  file.write(response.content)
              logging.info(f"File downloaded successfully and saved to: {full_path}")
          else:
              logging.error(f"Failed to download the file. Status code: {response.status_code}")
      except requests.exceptions.RequestException as e:
          logging.error(f"An error occurred: {e}")

if __name__=="__main__":
  currentfile_path = os.path.abspath(__file__)
  # Get the root directory (the directory containing the script)
  root_directory = os.path.dirname(os.path.dirname(currentfile_path))
  
  # Configure logging to write to a file
  logging.basicConfig(filename=os.path.join(root_directory,'log.txt'), level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

  # Redirect warnings to the logging system
  logging.captureWarnings(True)

  SectorScreener(root_directory)
  downloadVIX(root_directory)
