import akshare as ak
import os

df = ak.stock_zh_a_hist(symbol="601949", period="daily", start_date="20170907", end_date='20241206', adjust="qfq")
df.columns = ['Date', 'Stock_Code', 'Open', 'Close', 'High', 'Low', 'Volume', 
              'Turnover', 'Amplitude', 'Change_Percentage', 'Change_Amount', 'Turnover_Rate']

df = df[['Date', 'Open', 'High', 'Low', 'Close', 
         'Volume', 'Turnover', 'Amplitude', 'Change_Percentage', 
         'Change_Amount', 'Turnover_Rate']]


# Save the DataFrame to a CSV file at a specific path
currentfile_path = os.path.abspath('Ashare.py')
root_directory = os.path.dirname(currentfile_path)
file_path=os.path.join(root_directory, 'Data/OriginalStockData/China/ZhongGuoChuBan_daily.csv')
    
df.to_csv(file_path, index=False, encoding='utf-8')  # 'index=False' excludes the index column
