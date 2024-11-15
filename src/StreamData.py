from ibapi.client import *
from ibapi.wrapper import *
from datetime import datetime
import threading, time, pytz, csv, os

class TestApp(EClient, EWrapper):
    def __init__(self):
        EClient.__init__(self, self)
        
        self.csv_file=None
        self.csv_writer=None
        self.csv_filepath=None
        self.current_reqId = 0  # To track request IDs
        self.symb=None
        self.done = False  # Flag to indicate when to stop the application

    def set_symbol(self, symb):
        self.symb = symb
        self.csv_filepath=f"HistoricalData_{symb}.csv"

    def generate_reqId(self):
        self.current_reqId += 1
        return self.current_reqId
    
    def start(self):
        if self.symb is None:
            raise ValueError("Symbol is not set. Please use set_symbol() before starting.")
        
        # Define the NQ contract
        nq_contract = Contract()
        nq_contract.symbol = self.symb          # Symbol for the E-mini NASDAQ 100 futures
        nq_contract.secType = "FUT"        # Security type (Futures)
        nq_contract.exchange = "CME"    # Exchange for NQ futures (CME GLOBEX)
        nq_contract.currency = "USD"       # Currency for the contract
        nq_contract.lastTradeDateOrContractMonth = "20241220"  # Expiration year/month (example: Dec 2023)

        time_z=pytz.timezone("US/Central")
        current_time=datetime.now(time_z)
        current_time=current_time.strftime("%Y%m%d %H:%M:%S US/Central")

        # Request contract details for NQ contract
        reqId = self.generate_reqId()
        self.reqContractDetails(reqId=reqId, contract=nq_contract)

        # Request the historical data
        self.reqHistoricalData(reqId=reqId, contract=nq_contract,endDateTime=current_time, 
                        durationStr="2 W", barSizeSetting="30 mins", 
                        whatToShow="TRADES", useRTH=0,formatDate=1,keepUpToDate=False,chartOptions=[])
        print(f"current reqId is: {reqId}")
        
    def stop(self):
        self.done=True
        self.disconnect()
        
    def error(self, reqId, errorCode, errorString):
        print("Error. Id: " , reqId, " Code: " , errorCode , " Error Message: " , errorString)      
        
    def contractDetails(self, reqId, contract_info):
        print(f"Contract Details for Request ID {reqId}:\n")
        print(f"Symbol: {contract_info.contract.symbol}\n")
        print(f"Exchange: {contract_info.contract.exchange}\n")
        print(f"Currency: {contract_info.contract.currency}\n")
        print(f"Minimum Tick: {contract_info.minTick}\n")
        print(f"Multiplier: {contract_info.contract.multiplier}\n")

        attrs=vars(contract_info)
        print("\n".join(f"{name}: {value}" for name, value in attrs.items()))
        
    # This callback method will handle the historical data once received
    def historicalData(self, reqId: int, bar: BarData):
        originaldate=bar.date
        try:
            reformatted_date = datetime.strptime(originaldate, "%Y%m%d  %H:%M:%S").strftime("%Y-%m-%d %H:%M:%S")
        except ValueError:
            # If the format is different (e.g., for some historical data), use fallback
            reformatted_date = originaldate
        
        # Update the date field in the bar dictionary
        bar_dict = bar.__dict__.copy()  # Make a copy to avoid mutating the original
        bar_dict['date'] = reformatted_date
        
        
        # Open file or initiate it
        if os.path.exists(self.csv_filepath):
            self.csv_file=open(self.csv_filepath, mode="a", newline="")
            self.csv_writer=csv.DictWriter(self.csv_file, fieldnames=bar_dict.keys())
        else:
            self.csv_file=open(self.csv_filepath, mode="w", newline="")
            self.csv_writer=csv.DictWriter(self.csv_file, fieldnames=bar_dict.keys())
            self.csv_writer.writeheader()
        
        # Write the new row
        self.csv_writer.writerow(bar_dict)

    def historicalDataEnd(self, reqId: int, start: str, end: str):
        if self.csv_file:
            self.csv_file.close()
            print(f"Saving historical Data for Request ID {reqId} to:{self.csv_filepath}.")
            print(f"Historical Data End for Request ID {reqId}: Start Date: {start}, End Date: {end}")
            
            #self.disconnect()




# Create the application instance
app = TestApp()
app.connect('127.0.0.1', 7496, 2)    

app.set_symbol(symb="NQ")
app.start()
threading.Thread(target=app.run).start()
app.stop()
