from ibapi.client import *
from ibapi.wrapper import *
from ibapi.ticktype import TickTypeEnum
from datetime import datetime, timedelta
from decimal import Decimal
import threading, pytz, csv, os, time

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
        filetime=(datetime.now() + timedelta(days=1)).strftime('%Y%m%d')
        self.csv_filepath=os.path.join(os.getcwd(), 
                                       "Data/OriginalFuturesData/NQ/TickData", 
                                       f"TickData_{symb}_{filetime}.csv")


    def generate_reqId(self):
        self.current_reqId += 1
        return self.current_reqId


    def _initialize_csv(self, fieldnames):
        #Initialize the CSV file and writer if not already set.
        if os.path.exists(self.csv_filepath):
            self.csv_file = open(self.csv_filepath, mode="a", newline="")
            self.csv_writer = csv.DictWriter(self.csv_file, fieldnames=fieldnames)
        else:
            self.csv_file = open(self.csv_filepath, mode="a", newline="")
            self.csv_writer = csv.DictWriter(self.csv_file, fieldnames=fieldnames)
            self.csv_writer.writeheader()


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
        
        # Request contract details
        reqId = self.generate_reqId()
        print(f"-------------------------------------------------")
        print(f"Contract detail is obtained for {self.symb} with Request ID: {reqId}")
        print(f"-------------------------------------------------")
        self.reqContractDetails(reqId=reqId, contract=nq_contract)
        
        '''
        # Request the historical data        
        # print(f"Historical Data is obtained with reqId: {reqId}")
        self.reqHistoricalData(reqId=reqId, contract=nq_contract,endDateTime=current_time, 
                        durationStr="2 W", barSizeSetting="30 mins", 
                        whatToShow="TRADES", useRTH=0,formatDate=1,keepUpToDate=False,chartOptions=[])

        # Request the market data
        # print(f"Market data request sent for {self.symb} with reqId={reqId}")
        self.reqMarketDataType(1)
        self.reqMktData(reqId=reqId, contract=nq_contract, genericTickList="",snapshot=False, regulatorySnapshot=False, mktDataOptions=[])

        '''
        
        # Request the tick by tick data. If Historical ticks, change numberOfTicks=...
        print(f"-------------------------------------------------")
        print(f"Tick by Tick data request sent for {self.symb} with Request ID={reqId}")
        print(f"-------------------------------------------------")
        self.reqTickByTickData(reqId=reqId, contract=nq_contract, tickType="Last", numberOfTicks=0, ignoreSize=False)


    def stop(self):
        self.done=True
        self.cancelTickByTickData(reqId=self.current_reqId)


    def error(self, reqId, errorCode, errorString):
        print("Error. Id: " , reqId, " Code: " , errorCode , " Error Message: " , errorString)      


    def contractDetails(self, reqId, contract_info):
        print(f"Symbol: {contract_info.contract.symbol}\n")
        print(f"Exchange: {contract_info.contract.exchange}\n")
        print(f"Currency: {contract_info.contract.currency}\n")
        print(f"Minimum Tick: {contract_info.minTick}\n")
        print(f"Multiplier: {contract_info.contract.multiplier}\n")
        print(f"Contract Month: {contract_info.contractMonth}\n")
        print(f"timeZoneId: {contract_info.timeZoneId}\n")


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
        
        # Initialize the CSV writer if not already done
        self._initialize_csv(fieldnames=bar_dict.keys())
        
        # Write the new row
        self.csv_writer.writerow(bar_dict)


    def historicalDataEnd(self, reqId: int, start: str, end: str):
        if self.csv_file:
            self.csv_file.close()
            print(f"Saving historical Data for Request ID {reqId} to:{self.csv_filepath}.")
            print(f"Historical Data End for Request ID {reqId}: Start Date: {start}, End Date: {end}")
            
            #self.disconnect()


    # Callback for handling market data
    def tickPrice(self, reqId, tickType, price, attrib):
        print(f"reqId: {reqId}, tickType: {TickTypeEnum.to_str(tickType)}, price: {price}, attrib: {attrib}")


    def tickSize(self, reqId, tickType, size):
        print(f"reqId: {reqId}, tickType: {TickTypeEnum.to_str(tickType)}, size: {size}")


    # Store the market data in a CSV file
    def store_market_data(self, reqId, data_type, value):        
        # Prepare data dictionary to be written to CSV
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        data_dict = {
            "Timestamp": timestamp,
            "DataType": data_type,
            "Value": value
        }
        
        # Initialize the CSV writer if not already done
        self._initialize_csv(fieldnames=data_dict.keys())
        
        # Write the data row
        self.csv_writer.writerow(data_dict)


    def tickByTickAllLast(self, reqId: int, tickType: int, time: int, price: float,
                          size: Decimal, tickAtrribLast: TickAttribLast, exchange: str,
                          specialConditions: str):        
        formatted_time = datetime.fromtimestamp(time).strftime('%Y-%m-%d %H:%M:%S')  # Adjusted reference
        
        # Data dictionary to be stored
        data = {
            "Timestamp": formatted_time,
            "Price": round(price, 4),
            "Size": int(size)
        }
        
        # Initialize the CSV writer if not already done
        self._initialize_csv(fieldnames=data.keys())
        
        # Write data to CSV
        self.csv_writer.writerow(data)


    def historicalTicksLast(self, reqId: int, ticks, done: bool):
        print(f"Historical Ticks of Last Data for ReqId {reqId}:")
        
        # Fieldnames for the CSV
        fieldnames = ["Timestamp", "Price", "Size"]
        
        # Initialize the CSV writer if not already done
        self._initialize_csv(fieldnames=fieldnames)
        
        # Process each tick and write to CSV
        for tick in ticks:
            formatted_time = datetime.fromtimestamp(tick.time).strftime('%Y-%m-%d %H:%M:%S')
            data = {
                "Timestamp": formatted_time,
                "Price": round(tick.price, 2),
                "Size": tick.size,
            }
            self.csv_writer.writerow(data)






# Create the application instance
app = TestApp()
app.connect('127.0.0.1', 7496, 2)    
time.sleep(3)

app.set_symbol(symb="NQ")
app.start()
threading.Thread(target=app.run).start()
app.stop()
app.disconnect()