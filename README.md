# Introduction
This is my ongoing project that includes functionalities for streaming financial data, processing historical datasets, and performing technical analysis to support algorithmic trading strategies. The repository integrates Interactive Brokers' TWS API and advanced statistical and machine learning techniques to create a comprehensive toolkit for trading-related tasks.

## 1. Data Acquisition
The first functionality of this project focuses on acquiring financial market data through Interactive Brokers' TWS socket API. The following operations are implemented:
Streaming Tick Data: Real-time tick data of a selected financial instrument is continuously streamed and saved locally as DataFrames for further analysis.
Batch Data Requests: Historical data for instruments can be retrieved via batch processing, enabling robust backtesting and modeling.
Core Methods Used:
EClient: Used for sending requests to the TWS API, including streaming subscriptions and historical data queries.
EWrapper: Handles incoming messages, such as tick data updates, historical data responses, and error notifications.

## 2. Technical Analysis
The second functionality involves performing technical analysis on the acquired data to support algorithmic trading. Key techniques and features include:
Interactive Graphing: Create dynamic and interactive visualizations using R's Plotly package, enabling intuitive exploration of data trends and patterns.
LSTM RNNs: Leverage Long Short-Term Memory (LSTM) Recurrent Neural Networks for modeling time-series data and predicting future trends.
Bootstrap Methods: Apply statistical bootstrap techniques to estimate confidence intervals and assess the robustness of trading strategies.
These methods allow for in-depth analysis and informed decision-making in developing and testing trading algorithms.

# Content

## 1. Prerequisites
Python 3.8+ and R installed on your system.
Interactive Brokers Trader Workstation (TWS) installed and running.
Required Python and R libraries are included in requirements.txt.
A SQL server is recommended if data storage and management are part of your workflow.

## 2. Usage
Streaming Tick Data: To stream tick data for a specific financial instrument, configure the StreamData.py script according to your requirements.

Historical Data Requests:
Use the reqHistoricalData method within the TestApp class to request historical data in Python.
Alternatively, execute batch data requests directly in R.
For batch data download, interactive visualization, and metrics calculation, refer to the USFutures.R file, which serves as the main script for these tasks.

## 3. Data Storage
Data can be saved locally in CSV files or managed within a SQL database for improved organization and scalability.
If using a SQL database, you’ll need to set up your own server connection and configure databases to meet your specific requirements.