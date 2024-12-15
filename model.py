import yfinance as yf
import pandas as pd
import sys

# Function to fetch financial data for the given symbol
def fetch_financial_data(symbol):
    # Fetch the ticker data using yfinance
    ticker = yf.Ticker(symbol)
    
    # Get the financials (Income Statement, Balance Sheet, and Cash Flow)
    income_statement = ticker.financials.transpose()
    balance_sheet = ticker.balance_sheet.transpose()
    cashflow_statement = ticker.cashflow.transpose()
    
    # Print the available columns in the income statement for diagnostics
    print("Income Statement Columns:")
    print(income_statement.columns)  # Display the column names to check the structure
    
    # Check if 'Operating Revenue' exists or if the data format is different
    if 'Operating Revenue' not in income_statement.columns:
        print("Error: 'Operating Revenue' not found. Available columns:")
        print(income_statement.columns)  # Print available columns in the income statement
        return None
    
    # Extract relevant financial data for model (Revenue, Operating Income, Net Income)
    revenue = income_statement['Operating Revenue']
    operating_income = income_statement.loc[:, 'Operating Income'] if 'Operating Income' in income_statement.columns else None
    net_income = income_statement.loc[:, 'Net Income'] if 'Net Income' in income_statement.columns else None
    
    # Number of available rows (years)
    num_years = len(revenue)
    print(f"Available years: {num_years}")  # Print the number of available years (rows)
    
    # Ensure we have at least 4 years of data (to fill Y18 to Y22)
    if num_years < 4:
        print("Error: Not enough data for 5 years.")
        return None
    
    # Collect the data for the model (5 years of financial data)
    model_data = {
        'Metric': ['Revenue', 'Operating Income', 'Net Income'],
        'Y18': [revenue.iloc[4] if num_years > 4 else None, 
                operating_income.iloc[4] if operating_income is not None and num_years > 4 else None,
                net_income.iloc[4] if net_income is not None and num_years > 4 else None],
        'Y19': [revenue.iloc[3], 
                operating_income.iloc[3] if operating_income is not None else None, 
                net_income.iloc[3] if net_income is not None else None],
        'Y20': [revenue.iloc[2], 
                operating_income.iloc[2] if operating_income is not None else None, 
                net_income.iloc[2] if net_income is not None else None],
        'Y21': [revenue.iloc[1], 
                operating_income.iloc[1] if operating_income is not None else None, 
                net_income.iloc[1] if net_income is not None else None],
        'Y22': [revenue.iloc[0], 
                operating_income.iloc[0] if operating_income is not None else None, 
                net_income.iloc[0] if net_income is not None else None]
    }
    
    # Convert the model data to a DataFrame
    df_model = pd.DataFrame(model_data)
    
    return df_model

# Function to save the data to a CSV file
def save_to_csv(df_model, symbol):
    # Save the DataFrame to a CSV file
    output_file = f"[{symbol}] Model.csv"
    df_model.to_csv(output_file, index=False)
    print(f"model saved to {output_file}")

# Main function to orchestrate the data fetching and saving process
def main(symbol):
    # Fetch the financial data for the given symbol
    df_model = fetch_financial_data(symbol)
    
    # If there was an error in fetching the data, stop further execution
    if df_model is None:
        print("Error: Could not fetch the required data.")
        return
    
    # Save the data to CSV
    save_to_csv(df_model, symbol)

# Entry point for the script
if __name__ == "__main__":
    # Get the symbol from command-line arguments
    if len(sys.argv) < 2:
        print("Please provide the stock symbol as an argument (e.g., 'ABNB')")
    else:
        symbol = sys.argv[1]
        main(symbol)

