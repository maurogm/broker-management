# %%
import yfinance as yf
from argparse import ArgumentParser

# %% Argument parser
parser = ArgumentParser()
parser.add_argument(
    "--ticker",
    required=True,
    type=str,
    help="Ticker identification in Yahoo Finance",
)
parser.add_argument(
    "--output_path",
    required=True,
    type=str,
    help="Path to the dir where the csv will be written",
)
parser.add_argument(
    "--no_header",
    required=False,
    default = True,
    type=str,
    help="Omits the headers",
)

# %% Argument parsing
args = parser.parse_args()

TICKER = args.ticker
OUTPUT_PATH = args.output_path
NO_HEADERS = args.no_header

# %%
downloaded_data = yf.Ticker(TICKER).history(period="max").reset_index()
colunmn_names = ["Date", "currency", "Close", "Open", "High", "Low", "operated_amount", "Volume", "n_operations"]
data_adjusted = (downloaded_data
                 .assign(currency="USD")
                 .assign(operated_amount = 0)
                 .assign(n_operations = 0)
                 .assign(Date = downloaded_data['Date'].dt.strftime('%Y-%m-%dT%H:%M:%S'))
                 )[
    colunmn_names
] # Must add fields to conform to DailyData case class
if (NO_HEADERS):
    headers = False
else:
    headers = colunmn_names
data_adjusted.to_csv(
    OUTPUT_PATH,
    index=False,
    header=headers,
)
