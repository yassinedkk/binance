import requests
import pandas as pd
from datetime import datetime, timedelta
import pytz
import os

def get_yesterday_open_interest():
    symbol = "BTCUSDT"
    timeframe = "1h"
    utc = pytz.UTC
    now = datetime.now(utc)

    # Plage UTC de la veille : 00h à 23h inclus
    start = now.replace(hour=0, minute=0, second=0, microsecond=0) - timedelta(days=1)
    end = start + timedelta(hours=23, minutes=59)

    params = {
        "symbol": symbol,
        "period": timeframe,
        "startTime": int((start - timedelta(hours=12)).timestamp() * 1000),
        "endTime": int((end + timedelta(hours=12)).timestamp() * 1000),
        "limit": 500
    }

    url = "https://binance-fly-purple-wind-3762.fly.dev/futures/data/openInterestHist"
    try:
        r = requests.get(url, params=params, timeout=10)
        r.raise_for_status()
        data = r.json()
        df = pd.DataFrame(data)
        if df.empty:
            return None

        df['timestamp'] = pd.to_datetime(df['timestamp'], unit='ms', utc=True)
        df = df[(df['timestamp'] >= start) & (df['timestamp'] <= end)]

        df = df.rename(columns={
            "timestamp": "Date_Heure",
            "sumOpenInterest": "Open_Interest_BTC",
            "sumOpenInterestValue": "Open_Interest_USDT"
        })

        df["Date"] = df["Date_Heure"].dt.date
        df["Heure"] = df["Date_Heure"].dt.hour
        df["Open_Interest_BTC"] = pd.to_numeric(df["Open_Interest_BTC"], errors="coerce")
        df["Open_Interest_USDT"] = pd.to_numeric(df["Open_Interest_USDT"], errors="coerce")

        return df[["Date_Heure", "Date", "Heure", "Open_Interest_BTC", "Open_Interest_USDT"]]

    except Exception as e:
        print("Erreur récupération :", e)
        return None

def save_cumulative(df_today, filename="btc_oi_cumule.csv"):
    if os.path.exists(filename):
        df_existing = pd.read_csv(filename, parse_dates=["Date_Heure"])
        df_total = pd.concat([df_existing, df_today]).drop_duplicates(subset=["Date_Heure"])
    else:
        df_total = df_today

    df_total = df_total.sort_values("Date_Heure")
    df_total.to_csv(filename, index=False)
    print(f"Données sauvegardées dans {filename} (total: {len(df_total)} lignes)")

if __name__ == "__main__":
    print(" Récupération Open Interest - Veille")
    df = get_yesterday_open_interest()
    if df is not None:
        print(f" Données récupérées: {len(df)} lignes")
        save_cumulative(df)
    else:
        print(" Aucune donnée récupérée")
