
import ccxt
import numpy as np
from decimal import Decimal

def build_advanced_volume_profile(ohlcv, bins=100, va_percentage=0.70):
    prices = []
    volumes = []
    for candle in ohlcv:
        high, low, vol = candle[2], candle[3], candle[5]
        avg_price = (high + low) / 2
        prices.append(avg_price)
        volumes.append(vol)
    
    prices = np.array(prices)
    volumes = np.array(volumes)
    
    # Construire l'histogramme des prix pondéré par le volume
    hist, bin_edges = np.histogram(prices, bins=bins, weights=volumes)
    total_volume = sum(hist)
    
    # POC : Prix avec le volume maximal
    poc_index = np.argmax(hist)
    poc_price = (bin_edges[poc_index] + bin_edges[poc_index + 1]) / 2
    
    # Value Area : Zone couvrant va_percentage (70%) du volume
    sorted_indices = np.argsort(hist)[::-1]  # Indices des bins par volume décroissant
    cumulative_volume = 0
    selected_bins = []
    target_volume = total_volume * va_percentage
    
    for idx in sorted_indices:
        cumulative_volume += hist[idx]
        selected_bins.append(idx)
        if cumulative_volume >= target_volume:
            break
    
    val_low = bin_edges[min(selected_bins)]   # Value Area Low
    vah_high = bin_edges[max(selected_bins) + 1]  # Value Area High
    
    return poc_price, val_low, vah_high

def calculate_vwap(ohlcv):
    """
    Calcule le VWAP sur les données OHLCV (prix typique * volume / somme des volumes).
    """
    if not ohlcv:
        return 0.0
    
    total_pv = 0
    total_volume = 0
    for candle in ohlcv:
        high = Decimal(str(candle[2]))
        low = Decimal(str(candle[3]))
        close = Decimal(str(candle[4]))
        volume = Decimal(str(candle[5]))
        typical_price = (high + low + close) / 3
        total_pv += typical_price * volume
        total_volume += volume
    
    vwap = float(total_pv / total_volume) if total_volume > 0 else 0.0
    return vwap

def check_liquidity_and_volatility(exchange, symbol, body_candle_timestamp, timeframe="1d", min_avg_volume_usd=500_000, max_atr_ratio=0.20, atr_period=14):
    try:
        # Calculer le point de départ pour les données (30 jours + ATR avant N-k)
        since = body_candle_timestamp - (30 + atr_period) * 24 * 60 * 60 * 1000  # En millisecondes
        # Filtrer les données AVANT body_candle_timestamp uniquement
        ohlcv = exchange.fetch_ohlcv(symbol, timeframe, since=since, limit=atr_period + 60)
        ohlcv = [c for c in ohlcv if c[0] < body_candle_timestamp]
        
        # Vérifier qu’on a assez de bougies
        if len(ohlcv) < atr_period + 30:
            print(f"{symbol}: Pas assez de données pour liquidité/volatilité")
            return False
        
        # Liquidité : volume moyen en USD sur 30 jours avant N-k
        volumes_usd = [c[4] * c[5] for c in ohlcv[-30:]]  # Prix de clôture * volume
        avg_volume_usd = sum(volumes_usd) / len(volumes_usd) if volumes_usd else 0
        
        if avg_volume_usd < min_avg_volume_usd:
            print(f"{symbol}: Volume moyen USD ({avg_volume_usd:.2f}) < {min_avg_volume_usd}")
            return False
        
        # Volatilité : Calcul ATR sur 14 jours avant N-k
        tr_values = []
        for i in range(1, len(ohlcv)):
            high, low, prev_close = ohlcv[i][2], ohlcv[i][3], ohlcv[i-1][4]
            tr = max(high - low, abs(high - prev_close), abs(low - prev_close))
            tr_values.append(tr)
        
        atr = sum(tr_values[-atr_period:]) / atr_period if len(tr_values) >= atr_period else 0
        current_price = ohlcv[-1][4]
        atr_ratio = atr / current_price if current_price > 0 else 0
        
        if atr_ratio > max_atr_ratio:
            print(f"{symbol}: ATR ratio ({atr_ratio:.4f}) > {max_atr_ratio}")
            return False
        
        return True
    except Exception as e:
        print(f"Erreur dans filtre pour {symbol}: {e}")
        return False


def scan_candidates(min_volume_usd=1_000_000, body_ratio_threshold=0.05, n_weeks_back=2, vp_days=40, bins=100):
    exchange = ccxt.bybit()
    markets = exchange.load_markets()
    usdt_pairs = [s for s in markets if s.endswith('/USDT') and ':' not in s]

    candidates = []
    total_pairs = len(usdt_pairs)
    processed = 0

    for symbol in usdt_pairs:
        # Filtre tokens levier
        if any(x in symbol for x in ['UP/', 'DOWN/', 'BULL/', 'BEAR/', '1000']):
            continue

        try:
            # ---- Détection en weekly ----
            ohlcv_w = exchange.fetch_ohlcv(symbol, '1w', limit=n_weeks_back + 8)
            processed += 1
            print(f"Progression : {processed}/{total_pairs} ({symbol})")

            if len(ohlcv_w) < n_weeks_back + 1:
                continue

            body_candle = ohlcv_w[-n_weeks_back]        # semaine N-k
            reaction_candle = ohlcv_w[-(n_weeks_back-1)]  # semaine suivante
            body_candle_timestamp = body_candle[0]

            _open, high, low, close, volume = body_candle[1:6]
            _open  = float(body_candle[1])
            high   = float(body_candle[2])
            low    = float(body_candle[3])
            close  = float(body_candle[4])
            volume = float(body_candle[5])
            body = float(abs(close - _open))
            total = float(high - low) if high - low != 0 else 1e-8
            ratio = body / total
            upper_wick = high - max(_open, close)
            lower_wick = min(_open, close) - low
            wick_total = upper_wick + lower_wick

            #  Nouveau filtre anti-mèche
            wick_ratio = wick_total / body if body != 0 else 999
            
            volume_usd = float(volume * close)

            if ratio < body_ratio_threshold  and volume_usd > min_volume_usd:
                # Vérifier liquidité et volatilité
                if not check_liquidity_and_volatility(exchange, symbol, body_candle_timestamp):
                    print(f"{symbol}: Écarté pour manque de liquidité ou volatilité excessive")
                    continue
                
                
                # ---- Volume Profile en daily sur période récente ----
                # Récupère vp_days daily avant la bougie N-k
                since = body_candle_timestamp - vp_days * 24 * 60 * 60 * 1000
                ohlcv_d = exchange.fetch_ohlcv(symbol, '1d', since=since, limit=vp_days)

                history = [c for c in ohlcv_d if c[0] < body_candle_timestamp]
                if len(history) < 10:  # pas assez de données
                    continue

                poc_price, val_low, vah_high = build_advanced_volume_profile(history, bins=bins)
                vwap = calculate_vwap(history)
                if close <= val_low:
                    position_vs_va = "EN-DESSOUS VAL"
                elif close >= vah_high:
                    position_vs_va = "AU-DESSUS VAH"
                else:
                    position_vs_va = "DANS VA"

                # ---- Mouvement réaction ----
                react_open, react_high, react_low = reaction_candle[1:4]
                pump = (react_high - react_open) / react_open
                dump = (react_low - react_open) / react_open

                if pump >= 0.10:
                    direction = "PUMP 📈"
                elif dump <= -0.10:
                    direction = "DUMP 📉"
                else:
                    direction = "STABLE ⏸️"

                # ---- Append résultat ----
                candidates.append((
                    symbol,
                    round(ratio, 3),
                    round(float(volume), 2),
                    round(volume_usd, 2),
                    direction,
                    body_candle_timestamp,
                    position_vs_va,
                    round(float(poc_price), 4),
                    round(float(val_low), 4),
                    round(float(vah_high), 4),
                    round(vwap,4)

                ))

        except Exception as e:
            print(f"Erreur pour {symbol}: {e}")
            continue

    return sorted(candidates, key=lambda x: x[1])

# -----------------------
# Exécution
# -----------------------
if __name__ == "__main__":
    n_weeks_back = 2  # Changez pour tester N-6, N-10, etc.
    candidates = scan_candidates(n_weeks_back)
    
    print(f"\n✅ Liste des candidats avant filtre (semaine N-{n_weeks_back}) :\n")
    if candidates:
        for candidate in candidates:
            print(candidate)  # Copiez cette liste pour le code 2
    else:
        print("Aucun candidat trouvé.")
