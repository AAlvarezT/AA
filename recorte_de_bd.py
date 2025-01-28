#%%
import pandas as pd
import numpy as np

#%% Cargar dataset
df = pd.read_excel("C:/Users/ADMIN/Desktop/UP/UP 2024/CICLO 2024 2/Álgebra Lineal Aplicada/BD_Fubtol/base_futbol.xlsx")

# %% Ver info del dataset
df.head
df.info
df.describe
#%% ver el nombre de las columnas específicas
df.columns 
#%%Seleccionar columnas
columnas_elegidas = [
    'appearance_id',
    'player_id', 
    'yellow_cards', 
    'red_cards', 
    'goals', 
    'assists', 
    'minutes_played', 
    'players.height_in_cm', 
    'players.market_value_in_eur', 
    'clubs.foreigners_percentage', 
    'clubs.national_team_players', 
    'clubs.stadium_seats', 
    'clubs.net_transfer_record', 
    'players.date_of_birth', 
    'date'
]

df1 = df[columnas_elegidas]
df_2=df1[df1['date']>'2020-12-31']
#%%reemplazamos fecha y fecha de nac por edad a la aparición
df_2['date'] = pd.to_datetime(df_2['date'], errors='coerce')  # Convierte 'date' a formato datetime
df_2['players.date_of_birth'] = pd.to_datetime(df_2['players.date_of_birth'], errors='coerce')  # Convierte 'players.date_of_birth' a formato datetime
# Crear la columna 'age_appearance' calculando la diferencia en años
df_2['age_appearance'] = (df_2['date'] - df_2['players.date_of_birth']).dt.days / 365.25
df_2['age_appearance'] = df_2['age_appearance'].round(2)

df_futbol = df_2.drop(columns=['date','players.date_of_birth'])
#%%
df_futbol.info
df_futbol.columns

# %%
df_futbol = df_futbol.to_excel('basesita_futbol_ALA.xlsx','hoja1',index=False)

