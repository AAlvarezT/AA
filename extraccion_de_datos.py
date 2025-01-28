import pympi
import numpy as np
import argparse 
import os
from collections import Counter

import pandas as pd

def process_eaf_file(folder_path, file_name):

    complete_name = '\\'.join([folder_path, file_name])

    # Instead of modifying the file name, we'll keep the entire file name
    file_name_csv = file_name  # Keep the whole file name here
    file_parts = file_name.split("-")

    if '_' in file_parts[1]:
        file_parts[1] = file_parts[1].split('_')[-1]  # Make sure this is indented with 4 spaces

    year_csv = file_parts[2]
    month_csv = file_parts[3]
    day_csv = file_parts[4]

    aEAFfile = pympi.Elan.Eaf(os.path.join(complete_name))

    tiers = aEAFfile.tiers.keys()

    pd_eaf = pd.DataFrame.from_dict(aEAFfile.tiers)

    metadata = pd_eaf.iloc[2]

    pd_eaf_trans = pd_eaf.T

    pd_eaf_trans['nombre_persona'] = pd_eaf_trans.index.to_series().apply(lambda x: x.split('@')[1] if '@' in x else None)
    pd_eaf_trans['column_name'] = pd_eaf_trans.index.to_series().apply(lambda x: x.split('@')[0] if '@' in x else None)

    pd_eaf_trans = pd_eaf_trans[pd_eaf_trans['nombre_persona'] != "ShortName"]

    datos_finales = []
    for name, group in pd_eaf_trans.groupby("nombre_persona"):

        person_data = []

        for row_sent in group[0].items():
            if row_sent[0].split("@")[0] == 'trs':
                dictionary = row_sent[1]

                for sentence in dictionary.items():
                    sentence_cod = sentence[0]  # 0
                    time_init = sentence[1][0]  # 1
                    time_end = sentence[1][1]  # 2
                    sentence_data = sentence[1][2]  # 3
                    person_data.append([sentence_cod, time_init, time_end, sentence_data])

        the_row = []
        for i, per_data in enumerate(person_data):
            per_data_id = per_data[0]

            per_inf_dict = {}
            for row_inf in group[1].items():

                column = row_inf[0].split("@")[0]
                for inner_row in row_inf[1].items():
                    ref_per_data_id = inner_row[1][0]

                    if per_data_id == ref_per_data_id:
                        per_inf_dict[column] = inner_row[1][1]

            diccionario_combinado = per_inf_dict.copy()
            diccionario_combinado.update({
                'file_name': file_name_csv,  # Use the entire file name
                'short_name': folder_path.split('\\')[1],
                'date': f"{year_csv}-{month_csv}-{day_csv}",
                'utterance': per_data[3],
                "tiempo inicio": per_data[1],
                "tiempo fin": per_data[2],e
                'name': name,
                'DEFAULT_LOCALE': metadata.iloc[2].get("DEFAULT_LOCALE"),
                'PARTICIPANT': metadata.iloc[2].get("PARTICIPANT"),
                'TIER_ID': metadata.iloc[2].get("TIER_ID"),
                'PARENT_REF': metadata.iloc[2].get("PARENT_REF"),
                'LINGUISTIC_TYPE_REF': metadata.iloc[2].get("LINGUISTIC_TYPE_REF"),
            })

            datos_finales.append(diccionario_combinado)

    return datos_finales

def main():
    eaf_path_list = "DATOS"

    csv_final = []

    for folder_path, _ ,eaf_file_list in os.walk(eaf_path_list):

        for eaf_file in eaf_file_list:

            if eaf_file.split('.')[-1] != 'eaf':
                break

            archivo_bloque = process_eaf_file(folder_path, eaf_file)
            
            csv_final = csv_final + archivo_bloque
    
    df = pd.DataFrame(csv_final)

    print(df.to_csv('shp_utt_pointing_data.csv', index=False))

main()
