#Programa de cambio TXTs
import tkinter as tk
from tkinter import filedialog, messagebox
import pandas as pd
import os

# Función para cargar y procesar el archivo
def load_txt_to_dataframe(file_path):
    with open(file_path, 'r', encoding='ISO-8859-1') as f:
        lines = f.readlines()
    
    data = []
    for line in lines:
        row = line.strip().split('|')
        
        # Verificar si el valor en la columna 21 es negativo y moverlo a la columna 24 sin cambiar el signo
        try:
            if float(row[20]) < 0:  # Comprobamos si el valor en la columna 21 es negativo
                row[23] = row[20]  # Pasar el valor negativo a la columna 24
                row[20] = float(0.00)  # Poner 0 en la columna 21
        except ValueError:
            pass
        
        data.append(row)
    
    df = pd.DataFrame(data)
    
    # Redondear todos los valores numéricos a 2 decimales
    # df = df.applymap(lambda x: round(float(x), 2) if x.replace('.', '', 1).isdigit() else x)

    df.columns = [f'Col_{i+1}' for i in range(df.shape[1])]
    
    return df

# Función para seleccionar el archivo y procesarlo
def process_file():
    # Abrir el cuadro de diálogo para seleccionar un archivo
    file_path = filedialog.askopenfilename(title="Seleccionar archivo TXT", filetypes=(("Text files", "*.txt"),))
    
    if not file_path:
        return
    
    try:
        # Procesar el archivo
        df = load_txt_to_dataframe(file_path)
        
        # Obtener el nombre del archivo original
        file_name = os.path.basename(file_path)
        
        # Asegurarse de que el nombre del archivo termine en ".txt"
        if not file_name.endswith(".txt"):
            file_name += ".txt"
        
        # Obtener el directorio donde se guardará el archivo procesado
        output_directory = filedialog.askdirectory(title="Seleccionar ubicación para guardar el archivo procesado")
        
        if not output_directory:
            return
        
        # Crear la ruta completa para el archivo procesado con el mismo nombre que el original
        output_file_path = os.path.join(output_directory, file_name)
        
        # Guardar el DataFrame procesado en el archivo con el mismo nombre
        df.to_csv(output_file_path, sep='|', index=False, header=False)
        
        # Mostrar un mensaje de éxito
        messagebox.showinfo("Éxito", f"Archivo procesado y guardado como {output_file_path}")
    
    except Exception as e:
        messagebox.showerror("Error", f"Hubo un problema al procesar el archivo: {e}")

# Crear la ventana principal
root = tk.Tk()
root.title("Procesador de Archivos TXT")

# Crear un label para la bienvenida
welcome_label = tk.Label(root, text="Bienvenido al Procesador de Archivos TXT", font=("Arial", 14))
welcome_label.pack(pady=20)

# Crear un botón para cargar el archivo
process_button = tk.Button(root, text="Cargar archivo TXT", command=process_file, font=("Arial", 12), width=20)
process_button.pack(pady=10)

# Iniciar la interfaz gráfica
root.mainloop()

#Arturo Alvarez de la Torre 05042025