import os
from tkinter import Tk, Toplevel, Label, Button, messagebox, filedialog, Checkbutton, IntVar, Frame, Scrollbar, Canvas
import win32api
import re
def alfanumerico_natural(archivo):
    """Divide los nombres de los archivos en partes alfanuméricas para ordenarlos correctamente."""
    return [int(c) if c.isdigit() else c for c in re.split(r'(\d+)', archivo)]

# Función para seleccionar la carpeta
def seleccionar_carpeta():
    return filedialog.askdirectory(title="Selecciona la carpeta con las imágenes")

# Función para imprimir una imagen
def imprimir_imagen(ruta_imagen):
    try:
        visor_imagen = "mspaint.exe"  # Paint en Windows
        win32api.ShellExecute(0, "print", visor_imagen, f'"{ruta_imagen}"', ".", 0)
        print(f"Imprimiendo: {ruta_imagen}")
    except Exception as e:
        print(f"Error al imprimir {ruta_imagen}: {e}")

# Función para mostrar todos los archivos con casillas de verificación y scroll
def mostrar_archivos_para_imprimir(carpeta, archivos):
    top = Toplevel()
    top.title("Seleccionar Archivos para Imprimir")

    Label(top, text="Selecciona los archivos que deseas imprimir:").pack(pady=10)

    # Crear canvas para scroll
    canvas = Canvas(top, width=600, height=400)
    scrollbar = Scrollbar(top, orient="vertical", command=canvas.yview)
    scrollable_frame = Frame(canvas)

    # Configurar el scroll
    scrollable_frame.bind(
        "<Configure>",
        lambda e: canvas.configure(scrollregion=canvas.bbox("all"))
    )

    canvas.create_window((0, 0), window=scrollable_frame, anchor="nw")
    canvas.configure(yscrollcommand=scrollbar.set)

    canvas.pack(side="left", fill="both", expand=True)
    scrollbar.pack(side="right", fill="y")

    # Variables para los checkboxes
    variables = []
    for archivo in archivos:
        var = IntVar()
        variables.append((archivo, var))
        Checkbutton(scrollable_frame, text=archivo, variable=var, anchor="w").pack(fill="x", padx=5, pady=2)

    # Función para imprimir los seleccionados
    def imprimir_seleccionados():
        seleccionados = [os.path.join(carpeta, archivo) for archivo, var in variables if var.get() == 1]
        if not seleccionados:
            messagebox.showerror("Error", "No has seleccionado ningún archivo para imprimir.")
            return

        respuesta = messagebox.askyesno("Confirmar impresión", f"Se imprimirán {len(seleccionados)} archivos. ¿Deseas continuar?")
        if not respuesta:
            return

        for archivo in seleccionados:
            imprimir_imagen(archivo)

        messagebox.showinfo("Proceso completado", "Todos los archivos seleccionados se han enviado a la impresora.")
        top.destroy()

    # Botón para imprimir seleccionados
    Button(top, text="Imprimir seleccionados", command=imprimir_seleccionados).pack(pady=10)

    top.mainloop()

# Función principal
def iniciar_programa():
    root = Tk()
    root.withdraw()  # Ocultar ventana principal

    try:
        # Seleccionar carpeta
        carpeta = seleccionar_carpeta()
        if not carpeta:
            messagebox.showerror("Error", "No se seleccionó ninguna carpeta.")
            return

        # Listar y ordenar los archivos PNG en la carpeta (alfanumérico descendente)
        archivos = sorted([f for f in os.listdir(carpeta) if f.endswith(".png")], key=alfanumerico_natural, reverse=True)
        if not archivos:
            messagebox.showinfo("Sin resultados", "No se encontraron archivos PNG en la carpeta seleccionada.")
            return

        # Mostrar la lista de archivos con checkboxes y scroll
        mostrar_archivos_para_imprimir(carpeta, archivos)

    finally:
        root.destroy()  # Cerrar la ventana raíz

# Ejecutar el programa
if __name__ == "__main__":
    iniciar_programa()
