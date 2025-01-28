import tkinter as tk
from tkinter import ttk, messagebox, filedialog
from PIL import Image, ImageDraw, ImageFont
from datetime import datetime
from dateutil.relativedelta import relativedelta
import os
import calendar
import sys

# Determinar la ruta base (relativa para incluir recursos en el .exe)
if getattr(sys, "frozen", False):  # Si está ejecutándose como .exe
    base_path = sys._MEIPASS
else:
    base_path = os.path.dirname(__file__)

# Ruta relativa a la imagen base
imagen_base = os.path.join(base_path, "Letra_base.png")

# Función para obtener el último día del mes
def obtener_ultimo_dia_mes(fecha):
    mes = fecha.month
    año = fecha.year
    return calendar.monthrange(año, mes)[1]

# Nueva función para ajustar texto a un espacio
def dibujar_texto_ajustado(draw, text, position, font, max_width):
    palabras = text.split()
    linea_actual = ""
    y_offset = 0
    espacio_entre_lineas = 22

    for palabra in palabras:
        prueba_linea = f"{linea_actual} {palabra}".strip()
        ancho_texto = draw.textbbox((0, 0), prueba_linea, font=font)[2]
        if ancho_texto <= max_width:
            linea_actual = prueba_linea
        else:
            draw.text((position[0], position[1] + y_offset), linea_actual, fill="black", font=font)
            linea_actual = palabra
            y_offset += espacio_entre_lineas

    if linea_actual:
        draw.text((position[0], position[1] + y_offset), linea_actual, fill="black", font=font)

# Generador de letras
def generar_letras_con_imagen_v2(imagen_base, numero_inicial, cuotas, fecha_giro, fecha_primer_vencimiento, datos_generales, vencimiento_opcion, carpeta_destino):
    try:
        # Cargar la imagen base y la fuente
        base_ticket = Image.open(imagen_base)
        font = ImageFont.truetype("arial.ttf", 15)
    except Exception as e:
        return f"Error al cargar la imagen base o fuente: {e}"

    try:
        # Parsear las fechas
        fecha_giro_dt = datetime.strptime(fecha_giro, "%d%m%Y")
        fecha_giro_str = fecha_giro_dt.strftime("%d/%m/%Y")

        fecha_primer_vencimiento_dt = datetime.strptime(fecha_primer_vencimiento, "%d%m%Y")
    except ValueError:
        return "Las fechas deben estar en formato DDMMYYYY."

    # Ensure the destination folder exists
    os.makedirs(carpeta_destino, exist_ok=True)

    imagenes = []
    cuotas_especificas = datos_generales.get("cuotas_especificas", [])

    monto_primero = cuotas_especificas[0]["monto"]
    palabras_primero = cuotas_especificas[0]["palabras"]

    monto_intermedio = cuotas_especificas[1]["monto"] if len(cuotas_especificas) > 1 else "0.00"
    palabras_intermedio = cuotas_especificas[1]["palabras"] if len(cuotas_especificas) > 1 else ""

    monto_ultimo = cuotas_especificas[-1]["monto"] if len(cuotas_especificas) > 2 else "0.00"
    palabras_ultimo = cuotas_especificas[-1]["palabras"] if len(cuotas_especificas) > 2 else ""

    for i in range(cuotas):
        try:
            ticket = base_ticket.copy()
            draw = ImageDraw.Draw(ticket)

            numero = numero_inicial + i
            if i == 0:
                fecha_vencimiento = fecha_primer_vencimiento_dt
            else:
                fecha_vencimiento = fecha_primer_vencimiento_dt + relativedelta(months=i)
                if vencimiento_opcion == "A fin de mes":
                    ultimo_dia = obtener_ultimo_dia_mes(fecha_vencimiento)
                    fecha_vencimiento = fecha_vencimiento.replace(day=ultimo_dia)
                elif vencimiento_opcion == "En quincena":
                    fecha_vencimiento = fecha_vencimiento.replace(day=15)
            fecha_vencimiento_str = fecha_vencimiento.strftime("%d/%m/%Y")

            if i == 0:
                cuota_texto = monto_primero
                monto_palabras = palabras_primero
            elif i == cuotas - 1:
                cuota_texto = monto_ultimo
                monto_palabras = palabras_ultimo
            else:
                cuota_texto = monto_intermedio
                monto_palabras = palabras_intermedio

            nombre_girado = datos_generales.get("girado", "")
            dni_girado = datos_generales.get("dni", "")
            domicilio = datos_generales.get("domicilio", "")
            telefono = datos_generales.get("telefono", "")
            referencia_girador = datos_generales.get("referencia", "")

            draw.text((255, 60), str(numero), fill="black", font=font)
            draw.text((900, 60), cuota_texto.upper(), fill="black", font=font)
            draw.text((770, 70), fecha_vencimiento_str.upper(), fill="black", font=font)
            draw.text((225, 160), monto_palabras.upper(), fill="black", font=font)
            dibujar_texto_ajustado(draw, nombre_girado.upper(), (270, 222), font, max_width=300)
            draw.text((270, 295), dni_girado.upper(), fill="black", font=font)
            draw.text((277, 270), domicilio.upper(), fill="black", font=font)
            draw.text((485, 295), telefono.upper(), fill="black", font=font)
            draw.text((360, 60), referencia_girador.upper(), fill="black", font=font)
            draw.text((630, 70), fecha_giro_str.upper(), fill="black", font=font)

            draw.text((520, 60), "LIMA", fill="black", font=font)
            draw.text((240, 120), "COLEGIO DE NOTARIOS DE LIMA", fill="black", font=font)
            draw.text((610, 197), "AV, GIUSEPPE GARIBALDI 343, JESÚS MARÍA", fill="black", font=font)
            draw.text((755, 310), "COLEGIO DE NOTARIOS DE LIMA", fill="black", font=font)
            draw.text((755, 335), "RUC: 20122535917", fill="black", font=font)

            # Combine images in pairs
            if (i + 1) % 2 == 0 or i == cuotas - 1:
                img1 = imagenes[-1]["imagen"]
                img2 = ticket if i == cuotas - 1 else ticket

                nueva_imagen = Image.new("RGB", (img1.width, img1.height * 2), "white")
                nueva_imagen.paste(img1, (0, 0))
                nueva_imagen.paste(img2, (0, img1.height))

                numero_letra_1 = imagenes[-1]["numero"]
                numero_letra_2 = numero if i == cuotas - 1 else numero

                nombre_girado_sanitizado = "".join(
                    c for c in nombre_girado if c.isalnum() or c in ['_', '-']
                )
                nombre_archivo = os.path.join(
                    carpeta_destino,
                    f"Letra_Combinada_{nombre_girado_sanitizado}_{numero_letra_1}_{numero_letra_2}.png"
                )

                # Ensure the directory exists before saving
                os.makedirs(os.path.dirname(nombre_archivo), exist_ok=True)
                
                # Save the combined image
                nueva_imagen.save(nombre_archivo)

            # Add current image to the list of images
            imagenes.append({"imagen": ticket, "numero": numero, "fecha_vencimiento": fecha_vencimiento_str})
        except Exception as e:
            print(f"Error al generar la imagen para la cuota {i + 1}: {e}")
            continue

    return "Proceso completado con éxito."

def enviar_datos(
    entry_numero_inicial,
    entry_cuotas,
    entry_fecha_giro,
    entry_primer_vencimiento,
    entry_girado,
    entry_dni,
    entry_domicilio,
    entry_telefono,
    entry_referencia,
    combo_vencimiento,
    datos_cuotas,  # Recoge las cuotas aquí
):
    """
    Recopilar todos los datos y generar las letras con montos específicos para cada cuota.
    """
    datos_generales = {
        "numero_inicial": entry_numero_inicial.get(),
        "cuotas": int(entry_cuotas.get()),
        "fecha_giro": entry_fecha_giro.get(),
        "fecha_primer_vencimiento": entry_primer_vencimiento.get(),
        "girado": entry_girado.get(),
        "dni": entry_dni.get(),
        "domicilio": entry_domicilio.get(),
        "telefono": entry_telefono.get(),
        "referencia": entry_referencia.get(),
        "vencimiento": combo_vencimiento.get(),
        "cuotas_especificas": datos_cuotas,  # Lista con datos de cada cuota
    }

    resultado = generar_letras_con_imagen_v2(
        imagen_base,
        int(datos_generales["numero_inicial"]),
        datos_generales["cuotas"],
        datos_generales["fecha_giro"],
        datos_generales["fecha_primer_vencimiento"],
        datos_generales,
        datos_generales["vencimiento"],
        filedialog.askdirectory(title="Selecciona la carpeta para guardar las letras"),
    )
    messagebox.showinfo("Resultado", resultado)


def abrir_ventana_cuotas_iguales():
    ventana_cuotas_iguales = tk.Toplevel()
    ventana_cuotas_iguales.title("Generador de Letras - Cuotas Iguales")

    # Crear campos de entrada
    tk.Label(ventana_cuotas_iguales, text="Número inicial de la letra de cambio:").grid(row=0, column=0, padx=10, pady=5, sticky="e")
    entry_numero_inicial = tk.Entry(ventana_cuotas_iguales)
    entry_numero_inicial.grid(row=0, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_iguales, text="Número total de cuotas:").grid(row=1, column=0, padx=10, pady=5, sticky="e")
    entry_cuotas = tk.Entry(ventana_cuotas_iguales)
    entry_cuotas.grid(row=1, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_iguales, text="Fecha de Giro (DDMMYYYY):").grid(row=2, column=0, padx=10, pady=5, sticky="e")
    entry_fecha_giro = tk.Entry(ventana_cuotas_iguales)
    entry_fecha_giro.grid(row=2, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_iguales, text="Fecha del Primer Vencimiento (DDMMYYYY):").grid(row=3, column=0, padx=10, pady=5, sticky="e")
    entry_primer_vencimiento = tk.Entry(ventana_cuotas_iguales)
    entry_primer_vencimiento.grid(row=3, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_iguales, text="Monto de la cuota:").grid(row=4, column=0, padx=10, pady=5, sticky="e")
    entry_cuota = tk.Entry(ventana_cuotas_iguales)
    entry_cuota.grid(row=4, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_iguales, text="Monto en palabras:").grid(row=5, column=0, padx=10, pady=5, sticky="e")
    entry_monto_palabras = tk.Entry(ventana_cuotas_iguales)
    entry_monto_palabras.grid(row=5, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_iguales, text="Nombre del girado:").grid(row=6, column=0, padx=10, pady=5, sticky="e")
    entry_girado = tk.Entry(ventana_cuotas_iguales)
    entry_girado.grid(row=6, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_iguales, text="DNI del girado:").grid(row=7, column=0, padx=10, pady=5, sticky="e")
    entry_dni = tk.Entry(ventana_cuotas_iguales)
    entry_dni.grid(row=7, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_iguales, text="Domicilio del girado:").grid(row=8, column=0, padx=10, pady=5, sticky="e")
    entry_domicilio = tk.Entry(ventana_cuotas_iguales)
    entry_domicilio.grid(row=8, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_iguales, text="Teléfono del girado:").grid(row=9, column=0, padx=10, pady=5, sticky="e")
    entry_telefono = tk.Entry(ventana_cuotas_iguales)
    entry_telefono.grid(row=9, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_iguales, text="Referencia del girador:").grid(row=10, column=0, padx=10, pady=5, sticky="e")
    entry_referencia = tk.Entry(ventana_cuotas_iguales)
    entry_referencia.grid(row=10, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_iguales, text="Vencimiento:").grid(row=11, column=0, padx=10, pady=5, sticky="e")
    combo_vencimiento = ttk.Combobox(ventana_cuotas_iguales, values=["A fin de mes", "En quincena"])
    combo_vencimiento.grid(row=11, column=1, padx=10, pady=5)
    combo_vencimiento.current(0)

    # Botón para generar letras
    tk.Button(
        ventana_cuotas_iguales,
        text="Generar Letras",
        command=lambda: [
            generar_datos_cuotas_iguales(
                entry_numero_inicial,
                entry_cuotas,
                entry_fecha_giro,
                entry_primer_vencimiento,
                entry_cuota,
                entry_monto_palabras,
                entry_girado,
                entry_dni,
                entry_domicilio,
                entry_telefono,
                entry_referencia,
                combo_vencimiento,
            )
        ],
    ).grid(row=12, column=0, columnspan=2, pady=20)

def generar_datos_cuotas_iguales(
    entry_numero_inicial,
    entry_cuotas,
    entry_fecha_giro,
    entry_primer_vencimiento,
    entry_cuota,
    entry_monto_palabras,
    entry_girado,
    entry_dni,
    entry_domicilio,
    entry_telefono,
    entry_referencia,
    combo_vencimiento,
):
    """
    Genera todas las cuotas iguales y envía los datos para procesar las letras.
    """
    datos_cuotas = []
    total_cuotas = int(entry_cuotas.get())

    # Generar cuotas iguales
    for _ in range(total_cuotas):
        datos_cuotas.append({
            "monto": entry_cuota.get(),
            "palabras": entry_monto_palabras.get(),
        })

    # Llamar a enviar_datos con las cuotas generadas
    enviar_datos(
        entry_numero_inicial,
        entry_cuotas,
        entry_fecha_giro,
        entry_primer_vencimiento,
        entry_girado,
        entry_dni,
        entry_domicilio,
        entry_telefono,
        entry_referencia,
        combo_vencimiento,
        datos_cuotas,
    )


def abrir_ventana_cuotas_diferentes():
    ventana_cuotas_diferentes = tk.Toplevel()
    ventana_cuotas_diferentes.title("Generador de Letras - Cuotas Diferentes")

    # Crear campos de entrada
    tk.Label(ventana_cuotas_diferentes, text="Número inicial de la letra de cambio:").grid(row=0, column=0, padx=10, pady=5, sticky="e")
    entry_numero_inicial = tk.Entry(ventana_cuotas_diferentes)
    entry_numero_inicial.grid(row=0, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_diferentes, text="Número total de cuotas:").grid(row=1, column=0, padx=10, pady=5, sticky="e")
    entry_cuotas = tk.Entry(ventana_cuotas_diferentes)
    entry_cuotas.grid(row=1, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_diferentes, text="Fecha de Giro (DDMMYYYY):").grid(row=2, column=0, padx=10, pady=5, sticky="e")
    entry_fecha_giro = tk.Entry(ventana_cuotas_diferentes)
    entry_fecha_giro.grid(row=2, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_diferentes, text="Fecha del Primer Vencimiento (DDMMYYYY):").grid(row=3, column=0, padx=10, pady=5, sticky="e")
    entry_primer_vencimiento = tk.Entry(ventana_cuotas_diferentes)
    entry_primer_vencimiento.grid(row=3, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_diferentes, text="Nombre del girado:").grid(row=4, column=0, padx=10, pady=5, sticky="e")
    entry_girado = tk.Entry(ventana_cuotas_diferentes)
    entry_girado.grid(row=4, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_diferentes, text="DNI del girado:").grid(row=5, column=0, padx=10, pady=5, sticky="e")
    entry_dni = tk.Entry(ventana_cuotas_diferentes)
    entry_dni.grid(row=5, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_diferentes, text="Domicilio del girado:").grid(row=6, column=0, padx=10, pady=5, sticky="e")
    entry_domicilio = tk.Entry(ventana_cuotas_diferentes)
    entry_domicilio.grid(row=6, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_diferentes, text="Teléfono del girado:").grid(row=7, column=0, padx=10, pady=5, sticky="e")
    entry_telefono = tk.Entry(ventana_cuotas_diferentes)
    entry_telefono.grid(row=7, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_diferentes, text="Referencia del girador:").grid(row=8, column=0, padx=10, pady=5, sticky="e")
    entry_referencia = tk.Entry(ventana_cuotas_diferentes)
    entry_referencia.grid(row=8, column=1, padx=10, pady=5)

    tk.Label(ventana_cuotas_diferentes, text="Vencimiento:").grid(row=9, column=0, padx=10, pady=5, sticky="e")
    combo_vencimiento = ttk.Combobox(ventana_cuotas_diferentes, values=["A fin de mes", "En quincena"])
    combo_vencimiento.grid(row=9, column=1, padx=10, pady=5)
    combo_vencimiento.current(0)

    # Botón para continuar al siguiente paso
    tk.Button(
        ventana_cuotas_diferentes,
        text="Siguiente",
        command=lambda: abrir_flujo_captura_cuotas(
            entry_numero_inicial, entry_cuotas, entry_fecha_giro, entry_primer_vencimiento, entry_girado, entry_dni, entry_domicilio, entry_telefono, entry_referencia, combo_vencimiento
        ),
    ).grid(row=10, column=0, columnspan=2, pady=20)

def abrir_flujo_captura_cuotas(entry_numero_inicial, entry_cuotas, entry_fecha_giro, entry_primer_vencimiento, entry_girado, entry_dni, entry_domicilio, entry_telefono, entry_referencia, combo_vencimiento):
    """
    Inicia el flujo de captura para primera, intermedia y última cuota con navegación.
    """

    datos_cuotas = []  # Lista para almacenar los datos de todas las cuotas específicas
    ventanas = []  # Lista para almacenar referencias a las ventanas abiertas

    def capturar_cuota(titulo, texto_monto, texto_palabras, callback, datos_cuotas, ventana_anterior=None, *args):
        """
        Generaliza la captura de cuotas específicas (primera, intermedia, última).
        """
        # Ocultar la ventana anterior si existe
        if ventana_anterior:
            ventana_anterior.withdraw()

        # Crear una nueva ventana
        ventana_cuota = tk.Toplevel()
        ventana_cuota.title(titulo)

        # Almacenar la ventana en el historial
        ventanas.append(ventana_cuota)

        # Crear campos de entrada
        tk.Label(ventana_cuota, text=texto_monto).grid(row=0, column=0, padx=10, pady=5, sticky="e")
        entry_monto = tk.Entry(ventana_cuota)
        entry_monto.grid(row=0, column=1, padx=10, pady=5)

        tk.Label(ventana_cuota, text=texto_palabras).grid(row=1, column=0, padx=10, pady=5, sticky="e")
        entry_palabras = tk.Entry(ventana_cuota)
        entry_palabras.grid(row=1, column=1, padx=10, pady=5)

        # Botón para ir a la ventana anterior
        if len(ventanas) > 1:  # Si no es la primera ventana
            tk.Button(
                ventana_cuota,
                text="Anterior",
                command=lambda: ir_a_ventana_anterior(ventana_cuota, datos_cuotas),
            ).grid(row=2, column=0, pady=20)

        # Botón para continuar
        tk.Button(
            ventana_cuota,
            text="Siguiente" if "Última" not in titulo else "Generar Letras",
            command=lambda: callback(
                {"monto": entry_monto.get(), "palabras": entry_palabras.get()}, datos_cuotas, ventana_cuota, *args
            ),
        ).grid(row=2, column=1, pady=20)

    def ir_a_ventana_anterior(ventana_actual, datos_cuotas):
        """
        Oculta la ventana actual y reabre la anterior.
        """
        if len(ventanas) > 1:
            # Ocultar la ventana actual
            ventana_actual.withdraw()

            # Recuperar la ventana anterior
            ventana_anterior = ventanas[-2]
            ventana_anterior.deiconify()  # Mostrar la ventana anterior

            # Quitar la última cuota si se navegó hacia atrás
            if datos_cuotas:
                datos_cuotas.pop()

    def capturar_primera_cuota(cuota, datos_cuotas, ventana_anterior, *args):
        datos_cuotas.append(cuota)  # Agregar datos de la primera cuota
        capturar_cuota(
            "Monto de la Cuota Intermedia",
            "Monto de la Cuota Intermedia:",
            "Monto en Palabras (Intermedia):",
            capturar_cuota_intermedia,
            datos_cuotas,
            ventana_anterior,
            *args,
        )

    def capturar_cuota_intermedia(cuota, datos_cuotas, ventana_anterior, *args):
        datos_cuotas.append(cuota)  # Agregar datos de la cuota intermedia
        capturar_cuota(
            "Monto de la Última Cuota",
            "Monto de la Última Cuota:",
            "Monto en Palabras (Última):",
            capturar_ultima_cuota,
            datos_cuotas,
            ventana_anterior,
            *args,
        )

    def capturar_ultima_cuota(cuota, datos_cuotas, ventana_anterior, *args):
        datos_cuotas.append(cuota)  # Agregar datos de la última cuota
        ventana_anterior.withdraw()  # Ocultar la ventana de la última cuota
        enviar_datos(
            entry_numero_inicial,
            entry_cuotas,
            entry_fecha_giro,
            entry_primer_vencimiento,
            entry_girado,
            entry_dni,
            entry_domicilio,
            entry_telefono,
            entry_referencia,
            combo_vencimiento,
            datos_cuotas,  # Enviar todas las cuotas
        )

    # Iniciar con la ventana de la primera cuota
    capturar_cuota(
        "Monto de la Primera Cuota",
        "Monto de la Primera Cuota:",
        "Monto en Palabras (Primera Cuota):",
        capturar_primera_cuota,
        datos_cuotas,
        None,  # No hay ventana anterior para la primera cuota
        entry_numero_inicial,
        entry_cuotas,
        entry_fecha_giro,
        entry_primer_vencimiento,
        entry_girado,
        entry_dni,
        entry_domicilio,
        entry_telefono,
        entry_referencia,
        combo_vencimiento,
    )
#Abrir ventana inicial
def abrir_ventana_inicial():
    ventana_inicial = tk.Tk()
    ventana_inicial.title("Configuración de Letras de Cambio")

    tk.Label(ventana_inicial, text="¿Todas las cuotas serán iguales?").pack(pady=20)

    tk.Button(ventana_inicial, text="Sí", command=abrir_ventana_cuotas_iguales).pack(side="left", padx=20, pady=20)
    tk.Button(ventana_inicial, text="No", command=abrir_ventana_cuotas_diferentes).pack(side="right", padx=20, pady=20)

    ventana_inicial.mainloop()

### Esto inicia el programa recién, lo anterior es el programa
abrir_ventana_inicial()

