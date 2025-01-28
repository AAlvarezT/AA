#FILTRACIÓN_DE_MENSAJES_PRUEBA

# Leer el archivo de chat
with open(r'C:\Users\ADMIN\Desktop\PROYECTO_BASE_3\Chat de WhatsApp con Base 3.txt', 'r', encoding='utf-8') as file:
    chat_lines = file.readlines()

# Filtrar solo los mensajes de Sebas Guevara Up
mensajes_sebas_base3 = [line for line in chat_lines if 'Sebas:' in line]

# Guardar los mensajes filtrados en un archivo nuevo
with open(r'C:\Users\ADMIN\Desktop\PROYECTO_BASE_3\Mensajes_Base_3.txt', 'w', encoding='utf-8') as output_file:
    output_file.writelines(mensajes_sebas_base3)

print("Los mensajes de Sebas han sido filtrados y guardados en 'Mensajes_Sebas.txt'.")
