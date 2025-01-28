clear all;
close all;
clc;

% Leer datos
data = readtable('Petroleo.csv');
Precio = data.Value;
x = 1:length(Precio); % Crear un vector para los puntos en el eje X

% Crear el vector de los siguientes 10 meses
x_10m = (length(Precio) + 1):(length(Precio) + 10); 

% Inicialización de variables
vector = [2, 3, 10]; % Grados de los polinomios
num_modelos = length(vector);
num_puntos = length(Precio);

error_matriz = zeros(num_modelos, num_puntos); % Matriz para almacenar errores (fila = modelo, columna = error por punto)
mape_matriz = zeros(num_modelos, num_puntos); % Matriz para almacenar MAPEs (fila = modelo, columna = MAPE por punto)

% Graficar los modelos ajustados
figure(1)
plot(x, Precio, 'k-', 'DisplayName', 'Datos Originales'); % Graficar los datos originales
hold on
grid on
title('Ajuste polinomial de diferentes grados y predicciones')
xlabel('X')
ylabel('Precio')

% Ajuste, predicción, cálculo de errores y MAPE
for i = 1:num_modelos
    grado = vector(i);
    % Ajuste polinomial
    betas = polyfit(x, Precio, grado);
    % Generar valores ajustados
    y_train = polyval(betas, x);
    % Generar valores para las predicciones
    y_10m = polyval(betas, x_10m); 
    
    % Calcular errores punto a punto
    error_matriz(i, :) = y_train(:)' - Precio(:)'; % Error por punto
    mape_matriz(i, :) = abs((Precio(:)' - y_train(:)') ./ Precio(:)') * 100; % MAPE por punto
    
    % Graficar cada modelo
    plot(x, y_train, 'DisplayName', sprintf('Grado %d', grado));
    plot(x_10m, y_10m, '--', 'DisplayName', sprintf('Predicción Grado %d', grado));
end

legend show

% Graficar el error absoluto total para cada modelo
figure(2)
error_total = sum(abs(error_matriz), 2); % Sumar los errores absolutos por fila (cada modelo)
bar(vector, error_total) % Crear gráfico de barras para los errores absolutos
grid on
title('Error absoluto total para cada grado de polinomio')
xlabel('Grado del polinomio')
ylabel('Error absoluto total')

% Graficar el MAPE total para cada modelo
figure(3)
mape_total = sum(mape_matriz, 2) / num_puntos; % Promedio de los MAPEs por modelo
bar(vector, mape_total) % Crear gráfico de barras para los MAPEs
grid on
title('MAPE promedio para cada grado de polinomio')
xlabel('Grado del polinomio')
ylabel('MAPE promedio (%)')
