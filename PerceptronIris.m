clear all, close all, clc
%%se carga data
load fisheriris.mat

N = size(meas,1);

labels = [zeros(50,1);zeros(50,1);ones(50,1)];
data = meas(1:150,:);
%%
learningRate = 0.1;
numEpoch = 100;
[numSamples, numFeatures] = size(data);

%% pesos
weights = zeros(1,numFeatures);
bias = 0;

numSamples = size(data,1);
predictions = zeros(numSamples,1);

%entrenamiento
for epoch = 1:numEpoch
       for i = 1:numSamples
           % Calcular la salida del perceptron
           linearOutput = dot(weights,data(i,:)) + bias;
           predictions(i) = linearOutput >0;

           % Actualizar pesos y sesgo
           error = labels(i) - predictions(i);
           weights = weights + learningRate * error*data(i,:);
           bias = bias +learningRate*error;
       end
end

% Create confusion matrix
confusionMat = zeros(2, 2);
for i = 1:numSamples
    confusionMat(labels(i) + 1, predictions(i) + 1) = confusionMat(labels(i) + 1, predictions(i) + 1) + 1;
end


disp('Matriz de Confusion:');
disp(confusionMat);

TP = confusionMat(2, 2);  
TN = confusionMat(1, 1); 
FP = confusionMat(1, 2); 
FN = confusionMat(2, 1);

accuracy = (TP + TN) / numSamples;
precision = TP / (TP + FP);
recall = TP / (TP + FN);
f1Score = 2 * (precision * recall) / (precision + recall);


fprintf('Accuracy: %.2f\n', accuracy);
fprintf('Precision: %.2f\n', precision);
fprintf('Recall: %.2f\n', recall);
fprintf('F1 Score: %.2f\n', f1Score);
