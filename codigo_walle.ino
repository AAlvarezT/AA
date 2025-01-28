#include <Servo.h>
// Pines de los motores
int Pin_Motor_Der_A = 23;
int Pin_Motor_Der_B = 25;
int Pin_Motor_Izq_A = 27;
int Pin_Motor_Izq_B = 29;
// Pines de los servomotores
const int pinServo1 = 11;
const int pinServo2 = 10;
// Pines del sensor de color
const int S0 = 51;
const int S1 = 49;
const int S2 = 45;
const int S3 = 47;
const int sensorOut = 43;
// Pines del sensor ultrasónico
const int trigPin = 13;
const int echoPin = 12;
// Define el pin al que está conectado el sensor capacitivo
const int sensorPin = 9;
// Variables para medir los colores
int redFrequency = 0;
int greenFrequency = 0;
int blueFrequency = 0;
int clearFrequency = 0;
// Variables para la distancia
long duration;
float distance;
// Variable para controlar si se ha detectado un objeto
bool objetoDetectado = false;
//Uso de los servomotores
Servo servo1;
Servo servo2;

void setup() {
  Serial.begin(9600);
  // Configuración del los motores
  pinMode(Pin_Motor_Der_A, OUTPUT);
  pinMode(Pin_Motor_Der_B, OUTPUT);
  pinMode(Pin_Motor_Izq_A, OUTPUT);
  pinMode(Pin_Motor_Izq_B, OUTPUT);
  // Configuración del los servomotores
  servo1.attach(pinServo1);
  servo2.attach(pinServo2);
  // Configuración del sensor de color
  pinMode(S0, OUTPUT);
  pinMode(S1, OUTPUT);
  pinMode(S2, OUTPUT);
  pinMode(S3, OUTPUT);
  pinMode(sensorOut, INPUT);
  // Configuración del sensor ultrasónico
  pinMode(trigPin, OUTPUT);
  pinMode(echoPin, INPUT);
  // Configuración del sensor capacitivo
  pinMode(sensorPin, INPUT);
  // Configuración del sensor para medir frecuencias
  digitalWrite(S0, HIGH);
  digitalWrite(S1, LOW);
  // Configuración para subir la garra
  servo1.write(180);
  servo2.write(0); 
}

void loop() {
  // Uso de los motores y garra
  if (Serial.available()) {
    char dato = Serial.read();
    if (dato == 'a') {
      Mover_Adelante();
    }
    else if (dato == 'r') { 
      Mover_Retroceso();
    }
    else if (dato == 'd') { 
      Mover_Derecha();
    }
    else if (dato == 'i') { 
      Mover_Izquierda();
    }
    else if (dato == 'p') {
      Mover_Stop();
    }
    else if (dato == 'j') { // Cuando se presiona 'j', soltamos el objeto
      if (objetoDetectado) {
        Soltar_Objeto(); // Función para soltar el objeto
      }
    }
  }

  //Configuración para la detección de objetos 
  if (!objetoDetectado) {
    //Medir distancia de los objetos
    digitalWrite(trigPin, LOW);
    delayMicroseconds(2);
    digitalWrite(trigPin, HIGH);
    delayMicroseconds(10);
    digitalWrite(trigPin, LOW);

    duration = pulseIn(echoPin, HIGH);  // Duración del pulso en microsegundos
    distance = (duration * 0.034) / 2; // Convertir tiempo a distancia en cm

    Serial.print("Distancia: ");
    Serial.print(distance);
    Serial.println(" cm");

    // Lee el valor del sensor capacitivo y almacénalo en sensorValue
    sensorValue = digitalRead(sensorPin);

    // Se activa el sistema si el objeto esta cerca al carro
    if (distance >= 2 && distance <= 9) {
      Serial.println("Objeto detectado en rango. Analizando color...");

      Mover_Stop();
      
      digitalWrite(S2, LOW);
      digitalWrite(S3, LOW);
      redFrequency = pulseIn(sensorOut, LOW);

      digitalWrite(S2, HIGH);
      digitalWrite(S3, HIGH);
      greenFrequency = pulseIn(sensorOut, LOW);

      digitalWrite(S2, LOW);
      digitalWrite(S3, HIGH);
      blueFrequency = pulseIn(sensorOut, LOW);

      digitalWrite(S2, HIGH);
      digitalWrite(S3, LOW);
      clearFrequency = pulseIn(sensorOut, LOW);

      // Mostrar valores de color
      Serial.print("Red Frequency: ");
      Serial.println(redFrequency);
      Serial.print("Green Frequency: ");
      Serial.println(greenFrequency);
      Serial.print("Blue Frequency: ");
      Serial.println(blueFrequency);
      Serial.print("Clear Frequency: ");
      Serial.println(clearFrequency);

      // Identifica si es lata
      if (sensorValue == HIGH) {
        Serial.println("Lata detectada");
        delay(1000);
        servo1.write(0);   
        servo2.write(180);
      } else {
          Serial.println("No hay lata");
      }

      // Identificar si es cartón
      if (redFrequency >= 40 && redFrequency <= 60 &&  // Umbral para rojo
          greenFrequency >= 60 && greenFrequency <= 90 &&  // Umbral para verde
          blueFrequency >= 60 && blueFrequency <= 90 &&  // Umbral para azul
          clearFrequency >= 20 && clearFrequency <= 40) {
        Serial.println("Es cartón.");
        delay(1000);
        servo1.write(0);   
        servo2.write(180);

        // Marcar que ya se ha detectado un objeto
        objetoDetectado = true;
      } else {
        Serial.println("No es cartón.");
      }
    } else {
      Serial.println("Objeto fuera de rango. Esperando...");
    }
  } else {
    Serial.println("Objeto ya detectado.");
  }
  delay(1000);
}

// Función para soltar el objeto
void Soltar_Objeto() {
  Serial.println("Soltando el objeto...");
  servo1.write(180);
  servo2.write(0);
  objetoDetectado = false;
}

//Funciones para el uso de los motores
void Mover_Adelante() {
  digitalWrite(Pin_Motor_Der_A, HIGH);
  digitalWrite(Pin_Motor_Der_B, LOW);
  digitalWrite(Pin_Motor_Izq_A, HIGH);
  digitalWrite(Pin_Motor_Izq_B, LOW);
}

void Mover_Retroceso() {
  digitalWrite(Pin_Motor_Der_A, LOW);
  digitalWrite(Pin_Motor_Der_B, HIGH);
  digitalWrite(Pin_Motor_Izq_A, LOW);
  digitalWrite(Pin_Motor_Izq_B, HIGH);
}

void Mover_Derecha() {
  digitalWrite(Pin_Motor_Der_A, LOW);
  digitalWrite(Pin_Motor_Der_B, HIGH);
  digitalWrite(Pin_Motor_Izq_A, HIGH);
  digitalWrite(Pin_Motor_Izq_B, LOW);
}

void Mover_Izquierda() {
  digitalWrite(Pin_Motor_Der_A, HIGH);
  digitalWrite(Pin_Motor_Der_B, LOW);
  digitalWrite(Pin_Motor_Izq_A, LOW);
  digitalWrite(Pin_Motor_Izq_B, HIGH);
}

void Mover_Stop() {
  digitalWrite(Pin_Motor_Der_A, LOW);
  digitalWrite(Pin_Motor_Der_B, LOW);
  digitalWrite(Pin_Motor_Izq_A, LOW);
  digitalWrite(Pin_Motor_Izq_B, LOW);
}
