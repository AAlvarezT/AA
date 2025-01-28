// Incluir librerías necesarias
#include <Servo.h>
#include <SoftwareSerial.h>

// Definición de pines
// Sensor ultrasónico
const int TRIG_PIN = 2;
const int ECHO_PIN = 3;
// Motor driver L298N
const int ENA = 5;
const int IN1 = 6;
const int IN2 = 7;
const int IN3 = 8;
const int IN4 = 9;
const int ENB = 10;
// Servomotores
const int SERVO_RECOLECTOR = 11;
const int SERVO_DIRECCION = 12;
// Sensor de color TCS3200
const int S0 = 4;
const int S1 = 44;
const int S2 = 45;
const int S3 = 46;
const int COLOR_OUT = 47;
// Sensor capacitivo
const int CAPACITIVE_PIN = 48;
// Bluetooth
const int BT_RX = 50;
const int BT_TX = 51;

// Objetos
Servo servoRecolector;
Servo servoDireccion;
SoftwareSerial bluetooth(BT_RX, BT_TX);

// Variables globales
int distancia = 0;
int colorRojo = 0;
int colorVerde = 0;
int colorAzul = 0;
String comandoVoz = "";
bool objetoDetectado = false;

void setup() {
  // Inicializar comunicación serial
  Serial.begin(9600);
  bluetooth.begin(9600);
  
  // Configurar pines
  pinMode(TRIG_PIN, OUTPUT);
  pinMode(ECHO_PIN, INPUT);
  pinMode(ENA, OUTPUT);
  pinMode(IN1, OUTPUT);
  pinMode(IN2, OUTPUT);
  pinMode(IN3, OUTPUT);
  pinMode(IN4, OUTPUT);
  pinMode(ENB, OUTPUT);
  pinMode(S0, OUTPUT);
  pinMode(S1, OUTPUT);
  pinMode(S2, OUTPUT);
  pinMode(S3, OUTPUT);
  pinMode(COLOR_OUT, INPUT);
  pinMode(CAPACITIVE_PIN, INPUT);
  
  // Configurar servos
  servoRecolector.attach(SERVO_RECOLECTOR);
  servoDireccion.attach(SERVO_DIRECCION);
  
  // Configurar sensor de color
  digitalWrite(S0, HIGH);
  digitalWrite(S1, LOW);
  
  // Posición inicial
  servoRecolector.write(90);
  servoDireccion.write(90);
}

void loop() {
  // Leer sensores
  leerDistancia();
  leerColor();
  objetoDetectado = digitalRead(CAPACITIVE_PIN);
  
  // Procesar comandos de voz o Bluetooth
  if (Serial.available()) {
    comandoVoz = Serial.readString();
    procesarComando(comandoVoz);
  }
  
  if (bluetooth.available()) {
    String btComando = bluetooth.readString();
    procesarComando(btComando);
  }
  
  // Lógica principal
  if (distancia < 20) { // Objeto cercano
    detenerMotores();
    if (objetoDetectado) {
      identificarMaterial();
    }
  } else {
    moverAdelante();
  }
}

void leerDistancia() {
  digitalWrite(TRIG_PIN, LOW);
  delayMicroseconds(2);
  digitalWrite(TRIG_PIN, HIGH);
  delayMicroseconds(10);
  digitalWrite(TRIG_PIN, LOW);
  
  long duracion = pulseIn(ECHO_PIN, HIGH);
  distancia = duracion * 0.034 / 2;
}

void leerColor() {
  // Leer componente rojo
  digitalWrite(S2, LOW);
  digitalWrite(S3, LOW);
  colorRojo = pulseIn(COLOR_OUT, LOW);
  
  // Leer componente verde
  digitalWrite(S2, HIGH);
  digitalWrite(S3, HIGH);
  colorVerde = pulseIn(COLOR_OUT, LOW);
  
  // Leer componente azul
  digitalWrite(S2, LOW);
  digitalWrite(S3, HIGH);
  colorAzul = pulseIn(COLOR_OUT, LOW);
}

void identificarMaterial() {
  if (objetoDetectado) {
    if (colorRojo < 30 && colorVerde < 30 && colorAzul < 30) {
      // Probablemente metal
      recolectarMaterial("metal");
    } else if (colorRojo > 80 && colorVerde > 80 && colorAzul > 80) {
      // Probablemente papel/cartón
      recolectarMaterial("carton");
    } else {
      // Probablemente plástico
      recolectarMaterial("plastico");
    }
  }
}

void recolectarMaterial(String tipo) {
  detenerMotores();
  servoRecolector.write(0);   // Bajar recolector
  delay(1000);
  servoRecolector.write(180); // Subir recolector
  delay(1000);
  servoRecolector.write(90);  // Posición neutral
  
  Serial.println("Material recolectado: " + tipo);
  bluetooth.println("Material recolectado: " + tipo);
}

void moverAdelante() {
  digitalWrite(IN1, HIGH);
  digitalWrite(IN2, LOW);
  digitalWrite(IN3, HIGH);
  digitalWrite(IN4, LOW);
  analogWrite(ENA, 200);
  analogWrite(ENB, 200);
}

void detenerMotores() {
  digitalWrite(IN1, LOW);
  digitalWrite(IN2, LOW);
  digitalWrite(IN3, LOW);
  digitalWrite(IN4, LOW);
  analogWrite(ENA, 0);
  analogWrite(ENB, 0);
}

void girar(int grados) {
  servoDireccion.write(grados);
  delay(500);
}

void procesarComando(String comando) {
  comando.toLowerCase();
  if (comando.indexOf("recoger") >= 0) {
    if (comando.indexOf("metal") >= 0) {
      // Buscar metal
      girar(90);
    } else if (comando.indexOf("carton") >= 0 || comando.indexOf("papel") >= 0) {
      // Buscar cartón/papel
      girar(90);
    } else if (comando.indexOf("plastico") >= 0) {
      // Buscar plástico
      girar(90);
    }
  } else if (comando.indexOf("parar") >= 0) {
    detenerMotores();
  } else if (comando.indexOf("adelante") >= 0) {
    moverAdelante();
  }
}