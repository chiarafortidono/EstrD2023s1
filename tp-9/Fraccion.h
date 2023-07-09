// Ejercicio 5
// Dada la estructura de fracciones representada como struct en C++, definir las siguientes funciones sobre fracciones. 
// Recordar probar las implementaciones en un procedimiento main.

struct Fraccion {
    int numerador;
    int denominador;
};

Fraccion consFraccion(int numerador, int denominador);
int numerador(Fraccion f);
int denominador(Fraccion f);
float division(Fraccion f);
Fraccion multF(Fraccion f1, Fraccion f2);
Fraccion simplificada(Fraccion f);
Fraccion sumF(Fraccion f1, Fraccion f2);