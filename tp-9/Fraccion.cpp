// Ejercicio 5
// Dada la estructura de fracciones representada como struct en C++, definir las siguientes funciones sobre fracciones. 
// Recordar probar las implementaciones en un procedimiento main.

#include "Fraccion.h";

// Propósito: construye una fraccion. Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador) {
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return(f);
};

// Propósito: devuelve el numerador
int numerador(Fraccion f) {
    return(f.numerador);
};

// Propósito: devuelve el denominador
int denominador(Fraccion f) {
    return(f.denominador);
};

// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f) {
    return(f.numerador/f.denominador);
};

// Propósito: devuelve una fracción que resulta de multiplicar las fracciones (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2) {
    Fraccion resultado;
    resultado.numerador = f1.numerador * f2.numerador;
    resultado.denominador = f1.denominador * f2.denominador;
    return(resultado);
};

int mcd(int a, int b) {
    int x = 0;
    while (b != 0) {
        x = a;
        a = b;
        b = x % a;
    }
    return(a);
}

// Propósito: devuelve una fracción que resulta de simplificar la dada por parámetro
Fraccion simplificada(Fraccion f) {
    int d = mcd(f.numerador, f.denominador);
    f.numerador = f.numerador / d;
    f.denominador = f.denominador / d;
    return(f);
};

// Propósito: devuelve la primera componente ???
Fraccion sumF(Fraccion f1, Fraccion f2) {
    Fraccion res;
    res.denominador = f1.denominador * f2.denominador;
    res.numerador = (f1.numerador * f2.denominador) + (f2.numerador * f1.denominador);
    return(simplificada(res));
};