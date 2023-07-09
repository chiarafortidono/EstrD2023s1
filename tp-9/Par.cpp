// Ejercicio 3
// Dada la estructura de pares representada como struct en C++, definir las siguientes funciones sobre pares. 
// Recordar probar las implementaciones en un procedimiento main.

#include "Par.h";

// Propósito: construye un par
Par consPar(int x, int y) {
    Par par;
    par.x = x;
    par.y = y;
    return(par);
};

// Propósito: devuelve la primera componente
int fst(Par p) {
    return(p.x);
};

// Propósito: devuelve la segunda componente
int snd(Par p) {
    return(p.y);
};

// Propósito: devuelve la mayor componente
int maxDelPar(Par p) {
    if (p.x > p.y) {
        return(p.x);
    }
    return(p.y);
};

// Propósito: devuelve un par con las componentes intercambiadas
Par swap(Par p) {
    Par nuevo;
    nuevo.x = p.y;
    nuevo.y = p.x;
    return(nuevo);
};

// Propósito: devuelve un par donde la primer componente es la división y la segunda el resto entre ambos números
Par divisionYResto(int n, int m) {
    Par par;
    par.x = n / m;
    par.y = n % m;
    return(par);
};