#include "ArrayList.h"

// Ejercicio 4
// Definir las siguientes funciones utilizando la interfaz de ArrayList:

int sumatoria(ArrayList xs) { // Devuelve la suma de todos los elementos.
    int res = 0;
    for (int i = 0; i<= lengthAL(xs); i++) {
        res = res + get(i, xs);
    }
    return(res);
};

void sucesores(ArrayList xs) { // Incrementa en uno todos los elementos.
    for (int i = 0; i <= lengthAL(xs); i++) {
        set(i, get(i,xs)+1, xs);
    }
};

bool pertenece(int x, ArrayList xs) { // Indica si el elemento pertenece a la lista.
    int i = 0;
    while (get(i, xs) != x) {
        i++;
    }
    return(get(i,xs) == x);
}

int apariciones(int x, ArrayList xs) { // Indica la cantidad de elementos iguales a x.
    int res = 0;
    for (int i = 0; i <= lengthAL(xs); i++) {
        if (get(i,xs) == x) {
            res++;
        }
    }
    return(res);
};

ArrayList append(ArrayList xs, ArrayList ys) { // Crea una nueva lista a partir de la primera y la segunda (en ese orden).
    int i = 0;
    ArrayList zs = newArrayListWith(lengthAL(xs) + lengthAL(ys));
    while (i <= lengthAL(xs)) {
        add(get(i, xs), zs);
        i++;
    }
    i = 0;
    while (i <= lengthAL(ys)) {
        add(get(i, ys), zs);
        i++;
    }
    return(zs);
};

int minimo(ArrayList xs) { // Devuelve el elemento más chico de la lista.
    int min = get(0, xs);
    for (int i = 0; i <= lengthAL(xs); i++) {
        if (get(i,xs)<min) {
            min = get(i,xs);
        }
    }
    return(min);
};