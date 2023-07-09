#include "ArrayList.h"

struct ArrayListSt {
    int cantidad; // cantidad de elementos
    int* elementos; // array de elementos
    int capacidad; // tamaño del array
};

// Definir la siguiente interfaz de este tipo de listas:

ArrayList newArrayList() { // Crea una lista con 0 elementos. Nota: empezar el array list con capacidad 16.
    ArrayListSt* xs = new ArrayListSt;
    xs-> cantidad = 0;
    xs-> capacidad = 16;
    xs-> elementos = new int[xs-> capacidad];
    return(xs);
};

ArrayList newArrayListWith(int capacidad) { // Crea una lista con 0 elementos y una capacidad dada por parámetro.
    ArrayListSt* xs = new ArrayListSt;
    xs -> cantidad = 0;
    xs -> elementos = new int[capacidad];
    xs -> capacidad = capacidad;
    return(xs);
};

int lengthAL(ArrayList xs) { // Devuelve la cantidad de elementos existentes.
    return(xs->cantidad);
};

int get(int i, ArrayList xs) { // Devuelve el iésimo elemento de la lista.
    return(xs->elementos[i]);
};

void set(int i, int x, ArrayList xs) { // Reemplaza el iésimo elemento por otro dado.
    xs->elementos[i] = x;
};

void resize(int capacidad, ArrayList xs) { // Decrementa o aumenta la capacidad del array. Nota: en caso de decrementarla, se pierden los elementos del final de la lista.
    // No sé
};

void add(int x, ArrayList xs) { // Agrega un elemento al final de la lista.
    if (xs->capacidad > xs->cantidad) {
        xs->elementos[xs->cantidad+1] = x;
    }
    // Qué se supone que tengo que hacer si ya no tiene más capacidad? Porque no hay precondiciones ni notas ni sé cómo dar un error.
};

void remove(ArrayList xs) { // Borra el último elemento de la lista.
    xs->cantidad = xs->cantidad-1;
};