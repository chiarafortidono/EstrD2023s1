#include "LinkedList.h"
#include "Tree.h"
#include "ArrayList.h"

int sumatoria(LinkedList xs) { // Devuelve la suma de todos los elementos.
    int res = 0;
    ListIterator ixs = getIterator(xs);
    while (!atEnd(ixs)) {
        res += current(ixs);
        Next(ixs);
    }
    DisposeIterator(ixs);
    return res;
};

void Sucesores(LinkedList xs) { // Incrementa en uno todos los elementos.
    ListIterator ixs = getIterator(xs);
    while (!atEnd(ixs)) {
        int cur = current(ixs);
        SetCurrent(cur+1, ixs);
        Next(ixs);
    }
    DisposeIterator(ixs);
};

bool pertenece(int x, LinkedList xs) { // Indica si el elemento pertenece a la lista.
    ListIterator ixs = getIterator(xs);
    while (!atEnd(ixs) && !x==current(ixs)) {
        Next(ixs);
    }
    DisposeIterator(ixs);
    return(x==current(ixs));
};

int apariciones(int x, LinkedList xs) { // Indica la cantidad de elementos iguales a x.
    int res = 0;
    ListIterator ixs = getIterator(xs);
    while (!atEnd(ixs)) {
        if (current(ixs) == x) {
            res++;
        }
        Next(ixs);
    }
    DisposeIterator(ixs);
    return(res);
};

int minimo(LinkedList xs) { // Devuelve el elemento más chico de la lista.
    ListIterator ixs = getIterator(xs);
    int min = current(ixs);
    while (!atEnd(ixs)) {
        if (current(ixs) < min) {
            min = current(ixs);
        }
        Next(ixs);
    }
    DisposeIterator(ixs);
    return(min);
};

LinkedList copy(LinkedList xs) { // Dada una lista genera otra con los mismos elementos, en el mismo orden.
    LinkedList ys = nil();
    ListIterator ixs = getIterator(xs);
    while (!atEnd(ixs)) {
        Snoc(current(ixs),ys);
        Next(ixs);
    }
    DisposeIterator(ixs);
    return(ys);
};

void AppendUsuario(LinkedList xs, LinkedList ys) { // Agrega todos los elementos de la segunda lista al final de los de la primera. La segunda lista se destruye.
    ListIterator iys = getIterator(ys);
    while (!atEnd(iys)) {
        Snoc(current(iys),xs);
        Next(iys);
    }
    DisposeIterator(iys);
    DestroyL(ys);
};
// Nota: notar que los costos mejorarían si Snoc fuese O(1), ¿cómo podría serlo? --> si el struct de list tuviese un puntero al último elem.

int sumarT(Tree t) { // Dado un árbol binario de enteros devuelve la suma entre sus elementos.

};

int sizeT(Tree t) { // Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés).

};

bool perteneceT(int e, Tree t) { // Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.

};

int aparicionesT(int e, Tree t) { // Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.

};

int heightT(Tree t) { // Dado un árbol devuelve su altura.

};

ArrayList toList(Tree t) { // Dado un árbol devuelve una lista con todos sus elementos.

};

ArrayList leaves(Tree t) { // Dado un árbol devuelve los elementos que se encuentran en sus hojas.

};

ArrayList levelN(int n, Tree t) { // Dados un número n y un árbol devuelve una lista con los nodos de nivel n.

};