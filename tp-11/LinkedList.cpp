#include "LinkedList.h"

struct NodoL {
    int elem;         // valor del nodo
    NodoL* siguiente; // puntero al siguiente nodo
};

struct LinkedListSt {  // INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer 
                       // desde primero por siguiente hasta alcanzar a NULL
    int cantidad;   // cantidad de elementos
    NodoL* primero; // puntero al primer nodo
};

struct IteratorSt {
    NodoL* current;
};

LinkedList nil() { // Crea una lista vacía
    LinkedListSt* xs = new LinkedListSt;
    xs->cantidad = 0;
    xs->primero  = NULL;
    return(xs);
};

bool isEmpty(LinkedList xs) { // Indica si la lista está vacía.
    return(xs->cantidad == 0);
}; 

int head(LinkedList xs) { // Devuelve el primer elemento.
    return(xs->primero->elem);
};

void Cons(int x, LinkedList xs) { // Agrega un elemento al principio de la lista.
    NodoL* vprimero = xs->primero;
    NodoL* nprimero = new NodoL;
    nprimero->elem = x;
    nprimero->siguiente = vprimero;
    xs->primero = nprimero;
    xs->cantidad++; 
};

void Tail(LinkedList xs) { // Quita el primer elemento.
    NodoL* nprimero = xs->primero->siguiente;
    delete xs->primero;
    xs->primero = nprimero;
    xs->cantidad--;
};

int length(LinkedList xs) { // Devuelve la cantidad de elementos.
    int i = 0;
    int res = 0;
    while (i < xs->cantidad) { // podría hacerse con un int actual que guarde el nodo y comparar el siguiente de ese nodo con NULL
        res++;
        i++;
    }
    return(res);
};

void Snoc(int x, LinkedList xs) { // Agrega un elemento al final de la lista.
    NodoL* nuevo = new NodoL;
    nuevo->elem = x;
    nuevo->siguiente = NULL;
    NodoL* actual = xs->primero;
    while (actual->siguiente != NULL) {
        actual = actual->siguiente;
    }
    actual->siguiente = nuevo;
    xs->cantidad++;
};

ListIterator getIterator(LinkedList xs) { // Apunta el recorrido al primer elemento.
    IteratorSt* it = new IteratorSt;
    it->current = xs->primero;
    return(it);
};

int current(ListIterator ixs) { // Devuelve el elemento actual en el recorrido.
    return(ixs->current->elem);
};

void SetCurrent(int x, ListIterator ixs) { // Reemplaza el elemento actual por otro elemento.
    ixs->current->elem = x;
};

void Next(ListIterator ixs) { // Pasa al siguiente elemento.
    ixs->current = ixs->current->siguiente;
};

bool atEnd(ListIterator ixs) { // Indica si el recorrido ha terminado.
    return(ixs->current->siguiente == NULL);
}; 

void DisposeIterator(ListIterator ixs) { // Libera la memoria ocupada por el iterador.
    delete ixs;
};

void DestroyL(LinkedList xs) { // Libera la memoria ocupada por la lista.
    NodoL* temp = xs->primero;
    while (xs->primero != NULL) {
        xs->primero = xs->primero->siguiente;
        delete temp;
        temp = xs->primero;
    }
    delete xs;
};

void Append(LinkedList xs, LinkedList ys) { // Agrega todos los elementos de la segunda lista al final de los de la primera. La segunda lista se destruye.
    xs->cantidad += ys->cantidad;
    NodoL* primeroys = ys->primero;
    NodoL* actual = xs->primero;
    while (actual->siguiente != NULL) {
        actual = actual->siguiente;
    }
    actual->siguiente = primeroys;
    delete ys;
};