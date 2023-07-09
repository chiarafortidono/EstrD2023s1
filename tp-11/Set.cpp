#include "Set.h"

struct NodoS {
    int elem; // valor del nodo
    NodoS* siguiente; // puntero al siguiente nodo
};

struct SetSt {
    int cantidad; // cantidad de elementos diferentes
    NodoS* primero; // puntero al primer nodo
};

Set emptyS() { // Crea un conjunto vacío.
    SetSt* set = new SetSt;
    set->cantidad = 0;
    set->primero = NULL;
    return(set);
}; 

bool isEmptyS(Set s) { // Indica si el conjunto está vacío.
    return(s->cantidad==0);
};

bool belongsS(int x, Set s) { // Indica si el elemento pertenece al conjunto.
    NodoS* actual = s->primero;
    while (actual->elem != x) {
        actual = actual->siguiente;
    }
    return(actual->elem==x);   
}; 

void AddS(int x, Set s) { // Agrega un elemento al conjunto.
    NodoS* nuevo = new NodoS;
    nuevo->elem = x;
    if (s->primero != NULL) {
        s->primero = nuevo;
        nuevo->siguiente = NULL;
    } else {
        NodoS* current = s->primero;
        NodoS* next = current->siguiente;
        while (next != NULL && next->elem != x) {
            current = next;
            next = current->siguiente;
        }
        if (next == NULL) {
            current->siguiente = nuevo;
            s->cantidad++;
        }
    }
}; 

void RemoveS(int x, Set s) { // Quita un elemento dado.
    if (s->primero != NULL) {
        if (s->primero->elem == x) {
            NodoS* temp = s->primero;
            s->primero = s->primero->siguiente;
            s->cantidad--;
            delete temp;
        } else {
            NodoS* current = s->primero;
            NodoS* next = current->siguiente;
            while (next != NULL && next->elem != x) {
                current = next;
                next = current->siguiente;
            }
            if (next != NULL) {
                current->siguiente = next->siguiente;
                delete next;
                s->cantidad--;
            }
        }
    }
};

int sizeS(Set s) { // Devuelve la cantidad de elementos.
    return(s->cantidad);
};

LinkedList setToList(Set s) { // Devuelve una lista con los elementos del conjunto.
    LinkedList xs = nil();
    NodoS* actual = s->primero;
    while(actual->siguiente != NULL) {
        Cons(actual->elem, xs);
        actual = actual->siguiente;
    }
    return(xs);
};

void DestroyS(Set s) { // Libera la memoria ocupada por el conjunto.
    NodoS* temp = s->primero;
    while (s->primero != NULL) {
        s->primero = s->primero->siguiente;
        delete temp;
        temp = s->primero;
    }
    delete s;
};