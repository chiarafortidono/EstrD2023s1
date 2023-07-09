#include "Entrenador.h"
#include <iostream>
#include <string>
using namespace std;

struct EntrenadorSt {
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
};

// Una vez hecho eso, implementar la siguiente interfaz de Entrenador:

// Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, devuelve un entrenador.
Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon) {
    EntrenadorSt* e = new EntrenadorSt;
    e -> nombre = nombre;
    e-> cantPokemon = cantidad;
    e -> pokemon = new Pokemon[e->cantPokemon];
    return(e);
};

string nombreDeEntrenador(Entrenador e) { // Devuelve el nombre del entrenador.
    return(e->nombre);
};

int cantidadDePokemon(Entrenador e) { // Devuelve la cantidad de pokémon que posee el entrenador.
    return(e->cantPokemon);
};

// Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e) {
    int res = 0;
    int i = 0;
    while(i<e->cantPokemon) {
        if(e->pokemon[i]->tipo == tipo) {
            res++;
        }
        i++;
    }
    return(res);
};

// Devuelve el pokémon número i de los pokémon del entrenador.
// Precondición: existen al menos i−1 pokémon.
Pokemon pokemonNro(int i, Entrenador e) {
    return(e->pokemon[i]);
};

// Dados dos entrenadores, indica si, para cada pokémon del segundo entrenador, el primero posee al menos un pokémon que le gane.
bool leGanaATodos(Entrenador e1, Entrenador e2) {
    
};