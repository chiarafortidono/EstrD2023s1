// Ejercicio 3
// Dada la estructura de pares representada como struct en C++, definir las siguientes funciones sobre pares. 
// Recordar probar las implementaciones en un procedimiento main.

struct Par {
    int x;
    int y;
};

Par consPar(int x, int y);
int fst(Par p);
int snd(Par p);
int maxDelPar(Par p);
Par swap(Par p);
Par divisionYResto(int n, int m);