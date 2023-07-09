using namespace std;

// Ejercicio 4
// Dar dos implementaciones para las siguientes funciones, una iterativa y otra recursiva, y utilizando la menor cantidad posible de variables. 
// Recordar definir subtareas en caso de que sea estrictamente necesario.

// Propósito: imprime n veces un string s.
void printN(int n, string s) {
    while (n!=0) {
        cout << s << endl;
        n--;
    }
};

void rprintN(int n, string s) {
    cout << s << endl;
    n--;
    if (n>0) printN(n,s);
};

// Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
void cuentaRegresiva(int n) {
    while (n>(-1)) {
        cout << n << endl;
        n--;
    }    
};

void rcuentaRegresiva(int n) {
    cout << n << endl;
    n--;
    if (n>(-1)) rcuentaRegresiva(n);
};

// Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
void desdeCeroHastaN(int n) {
    int i = 0;
    while (i<=n) {
        cout << i << endl;
        i++;
    }
};

void rdesdeCeroHastaN(int n) {

};


// Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
int mult(int n, int m) {
    int res = 0;
    while (m<0) {
        res = res + n;
        m--;
    }
    return(res);
};

int rmult(int n, int m) {
    if (n==0) {
        return(0);
    }
    return(n+(rmult(n,m--)));
};

// Propósito: imprime los primeros n char del string s, separados por un salto de línea.
// Precondición: el string tiene al menos n char.
void primerosN(int n, string s) {
    for (int i = 0; i < n; i++) {
        cout << s[i] << endl;
    }
};

void rprimerosN(int n, string s) {

};

int length(string s) {
    int i = 0;
    while (s[i] != 0) {
        i++;
    }
    return(i);
};

// Propósito: indica si un char c aparece en el string s.
bool pertenece(char c, string s) {
    int i = 0;
    while (s[i] != c && i < length(s)) {
        i++;
    }
    return(s[i]==c);
};

bool rpertenece(char c, string s) {

};

// Propósito: devuelve la cantidad de apariciones de un char c en el string s.
int apariciones(char c, string s) {
    int a = 0;
    for (int i = 0; i < length(s); i++) {
       if (s[i] == c) {
        a++;
       }
    }
    return(a);
};

int rapariciones(char c, string s) {

};