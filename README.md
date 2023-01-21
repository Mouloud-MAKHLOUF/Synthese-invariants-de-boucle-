

# Synthese D'invariants de boucle
_____________________________

### Objectif du mini projet

Dans ce projet, nous allons automatiser la synthèse d'invariants de
boucle pour un très petit langage de programmation, décrit
ci-dessous. Le but est de pouvoir ecire dans un fichier le code au format SMT-LIB permettant de vérifier si la boucle a bel et bien un invariant et que les assertions finalles sont respectées.

### Prerequis

Il est nécessaire d'avoit ocaml installé
Pour l'installer sur une Ubuntu 20.04+ ou Debian 11 :
```console
sudo apt install ocaml ocaml-findlib ocaml-dune
```

### Compilation et exécution 

Pour compiler 
```console
make
```
Pour exécuter 
```console
./invariants
```

### Consignes

Nous définissons un langage de programmation « WA » (While-Assert),
qui modélise des programmes Java de la forme générale suivante :

int x1 = a1;
// (...)
int xk = ak;
while (s) {
    x1 = b1;
    // (...)
    xk = bk;
}
assert (t);

Plus formellement, un _programme WA_ est défini comme un uplet `(k,
a1, ..., ak, b1, ..., bk, s, t)`, où `k` est un entier représentant le
nombre de variables utilisées par le programme, `a1`, ..., `ak` et
`b1`, ..., `bk` sont des _termes_, et `s` et `t` sont des _tests_. Les
variables utilisées dans les termes et les tests seront `x1`, ...,
`xk`, toujours de type entier. Ici, un _terme_ de WA est construit à
partir de variables et de constantes entières en appliquant les
opérations arithmétiques `+` et `*`, et un _test_ est défini comme une
formule atomique, qui compare deux termes par l'égalité ou par
l'ordre. Par exemple, le programme Java donné dans l'exercice 1
correspond, après l'application du renommage de `i` en `x1`
et de `v` en `x2`, au programme WA suivant :
`(2, 0, 0, x1 + 1, x2 + 3, x1 < 3, x2 = 9)`.

La fonction OCaml `smtlib_of_wa : program -> string` qui prend
en argument un programme WA et qui renvoie un programme SMT-LIB, qui,
lorsqu'il est donné à Z3, vérifiera l'existence d'un invariant de
boucle pour le programme WA donné en argument.


## Teste du mini-projet
Trois exemples de programmes WA sont donnés comme tests `p1, p2 et p3` à la fin de l'execution un executables est crée, son execution affichera le programme SMT-LIB correspondant au progamme WA donné en entré.

