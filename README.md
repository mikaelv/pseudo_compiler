# Pseudo-code interpreter
This projet allows to interpret the Efrei pseudo-code, as taught in the module TI102 Algorithms.

## Missing features
- arrays
- int modulo
- Ecrire with multiple arguments
 
## Pseudo-code language
Here is an example in French an in English, highlighting the syntax of the language :

## Example

French:
```
Algorithme : recherche_dichotomique
Variables :
    l, r, m : entier
    iter, pos, val : entier
    T [10] : tableau d’entier
Début
    T ← {1, 1, 2, 3, 5, 8, 13, 21, 34, 55}
    Lire(val)
    pos ← −1
    iter ← 0
    l ← 1
    r ← 10
    Tant que l ≤ r ET pos = −1 Faire
        iter ← iter + 1
        m ← l + (r − l)/2
        Si T [m] = val Alors
            pos ← m
        Sinon
            Si T [m] < val Alors
                l ← m + 1
            Sinon
                r ← m − 1
            Fin Si
        Fin Si
    Fin Tant que
    Si pos = −1 Alors
        Ecrire("Valeur trouvée en ", iter, "itérations\NL")
    Fin Si
Fin
```

English:
```
Algorithm : binary_search
Variables :
    l, r, m : integer
    iter, pos, val : integer
    T[10] : array of integer
Begin
    T ← {1, 1, 2, 3, 5, 8, 13, 21, 34, 55}
    Read(val)
    pos ← −1
    iter ← 0
    l ← 1
    r ← 10
    While l ≤ r AND pos = −1 Do
        iter ← iter + 1
        m ← l + (r − l)/2
        If T [m] = val Then
            pos ← m
        Else
            If T [m] < val Then
                l ← m + 1
            Else
                r ← m − 1
            End If
        End If
    End While
    If pos = −1 Then
        Write("Value found after ", iter, "iterations\NL")
    End If
End
```