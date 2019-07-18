<!-- 
- title : partie 1
Créer son type date en elm

- description :
 Le 29 Février 2100 existe t il ?
Nous allons voir comment grace aux types d Elm nous allons garantir la validité d une date à la compilation.
Cet exemple permettra de comprendre l usage des let-exressions et des types algebriques.

- author : Laure Juglaret
- theme : solarized.css
- transition : convex
- slideNumber : true -->  



#  partie 1 Créer un type Date en elm

<a href="https://ljuglaret.github.io/Date/DateDemo3.html">Lien calendrier </a> 

---

## Première approche


```elm

type alias Date  = {jour : Int , mois : Int, annee : Int}



```
---
Quel est le problème

- jour est un entier relatif , or il doit etre dans intervalle dependant du mois
- mois est un entier relatif, or il doit etre compris entre 1 et 12

----

Exemples 

```elm
(jour = 11 , mois = 12, annee = 1988) est une date valide 
```
mais
```elm
(jour = -2, mois =  36, annee = 2100) est aussi une date valide ...
```
---

Début de solution

- Représenter les mois sous forme d une énumeration
``` elm
type Mois = Janvier
            |Fevrier
            |Mars
            |Avril
            .
            .
            .
```


---

Cela ne repond qu a la moitié du problème
il faut aussi avoir un jour qui a du sens

On aura besoin d une fonction permettant 
pour chaque element de type Mois d avoir les informations le concernant :

``` elm
listeInfoMois : Mois -> {mois : Mois , numeroDuMois : Int, nombreDeJours : Int}
listeInfoMois m = 
        case m of
            Janvier  -> {mois = Janvier , numeroDuMois =1 , nombreDeJours = 31}
            Fevrier  -> {mois = Fevrier , numeroDuMois =2 , nombreDeJours = 27}
            Mars     -> {mois = Mars    , numeroDuMois =3 , nombreDeJours = 31} 
            .
            .
            .
```

----
première solution

Ecrire une fonction dateValide qui en fonction de l annee (bissextile ou non),
    de la cohérence du jour en fonction du mois renverra un Maybe de date

```elm
    date1 =  {jour = 28, mois = Decembre, annee = 2018}
    dateValide date1 {- Just {jour = 28, mois = Decembre, annee = 2018}-}

    date2 =  {jour = -42, mois = Decembre, annee = 1950}
    dateValide date1 {- Nothing -}

```

----
 Explication  de type Maybe a 

```elm
type Maybe a = Just a
             |Nothing
```

 Utile dans notre cas puisque on veut tester une date mais aussi la récupérer, ce qui n est pas possible avec le type Bool

---
## Autre Solution, les *types opaques*

Créer son propre type Date.
On utilisera
- newDate : le constructeur d un objet de type date à partir d un triplet DateAlias.
- dateVersAlias : l operation reciproque.   A un objet de type Date elle renvoie un triplet DateAlias
    - Utile pour récuperer les champs jour mois et année 
----

```elm

type alias Annee = Int 
type alias DateAlias  = {jour : Int ,  mois : Mois, annee: Annee}
type Date2 = D DateAlias

newDate : DateAlias -> Maybe Date2
newDate date = 
    (if cond)
    then Just (D date)
    else Nothing

dateVersAlias : Date2 -> DateAlias  
dateVersAlias (D da) = da
```

---

## Avantages

- Avec cette forme on a la garantie que pour toute date
entrée elle sera soit valide et sera traitée
soit invalide et ne renverra rien

## Autour des dates
**Comparaison**   
Comparer deux dates revient à déterminer si une date est
avant ou après  une autre, pour éviter les répétitions on introduit un ordre sur le type Date.

Rappel
```elm 
type Order = LT -- <
            |GT -- >
            |EQ --  =
```
Les 5 comparaisons nécessaires sont : 
Strictement inferieur, inferieur ou égal, Strictement supérieur,supérieur ou égal, égal.

Un moyen de procéder est de copier coller 4 fois en adaptant ce code :

```elm 
unDesOperateurs : Maybe Date2 -> Maybe Date2 -> Result String Bool  
unDesOperateurs d1 d2 = 
    let 
        comp = compD1D2 d1 d2
    in
        case comp of 
            Just LT -> Ok ...
            Just EQ -> Ok ...
            Just GT -> Ok ... 
            Nothing -> Err "format date incorrect"
``` 

Ou bien, on peut on peut en construire deux et deduire les autres.
Par exemple avec < et > on obtient logiquement : 
== , c est ce qui n est ni strictement inferieur ni strictement supérieur.
Donc on obtient facilement >= , qui est soit > soit ==
De même on obtient facilement <= , qui est soit < soit ==



**Opérations arithmétiques**   

Ajouter ou enlever plusieurs jours, mois ou années.
   
   
   
   
Toutes les fonctions sont disponibles <a href= https://github.com/ljuglaret/Date/blob/v19/DateElm19/BibliDate2.elm> ici</a>
Pour un aperçu de l utilisation des fonctions définies précédemment : <a href = https://ljuglaret.github.io/Date/DateDemo3.html >Lien </a> 
