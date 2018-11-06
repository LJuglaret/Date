

module Date2 exposing ( newDate,Date2,DateAlias, dateVersAlias, estBissextile)


import Mois exposing (..)


type alias Annee = Int
type alias DateAlias  = {jour : Int ,  mois : Mois, annee: Annee}
type Date2 = D DateAlias

--estBissextile ne compilera pas en 0.19 (a cause de l operateur modulo)
-- pour contourner ce probleme il faut utiliser remainderBy
-- (a % b) devient (remainderBy b a)

estBissextile : Annee -> Bool
estBissextile annee =  ((modBy annee  400 ==0) || (( modBy annee 4 == 0 ) &&( modBy annee 100 /= 0) ) )


newDate : DateAlias -> Maybe Date2
newDate date = 
    let 
        jour = date.jour
        mois = date.mois 
        annee = date.annee
    in
        if (mois == (listeInfoMois Fevrier).mois )
        then
            if (estBissextile annee)
            then
                if (jour> 0 && jour<=29)
                then Just(D date)
                else Nothing
            else 
                if (jour> 0 && jour<=28)
                then Just(D date)
                else Nothing

        else 
            if (jour> 0 && jour <= (listeInfoMois mois).nombreDeJours)
            then Just(D date)
            else Nothing


d : Maybe Date2
d = newDate  {jour  = 25 , mois = Mois.Septembre, annee = 2018}

dateVersAlias : Date2 -> DateAlias  
dateVersAlias (D da) = da

