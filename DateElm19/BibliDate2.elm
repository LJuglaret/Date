module BibliDate2 exposing (..)
import Mois exposing (..)
import Date2 exposing (..)

---     COMPARAISONS    ---

mois1AvantMois2 : Mois -> Mois -> Bool
mois1AvantMois2 mois1 mois2 = 
    let 
        numM1 = (listeInfoMois mois1).numeroDuMois
        numM2 = (listeInfoMois mois2).numeroDuMois
    in
        if (numM1 <= numM2)
        then True
        else False


compD1D2 : Maybe  Date2 -> Maybe  Date2 -> Maybe Order
compD1D2 date1c date2c = 
    case (date1c, date2c) of 
        (Just date1, Just date2) ->
            if ((dateVersAlias date1).annee > (dateVersAlias date2).annee )
            then Just GT 
            else
                if ((dateVersAlias date1).annee < (dateVersAlias date2).annee )
                then Just LT
                else
                    if (mois1AvantMois2 (dateVersAlias date1).mois  (dateVersAlias date2).mois)
                    then
                        case compare (dateVersAlias date1).jour (dateVersAlias date2).jour of
                            LT -> Just LT
                            EQ -> Just EQ
                            GT -> Just GT
                    else Just GT
        (_,_) -> Nothing

sup : Maybe Date2 -> Maybe Date2 -> Result String Bool  
sup d1 d2 = 
    let 
        comp = compD1D2 d1 d2
    in
        case comp of 
            Just LT -> Ok False
            Just EQ -> Ok False
            Just GT -> Ok True 
            Nothing -> Err "format date incorrect"


inf : Maybe Date2 -> Maybe Date2 -> Result String Bool  
inf d1 d2 = 
    let 
        comp = compD1D2 d1 d2
    in
        case comp of 
            Just LT -> Ok True
            Just EQ -> Ok False
            Just GT -> Ok False 
            Nothing -> Err "format date incorrect"

egal : Maybe Date2 -> Maybe Date2 -> Result String Bool  
egal d1 d2 = 
    let 
        inferieur = inf d1 d2
        superieur = sup d1 d2

    in
        case (inferieur,superieur) of 
            (Ok  inferieurb, Ok superieurb) -> Ok (not inferieurb && not superieurb)
            (_,_) -> Err "format date incorrect"


different : Maybe Date2 -> Maybe Date2 -> Result String Bool  
different d1 d2 =
    let 
        eg = egal d1 d2
    in
        case eg  of 
            Ok egb -> Ok (not egb)
            _ -> Err "format date incorrect"

infOuEgal : Maybe Date2 -> Maybe Date2 -> Result String Bool  
infOuEgal d1 d2 =  
    let  
        inferieur = inf d1 d2
        eg        = egal d1 d2 
    in
        case (inferieur, eg) of
            (Ok inferieurb, Ok egb) -> Ok (inferieurb ||  egb)  
            (_ , _)            -> Err "format date incorrect" 

supOuEgal : Maybe Date2 -> Maybe Date2 -> Result String Bool  
supOuEgal d1 d2 =  
    let  
        superieur = sup d1 d2
        eg        = egal d1 d2 
    in
        case (superieur, eg) of
            (Ok superieurb, Ok egb) -> Ok (superieurb ||  egb)  
            (_ , _)            -> Err "format date incorrect" 


---     OPERATIONS SUR LES DATES    --- 

---     ANNEE      ---

anneePlus : Maybe Date2 -> Maybe Date2
anneePlus dateM = 
    dateM |> Maybe.andThen ( \date ->
        
            let jour0  = (dateVersAlias  date).jour
                mois0  =  (dateVersAlias  date).mois
                annee0 = (dateVersAlias  date).annee
            in newDate{jour = jour0, mois = mois0, annee = annee0 + 1}
    )

anneeMoins : Maybe Date2 -> Maybe Date2
anneeMoins dateM = 
    dateM |> Maybe.andThen ( \date ->
            let jour0  = (dateVersAlias  date).jour
                mois0  =  (dateVersAlias  date).mois
                annee0 = (dateVersAlias  date).annee
            in newDate{jour = jour0, mois = mois0, annee = annee0 - 1}
    )


---     MOIS    ---

moisPrecedent :Mois -> Mois 
moisPrecedent mois = 
    let
        moisActuel       = complete ( (listeInfoMois mois).numeroDuMois ) correspondances
        moisActuelNumero = (listeInfoMois mois).numeroDuMois 
    in
        if moisActuelNumero == 1
        then Decembre 
        else  complete (moisActuelNumero  - 1 ) correspondances



moisSuivant :Mois -> Mois 
moisSuivant mois = 
    let
        moisActuel       = complete ( (listeInfoMois mois).numeroDuMois ) correspondances
        moisActuelNumero = (listeInfoMois mois).numeroDuMois 
    in
        if moisActuelNumero == 12
        then Janvier
        else  complete (moisActuelNumero  + 1) correspondances




moisPlus  : Maybe  Date2 -> Maybe Date2
moisPlus dateM = 
    dateM |> Maybe.andThen ( \date -> 
            let jour0  = (dateVersAlias  date).jour
                mois0  =  (dateVersAlias  date).mois
                annee0 = (dateVersAlias  date).annee
            in
                if (mois0 == (listeInfoMois Decembre).mois)
                then   newDate{jour = 1, mois = Janvier, annee = annee0 + 1}
                else newDate{jour = 1, mois = moisSuivant mois0, annee = annee0 }
    )


moisMoins  : Maybe  Date2 -> Maybe Date2
moisMoins dateM = 
    dateM |> Maybe.andThen ( \date ->
            let jour0  = (dateVersAlias  date).jour
                mois0  =  (dateVersAlias  date).mois
                annee0 = (dateVersAlias  date).annee
            in
                if (mois0 == (listeInfoMois Janvier).mois)
                then   newDate{jour = 1, mois = Decembre, annee = annee0 - 1}
                else newDate{jour = 1, mois = moisPrecedent mois0, annee = annee0 }
    )





---     JOURS   ---

jourPrecedent : Maybe Date2 -> Maybe Date2
jourPrecedent dateM = 
    dateM |> Maybe.andThen ( \date ->
            let jour0  = (dateVersAlias  date).jour
                mois0  = (dateVersAlias  date).mois
                annee0 = (dateVersAlias  date).annee
            in
                if (jour0 == 1 && mois0 == (listeInfoMois Janvier).mois ) 
                then newDate {jour = 31, mois = Decembre, annee = annee0 - 1}
                else
                    if ((jour0 == 1) && (estBissextile annee0))
                    then newDate{jour = 29, mois = Fevrier, annee = annee0}
                    else
                        if (jour0 == 1)
                        then  newDate {jour = (listeInfoMois (moisPrecedent mois0)).nombreDeJours, mois = moisPrecedent mois0, annee = annee0}
                        else  newDate{jour = jour0 - 1 , mois = mois0, annee = annee0  }

    )


jourSuivant : Maybe Date2 -> Maybe Date2
jourSuivant dateM = 
    dateM |> Maybe.andThen ( \date ->
    
            let jour0  = (dateVersAlias  date).jour
                mois0  =  (dateVersAlias  date).mois
                annee0 = (dateVersAlias  date).annee
            in
                if (jour0 == 31 && mois0 == (listeInfoMois Decembre).mois ) 
                then newDate {jour = 1, mois = Janvier, annee = annee0 + 1}
                else
                    if ((jour0 == 28)&&(estBissextile annee0 )&& mois0 == (listeInfoMois Fevrier).mois)
                    then newDate {jour = 29, mois = Fevrier, annee = annee0 }
                    else 
                        if (jour0 < (listeInfoMois (dateVersAlias  date).mois).nombreDeJours)
                        then  newDate {jour = (dateVersAlias  date).jour + 1, mois =mois0, annee = annee0}
                        else  newDate{jour = 1 , mois = complete ( (listeInfoMois mois0).numeroDuMois + 1 ) correspondances, annee = annee0  }
        )



niemeJourAvant : Maybe Date2 -> Int -> Maybe Date2
niemeJourAvant date n = 
    if (n == 0) then date  
    else  
        if (n > 0)
        then niemeJourAvant (jourPrecedent date ) (n - 1 )
        else niemeJourApres ( jourSuivant date ) (-1 * n - 1)


niemeJourApres : Maybe Date2 -> Int -> Maybe Date2
niemeJourApres date n = 
    if (n == 0)
    then date  
    else
        if (n > 0) then niemeJourApres (jourSuivant date ) ( n  - 1)
       else  niemeJourAvant (jourPrecedent date ) (-1 * n - 1 )



---     EXEMPLES    ---

d0 = newDate  {jour  = 25 , mois = Mois.Septembre, annee = 2018}
d01 = newDate  {jour  = 27 , mois = Mois.Janvier, annee = 2018}
d03 = newDate{jour = 18, mois = Juillet , annee = 2019}



---     AFFICHAGE   ---

type Jour = Lundi
            |Mardi
            |Mercredi
            |Jeudi
            |Vendredi
            |Samedi
            |Dimanche

jourDeLaSemaine : Maybe Date2 -> Int
jourDeLaSemaine dateM =  
    case dateM of 
        Just date -> 
                let 
                        d = (dateVersAlias date).jour
                        mois = (dateVersAlias date).mois 
                        m = (listeInfoMois mois).numeroDuMois
                        y = (dateVersAlias date).annee  
                        z = y - 1
                in
                    if (m >= 3)
                    then modBy 7 (  23 * m//9 + d + 4 + y + y//4 - y//100 + y//400 - 2 ) 
                    else modBy 7 ( 23 * m//9 + d + 4 + y + z//4 - z//100 + z//400 ) 
        Nothing  -> -1



numeroEnJour : Int -> Jour  
numeroEnJour i = 
    if (i==0) then Dimanche
    else if (i == 1) then Lundi
    else if (i == 2) then Mardi
    else if (i == 3) then Mercredi
    else if (i == 4) then Jeudi
    else if (i == 5) then Vendredi
    else  Samedi

affichage : Maybe Date2 -> String 
affichage d =  
    case d of 
        Just dm -> Debug.toString((numeroEnJour (jourDeLaSemaine d)))++" "++ 
                  Debug.toString(((dateVersAlias dm).jour))++ " "++
                 Debug.toString((dateVersAlias dm).mois)++ " "++
                  Debug.toString(((dateVersAlias dm).annee))
        Nothing -> ""
