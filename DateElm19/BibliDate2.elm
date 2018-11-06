module BibliDate2 exposing (..)
import Mois exposing (..)
import Date2 exposing (..)

import Html exposing (..)

import Html.Events exposing (..)




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

supOuEgal : Maybe Date2 -> Maybe Date2 -> Result String Bool  
supOuEgal d1 d2 = 
    let 
        comp = compD1D2 d1 d2
    in
        case comp of 
            Just LT -> Ok False
            Just EQ -> Ok True 
            Just GT -> Ok True 
            Nothing -> Err "format date incorrect"


egal : Maybe Date2 -> Maybe Date2 -> Result String Bool  
egal d1 d2 = 
    let 
        comp = compD1D2 d1 d2
    in
        case comp of 
            Just LT -> Ok False
            Just EQ -> Ok True 
            Just GT -> Ok False 
            Nothing -> Err "format date incorrect"



inf : Maybe Date2 -> Maybe Date2 -> Result String Bool  
inf d1 d2 = 
    let
        supE = supOuEgal d1 d2
    in
        case supE of
            Ok supEb -> Ok (not supEb )  
            _            -> Err "format date incorrect" 


infOuEgal : Maybe Date2 -> Maybe Date2 -> Result String Bool  
infOuEgal d1 d2 =  
    let  
        inferieur = inf d1 d2
        eg        = egal d1 d2 
    in
        case (inferieur, eg) of
            (Ok inferieurb, Ok egb) -> Ok (inferieurb ||  egb)  
            (_ , _)            -> Err "format date incorrect" 

sup : Maybe Date2 -> Maybe Date2 -> Result String Bool  
sup d1 d2 = 
    let
        supE = supOuEgal d1 d2
        infE = infOuEgal d1 d2
        eg   = egal d1 d2
    in
        case (supE, infE, eg) of
            (Ok supEb, Ok infEb, Ok egb) -> Ok (supEb && not egb)  
            (_ , _, _)            -> Err "format date incorrect" 


---


---

--- Jour De La Semaine 

type Jour = Lundi
            |Mardi
            |Mercredi
            |Jeudi
            |Vendredi
            |Samedi
            |Dimanche

datetest : Maybe Date2
datetest = newDate {jour = 11, mois = Septembre, annee = 2018}
nomJourtest : Jour
nomJourtest = Mardi

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
                    then modBy (  23 * m//9 + d + 4 + y + y//4 - y//100 + y//400 - 2 ) 7
                    else modBy ( 23 * m//9 + d + 4 + y + z//4 - z//100 + z//400 ) 7
        Nothing  -> -1

date3 = newDate{jour = 11, mois = Septembre, annee = 2018}
j : Jour
j = numeroEnJour (jourDeLaSemaine date3)

numeroEnJour : Int -> Jour  
numeroEnJour i = 
    if (i==0) then Dimanche
    else if (i == 1) then Lundi
    else if (i == 2) then Mardi
    else if (i == 3) then Mercredi
    else if (i == 4) then Jeudi
    else if (i == 5) then Vendredi
    else  Samedi
----

fromJour : Jour -> String 
fromJour jourj = "jourj"

--- affichage 

aff2 : Maybe Date2 -> String 
aff2 dateM = 
    case dateM of 
        Just date -> let 
                        d = (dateVersAlias date).jour
                  
                in  String.fromInt d
        Nothing -> ""


--sortWith : (a -> a -> Order) -> List a -> List a

compD1D2Correct : List (Maybe Date2) -> List Date2
compD1D2Correct l0 = case l0 of 
        x :: l -> 
            case x of 
                Just x0 -> x0 :: (compD1D2Correct l)
                Nothing  ->  (compD1D2Correct l)
        _ -> []
avancement : Maybe Date2 -> Int
avancement dM =
    case dM of   
        Just d -> 
            let
                annee = (dateVersAlias d).annee
                mois = (dateVersAlias d).mois
                numMois =  (listeInfoMois mois).numeroDuMois
                z = List.map (\y -> (listeInfoMois (complete y correspondances)).nombreDeJours) (List.range 1 (numMois - 1 ) )
            in ((List.sum z) + (dateVersAlias d).jour)
        _ -> 0

--   31 decembre - avancement date1 + avancement date2
ecartD1D2 :  Maybe Date2 ->Maybe Date2 -> Int
ecartD1D2 date1 date2  = 
    case (date1, date2) of 
        (Just d1, Just d2) -> 
            let d3 = newDate{jour = 31, mois = Decembre, annee = (dateVersAlias d1).annee}
            in  ((avancement d3) -  (avancement date1)) + (avancement date2)
        (_ ,_) ->0


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

estMardi : Maybe Date2 -> Bool
estMardi dM = (numeroEnJour (jourDeLaSemaine dM) == Mardi )
    
f date =List.concat ( List.map (List.filter (\l -> (\y -> estMardi y) l )) (partage2 (joursMois2 date)))

--f date = List.map (List.filter (\l  -> String.contains "Mardi"  l )) (partage (joursMois date))
meetupElm : Maybe Date2 -> Maybe Date2 
meetupElm date =   
    let 
        aux : List (Maybe Date2 ) -> Int -> Maybe Date2 
        aux l0 cpt = 
            case l0 of 
                x::l -> if (cpt /= 2 )
                        then aux l (cpt + 1)
                        else x
                []  -> Nothing

    in aux (f date ) 0


premierLundi : Maybe Date2 -> Maybe Date2
premierLundi dateM = 
    dateM |> Maybe.andThen ( \date ->
             let acc date0 =
                            if (numeroEnJour (jourDeLaSemaine date0) /= Lundi)
                            then acc (jourSuivant date0)
                            else date0
                    
            in acc (newDate {jour = 1, mois = (dateVersAlias date).mois, annee =(dateVersAlias date).annee}) 
    )

dernierDimanche : Maybe Date2 -> Maybe Date2
dernierDimanche  dateM = 
    dateM |> Maybe.andThen ( \date ->
             let acc date0 =
                            if (numeroEnJour (jourDeLaSemaine date0) /= Dimanche)
                            then acc (jourPrecedent date0)
                            else date0
                                
            in acc (newDate {jour = (listeInfoMois (dateVersAlias date).mois).nombreDeJours, mois = (dateVersAlias date).mois, annee =(dateVersAlias date).annee}) 
    )

----------- 

joursMois2 : Maybe Date2 -> List (Maybe Date2)
joursMois2  dateM  =  
    case dateM of   
        Just date -> 
            let  
                acc dateDebut debut n fin = 
                    if (n > fin)
                    then []
                    else dateDebut :: (acc (jourSuivant dateDebut) (debut + 1 ) ( n + 1) fin)
            in acc (newDate {jour = 1, mois = (dateVersAlias date).mois, annee =(dateVersAlias date).annee})  1 1 (listeInfoMois  (dateVersAlias date).mois ).nombreDeJours 
        Nothing -> []

completeAvant2 : Maybe Date2 -> List (Maybe Date2)
completeAvant2 dateM =
    case dateM of   
        Just date -> 
            let premLundi = premierLundi dateM
            in
                let 
                    jMoisAvant = (listeInfoMois  (moisPrecedent (dateVersAlias date).mois )).nombreDeJours 
                    numLundi   = 
                        case premLundi of 
                            Just x -> (dateVersAlias x).jour 
                            Nothing  -> 0
                in
                    let  
                        acc dateDebut  n fin = 
                            if (n > fin)
                            then []
                            else dateDebut :: (acc (jourSuivant dateDebut)  ( n + 1) fin) -- (toString (numeroEnJour (jourDeLaSemaine dateDebut))++ " " ++ (toString (dateVersAlias x).jour)):: (acc (jourSuivant dateDebut)  ( n + 1) fin)
                    in acc (niemeJourAvant premLundi 7 ) ( jMoisAvant - (7 - numLundi ))  jMoisAvant 
        Nothing -> []


completeApres2 : Maybe Date2 -> List (Maybe Date2)
completeApres2 dateM =
    case dateM of   
        Just date -> 
            let dernDimanche = dernierDimanche dateM
            in
                let 
                    jMois = (listeInfoMois   (dateVersAlias date).mois ).nombreDeJours 
                    numDimanche   = 
                        case dernDimanche of 
                            Just x -> (dateVersAlias x).jour 
                            Nothing  -> 0
                in
                    let  
                        acc dateDebut  n fin = 
                            if (n > fin)
                            then []
                            else dateDebut :: (acc (jourSuivant dateDebut)  ( n + 1) fin) 
                    in acc (jourSuivant (newDate {jour = jMois, mois = (dateVersAlias date).mois, annee =(dateVersAlias date).annee})) 1 ( 7 - (jMois  - numDimanche ))
        Nothing -> []



partage2 : List (Maybe Date2) -> List(List (Maybe Date2)) 
partage2 l0 = 
    let aux l cpt =
                    if (not (List.isEmpty l))
                    then if (( modBy cpt 7 ) /= 0)
                        then [(List.take 7 l)]++ (aux (List.drop 7 l) ( 1  + cpt))
                        else (aux l ( 1  + cpt))
                    else []
    in aux l0 0 

--------------- pareil mais avec des monade 

-- rappel  : Maybe.andThen : ( a -> Maybe b  ) -> Maybe a -> Maybe b

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







niemeJourApres : Maybe Date2 -> Int -> Maybe Date2
niemeJourApres date n = 
    if (n == 0)
    then date  
    else
        if (n > 0) then niemeJourApres (jourSuivant date ) ( n  - 1)
       else  niemeJourAvant (jourPrecedent date ) (-1 * n - 1 )




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

niemeJourAvant : Maybe Date2 -> Int -> Maybe Date2
niemeJourAvant date n = 
    if (n == 0) then date  
    else  
        if (n > 0)
        then niemeJourAvant (jourPrecedent date ) (n - 1 )
        else niemeJourApres ( jourSuivant date ) (-1 * n - 1)

