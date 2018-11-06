module Mois exposing (..)


type Mois = Janvier
            |Fevrier
            |FevrierBis
            |Mars            
            |Avril
            |Mai
            |Juin
            |Juillet
            |Aout
            |Septembre
            |Octobre
            |Novembre
            |Decembre
            |Faux




listeInfoMois : Mois -> {mois : Mois , numeroDuMois : Int, nombreDeJours : Int}
listeInfoMois m = 
        case m of
            Janvier  -> {mois = Janvier , numeroDuMois =1 , nombreDeJours = 31}
            Fevrier  -> {mois = Fevrier , numeroDuMois =2 , nombreDeJours = 28}
            FevrierBis  -> {mois = Fevrier , numeroDuMois =2 , nombreDeJours = 29}
            Mars     -> {mois = Mars    , numeroDuMois =3 , nombreDeJours = 31}       
            Avril    -> {mois = Avril , numeroDuMois =4 , nombreDeJours = 30}
            Mai      -> {mois = Mai , numeroDuMois =5 , nombreDeJours = 31}
            Juin     -> {mois = Juin , numeroDuMois =6 , nombreDeJours = 30}
            Juillet   -> {mois = Juillet , numeroDuMois =7 , nombreDeJours = 31}
            Aout     -> {mois = Aout , numeroDuMois = 8 , nombreDeJours = 31}
            Septembre -> {mois = Septembre , numeroDuMois = 9 , nombreDeJours = 30}
            Octobre   -> {mois = Octobre , numeroDuMois = 10 , nombreDeJours = 31}
            Novembre -> {mois = Novembre , numeroDuMois = 11 , nombreDeJours = 30}
            Decembre   -> {mois = Decembre , numeroDuMois =12 , nombreDeJours = 31}
            Faux       -> {mois = Faux , numeroDuMois = -1 , nombreDeJours = -1}


pair : Int  -> Mois -> { moisNum : Int , mois : Mois}
pair x y = {moisNum = x , mois = y}

zip : List Int -> List Mois -> List { moisNum : Int , mois : Mois}
zip xs ys =   List.map2 pair xs ys

correspondances : List { moisNum : Int , mois : Mois}
correspondances = zip (List.range 1 12) listeNomMois

listeNomMois = [Janvier , Fevrier , Mars  ,    Avril  ,Mai,Juin ,Juillet,Aout,Septembre ,Octobre,Novembre,Decembre]

complete : Int ->  List {moisNum: Int , mois : Mois} -> Mois
complete num l =
    case l of
        l0::ls -> if (l0.moisNum == num) then l0.mois
                    else complete num ls
        []      -> Janvier
