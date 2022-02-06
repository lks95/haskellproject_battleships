type Koordinate = (Int, Int)
type Schiff = [Koordinate]
type Spielfeld = [[Bool]]
type Spieler = String

--spielfeldgröße setzen (erweiterbar)
spielFeldGroesse = 5
--schiffsgroesse implementieren
minSchiffe = 1
maxSchiffe = 2

--spielfeld setzen, groesse soll variabel sein, feldgroesse mit sich selber multiplizieren
spielFeldStart :: Spielfeld
spielFeldStart = take spielFeldGroesse (repeat (take spielFeldGroesse (repeat False)))

--tristan input, um die koordinaten aus einem string zu extrahieren
--bei invalider eingabe von koordinaten ? --> (-1, -1)
stringInKoordinateUmwandeln :: String -> Koordinate
stringInKoordinateUmwandeln ['(', x, ',', y, ')'] = ((read [x] :: Int) + 1, (read [y] :: Int) + 1)
stringInKoordinateUmwandeln x | otherwise = (-1, -1)

listeVonKoordinatenStrings :: String -> [String]
listeVonKoordinatenStrings [] = [[]]
listeVonKoordinatenStrings (x:xs) | x == ';' = [] : listeVonKoordinatenStrings xs
                                | otherwise = (x : head (listeVonKoordinatenStrings xs)) : tail (listeVonKoordinatenStrings xs)

namenEingeben :: IO [String]
namenEingeben = do
                putStrLn "Spieler 1 bitte Namen eingeben:"
                name1 <- getLine

                putStrLn "Spieler 2 bitte Namen eingeben:"
                name2 <- getLine

                return [name1, name2]

schiffKoordinatenValidieren :: [Schiff] -> Schiff -> Int -> Bool
schiffKoordinatenValidieren gesetzteSchiffe schiff schiffLaenge
    -- Überprüfung, ob genügend Koordinaten angegeben wurden für die Schifflänge
    | length schiff /= schiffLaenge = False
    -- Überprüfung, ob eine Koordinate des neuen Schiffs mit einem vorhandenen Schiff kollidiert 
    | or [coord1 == coord2 | schiff2 <- gesetzteSchiffe, coord1 <- schiff, coord2 <- schiff2] = False
    -- Überprüfung, ob alle Koordinaten im Spielfeld liegen
    | not (and [koordinateValidieren coord | coord <- schiff]) = False
    -- Überprüfung, ob alle Koordinaten des neuen Schiffes in einer Spalte liegen
    | and (map (== 0) [abs ((fst coord1) - (fst coord2)) | coord1 <- schiff, coord2 <- schiff])
        = (sum [abs ((snd coord1) - (snd coord2)) | coord1 <- schiff, coord2 <- schiff]) * 3 == (schiffLaenge-1) * (schiffLaenge^2 + schiffLaenge)
    -- Überprüfung, ob alle Koordinaten des neuen Schiffes in einer Zeile liegen
    | and (map (== 0) [abs ((snd coord1) - (snd coord2)) | coord1 <- schiff, coord2 <- schiff])
        = (sum [abs ((fst coord1) - (fst coord2)) | coord1 <- schiff, coord2 <- schiff]) * 3 == (schiffLaenge-1) * (schiffLaenge^2 + schiffLaenge)
    -- Wenn die Koordinaten nicht in einer Reihe liegen
    | otherwise = False

-- einzelnes Schiff anlegen via Koordinaten
-- x als laenge des schiffes
schiffEingeben :: [Schiff] -> Int -> IO Schiff
schiffEingeben gesetzteSchiffe x = do
                                    putStrLn ("Bitte Koordinaten des Schiffes mit der Größe " ++ show x ++ " eingeben.")
                                    string <- getLine
                                    let stringCoords = listeVonKoordinatenStrings string
                                    let coords = map stringInKoordinateUmwandeln stringCoords
                                    if schiffKoordinatenValidieren gesetzteSchiffe coords x then
                                        return coords
                                    else
                                        schiffEingeben gesetzteSchiffe x

-- alle Schiffe auf dem Feld platzieren
schiffeEingeben :: Int -> [Schiff] -> IO [Schiff]
schiffeEingeben schiffsGroesse gesetzteSchiffe = if schiffsGroesse <= maxSchiffe then
                                                    do
                                                        schiff <- schiffEingeben gesetzteSchiffe schiffsGroesse
                                                        alleSchiffe <- schiffeEingeben (schiffsGroesse +1) (schiff : gesetzteSchiffe)
                                                        return (schiff : alleSchiffe)
                                                else
                                                    return []

-- String-Repräsentation eines Felds ausgeben
feldZuString :: Spielfeld -> [Schiff] -> Koordinate -> String
feldZuString spielfeld schiffe koordinate
        | fst koordinate <= spielFeldGroesse
          && snd koordinate <= spielFeldGroesse = if select (fst koordinate) (select (snd koordinate) spielfeld) == True then
                                               if or [koordinate == coord | schiff <- schiffe, coord <- schiff] then 'O' : feldZuString spielfeld schiffe (fst koordinate + 1, snd koordinate)
                                                   else '*' : feldZuString spielfeld schiffe (fst koordinate + 1, snd koordinate)
                                           else ' ' : feldZuString spielfeld schiffe (fst koordinate + 1, snd koordinate)
                                        
        | snd koordinate <= spielFeldGroesse = "H\nH" ++ feldZuString spielfeld schiffe (1, snd koordinate + 1)
        | otherwise = []

--anzeigen des spielfelds
spielFeldUI :: String -> Spielfeld -> [Schiff] -> IO ()
spielFeldUI spielerName spielfeld schiffe = do
                                            putStrLn (spielerName ++ "'s Spielfeld:")
                                            putStrLn (take (spielFeldGroesse+2) (repeat 'H') ++ "\nH" ++ feldZuString spielfeld schiffe (1,1) ++ take (spielFeldGroesse+1) (repeat 'H'))
                                            putStrLn ""

--dass eigentliche spielen des spiels
--jeder spieler abwechselnd, es sei denn er trifft

--benoetigte, erfuellte parameter: namen von beiden spielern, spielfeld von beiden spielern und die schiffe
start :: [String] -> [Spielfeld] -> [[Schiff]] -> IO ()
start namen feld schiffe = do
                                putStrLn ("\n" ++ head namen ++ " ist dran.")
                                spielFeldUI (last namen) (last feld) (last schiffe)
                                (neuesFeld, neueSchiffListe) <- angreifenMitAllenSchiffen (last feld, last schiffe) (head schiffe)
                                if length neueSchiffListe == 0 then
                                                               do
                                                                 putStrLn ("\n" ++ head namen ++ " hat gewonnen!\n")
                                                                 spielFeldUI (last namen) neuesFeld neueSchiffListe
                                                                 spielFeldUI (head namen) (head feld) (head schiffe)
                                                           else
                                                               start [last namen, head namen] [neuesFeld, head feld] [neueSchiffListe, head schiffe]

-- Das n-te Element einer Liste ausgeben (ähnlich list !! (n - 1))
select :: Int -> [a] -> a
select n xs = xs !! (n-1)

--schiff als "getroffen" markieren
-- ein einzelenes feld des koordinatensystems als getroffen markieren
alsGetroffenMarkieren :: Spielfeld -> Int -> Int -> Spielfeld
alsGetroffenMarkieren feld x y = ersetzen x feld (ersetzen y (select x feld) True)

-- dass nte Element einer liste ersetzen
ersetzen :: Int -> [a] -> a -> [a]
ersetzen n xs x = take (n-1) xs ++ [x] ++ drop n xs

--schiffe kaputt schiessen/angreifen
    --wiederholender angriff bei treffer

angreifen :: (Spielfeld, [Schiff]) -> Koordinate -> (Spielfeld, [Schiff], Bool)
angreifen (gegnerSpielfeld, gegnerSchiffe) koordinate = (alsGetroffenMarkieren gegnerSpielfeld (snd koordinate) (fst koordinate),
                                            zerstoerteSchiffeEntf [fst (checkSchiffZerstoert gegnerSpielfeld schiff koordinate) | schiff <- gegnerSchiffe],
                                            or [snd (checkSchiffZerstoert gegnerSpielfeld schiff koordinate) | schiff <- gegnerSchiffe])
--validierungs sachen
-- überprüft, ob Schiff getroffen wurde
checkSchiffZerstoert :: Spielfeld -> Schiff -> Koordinate -> (Schiff, Bool)
checkSchiffZerstoert feld schiff koordinate
                                            -- Schiff wurde nicht getroffen
                                            | not (or [koordinate == koord | koord <- schiff]) = (schiff, False)
                                            -- Schiff wurde getroffen, aber nicht versenkt
                                            | not (and [select (fst koord) (select (snd koord) feld) == True | koord <- schiff, koord /= koordinate]) = (schiff, True)
                                            -- Schiff wurde nicht getroffen
                                            | otherwise = ([], True)

zerstoerteSchiffeEntf :: [Schiff] -> [Schiff]
zerstoerteSchiffeEntf [] = []
zerstoerteSchiffeEntf (x:xs) | null x = zerstoerteSchiffeEntf xs
                             | otherwise = x : zerstoerteSchiffeEntf xs


angreifenMitAllenSchiffen :: (Spielfeld, [Schiff]) -> [Schiff] -> IO (Spielfeld, [Schiff])
angreifenMitAllenSchiffen (gegnerSpielfeld, gegnerSchiffe) [] = return (gegnerSpielfeld, gegnerSchiffe)
angreifenMitAllenSchiffen (gegnerSpielfeld, gegnerSchiffe) eigeneSchiffe = do

                                                        putStrLn ("Koordinaten eingeben auf die gefeuert werden soll (noch " ++ show (length eigeneSchiffe) ++ " Schüsse übrig)")
                                                        string <- getLine

                                                        let koordinate = stringInKoordinateUmwandeln string
                                                        if koordinateValidieren koordinate then
                                                            do
                                                              let (gegnerSpielfeldNeu, gegnerSchiffeNeu, getroffen) = angreifen (gegnerSpielfeld, gegnerSchiffe) koordinate

                                                              if getroffen then
                                                                  putStrLn ("Auf Koordinate (" ++ show ((fst koordinate) - 1) ++ "," ++ show ((snd koordinate) - 1) ++ ") gefeuert und getroffen!")
                                                              else
                                                                  putStrLn ("Auf Koordinate (" ++ show ((fst koordinate) - 1) ++ "," ++ show ((snd koordinate) - 1) ++ ") gefeuert und verfehlt!")

                                                              if length gegnerSchiffeNeu < length gegnerSchiffeNeu then
                                                                  do
                                                                    putStrLn "Du hast mein Schiff getroffen und versunken!"
                                                                    angreifenMitAllenSchiffen (gegnerSpielfeldNeu, gegnerSchiffeNeu) (tail eigeneSchiffe)
                                                              else
                                                                  angreifenMitAllenSchiffen (gegnerSpielfeldNeu, gegnerSchiffeNeu) (tail eigeneSchiffe)
                                                        else
                                                            -- putStrLn "Koordinaten fehlerhaft, Format: (0,0)"
                                                            angreifenMitAllenSchiffen (gegnerSpielfeld, gegnerSchiffe) eigeneSchiffe


--gesunkene schiffe markieren

--schreibweise koordinaten input

-- Überprüfe, ob eine Koordinate im gültigen Bereich liegt
    --liegt die vom user eingegebene koordinate im spielfeld? (angriff)
    --liegt die vom user eingegebene koordinate im spielfeld? (schiffe platzieren)
koordinateValidieren :: Koordinate -> Bool
koordinateValidieren koord = fst koord >= 1 &&
                            snd koord >= 1 &&
                            fst koord <= spielFeldGroesse &&
                            snd koord <= spielFeldGroesse

main :: IO ()
main = do

    --auslagern in methode
    spielernamen <- namenEingeben

    putStrLn ("Jeder Spieler verfügt über " ++ show (maxSchiffe - minSchiffe + 1) ++ " Schiffe.")
    putStrLn("Das Spielfeld ist " ++ show spielFeldGroesse ++ " * " ++ show spielFeldGroesse ++ " Felder groß.")
    --head and tail bzw. last fuer erstes und letztes element (der namen)
    --bei schiffeEingeben muessen alle Schiffe uergeben werden, nicht nur eins
    putStrLn (head spielernamen ++ ", bitte setze deine Schiffe")
    putStrLn ("Das Eingabeformat entspricht '(1,1);(1,2)'")
    spieler1 <- schiffeEingeben minSchiffe []

    putStrLn (last spielernamen ++ ", bitte setze deine Schiffe")
    putStrLn ("Das Eingabeformat entspricht '(1,1);(1,2)'")
    spieler2 <- schiffeEingeben minSchiffe []

    --funktion fehlt zum starten, start funktion sollte auch UI beinhalten
    start spielernamen [spielFeldStart, spielFeldStart] [spieler1, spieler2]