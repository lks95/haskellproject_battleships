type Koordinaten = (Int, Int)
type Schiff = [Koordinaten]
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
convertStringToCoordinates :: String -> Koordinaten
convertStringToCoordinates ['(', x, ',', y, ')'] = ((read [x] :: Int) + 1, (read [y] :: Int) + 1)
convertStringToCoordinates  = (-1, -1)

namenEingeben :: IO [String]
namenEingeben = do
                putStrLn "Spieler 1 bitte Namen eingeben:"
                name1 <- getLine

                putStrLn "Spieler 2 bitte Namen eingeben:"
                name2 <- getLine

                return [name1, name2]

--schiffEingeben implementieren
--grundgedanke: wie werden die koordinaten eingegeben? (0|0) oder 0,0 oder (0,0) etc.

--(0,1);(0,2)
--einzelnes schiff anlegen via koordinaten
--x als laenge des schiffes
schiffEingeben :: Int -> [Schiff] -> IO [Schiff]
schiffEingeben gesetztesSchiff x = do
                                    putStrLn ("Bitte Koordinaten des Schiffes" ++ show x ++ "eingeben.")
                                    string <- getLine
                                    --wirft noch error hier, convertstring hier einbinden

--alle schiffe auf dem feld platzieren
schiffeEingeben :: Int -> [Schiff] -> IO [Schiff]
schiffeEingeben schiffsGroesse gesetzteSchiffe = if schiffsgroesse <= maxSchiffe then
                                                    do
                                                    schiff <- schiffEingeben gesetzteSchiffe schiffsGroesse
                                                    alleschiffe <- schiffeEingeben (schiffsGroesse +1) (schiff : gesetzteSchiffe)
                                                    return (schiff : alleschiffe)
                                                    else
                                                    return []

--anzeigen des spielfelds
spielFeldUI :: String -> Spielfeld -> [Schiff] -> IO ()
spielFeldUI spielerName spielfeld schiffe = do
                                            putStrLn (spielerName ++ "'s Spielfeld:")
                                            putStrLn (take (spielFeldGroesse+2) (repeat '*') ++ "\n*" ++ spielfeld schiffe (1,1) ++ take (spielFeldGroesse+1) (repeat '*'))
                                            putStrLn ""

--dass eigentliche spielen des spiels
--jeder spieler abwechselnd, es sei denn er trifft

--benoetigte, erfuellte parameter: namen von beiden spielern, spielfeld von beiden spielern und die schiffe
start :: [String] -> [Spielfeld] -> [[Schiff]] -> IO()
start namen feld schiffe = do
                                putStrLn ("\n" ++ head namen ++ "ist dran.")
                                spielFeldUI (last namen) (last feld) (last schiffe)

--schiff als "getroffen" markieren
-- ein einzelenes feld des koordinatensystems als getroffen markieren
alsGetroffenMarkieren :: Spielfeld -> Int -> Int -> Spielfeld
alsGetroffenMarkieren feld x y = ersetzen x feld (ersetzen y (select x feld) True)

-- dass nte Element einer liste ersetzen
ersetzen :: Int -> [a] -> a -> [a]
ersetzen n xs x = take (n-1) xs ++ [x] ++ drop n xs

--schiffe kaputt schiessen/angreifen
    --wiederholender angriff bei treffer

angreifen :: (Spielfeld, [Schiff]) -> Koordinaten -> (Spielfeld, [Schiff], Bool)
angreifen (gegnerSpielfeld, gegnerSchiffe) koordinate = (alsGetroffenMarkieren gegnerSpielfeld (snd koordinate) (fst koordinate),
                                            zerstoerteSchiffeEntf [fst (checkSchiffZerstoert gegnerSpielfeld schiff koordinate) | schiff <- gegnerSchiffe],
                                            or [snd (checkSchiffZerstoert gegnerSpielfeld schiff koordinate) | schiff <- gegnerSchiffe])
--validierungs sachen
-- überprüft, ob Schiff getroffen wurde
checkSchiffZerstoert :: Feld -> Schiff -> Koordinate -> (Schiff, Bool)
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


angreifenMitAllenSchiffen :: (Field, [Ship]) -> [Ship] -> IO (Field, [Ship])
angreifenMitAllenSchiffen (gegnerSpielfeld, gegnerSchiffe) [] = return (gegnerSpielfeld, gegnerSchiffe)
angreifenMitAllenSchiffen (gegnerSpielfeld, gegnerSchiffe) eigeneSchiffe = do

                                                        putStrLn ("Koordinaten eingeben auf die gefeuert werden soll (noch" ++ show (length ourShips) ++ "Schuesse uebrig)")
                                                        string <- getLine

                                                        let koordinate = convertStringToCoordinates string

                                                        if koordinateValidieren koordinate then
                                                            do
                                                              let (gegnerSpielfeldNeu, gegnerSchiffeNeu, getroffen) = angreifen (gegnerSpielfeld, gegnerSchiffe) koordinate

                                                              if hit then
                                                                  putStrLn ("Auf Koordinate (" ++ show ((fst koordinate) - 1) ++ "," ++ show ((koordinate) - 1) ++ ") gefeuert und getroffen!")
                                                              else
                                                                  putStrLn ("Auf Koordinate (" ++ show ((fst koordinate) - 1) ++ "," ++ show ((koordinate) - 1) ++ ") gefeuert und verfehlt!")

                                                              if length gegnerSchiffeNeu < length gegnerSchiffeNeu then
                                                                  do
                                                                    putStrLn "Du hast mein Schiff getroffen und versunken!"
                                                                    angreifenMitAllenSchiffen (gegnerSpielfeldNeu, gegnerSchiffeNeu) (tail eigeneSchiffe)
                                                              else
                                                                  angreifenMitAllenSchiffen (gegnerSpielfeldNeu, gegnerSchiffeNeu) (tail eigeneSchiffe)
                                                        else
                                                            putStrLn "Koordinaten fehlerhaft, Format: (0,0)"
                                                            angreifenMitAllenSchiffen (gegnerSpielfeld, gegnerSchiffe) eigeneSchiffe


--gesunkene schiffe markieren

--schreibweise koordinaten input

--funktion zum pruefen ob ein schiff komplett getroffen wurde
    --laenge des schiffs
    --getroffenes feld/koordinate eines schiffs




-- Überprüfe, ob eine Koordinate im gültigen Bereich liegt
    --liegt die vom user eingegebene koordinate im spielfeld? (angriff)
    --liegt die vom user eingegebene koordinate im spielfeld? (schiffe platzieren)
koordinatenValidieren :: Koordinaten -> Bool
koordinatenValidieren koord = fst koord >= 1 &&
                            snd koord >= 1 &&
                            fst koord <= fieldSize &&
                            snd koord <= fieldSize

main :: IO ()
main = do

    --auslagern in methode
    spielernamen <- namenEingeben

    --head and tail bzw. last fuer erstes und letztes element (der namen)
    --bei schiffeEingeben muessen alle Schiffe uergeben werden, nicht nur eins
    putStrLn (head spielernamen ++ ", bitte setze deine Schiffe")
    spieler1 <- schiffeEingeben schiffGroesse []

    putStrLn (last spielernamen ++ ", bitte setze deine Schiffe")
    spieler2 <- schiffeEingeben schiffGroesse []

    --funktion fehlt zum starten, start funktion sollte auch UI beinhalten
    spielernamen [spielFeldStart, spielFeldStart] [spieler1, spieler2]
