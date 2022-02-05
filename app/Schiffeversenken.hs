type Koordinaten = (Int, Int)
type Schiff = [Koordinaten]
type Spielfeld = [[Bool]]
type Spieler = String

--spielfeldgröße setzen (erweiterbar)
spielFeldGroesse = 10
--schiffsgroesse implementieren
minSchiffe = 1
maxSchiffe = 3

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

--schiffe kaputt schiessen/angreifen
    --wiederholender angriff bei treffer

--schiff als "getroffen" markieren

--gesunkene schiffe markieren

--schreibweise koordinaten input

--funktion zum pruefen ob ein schiff komplett getroffen wurde
    --laenge des schiffs
    --getroffenes feld/koordinate eines schiffs

--validierungs sachen
    --liegt die vom user eingegebene koordinate im spielfeld? (angriff)
    --liegt die vom user eingegebene koordinate im spielfeld? (schiffe platzieren)


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
