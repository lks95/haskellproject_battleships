type Koordinaten = (Int, Int)
type Schiff = [Koordinaten]
type Spielfeld = [[Bool]]
type Spieler = String

--spielfeldgröße setzen (erweiterbar)
spielFeldGroesse = 10

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

--schieffgroesse implementieren
minSchiffe = 1
maxSchiffe = 3

--schiffEingeben implementieren

--grundgedanke: wie werden die koordinaten eingegeben? (0|0) oder 0,0 oder (0,0) etc.
schiffEingeben :: Int -> [Schiff] -> IO [Schiff]
schiffEingeben gesetztesSchiff x = do
                                    putStrLn ("Bitte Koordinaten des Schiffes" ++ show x ++ "eingeben.")
                                    string <- getLine
                                    --wirft noch error hier, convertstring hier einbinden

--alle schiffe uebergeben fuer main
schiffeEingeben :: Int -> [Schiff] -> IO [Schiff]
schiffeEingeben schiffsgroesse gesetztesSchiff = if schiffsgroesse <= maxSchiffe then
                                                    do
                                                    schiff <- schiffEingeben gesetztesSchiff schiffsgroesse
                                                    alleschiffe <- schiffeEingeben (schiffsgroesse +1) (schiff : gesetztesSchiff)
                                                    return (schiff : alleschiffe)
                                                    else
                                                    return []


spielFeldUI :: String -> Spielfeld -> [Schiff] -> IO ()
spielFeldUI spielerName spielfeld schiffe = do
                                            putStrLn (spielerName ++ "'s Spielfeld:")
                                            putStrLn (take (spielFeldGroesse+2) (repeat '*') ++ "\n*" ++ spielfeld schiffe (1,1) ++ take (spielFeldGroesse+1) (repeat '*'))
                                            putStrLn ""

start :: [String] -> [Spielfeld] -> [[Schiff]] -> IO()
start namen feld schiffe = do
                                putStrLn ("\n" ++ head namen ++ "ist dran.")
                                spielFeldUI (last namen) (last feld) (last schiffe)

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
