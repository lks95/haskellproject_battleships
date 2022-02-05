type Koordinaten = (Int, Int)
type Schiff = [Koordinaten]
type Spielfeld = [[Bool]]
type Spieler = String

--spielfeldgröße setzen (erweiterbar)
spielFeldGroesse = 10

--spielfeld setzen, groesse soll variabel sein, feldgroesse mit sich selber multiplizieren
spielFeldStart :: Field
spielFeldStart = take spielFeldGroesse (repeat (take spielFeldGroesse (repeat False)))


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

--alle schiffe uebergeben fuer main
schiffeEingeben :: Int -> [Schiff] -> IO [Schiff]
schiffeEingeben schiffsgroesse gesetztesSchiff = if schiffsgroesse <= maxSchiffe then
                                                    do
                                                    schiff <- schiffEingeben gesetztesSchiff schiffsgroesse
                                                    alleschiffe <- schiffeEingeben (schiffsgroesse +1) (schiff : gesetztesSchiff)
                                                    return (schiff : alleschiffe)
                                                    else
                                                    return []
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

    --variable fehlt zum starten
    spielernamen [spielFeldStart, spielFeldStart] [spieler1, spieler2]
