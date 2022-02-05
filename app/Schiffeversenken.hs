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
--schiffeEingeben implementieren


main :: IO ()
main = do

    --auslagern in methode
    spielernamen <- namenEingeben

    --head and tail bzw. last fuer erstes und letztes element (der namen)
    putStrLn (head spielernamen ++ ", bitte setze deine Schiffe")
    schiffeSpieler1 <- schiffeEingeben schiffGroesse []
