
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import qualified GI.Gtk as Gtk 
import Data.GI.Base
import System.Environment
import System.Directory
import System.IO ( hClose, hIsEOF, openFile, hGetContents, IOMode(ReadMode) )
import System.Glib.UTFString


donate :: String -> String -> Bool 
donate _ "AB" = True 
donate "O" _ = True 
donate "A" "A" = True 
donate "B" "B" = True 
donate _ _ = False 


bmi :: Double -> Double -> (Int, String)
bmi v l | x >= 30 = (round x,"Obese")
        | x < 30 && x > 25 = (round x, "Overweight")
        | x < 25 && x > 18.5 = (round x, "Normal weight")
        | otherwise = (round x, "Underweight")
            where
                x = v / (l * l)


main :: IO()
main = do
    Gtk.init Nothing 

    win <- new Gtk.Window[#title := "Journal System"]
    
    #resize win 640 480
    #setPosition win Gtk.WindowPositionCenter 

    vertBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add win vertBox

    msg <- new Gtk.Label [#label := "Tommys sjukstuga"]
    #add vertBox msg

    horBox1 <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #add vertBox horBox1

    nameLabel <- new Gtk.Label [#label := "Username:  "]
    #add horBox1 nameLabel

    usernameEntry <- Gtk.entryNew
    Gtk.containerAdd horBox1 usernameEntry

    horBox2 <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #add vertBox horBox2

    passLabel <- new Gtk.Label [#label := "Password:   "]
    #add horBox2 passLabel

    passEntry <- Gtk.entryNew
    Gtk.containerAdd horBox2 passEntry
    Gtk.entrySetVisibility passEntry False 
    
    loginBtn <- new Gtk.Button [#label := "Login"]
    #add vertBox loginBtn

    exitBtn <- new Gtk.Button [#label := "Exit"]
    #add vertBox exitBtn

    on exitBtn #clicked Gtk.mainQuit 

    

    on loginBtn #clicked $ do 
        userIn <- Gtk.entryGetText usernameEntry
        passIn <- Gtk.entryGetText passEntry

        if userIn == "benjamin" && passIn == "strandberg" then do invokeMenuScreen win
        else set msg [#label := "Incorrect login credentials"]
        

    #showAll win

    Gtk.main 



invokeMenuScreen :: Gtk.Window  -> IO()
invokeMenuScreen win = do 
    #destroy win
    
    menuState <- new Gtk.Window [#title := "Journal System"]

    #resize menuState 640 480
    #setPosition menuState Gtk.WindowPositionCenter 
    on menuState #destroy main

    vertBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add menuState vertBox

    qaButton <- new Gtk.Button [#label := "Quick access"]
    #add vertBox qaButton

    getRecordBtn <- new Gtk.Button [#label := "Open record"]
    #add vertBox getRecordBtn

    addPatientbtn <- new Gtk.Button [#label := "Add patient"]
    #add vertBox addPatientbtn

    bmiCalcBtn <- new Gtk.Button [#label := "BMI Calculator"]
    #add vertBox bmiCalcBtn

    donationCalcBtn <- new Gtk.Button [#label := "Blood Donation Calculator"]
    #add vertBox donationCalcBtn


    on addPatientbtn #clicked $ do
        invokeAddPatientWindow 

    on qaButton #clicked $ do
        invokeQAWindow

    on getRecordBtn #clicked $ do
        openRecordWindow

    on bmiCalcBtn #clicked $ do
        invokeBmiCalcWin

    on donationCalcBtn #clicked $ do 
        invokeBTCalcWin
    

    horBox1 <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #add vertBox horBox1
    
    #showAll menuState


invokeQAWindow :: IO()
invokeQAWindow = do
    qaWindow <- new Gtk.Window [#title := "Journal System"]

    #resize qaWindow 320 240
    #setPosition qaWindow Gtk.WindowPositionCenter 
    vertBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add qaWindow vertBox

    horBox1 <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #add vertBox horBox1
    
    horBox2 <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #add vertBox horBox2

    label1 <- new Gtk.Label [#label := "Patient: "]
    #add horBox1 label1

    label2 <- new Gtk.Label [#label := "Info:       "]
    #add horBox2 label2

    patientEntry <- Gtk.entryNew 
    #add horBox1 patientEntry

    info <- Gtk.entryNew 
    #add horBox2 info

    confirmBtn <- new Gtk.Button [#label := "Search"]
    #add vertBox confirmBtn

    label <- new Gtk.Label [#label := " "]
    #add vertBox label

    

    on confirmBtn #clicked $ do
        name <- Gtk.entryGetText patientEntry
        
        let filename = filter ( /= ' ') $ glibToString name ++ ".txt"
        exist <- doesFileExist filename
        if not exist then set label [#label := "The patient is not in our records"]
        else do
            journal <- openFile filename ReadMode
            hasLine <- hIsEOF journal
            content <- if not hasLine
                            then hGetContents journal
                            else return "empty"
            i <- Gtk.entryGetText info
            let contentF = lines content

            let index | i == "age" = "3"
                      | i == "gender" = "4"
                      | i == "height" = "5"
                      | i == "weight" = "6"
                      | i == "bloodtype" = "7"
                      | i == "bmi" = "8"
                      | otherwise = "null"
            
            let outPut = searchList contentF index
            print outPut

            set label [#label := stringToGlib outPut]
            hClose journal


    #showAll qaWindow
   
        


searchList :: [String] -> String -> String 
searchList [] _ = "This info doesn't exist in this system."
searchList (x:xs) y | y == [head x] = drop 1 x
                    | otherwise = searchList xs y
        
    





invokeAddPatientWindow :: IO()
invokeAddPatientWindow = do
    

    addWindow <- new Gtk.Window [#title := "Journal System"]

    #resize addWindow 320 240
    #setPosition addWindow Gtk.WindowPositionCenter 
    
    vertBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add addWindow vertBox

    horBox1 <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #add vertBox horBox1

    horBox2 <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #add vertBox horBox2

    horBox3 <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #add vertBox horBox3

    horBox4 <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #add vertBox horBox4

    horBox5 <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #add vertBox horBox5

    horBox6 <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #add vertBox horBox6

    horBox7 <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #add vertBox horBox7

    fnameLabel <- new Gtk.Label [#label := "First Name: "]
    #add horBox1 fnameLabel

    lnameLabel <- new Gtk.Label [#label := "Last Name: "]
    #add horBox2 lnameLabel

    ageLabel <- new Gtk.Label   [#label := "Age:             "]
    #add horBox3 ageLabel

    gLabel <- new Gtk.Label     [#label := "Gender:      "]
    #add horBox4 gLabel

    hLabel <- new Gtk.Label     [#label := "Height:        "]
    #add horBox5 hLabel

    wLabel <- new Gtk.Label     [#label := "Weight:       "]
    #add horBox6 wLabel

    bTLabel <- new Gtk.Label    [#label := "Bloodtype:  "]
    #add horBox7 bTLabel

    fName <- Gtk.entryNew
    Gtk.containerAdd horBox1 fName

    lName <- Gtk.entryNew 
    Gtk.containerAdd horBox2 lName

    age <- Gtk.entryNew 
    Gtk.containerAdd horBox3 age

    gender <- Gtk.entryNew 
    Gtk.containerAdd horBox4 gender

    height <- Gtk.entryNew 
    Gtk.containerAdd horBox5 height

    weight <- Gtk.entryNew 
    Gtk.containerAdd horBox6 weight

    bloodType <- Gtk.entryNew 
    Gtk.containerAdd horBox7 bloodType

    addBtn <- new Gtk.Button [#label := "Add patient"]
    #add vertBox addBtn


    on addBtn #clicked $ do
        
        f <- Gtk.entryGetText fName
        l <- Gtk.entryGetText lName
        a <- Gtk.entryGetText age
        g <- Gtk.entryGetText gender
        h <- Gtk.entryGetText height
        w <- Gtk.entryGetText weight
        b <- Gtk.entryGetText bloodType

        let height = read (glibToString h) :: Double
        let weight = read (glibToString w) :: Double
        let bmiCalc = bmi weight height 

        let fileName = glibToString f ++ glibToString l ++ ".txt" 
        let text = "1" ++ glibToString f ++ "\n" ++ "2" ++ glibToString l ++ "\n" ++ "3" ++ glibToString a ++ "\n" ++ "4" ++ 
                    glibToString g ++ "\n" ++ "5" ++ glibToString h ++ "\n" ++ "6" ++ glibToString w ++ "\n" ++ "7" ++ glibToString b ++ "\n" ++ "8" ++ show bmiCalc

        writeFile fileName text

        #destroy addWindow
    #showAll addWindow
    

openRecordWindow :: IO()
openRecordWindow = do
    openRecordWin <- new Gtk.Window [#title := "Journal System"]
    #resize openRecordWin 320 240
    #setPosition openRecordWin Gtk.WindowPositionCenter 
        
    vertBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add openRecordWin vertBox

    horBox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #add vertBox horBox

    label <- new Gtk.Label [#label := "Patient: "]
    #add horBox label
        
    patientEntry <- Gtk.entryNew 
    Gtk.containerAdd horBox patientEntry

    searchBtn <- new Gtk.Button [#label := "Open"]
    #add vertBox searchBtn

    out <- new Gtk.Label [#label := " "]
    #add vertBox out

    on searchBtn #clicked $ do
        patientEntry <- Gtk.entryGetText patientEntry
        let filename = filter ( /= ' ') $ glibToString patientEntry ++ ".txt"
        exist <- doesFileExist filename
        
        if not exist then set out [#label := "The patient is not in our records"]
        else do
            journal <- openFile filename ReadMode
            hasLine <- hIsEOF journal
            content <- if not hasLine
                            then hGetContents journal
                            else return "empty"
            let contentF = formatFile $ lines content
        
            let output = unlines contentF

            set out [#label := stringToGlib output]
            hClose journal



    #showAll openRecordWin

formatFile :: [String] -> [String]
formatFile [] = []
formatFile (x:xs) | head x == '1' = ("First name: " ++ drop 1 x) : formatFile xs
                  | head x == '2' = ("Last name: " ++ drop 1 x) : formatFile xs
                  | head x == '3' = ("Age: " ++ drop 1 x) : formatFile xs
                  | head x == '4' = ("Gender: " ++ drop 1 x) : formatFile xs
                  | head x == '5' = ("Height: " ++ drop 1 x ++ " m") : formatFile xs
                  | head x == '6' = ("Weight: " ++ drop 1 x ++ " kg") : formatFile xs
                  | head x == '7' = ("BloodType: " ++ drop 1 x) : formatFile xs
                  | head x == '8' = ("BMI: " ++ drop 1 x) : formatFile xs
                  


invokeBmiCalcWin :: IO()
invokeBmiCalcWin = do
    bmiCalcWin <- new Gtk.Window [#title := "Journal System"]
    #resize bmiCalcWin 320 240
    #setPosition bmiCalcWin Gtk.WindowPositionCenter 
        
    vertBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add bmiCalcWin vertBox

    horBox1 <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add vertBox horBox1

    horBox2 <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add vertBox horBox2

    label1 <- new Gtk.Label [#label := "Height (m): "]
    #add horBox1 label1
    
    label2 <- new Gtk.Label [#label := "Weight (kg): "]
    #add horBox2 label2

    heightEntry <- Gtk.entryNew
    #add horBox1 heightEntry

    weightEntry <- Gtk.entryNew
    #add horBox2 weightEntry

    calcBtn <- new Gtk.Button [#label := "Calculate"]
    #add vertBox calcBtn

    label3 <- new Gtk.Label [#label := " "]
    #add vertBox label3 

    on calcBtn #clicked $ do
        h <- Gtk.entryGetText heightEntry
        w <- Gtk.entryGetText weightEntry

        let height = read (glibToString h) :: Double
        let weight = read (glibToString w) :: Double
        let bmiCalc = bmi weight height 
        
        set label3 [#label := stringToGlib $ show bmiCalc]

    #showAll bmiCalcWin

invokeBTCalcWin :: IO()
invokeBTCalcWin = do
    btCalcWin <- new Gtk.Window [#title := "Journal System"]
    #resize btCalcWin 320 240
    #setPosition btCalcWin Gtk.WindowPositionCenter 
        
    vertBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add btCalcWin vertBox

    horBox1 <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add vertBox horBox1

    horBox2 <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add vertBox horBox2

    label1 <- new Gtk.Label [#label := "Blood type 1/Patient 1"]
    #add horBox1 label1
    
    label2 <- new Gtk.Label [#label := "Blood type 2/Patient 2"]
    #add horBox2 label2

    patient1Entry <- Gtk.entryNew
    #add horBox1 patient1Entry

    patient2Entry <- Gtk.entryNew
    #add horBox2 patient2Entry

    calcBtn <- new Gtk.Button [#label := "Calculate"]
    #add vertBox calcBtn

    label3 <- new Gtk.Label [#label := " "]
    #add vertBox label3 

    on calcBtn #clicked $ do 
        pat1 <- Gtk.entryGetText patient1Entry
        pat2 <- Gtk.entryGetText patient2Entry

        let filename = filter ( /= ' ') $ glibToString pat1 ++ ".txt"
        let filename2 = filter ( /= ' ') $ glibToString pat2 ++ ".txt"
        
        patient1 <- getRowFromFile filename
        patient2 <- getRowFromFile filename2

        if patient1 == "empty" then
            set label3 [#label := "Patient 1 is not in our records"]
        else if patient2 == "empty" then
            set label3 [#label := "Patient 2 is not in our records"]
        else do
            let donateResult = donate (strip patient1) (strip patient2)
                            where strip = filter(\x -> x == 'A' || x == 'B' || x == 'O')

            let print | donateResult = "Patient 1 can donate to Patient 2"
                      | otherwise = "Patient 1 can't donate to Patient 2"
            set label3 [#label := stringToGlib print ]


    #showAll btCalcWin


getRowFromFile :: String -> IO String
getRowFromFile filename = do 
    exist <- doesFileExist filename
    if not exist then return "empty"
        else do
            journal <- openFile filename ReadMode
            hasLine <- hIsEOF journal
            content <- if not hasLine
                            then hGetContents journal
                            else return "empty"
            let contentF = lines content
            
            let outPut = searchList contentF "7"
            return outPut



        




    


                



    

    


    

