
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import qualified GI.Gtk as Gtk 
import Data.GI.Base
import System.Environment
import System.Directory
import System.IO
import System.Glib.UTFString



data RhFactor = Pos | Neg 
data ABOType = A | B | AB | O deriving (Show)
data BloodType = BloodType ABOType RhFactor deriving (Show)


instance Show RhFactor where
    show Pos = "+"
    show Neg = "-"

--                     FName  LName Gender Age Height Weight BT  
data Patient = Patient {firstName :: String,
                        lastName :: String,
                        age :: Int, 
                        gender :: String,
                        height :: Double,
                        weight :: Double,
                        bloodType :: BloodType} deriving (Show)


donateTo :: BloodType -> BloodType -> Bool 
donateTo _ (BloodType AB _) = True -- Kan ta emot från alla
donateTo (BloodType O _) _ = True -- O är universal donerare
donateTo (BloodType A _) (BloodType A _) = True 
donateTo (BloodType B _) (BloodType B _) = True 
donateTo _ _ = False -- Om de inte matchar är det kört 

bmi :: Double -> Double -> (Double, String)
bmi v l | bmiCalculator v l >= 30 = (bmiCalculator v l,"Svår övervikt")
        | bmiCalculator v l < 30 && bmiCalculator v l > 25 = (bmiCalculator v l, "Övervikt")
        | bmiCalculator v l < 25 && bmiCalculator v l > 18.5 = (bmiCalculator v l, "Normalvikt")
        | otherwise = (bmiCalculator v l, "Undervikt")


bmiCalculator :: Double -> Double -> Double
bmiCalculator v l = v / (l * l)

benjaminStrandberg = BloodType B Neg 

tommyKomo = BloodType AB Pos




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

    addPatientbtn <- new Gtk.Button [#label := "Add patient"]
    #add vertBox addPatientbtn

    on addPatientbtn #clicked $ do
        invokeAddPatientWindow 

    on qaButton #clicked $ do
        invokeQAWindow

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

    label <- new Gtk.Label [#label := " "]
    #add vertBox label

    fNameEntry <- Gtk.entryNew 
    #add vertBox fNameEntry

    lNameEntry <- Gtk.entryNew 
    #add vertBox lNameEntry

    info <- Gtk.entryNew 
    #add vertBox info

    confirmBtn <- new Gtk.Button [#label := "Search"]
    #add vertBox confirmBtn

    

    on confirmBtn #clicked $ do
        f <- Gtk.entryGetText fNameEntry
        l <- Gtk.entryGetText lNameEntry


        let filename = glibToString f ++ glibToString l ++ ".txt"
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
                      | otherwise = "null"
            
            let outPut = searchList contentF index
            print outPut

            set label [#label := stringToGlib outPut]
            hClose journal


    #showAll qaWindow
   
        


searchList :: [String] -> String -> String 
searchList [] _ = "This info doesn't exist in this system."
searchList l@(x:xs) y | y == [head x] = drop 1 x
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

        let fileName = glibToString f ++ glibToString l ++ ".txt" 
        let text = "1" ++ glibToString f ++ "\n" ++ "2" ++ glibToString l ++ "\n" ++ "3" ++ glibToString a ++ "\n" ++ "4" ++ glibToString g ++ "\n" ++ "5" ++ glibToString h ++ "\n" ++ "6" ++ glibToString w ++ "\n" ++ "7" ++ glibToString b

        writeFile fileName text

        #destroy addWindow
    #showAll addWindow
    
    

    


    

