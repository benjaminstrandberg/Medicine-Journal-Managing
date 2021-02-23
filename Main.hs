
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import qualified GI.Gtk as Gtk 
import Data.GI.Base
import System.Environment



data RhFactor = Pos | Neg 
data ABOType = A | B | AB | O deriving (Show)
data BloodType = BloodType ABOType RhFactor deriving (Show)


instance Show RhFactor where
    show Pos = "+"
    show Neg = "-"


donateTo :: BloodType -> BloodType -> Bool 
donateTo _ (BloodType AB _) = True -- Kan ta emot från alla
donateTo (BloodType O _) _ = True -- O är universal donerare
donateTo (BloodType A _) (BloodType A _) = True 
donateTo (BloodType B _) (BloodType B _) = True 
donateTo _ _ = False -- Om de inte matchar är det kört 


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
    Gtk.widgetDestroy win
    
    
    menuState <- new Gtk.Window [#title := "Journal System"]

    #resize menuState 640 480
    #setPosition menuState Gtk.WindowPositionCenter 

    vertBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add menuState vertBox

    addPatientbtn <- new Gtk.Button [#label := "Add patient"]
    #add vertBox addPatientbtn

    horBox1 <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #add vertBox horBox1

    patient1Entry <- Gtk.entryNew
    Gtk.containerAdd horBox1 patient1Entry
    

    patient2Entry <- Gtk.entryNew
    Gtk.containerAdd horBox1 patient2Entry
    

    checkBtn <- new Gtk.Button [#label := "Check donation"]
    #add vertBox checkBtn

    label <- new Gtk.Label [#label := "asg"]
    #add vertBox label

    on checkBtn #clicked $ do

        patient1 <- Gtk.entryGetText patient1Entry
        patient2 <- Gtk.entryGetText patient2Entry

        if patient1 == "Tommy Komo" && patient2 == "Benjamin Strandberg" then
            if donateTo tommyKomo benjaminStrandberg then
                set label [#label := "Tommy can donate to Benjamin"]
            else set label [#label := "Tommy can't donate to Benjamin"]
        else set label [#label := "The patients aren't in our database"]
        
        
        


    
    
    #showAll menuState

