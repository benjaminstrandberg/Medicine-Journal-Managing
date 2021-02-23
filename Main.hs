
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import qualified GI.Gtk as Gtk 
import Data.GI.Base






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
        else set msg [#label := "Fel"]
        

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

    patientEntry <- Gtk.entryNew
    Gtk.containerAdd horBox1 patientEntry


    findRecordBtn <- new Gtk.Button [#label := "Find patient"]
    #add horBox1 findRecordBtn

    logOutBtn <- new Gtk.Button [#label := "Log out"]
    #add vertBox logOutBtn

    on logOutBtn #clicked $ do
        #destroy menuState
        main

    
    #showAll menuState

