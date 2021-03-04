
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import qualified GI.Gtk as Gtk 
import Data.GI.Base
import System.Directory
import System.IO ( hClose, hIsEOF, openFile, hGetContents, IOMode(ReadMode, WriteMode), IOMode(WriteMode) )
import System.Glib.UTFString
import Logic


--NOTE: The explanation of the functions is a lot more in depth in the documentation

{-  main
    The main function that runs the program
    SIDE EFFECTS: Invokes all necessary widgets, initializes the gtk library, and starts the main gtk main loop, the user is asked to input a username and a password, if these are correct when the button is clicked,
                  we enter the program
    EXAMPLES: main == runs the program
-}  
main :: IO()
main = do
    Gtk.init Nothing 

    win <- new Gtk.Window[#title := "Medinet"]
    
    #resize win 640 480
    #setPosition win Gtk.WindowPositionCenter 

    vertBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add win vertBox

    title <- new Gtk.Label [#label := "Medinet"]
    #add vertBox title

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
        else set title [#label := "Incorrect login credentials"]
        

    #showAll win

    Gtk.main 


{-  invokeMenuScreen
    Invokes the main menu of the program
    SIDE EFFECTS: Invokes all necessary widgets, all features are displayed as a list of labeled buttons
    EXAMPLES: openRecordWin == invokes the menu screen
-}  
invokeMenuScreen :: Gtk.Window  -> IO()
invokeMenuScreen win = do 
    #destroy win
    menuState <- new Gtk.Window [#title := "Medinet"]

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

    tdeeCalcBtn <- new Gtk.Button [#label := "TDEE Calculator"]
    #add vertBox tdeeCalcBtn

    donationCalcBtn <- new Gtk.Button [#label := "Blood Donation Calculator"]
    #add vertBox donationCalcBtn

    additionalBtn <- new Gtk.Button [#label := "Add TDEE"]
    #add vertBox additionalBtn

    deleteRecord <- new Gtk.Button [#label := "Delete record"]
    #add vertBox deleteRecord

    changeRecord <- new Gtk.Button [#label := "Edit record"]
    #add vertBox changeRecord

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

    on additionalBtn #clicked $ do
        invokeTdeeAddWin
    
    on deleteRecord #clicked $ do
        invokeDeleteWin
    
    on tdeeCalcBtn #clicked $ do
        invokeTdeeCalcWin

    on changeRecord #clicked $ do
        invokeChangeWin
    horBox1 <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #add vertBox horBox1
    
    #showAll menuState

{-  invokeQAWindow
    Invokes the window where you are able to access specific pieces of information from a patient recor
    SIDE EFFECTS: Invokes all necessary widgets, when the button is clicked, the info the user searched for is displayed in the output label
    EXAMPLES: openRecordWin == invokes the window where you have a quick access to individual pieces of information
-}  
invokeQAWindow :: IO()
invokeQAWindow = do
    qaWindow <- new Gtk.Window [#title := "Medinet"]

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
        i <- Gtk.entryGetText info
        outPut <- getRowFromFile filename (glibToString i)
        set label [#label := stringToGlib outPut]
       
    #showAll qaWindow
   
{-  invokeAddPatientWindow
    Invokes the window where you are able to add a patient record
    SIDE EFFECTS: Invokes all necessary widgets, when the button is clicked, the data of the patient is gathered from the text entries and written to a textfile whose name is the name of the patient
    EXAMPLES: invokeAddPatientWindow == invokes the window where you can add a patient to the data type
-}  
invokeAddPatientWindow :: IO()
invokeAddPatientWindow = do
    addWindow <- new Gtk.Window [#title := "Medinet"]

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
    
{-  openRecordWin
    Invokes the window where you are able to output an entire record
    SIDE EFFECTS: Invokes all necessary widgets, when the button is clicked, the record of the patient that the user inputted is outputted in a formatted way so that everything is clear
    EXAMPLES: openRecordWin == invokes the window where you can open an entire record
-}     
openRecordWindow :: IO()
openRecordWindow = do
    openRecordWin <- new Gtk.Window [#title := "Medinet"]
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

    box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add vertBox box
    out <- new Gtk.Label [#label := " "]
    #add box out

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

{-  invokeBmiCalcWin
    Invokes the window where you calculate a bmi value
    SIDE EFFECTS: Invokes all necessary widgets, when the button is clicked, the height and weight input from the user is used in a bmi calculation whose result is printed on the screen
    EXAMPLES: invokeTdeeCalcWIn == invokes the window where you calculate a bmi value
-}                   
invokeBmiCalcWin :: IO()
invokeBmiCalcWin = do
    bmiCalcWin <- new Gtk.Window [#title := "Medinet"]
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
{-  invokeBTCalcWin
    Invokes the window where you calculate if a blood donation is possible
    SIDE EFFECTS: Invokes all necessary widgets, when the button is clicked, based on what mode that is currently selected, the function either computes if a donation between two patients in the data base is possible
                  or if a donation between to arbitrary blood types is possible. Then it prints the result on the screen
    EXAMPLES: invokeTdeeCalcWIn == invokes the window where you calculate if a blood donation is possible or not
-} 
invokeBTCalcWin :: IO()
invokeBTCalcWin = do
    btCalcWin <- new Gtk.Window [#title := "Medinet"]
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

    modeBtn <- new Gtk.Button [#label := "Patient Mode"]
    #add vertBox modeBtn

    label3 <- new Gtk.Label [#label := " "]
    #add vertBox label3 


    on modeBtn #clicked $ do
        text <- Gtk.buttonGetLabel modeBtn
        if text == "Patient Mode" then
            set modeBtn [#label := "Bloodtype Mode"]
        else 
            set modeBtn [#label := "Patient Mode"]

    on calcBtn #clicked $ do 
        pat1 <- Gtk.entryGetText patient1Entry
        pat2 <- Gtk.entryGetText patient2Entry
        mode <- Gtk.buttonGetLabel modeBtn

        if mode == "Patient Mode" then do
            let filename = filter ( /= ' ') $ glibToString pat1 ++ ".txt"
            let filename2 = filter ( /= ' ') $ glibToString pat2 ++ ".txt"

            patient1 <- getRowFromFile filename "7"
            patient2 <- getRowFromFile filename2 "7"
    
            if patient1 == "empty" && patient2 == "empty" then
                set label3 [#label := "These patients are not in our records."]
            else if patient1 == "empty" then 
                set label3 [#label := stringToGlib $ glibToString pat1 ++ " is not in our records"]
            else if patient2 == "empty" then
                set label3 [#label := stringToGlib $ glibToString pat2 ++ " is not in our records"]
            else do
                let donateResult = donate (strip patient1) (strip patient2)
                                where strip = filter(\x -> x == 'A' || x == 'B' || x == 'O')

                let print | donateResult = "Patient 1 can donate to Patient 2"
                          | otherwise = "Patient 1 can't donate to Patient 2"
                set label3 [#label := stringToGlib print ]
        else do
            let donateResult = donate (strip $ glibToString pat1) (strip $ glibToString pat2)
                            where strip = filter(\x -> x == 'A' || x == 'B' || x == 'O')
            let print | donateResult = "Patient 1 can donate to Patient 2"
                      | otherwise = "Patient 1 can't donate to Patient 2"
            set label3 [#label := stringToGlib print ]
    #showAll btCalcWin

{-  getRowFromFile filename i
    Gets the row from a medical record that matches i
    SIDE EFFECTS: Converts i to an index so it's matchable in the list, converts the content of a file to a list then uses the searchList function with the index to get the correct row and then returns the row
    EXAMPLES: getRowFromFile i == the row that matches the index of i from the file
-} 
getRowFromFile :: String -> String -> IO String
getRowFromFile filename i = do
    let index = convertIndex i
    exist <- doesFileExist filename
    if not exist then return "empty"
        else do
            journal <- openFile filename ReadMode
            hasLine <- hIsEOF journal
            content <- if not hasLine
                            then hGetContents journal
                            else return "empty"
            let contentF = lines content
            
            let outPut = searchList contentF index
            return outPut
{-  invokeTdeeAddWin
    Invokes the window where you add a TDEE value to a record
    SIDE EFFECTS: Invokes all necessary widgets, when the button is clicked, the TDEE value is calculated and inserted to the record
    EXAMPLES: invokeTdeeCalcWIn == invokes the window where you calculate a TDEE value
-}  
invokeTdeeAddWin :: IO()
invokeTdeeAddWin = do
    win <- new Gtk.Window [#title := "Medinet"]
    #resize win 320 240
    #setPosition win Gtk.WindowPositionCenter 

    vertBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add win vertBox

    label1 <- new Gtk.Label [#label := "Patient: "]
    #add vertBox label1

    patientEntry <- Gtk.entryNew 
    #add vertBox patientEntry

    label2 <- new Gtk.Label [#label := "Amount of workouts per week"]
    #add vertBox label2

    workOutEntry <- Gtk.entryNew 
    #add vertBox workOutEntry

    addBtn <- new Gtk.Button [#label := "Add tdee"]
    #add vertBox addBtn

    on addBtn #clicked $ do
        patient <- Gtk.entryGetText patientEntry
        let filename = filter ( /= ' ') $ glibToString patient ++ ".txt"

        weight <- getRowFromFile filename "weight"
        height <- getRowFromFile filename "height"
        age <- getRowFromFile filename "age"
        sex <- getRowFromFile filename "gender"
        freq <- Gtk.entryGetText workOutEntry
        let tdeeVal = round $ tdeeCalculator (read weight :: Double) (read height :: Double) (read age :: Double) sex (read $ glibToString freq :: Int)
        let append = "\n9" ++ show tdeeVal

        content <- getRowFromFile filename "tdee"
        
        if content == "This info doesn't exist in this system." then do
            editFile filename append
        else do
            replaceInFile filename "tdee" (show tdeeVal)


        
        #destroy win
    #showAll win
{-  invokeTdeeCalcWin
    Invokes the window where you calculate a TDEE value
    SIDE EFFECTS: Invokes all necessary widgets, when the button is clicked, the TDEE value is calculated and printed by the inputs the user has entered
    EXAMPLES: invokeTdeeCalcWIn == invokes the window where you calculate a TDEE value
-}   
invokeTdeeCalcWin :: IO()
invokeTdeeCalcWin= do
    win <- new Gtk.Window [#title := "Medinet"]

    #resize win 320 240
    #setPosition win Gtk.WindowPositionCenter 
    
    vertBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add win vertBox

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

    ageLabel <- new Gtk.Label   [#label := "Age:             "]
    #add horBox3 ageLabel

    gLabel <- new Gtk.Label     [#label := "Gender:      "]
    #add horBox4 gLabel

    hLabel <- new Gtk.Label     [#label := "Height:        "]
    #add horBox5 hLabel

    wLabel <- new Gtk.Label     [#label := "Weight:       "]
    #add horBox6 wLabel

    wpwLabel <- new Gtk.Label     [#label := "Workouts per week:       "]
    #add horBox7 wpwLabel

    age <- Gtk.entryNew 
    Gtk.containerAdd horBox3 age

    gender <- Gtk.entryNew 
    Gtk.containerAdd horBox4 gender

    height <- Gtk.entryNew 
    Gtk.containerAdd horBox5 height

    weight <- Gtk.entryNew 
    Gtk.containerAdd horBox6 weight

    wpw <- Gtk.entryNew 
    Gtk.containerAdd horBox7 wpw

    calcBtn <- new Gtk.Button [#label := "Calculate Tdee"]
    #add vertBox calcBtn

    oLabel <- new Gtk.Label [#label := " "]
    #add vertBox oLabel

    on calcBtn #clicked $ do
        a <- Gtk.entryGetText age
        g <- Gtk.entryGetText gender
        h <- Gtk.entryGetText height
        w <- Gtk.entryGetText weight
        wpw <- Gtk.entryGetText wpw

        let g' | glibToString g == "male" = "Male"
               | glibToString g == "female" = "Female"
        

        let tdeeVal = round $ tdeeCalculator (read $ glibToString w :: Double) (read $ glibToString h :: Double) (read $ glibToString a :: Double) g' (read $ glibToString wpw :: Int)
        set oLabel [#label := stringToGlib $ show tdeeVal ++ " kcal per day"]

        
    #showAll win
{-  invokeDeleteWin
    Invokes the window where you delete a record
    SIDE EFFECTS: Invokes all necessary widgets, when the button is clicked, the file in the input is deleted
    EXAMPLES: invokeDeleteWin == invokes the window where you delete a file
-}       
invokeDeleteWin :: IO()
invokeDeleteWin = do
    win <- new Gtk.Window [#title := "Medinet"]
    #resize win 320 240
    #setPosition win Gtk.WindowPositionCenter 

    vertBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add win vertBox

    label1 <- new Gtk.Label [#label := "Patient: "]
    #add vertBox label1

    patientEntry <- Gtk.entryNew 
    #add vertBox patientEntry

    delBtn <- new Gtk.Button [#label := "Delete"]
    #add vertBox delBtn

    label2 <- new Gtk.Label [#label := ""]
    #add vertBox label2

    on delBtn #clicked $ do
        patient <- Gtk.entryGetText patientEntry
        let filename = filter ( /= ' ') $ glibToString patient ++ ".txt"
        exists <- doesFileExist filename

        if exists then do
            delete filename
            #destroy win
        else do
            set label2 [#label := "File couldn't be found"]
    #showAll win

{-  invokeChangeWin
    Invokes the window where you edit a record
    SIDE EFFECTS: Invokes all necessary widgets, when the button is clicked, input is gathered from the text entries. The filename is derived from the patient entry. Then the info is replaced into the text file
    EXAMPLES: invokeChangeWin == invokes the window where you edit a record and add that into a text file
-}
invokeChangeWin :: IO()
invokeChangeWin = do
    win <- new Gtk.Window [#title := "Medinet"]
    #resize win 320 240
    #setPosition win Gtk.WindowPositionCenter 

    vertBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #add win vertBox

    label1 <- new Gtk.Label [#label := "Patient name"]
    #add vertBox label1

    patientEntry <- Gtk.entryNew 
    #add vertBox patientEntry

    label2 <- new Gtk.Label [#label := "Info"]
    #add vertBox label2

    infoEntry <- Gtk.entryNew 
    #add vertBox infoEntry

    label3 <- new Gtk.Label [#label := "New info"]
    #add vertBox label3

    newEntry <- Gtk.entryNew 
    #add vertBox newEntry

    btn <- new Gtk.Button [#label := "Change info"]
    #add vertBox btn

    on btn #clicked $ do
        pat <- Gtk.entryGetText patientEntry
        info <- Gtk.entryGetText infoEntry
        infoNew <- Gtk.entryGetText newEntry

        let filename = filter ( /= ' ') $ glibToString pat ++ ".txt"

        if glibToString info == "height" then do
            let height = read (glibToString infoNew) :: Double
            weight <- getRowFromFile filename "weight"
            let weight' = read weight :: Double
            let bmiCalc = bmi weight' height 
            replaceInFile filename "bmi" (show bmiCalc)
            replaceInFile filename (glibToString info) (glibToString infoNew) 
        else if glibToString info == "weight" then do
            height <- getRowFromFile filename "height"
            let height' = read height :: Double
            let weight = read (glibToString infoNew) :: Double
            let bmiCalc = bmi weight height'
            replaceInFile filename "bmi" (show bmiCalc)
            replaceInFile filename (glibToString info) (glibToString infoNew) 
        else do
            replaceInFile filename (glibToString info) (glibToString infoNew) 
        
        #destroy win

    #showAll win

{-  replaceInFile filename info infoNew
    Replaces a row in the textfile with a new one. Info signify which row to be replaced, and infoNew is the new info to be added.
    PRE: filename must be a file that exists in the program folder
    SIDE EFFECTS: Opens the file in readmode, checks if it's empty, in that case the function terminates, otherwise it stores the content of the file in a variable, replaces the element in the content by using auxiliary functions and a sequence of conversion, deletes the file,
                  and writes to a new one with the same name and modified content
    EXAMPLES: replaceInFile filename info infoNew == new file with the modified content

-}
replaceInFile :: String -> String -> String -> IO()
replaceInFile filename info infoNew = do
    journal <- openFile filename ReadMode 
    hasLine <- hIsEOF journal
    content <- if not hasLine
                    then hGetContents journal
                else return()
    let contentF = lines content
    let index = convertIndex info
    let info' = index ++ infoNew
    let newContent = unlines $ replaceElement info' contentF

    delete filename
    writeFile filename newContent
    hClose journal

{-  editFile filename append
    Appends a new row of text at the bottom of a textfield
    PRE: filename must be a file that exists in the program folder
    SIDE EFFECTS: Opens the file in readmode, checks if it's empty, in that case the function terminates, otherwise the content is stored in a variable and the file is deleted,
                  then a new file with the same name is created and written to with the same content plus append.
    EXAMPLES: editFile filename append == new file with the same content plus append
                  
-}
editFile :: String -> String -> IO()
editFile filename append = do
    journal <- openFile filename ReadMode
    hasLine <- hIsEOF journal
    content <- if not hasLine
                    then hGetContents journal
                else return()
    delete filename
    writeFile filename (content ++ append)
    hClose journal

{-  delete file
    Deletes a text file if it exists
    SIDE EFFECTS: Checks if the file exists, if it does, deletes it, otherwise nothing happens
    EXAMPLES: delete "BenjaminStrandberg.txt" == deletes the file with the given name 
              delete "BenjaminStrandberg.txt" == "File does not exist" if the file doesn't exist
-}
delete :: String -> IO ()
delete file = do
    exists <- doesFileExist file
    if exists then removeFile file
    else return()



        




    


                



    

    


    

