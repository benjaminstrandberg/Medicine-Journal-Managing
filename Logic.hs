module Logic where


{-  donate bloodtype1 bloodtype2
    Checks if bloodtype1 matches with bloodtype2 for a blood donation. 
    RETURNS: True if the bloodtypes match, False if they dont.
    EXAMPLES:   donate "A" "B" == False
                donate "AB" "AB" == True
                donate "" "" == False
-}
donate :: String -> String -> Bool 
donate _ "AB" = True 
donate "O" _ = True 
donate "A" "A" = True 
donate "B" "B" = True 
donate _ _ = False 

{-  bmi weight height
    Calculates Body Mass Index "BMI" to tell which weight-class a person belongs to.
    PRE: The arguments can't be smaller than 0.
    RETURNS: A tuple pair with the BMI score in the fst position and a description of the result in the snd position.
    EXAMPLES:   bmi 70 1.90 == (19,"Normal Weight")
-}
bmi :: Double -> Double -> (Int, String)
bmi v l | x >= 30 = (round x,"Obese")
        | x < 30 && x > 25 = (round x, "Overweight")
        | x < 25 && x > 18.5 = (round x, "Normal weight")
        | otherwise = (round x, "Underweight")
            where
                x = v / (l * l)

{-  tdeeCalculator weight height age sex exercisefrequency 
    Calculates the total daily energy expenditure "TDEE"
    PRE: All of the arguments that are doubles or ints can't be smaller than 0.
    RETURNS: Rougly the amount of calories the person burns in a day as a double.
    EXAMPLES:   tdeeCalculator 80 1.90 21 "Male" 7 == 3303.125
                tdeeCalculator 70 1.70 20 "Female" 6 == 2627.625
-}
tdeeCalculator :: Double -> Double -> Double -> String -> Int -> Double
tdeeCalculator weight height age sex exercisefrequency = bmr * frequencyQuota
        where bmr 
                | sex == "Male" = (10 * weight) + (6.25 * (height*100)) - (5 * age) + 5 
                | otherwise = (10 * weight) + (6.25 * (height*100)) - (5 * age) -161
              frequencyQuota 
                | exercisefrequency == 0 = 1.2
                | exercisefrequency < 3 = 1.37
                | exercisefrequency < 6 = 1.55
                | exercisefrequency < 8 = 1.75
                | otherwise = 1.9

{-  searchList list index
    Gets an element from a list based on an index
    RETURNS: The element with the correct index with the index dropped
    EXAMPLES: searchList [”1Benjamin”, ”2Strandberg”, ”319”, ”4Male”] "3" == "19"
              searchList [”1Benjamin”, ”2Strandberg”, ”319”, ”4Male”] "6" == "This info doesn't exist in this system."
              searchList [] "6" == "This info doesn't exist in this system."
-}
--VARIANT: length (x:xs)
searchList :: [String] -> String -> String 
searchList [] _ = "This info doesn't exist in this system."
searchList (x:xs) index | index == [head x] = drop 1 x
                        | otherwise = searchList xs index

{-  formatFile list
    Formats a list of information into a more suitable output format
    RETURNS: A formatted version of the input list
    EXAMPLES: formatFile [”1Benjamin”, ”2Strandberg”, ”319”, ”4Male”] == ["First name: Benjamin","Last name: Strandberg","Age: 19","Gender: Male"]
              formatFile [] =≈ []

-}
--VARIANT: length (x:xs)
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
                  | head x == '9' = ("Total daily energy expenditure: " ++ drop 1 x ++ " kcal") : formatFile xs


{-  convertIndex i
    Converts a piece of information to an index
    RETURNS: the index associated with the argument string
    EXAMPLES: convertIndex "age" == "3"
              convertIndex "favorite color" == "null"

-}
convertIndex :: String -> String 
convertIndex i| i == "age" = "3"
              | i == "gender" = "4"
              | i == "height" = "5"
              | i == "weight" = "6"
              | i == "bloodtype" = "7"
              | i == "bmi" = "8"
              | i == "tdee" = "9"
              | otherwise = "null"


{-  replaceElement s list
    Replaces the element in a list with a new one
    RETURNS: A new list with the new element in the old ones place
    EXAMPLES: replaceElement "320" [”1Benjamin”, ”2Strandberg”, ”319”, ”4Male”] == [”1Benjamin”, ”2Strandberg”, ”320”, ”4Male”] 
              replaceElement "320" [] = []

-}
--VARIANT: length (x:xs)
replaceElement :: String -> [String] -> [String]
replaceElement _ [] = []
replaceElement s (x:xs) | head x == head s = s : replaceElement s xs
                        | otherwise = x : replaceElement s xs
        
