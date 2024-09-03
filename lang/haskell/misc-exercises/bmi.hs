bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= thin       = "Let's go get some brunch."
  | bmi <= average    = "You're right about normal!"
  | bmi <= overweight = "Let's go jogging?"
  | otherwise         = "Maybe you should see a doctor."
  where bmi = weight / height ^ 2
        thin       = 18.5
        average    = 25.0
        overweight = 30.0

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2
