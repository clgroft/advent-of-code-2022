-- | Day 10 solution
module Day10 (day10) where

data Instruction = Noop | Add Int

parseInstruction :: String -> Instruction
parseInstruction "noop" = Noop
parseInstruction s = Add . read $ drop 5 s

expandInstructions :: [Instruction] -> [Instruction]
expandInstructions = concatMap f
  where
    f Noop = [Noop]
    f (Add n) = [Noop, Add n]

registerValues :: [Instruction] -> [Int]
registerValues instrs = xs
  where
    f Noop n = n
    f (Add k) n = n + k
    xs = 1 : zipWith f instrs xs

signalStrengths :: [Int] -> [Int]
signalStrengths = zipWith (*) [1 ..]

pixelsDrawn :: [Int] -> String
pixelsDrawn = zipWith f $ cycle [0 .. 39]
  where
    f k n = if abs (k - n) <= 1 then '#' else '.'

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

day10 :: String -> String
day10 input = unlines $ show sigStrengthSum : take 6 (chunksOf 40 pixels)
  where
    regValues = registerValues . expandInstructions . map parseInstruction $ lines input
    sigStrengths = signalStrengths regValues
    sigStrengthSum =
      (sigStrengths !! 19)
        + (sigStrengths !! 59)
        + (sigStrengths !! 99)
        + (sigStrengths !! 139)
        + (sigStrengths !! 179)
        + (sigStrengths !! 219)
    pixels = pixelsDrawn regValues
