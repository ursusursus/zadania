input xs  | (null xs) = []
          | otherwise = (sum (fst splitted)) : input(dropWhile (== 0) (snd splitted))
            where splitted = break (== 0) xs
