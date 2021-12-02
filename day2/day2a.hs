type Cmd = (String, Int)
type Position = (Int, Int)

parse :: String -> [Cmd]
parse s = [(x, (read y::Int)) | [x, y] <- xs]
    where xs = [words x | x <- lines s]

run :: Position -> Cmd -> Position
run (h, d) ("forward", x) = (h + x, d)
run (h, d) ("down", x) = (h, d + x)
run (h, d) ("up", x) = (h, d - x)

runAll :: Position -> [Cmd] -> Position
runAll p [] = p
runAll p (x:xs) = runAll pp xs
    where pp = run p x

main :: IO ()
main = do
    str <- getContents
    let commands = parse str
    let position = runAll (0, 0) commands
    print $ position
    print $ fst position * snd position
