type Cmd = (String, Int)
type Position = (Int, Int, Int) -- horizontal, depth, aim

parse :: String -> [Cmd]
parse s = [(x, (read y::Int)) | [x, y] <- xs]
    where xs = [words x | x <- lines s]

run :: Position -> Cmd -> Position
run (h, d, a) ("forward", x) = (h + x, d + (a * x), a)
run (h, d, a) ("down", x) = (h, d, a + x)
run (h, d, a) ("up", x) = (h, d, a - x)

runAll :: Position -> [Cmd] -> Position
runAll p [] = p
runAll p (x:xs) = runAll pp xs
    where pp = run p x

main :: IO ()
main = do
    str <- getContents
    let commands = parse str
    let position = runAll (0, 0, 0) commands
    let (x, y, _) = position
    print $ position
    print $ x * y
