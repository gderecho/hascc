module IntegrationTest where


import System.IO
import System.Exit
import System.Process

build_and_run_cpp :: IO ()
build_and_run_cpp = do
    code <- system "./buildcpp.sh"
    case code of 
        ExitFailure a -> do
            hPutStrLn stderr "Failure compiling c++."
            exitFailure
        ExitSuccess -> return ()
    code <- system "./generate_paths"
    case code of 
        ExitFailure a -> do
            hPutStrLn stderr "Failure running generate_pahts."
            exitFailure
        ExitSuccess -> return ()

get_paths :: IO [[String]]
get_paths = fmap (filter (/=[])) . fmap (map words) . 
                fmap lines $ readFile "paths.txt"

main :: IO ()
main = do
    paths <- build_and_run_cpp >> get_paths
    code <- system "rm paths.txt; rm generate_paths"
    case code of 
        ExitFailure a -> do
            hPutStrLn stderr "Failure cleaning up generated files."
            exitFailure
        ExitSuccess -> return ()
