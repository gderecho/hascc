
import System.IO
import System.Exit
import System.Process
import System.IO.Error

p_code :: String -> ExitCode -> IO ()
p_code msg code = do
    case code of 
        ExitFailure a -> do
            hPutStrLn stderr msg
            exitFailure
        ExitSuccess -> return ()

build_and_run_cpp :: IO ()
build_and_run_cpp = do
    code <- system "./buildcpp.sh"
    p_code "Failure compiling c++." code 
    code <- system "./generate_paths"
    p_code "Failure running generate_pahts." code

get_paths :: IO [[String]]
get_paths = fmap (filter (/=[])) . fmap (map words) . 
                fmap lines $ readFile "paths.txt"

clean_up :: IO ()
clean_up = 
    p_code "Failure cleaning up generated files." 
        =<< system "rm paths.txt; rm generate_paths"

code_to_int :: ExitCode -> Int
code_to_int ExitSuccess = 0
code_to_int (ExitFailure a) = a

run_cfile :: String -> String -> IO ()
run_cfile cfile expectfile = do
    putStrLn $ "Running " ++ (cfile ++ "...")
    
    compile <- fmap code_to_int $ system $ "cat " ++ (cfile ++ " | stack run > temp.asm")

    case (compile == 0) of
        False -> do
            putStrLn "Compile failed"
            code <- system "echo \' SECTION .TEXT \n GLOBAL main \n main: \n ret \' > temp.asm"
            return ()
        _ -> putStrLn "Compile succeeded."
    
    code <- system "nasm -felf64 temp.asm"
    p_code "Failure to assemble." code 

    code <- system "gcc temp.o -o temp.out"
    p_code "Failure to link." code 

    ret_val <- fmap code_to_int $ system "./temp.out"
    
    putStrLn $ "Exited with " ++ show ret_val
    putStrLn "Expected: "
    putStrLn =<< readFile expectfile

    code <- system "rm temp*"
    p_code "Failure to clean up temporary files." code
    

run_tests :: [[String]] -> IO ()
run_tests [] = return ()
run_tests (x:xs) = do
    if length x /= 2 
        then fail $ "Wrong length: " ++ show x
        else return ()
    run_cfile (x!!0) (x!!1)
    run_tests xs


main :: IO ()
main = do
    paths <- build_and_run_cpp >> get_paths
    run_tests paths
    clean_up


