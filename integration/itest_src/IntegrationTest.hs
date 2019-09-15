import System.IO
import System.Exit
import System.Process
import System.IO.Error
import System.Directory
import Data.Aeson



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
    code <- system "./integration_test"
    p_code "Failure running integration tests." code
