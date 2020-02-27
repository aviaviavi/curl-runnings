{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import qualified Codec.Archive.Tar             as Tar
import qualified Codec.Compression.GZip        as GZ
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import           Data.Foldable
import           Data.List
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                     as T
import           Data.Version                  (showVersion)
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import           Paths_curl_runnings           (version)
import           System.Console.CmdArgs
import           System.Directory
import           System.Environment
import           System.Exit
import           System.Info
import           Testing.CurlRunnings
import           Testing.CurlRunnings.Internal
import           Testing.CurlRunnings.Types

-- | Command line flags
data CurlRunnings = CurlRunnings
  { file           :: FilePath
  , grep           :: Maybe T.Text
  , upgrade        :: Bool
  , json_output    :: Maybe FilePath
  , skip_tls_check :: Bool
  } deriving (Show, Data, Typeable, Eq)

-- | cmdargs object
argParser :: CurlRunnings
argParser =
  CurlRunnings
    { file = def &= typFile &= help "File to run"
    , grep = def &= help "Regex to filter test cases by name"
    , upgrade = def &= help "Pull the latest version of curl runnings"
    , json_output =
        def &= typFile &=
        help "Write test results to a json file specified by path"
    , skip_tls_check =
        def &=
        help
          "Don't perform a TLS check (USE WITH CAUTION. Only use this if you signed your own certs)"
    } &=
  summary ("curl-runnings " ++ showVersion version) &=
  program "curl-runnings" &=
  verbosity &=
  help "Use the --file or -f flag to specify an intput file spec to run"

-- | A single release asset in a github release. Eg curl-runnings-${version}.tar.gz
data GithubReleaseAsset = GithubReleaseAsset
  { name                 :: T.Text
  , browser_download_url :: T.Text -- snake case because that's what we get back from github
  } deriving (Show, Generic)

instance FromJSON GithubReleaseAsset

-- | The json response we expect from github when we check for the latest release
data GithubRelease = GithubRelease
  { assets   :: [GithubReleaseAsset]
  , tag_name :: T.Text -- snake case because that's what we get back from github
  } deriving (Show, Generic)
instance FromJSON GithubRelease

newtype GithubReleasesResponse =
  GithubReleasesResponse (NE.NonEmpty GithubRelease)
  deriving (Show, Generic)
instance FromJSON GithubReleasesResponse

-- | Github requires a user agent, preferably with a username
setGithubReqHeaders :: Request -> Request
setGithubReqHeaders = setRequestHeaders [("User-Agent", "aviaviavi")]

runFile ::
     FilePath
  -> Verbosity
  -> Maybe T.Text
  -> TLSCheckType
  -> Maybe FilePath
  -> IO ()
runFile "" _ _ _ _ =
  putStrLn
    "Please specify an input file with the --file (-f) flag or use --help for more information"
runFile path verbosityLevel regexp tlsType maybeOutputFile = do
  home <- getEnv "HOME"
  suite <- decodeFile . T.unpack $ T.replace "~" (T.pack home) (T.pack path)
  case suite of
    Right s -> do
      results <-
        runSuite (s {suiteCaseFilter = regexp}) (toLogLevel verbosityLevel) tlsType
      for_ maybeOutputFile $ \outputFile -> do
        let jsonSummary = encode results
        B.writeFile (outputFile) jsonSummary
      if any isFailing results
        then putStrLn (T.unpack $ makeRed "Some tests failed") >>
             exitWith (ExitFailure 1)
        else putStrLn . T.unpack $ makeGreen "All tests passed!"
    Left message ->
      (putStrLn . T.unpack . makeRed . T.pack $
       "Couldn't read input json or yaml file: " <> message) >>
      exitWith (ExitFailure 1)

-- | If we're on mac, we want a *-mac tarball from the releases page. If we're on linux we
-- do not
filterAsset :: [GithubReleaseAsset] -> Maybe GithubReleaseAsset
filterAsset assetList = find filterFn assetList
  where
    filterFn' a = "mac" `T.isInfixOf` Main.name a
    filterFn =
      if os == "darwin"
        then filterFn'
        else not . filterFn'

-- | From the list of releases on github, find the newest release that has a binary
-- tarball for the platform we're on.
findAssetFromReleases :: GithubReleasesResponse -> Maybe GithubReleaseAsset
findAssetFromReleases (GithubReleasesResponse releases) =
  let assetList = map (filterAsset . assets) $ NE.toList releases
  in join $ find isJust assetList

-- | We'll upgrade any time the latest version is different from what we have
shouldUpgrade :: GithubReleaseAsset -> Bool
shouldUpgrade asset =
  let assetNameTokens = T.splitOn "-" (Main.name asset)
  in if length assetNameTokens `notElem` [3, 4]
       then False
       else (T.replace  ".tar.gz" "" (assetNameTokens !! 2)) /= T.pack (showVersion version)

-- | If conditions are met, download the appropriate tarball from the latest
-- github release, extract and copy to /usr/local/bin
upgradeCurlRunnings :: IO ()
upgradeCurlRunnings =
  let tmpArchive = "/tmp/curl-runnings-latest.tar.gz"
      tmpArchiveExtracedFolder = "/tmp/curl-runnings-latest"
      tmpExtractedBin = tmpArchiveExtracedFolder ++ "/curl-runnings"
      installPath = "/usr/local/bin/curl-runnings"
  in do req <-
          parseRequest
            "https://api.github.com/repos/aviaviavi/curl-runnings/releases"
        req' <- return $ setGithubReqHeaders req
        resp <- httpBS req'
        decoded <- return $ eitherDecode' . B.fromStrict $ getResponseBody resp
        case decoded of
          Right r ->
            let asset = findAssetFromReleases r
            in if maybe False shouldUpgrade asset
                 then case asset of
                        (Just a) -> do
                          let downloadUrl = browser_download_url a
                          putStrLn
                            "Getting the latest version of curl-runnings..."
                          downloadResp <-
                            httpBS . setGithubReqHeaders =<<
                            (parseRequest $ T.unpack downloadUrl)
                          _ <-
                            B.writeFile
                              tmpArchive
                              (B.fromStrict $ getResponseBody downloadResp)
                          putStrLn "Extracting..."
                          Tar.unpack tmpArchiveExtracedFolder .
                            Tar.read . GZ.decompress =<<
                            B.readFile tmpArchive
                          putStrLn "Copying..."
                          permissions <- getPermissions tmpExtractedBin
                          setPermissions
                            tmpExtractedBin
                            (setOwnerExecutable True permissions)
                          copyFile tmpExtractedBin installPath
                          putStrLn $
                            "The latest version curl-runnings has been installed to /usr/local/bin. " ++
                            "If you are using curl-runnings from a different location," ++
                            " please update your environment accordingly."
                        -- We got a good response from github, but no asset for our platform was there
                        Nothing -> do
                          putStrLn . T.unpack $
                            makeRed "Error upgrading curl-runnings"
                          putStrLn $
                            "No asset found from github. It's possible the binary for your " ++
                            "platform hasn't yet been uploaded to the latest release. Please wait and try again. " ++
                            "If the issue persists, please open an issue at https://github.com/aviaviavi/curl-runnings/issues."
                          exitWith (ExitFailure 1)
              -- No upgrade required
                 else putStrLn $
                      "curl-runnings is already at the newest version: " ++
                      showVersion version ++ ". Nothing to upgrade."
          -- Coudn't decode github response
          Left err -> do
            putStrLn . T.unpack $ makeRed "Error upgrading curl-runnings"
            putStrLn err
            exitWith (ExitFailure 1)

toLogLevel :: Verbosity -> LogLevel
toLogLevel v = toEnum $ fromEnum v

main :: IO ()
main = do
  userArgs <- cmdArgs argParser
  verbosityLevel <- getVerbosity
  let tlsCheckType =
        if skip_tls_check userArgs
          then SkipTLSCheck
          else DoTLSCheck
  print (json_output userArgs)
  if upgrade userArgs
    then upgradeCurlRunnings
    else runFile
           (file userArgs)
           verbosityLevel
           (grep userArgs)
           tlsCheckType
           (json_output userArgs)
