-- Shakefile Stuff
import Development.Shake
    ( liftIO,
      ShakeOptions(shakeColor),
      cmd_,
      shakeArgs,
      copyFileChanged,
      shakeOptions,
      getDirectoryFiles,
      (%>),
      need,
      phony,
      want,
      (|%>) )
import Development.Shake.FilePath
import Text.Regex
import qualified Data.Text as T
import Data.Text.ICU (regex, Regex)
-- import Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import qualified Data.Text.ICU.Replace as TR

-- Server stuff
import Network.Wai.Application.Static (defaultFileServerSettings, ssListing, staticApp)
import qualified Network.Wai.Handler.Warp as Warp
import WaiAppStatic.Types (StaticSettings)

import Main.Utf8 (withUtf8)
import Lucid (renderToFile)
import Template ( pageHtml )


main :: IO ()
main = withUtf8 $ shakeArgs shakeOptions{shakeColor=True} $ do
    want [ "dest/index.html"
         , "dest/ch-1.html"
         , "dest/ch-2.html"
         , "dest/ch-3.html"
         ]

    -- To serve the generated files (useful for previewing),
    -- run `shake serve`.
    phony "serve" $
        liftIO $ serve 8080 "dest/"

    "templates/template.html" %> \f -> do
        -- Build templates/template.html from our module, Template.hs
        need ["Template.hs"]
        liftIO $ renderToFile f pageHtml

    "dest/*.html" %> \f -> do
        let bib = "source/references.bib"
            csl = "templates/modern-language-association.csl"
            source = "source" </> dropDirectory1 ( f -<.> "md" )
            template = "templates/template.html"
        includes <- getDirectoryFiles "" ["source/includes/**/*", "source/images/**/*"]
        let includesDests = map (\f -> "dest" </> dropDirectory1 f) includes
        need $ [ source, template, bib ] ++ includesDests
        liftIO $ print includes
        cmd_ "pandoc" ["--template", template,
                        "--standalone",
                        "--section-divs",
                        "--reference-location=block",
                        "--csl=" ++ csl,
                        "--toc",
                        "--variable=autoSectionLabels:true",
                        "--metadata=linkReferences:true",
                        "--metadata=link-citations:true",
                        "--metadata=tblPrefix:table",
                        "--filter=templates/PandocSidenote.hs",
                        "--filter=pandoc-crossref",
                        "--citeproc",
                        "--mathjax",
                        "--bibliography", bib,
                        "-o", f,
                        source
                      ]

    -- Anything in /source/images and source/includes we just copy over.
    ["dest/images/**/**", "dest/includes/**/**"] |%> \f -> do
        let source = "source" </> dropDirectory1 f
        need [source]
        copyFileChanged source f

-- | WAI Settings suited for serving statically generated websites.
staticSiteServerSettings :: FilePath -> StaticSettings
staticSiteServerSettings root =
  defaultSettings
    -- Disable directory listings
    { ssListing = Nothing }
  where
    defaultSettings = defaultFileServerSettings root

-- | Run a HTTP server to serve a directory of static files
serve ::
  -- | Port number to bind to
  Int ->
  -- | Directory to serve.
  FilePath ->
  IO ()
serve port path = do
  putStrLn $ "Serving at http://localhost:" <> show port
  Warp.run port $ staticApp $ staticSiteServerSettings path
