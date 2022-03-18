-- Shakefile Stuff
import Development.Shake
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
         , "dest/01/ch-1.html"
         , "dest/02/ch-2.html"
         , "dest/03/ch-3.html"
         ]

    -- To serve the generated files (useful for previewing),
    -- run `shake serve`.
    phony "serve" $
      liftIO $ serve 8080 "dest/"

    "templates/template.html" %> \f -> do
        need ["Template.hs"]
        liftIO $ renderToFile f pageHtml

    "dest/index.html" %> \f -> do
        let source = "source/index.md"
            template = "templates/template.html"
        need ([ source, template ])
        contents <- liftIO $ readFile source
        cmd (Stdin contents) "pandoc" ["--template", template,
                                       "--standalone",
                                       "--section-divs",
                                       "--variable=autoSectionLabels:true",
                                       "-o", f
                                       ]

    let bib = "references.bib"
        csl = "templates/modern-language-association.csl"
        template = "templates/template.html"

    ["dest//images/*", "dest//includes/*", "dest/assets/**"] |%> \f -> do
        let source = dropDirectory1 f
        need [source]
        copyFileChanged source f

    "dest/03/ch-3.html" %> \f -> do
        assets <- getDirectoryFiles "" [ "03/images/*"
                                       , "assets/*/*"
                                       , "03/includes/*" ]
        liftIO $ print assets
        let outAssets = map ("dest/" <>) assets
        let source = "03/ch-3.md"
            filters = [ "templates/PandocSidenote.hs"
                      , "templates/hex-filter.hs"
                      ]
        need ([ source, template, csl, bib ]
              ++ outAssets
              ++ filters)
        contents <- readFileText source
        cmd (Stdin replaced) "pandoc" ["--template", template,
                                       "--standalone",
                                       "--section-divs",
                                       "--reference-location=block",
                                       "--csl=" ++ csl,
                                       "--toc",
                                       "--variable=autoSectionLabels:true",
                                       "--metadata=linkReferences:true",
                                       "--metadata=link-citations:true",
                                       "--metadata=tblPrefix:table",
                                       "--citation-abbreviations=03-colors/abbreviations.json",
                                       "--filter=templates/PandocSidenote.hs",
                                       "--filter=pandoc-crossref",
                                       "--citeproc",
                                       "--filter=templates/hex-filter.hs",
                                       "--mathjax",
                                       "--bibliography", bib,
                                       "-o", f
                                       ]

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
