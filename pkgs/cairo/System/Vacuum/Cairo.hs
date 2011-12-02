--
-- |
-- Module      :  System.Vacuum.Cairo
-- Copyright   :  (c) Tim Docker 2006, Don Stewart 2009
-- License     :  BSD-style
--
-- Interactively visualize Haskell heap values as SVG graphs in a Cairo canvas
-- using graphviz.
--
-- > view [1..10]
--
-- Will display a pop-up window of the data structure produced
--

module System.Vacuum.Cairo (

    view
,   viewFile -- export to .svg file
 ) where

import qualified Graphics.UI.Gtk as G hiding (eventKeyName)
import qualified Graphics.UI.Gtk.Gdk.Events as G
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.SVG as C
import qualified Graphics.UI.Gtk.Gdk.DrawWindow as G
import qualified Graphics.UI.Gtk.Gdk.Gdk as G

import GHC.Vacuum
import Text.PrettyPrint
import Text.Printf
import Data.List

import System.Process
import Control.Concurrent (forkIO,yield)
import System.IO
import Control.Monad
import System.Exit
import Data.Char
import qualified Control.Exception as C

import System.Directory
import Control.Concurrent.MVar.Strict
import System.IO.Unsafe

import Control.DeepSeq

------------------------------------------------------------------------

data Session = Session {
        sWindow  :: !G.Window ,
        sDrawing :: !G.DrawingArea ,
        sSVG     :: !C.SVG
    }

instance NFData Session where
    rnf x = x `seq` ()

sessionRef = unsafePerformIO $ newEmptyMVar
{-# NOINLINE sessionRef #-}

-- | Create a new vacuum session
newSession :: IO ()
newSession = do
    G.unsafeInitGUIForThreadedRTS
    window <- G.windowNew
    canvas <- G.drawingAreaNew

    svg    <- C.svgNewFromString welcome

    G.onKeyPress window $ anyKey (G.widgetDestroy window)
    G.onDestroy  window (takeMVar sessionRef >> G.mainQuit)

    G.onExposeRect canvas $ const $ do
--        print "EXPOSE"
        withMVar sessionRef $ \(Session _ c svg) ->
             updateCanvas svg c
        return ()

    -- not needed.
    G.onExposeRect window $ const $ do
--        print "EXPOSE"
        withMVar sessionRef $ \(Session _ c svg) ->
             updateCanvas svg c
        return ()

    G.set window [G.containerChild G.:= canvas]

    G.windowSetDefaultSize window 400 200
    G.widgetShowAll window
    forkIO G.mainGUI

    let s = Session {
                    sWindow  = window,
                    sDrawing = canvas,
                    sSVG     = svg
              }

    putMVar sessionRef $! s

------------------------------------------------

-- | Set the style
myPpDot :: [(String, [String])] -> Doc
myPpDot = graphToDot id


-- | .
graphToDot :: (a -> String) -> [(a, [a])] -> Doc
graphToDot f = ppGraph . fmap (f *** fmap f)
  where f *** g = \(a, b)->(f a, g b)

------------------------------------------------

-- | Graph style
gStyle :: String
gStyle = unlines
    [
    "graph [splines=true];"

--     "graph [concentrate=true];" 
    ,"node [fontcolor=\"#1f33b3\", fontsize=12, shape=none, fontname=\"Helvetica\"];"
    ,"edge [color=\"#000000\", style=dotted, fontname=\"Helvetica\", arrowhead=normal, arrowsize=0.3];"
    ]

ppGraph :: [(String, [String])] -> Doc
ppGraph xs = (text "digraph g" <+> text "{")
              $+$ text gStyle
                $+$ nest indent (vcat . fmap ppEdge $ xs)
                    $+$ text "}"
                        where indent = 4

ppEdge :: (String, [String]) -> Doc
ppEdge (x,xs) =
    -- label node
     (dQText x) <+> brackets (text "label=" <> dQText (takeWhile (/= '|') x))
    $$
    -- node data
     (dQText x) <+> (text "->")
                    <+> (braces . hcat . punctuate comma
                        . fmap dQText $ xs)

dQText :: String -> Doc
dQText = doubleQuotes . text

------------------------------------------------------------------------

-- Display node data. Everything before the | will be used as a label.
myStyle = ShowHNode {
          showHNode    = renderNode
         ,externHNode  = \_ -> "..." }

  where
    renderNode i n = node ++ "|" ++ show i -- unique id

        -- some kinda atomic types:
      where node = case nodeName n of
                 ":"  -> "(:)"

                -- atomic stuff is special
                 k | k `elem` ["S#" ,"I#" ,"W#"
                              ,"I8#" ,"I16#" ,"I32#" ,"I64#"
                              ,"W8#" ,"W16#" ,"W32#" ,"W64#"] -> showLit n
                 -- chars
                 "C#" -> show . chr . fromIntegral . head . nodeLits $ n
                 "D#" -> "Double"
                 "F#" -> "Float"

                 -- bytestrings
                 "PS"    -> printf "ByteString[%d,%d]" (nodeLits n !! 1) (nodeLits n !! 2)
                 "Chunk" -> printf "Chunk[%d,%d]" (nodeLits n !! 1) (nodeLits n !! 2)

                 -- otherwise just the constructor and local fields
                 c   | z > 0 ->
                         c ++ show (take (fromIntegral z) $ nodeLits n)
                     | otherwise -> c
                         where z = itabLits (nodeInfo n)

            showLit n = show (head $ nodeLits n)

-- | Render a value using the current session
view :: a -> IO ()
view a = do
    noSession <- isEmptyMVar sessionRef
    () <- when noSession $ newSession

    -- actually call into vacuum
    let dot = render. myPpDot . (showHNodes myStyle) $ vacuum a

    -- TODO check for path
    mdot <- findExecutable "dot" 
    let exe = case mdot of
                Nothing -> error "\"dot\" executable not found. Please install graphviz"
                Just p  -> p

    svgstring <- myReadProcess exe ["-Tsvg"] dot
    svg       <- C.svgNewFromString svgstring
    writeFile "/tmp/demo" svgstring

--  TODO destroy old svg canvas here.
    c <- modifyMVar sessionRef $ \(Session win canvas svg') -> do
        updateCanvas svg canvas
        return ((Session win canvas svg), (canvas))

    -- hides expose events!
    G.widgetQueueDraw c
    yield
    return ()

-- | Render a value to a file
viewFile :: String -> a -> IO ()
viewFile file a = do
   let dot = render . myPpDot . (showHNodes myStyle) $ vacuum a
   mdot <- findExecutable "dot"
   let exe = case mdot of
               Nothing -> error "\"dot\" executable not found. please install graphviz"
               Just p  -> p 
   svgstring <- myReadProcess exe ["-Tsvg"] dot
   writeFile file svgstring

updateCanvas :: C.SVG -> G.DrawingArea  -> IO Bool
updateCanvas svg canvas = do
    win             <- G.widgetGetDrawWindow canvas
    (width, height) <- G.widgetGetSize canvas
    let (w,h)   = (fromIntegral width,fromIntegral height)
        (sw,sh) = C.svgGetSize svg

    G.renderWithDrawable win $ do

        C.setAntialias C.AntialiasDefault
        C.setLineCap C.LineCapSquare

        C.scale (w / fromIntegral sw) (h / fromIntegral sh)
        C.svgRender svg

    return True

------------------------------------------------------------------------
-- UI

-- do action m for any keypress (except meta keys)
anyKey :: (Monad m) => m a -> G.Event -> m Bool
anyKey m (G.Key {G.eventKeyName=key})
    | any (`isPrefixOf` key) ignores = return True
    | otherwise                      = m >> return True
  where ignores = ["Shift","Control","Alt",
                   "Super","Meta","Hyper"]

type SVGString = String

renderableToWindow :: SVGString -> IO ()
renderableToWindow chart = do
    svg <- C.svgNewFromString chart

    G.unsafeInitGUIForThreadedRTS
    -- G.initGUI
    window <- G.windowNew
    canvas <- G.drawingAreaNew
    -- fix size
    --   G.windowSetResizable window False
--    G.widgetSetSizeRequest window windowWidth windowHeight

    -- press any key to quit
    G.onKeyPress window $ \e -> case e of
         G.Key {G.eventKeyName=key}
            | key == "r" -> do
                return True
         _               -> anyKey (G.widgetDestroy window) e

    G.onDestroy window G.mainQuit
    G.onExpose canvas $ const (updateCanvas svg canvas)
    G.set window [G.containerChild G.:= canvas]
    G.widgetShowAll window
    G.mainGUI

------------------------------------------------------------------------
-- Talking to dot

myReadProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout + stderr
myReadProcess cmd args input = do
    (Just inh, Just outh, _, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = Inherit }

    -- fork off a thread to start consuming the output
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    forkIO $ C.evaluate (length output) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    case ex of
     ExitSuccess   -> return output
     ExitFailure r -> return output

{-
      ioError (mkIOError OtherError ("readProcess: " ++ cmd ++ 
                                     ' ':unwords (map show args) ++ 
                                     " (exit " ++ show r ++ ")")
                                 Nothing Nothing)

-}


------------------------------------------------------------------------

welcome = unlines
   ["<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>",
    "<!-- Created with Inkscape (http://www.inkscape.org/) -->",
    "<svg",
    "   xmlns:dc=\"http://purl.org/dc/elements/1.1/\"",
    "   xmlns:cc=\"http://creativecommons.org/ns#\"",
    "   xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"",
    "   xmlns:svg=\"http://www.w3.org/2000/svg\"",
    "   xmlns=\"http://www.w3.org/2000/svg\"",
    "   xmlns:xlink=\"http://www.w3.org/1999/xlink\"",
    "   xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"",
    "   xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"",
    "   width=\"744.09448819\"",
    "   height=\"1052.3622047\"",
    "   id=\"svg2670\"",
    "   sodipodi:version=\"0.32\"",
    "   inkscape:version=\"0.46\"",
    "   sodipodi:docname=\"drawing-1.svg\"",
    "   inkscape:output_extension=\"org.inkscape.output.svg.inkscape\">",
    "  <defs",
    "     id=\"defs2672\">",
    "    <linearGradient",
    "       inkscape:collect=\"always\"",
    "       id=\"linearGradient2259\">",
    "      <stop",
    "         style=\"stop-color:#fe798e;stop-opacity:1\"",
    "         offset=\"0\"",
    "         id=\"stop2261\" />",
    "      <stop",
    "         style=\"stop-color:#9c2219;stop-opacity:1\"",
    "         offset=\"1\"",
    "         id=\"stop2263\" />",
    "    </linearGradient>",
    "    <linearGradient",
    "       inkscape:collect=\"always\"",
    "       xlink:href=\"#linearGradient2259\"",
    "       id=\"linearGradient2289\"",
    "       gradientUnits=\"userSpaceOnUse\"",
    "       gradientTransform=\"translate(92.2254,-0.313709)\"",
    "       x1=\"-157.5\"",
    "       y1=\"222.86218\"",
    "       x2=\"-247.5\"",
    "       y2=\"276.86218\" />",
    "    <linearGradient",
    "       inkscape:collect=\"always\"",
    "       id=\"linearGradient2251\">",
    "      <stop",
    "         style=\"stop-color:#bff872;stop-opacity:1\"",
    "         offset=\"0\"",
    "         id=\"stop2253\" />",
    "      <stop",
    "         style=\"stop-color:#7de567;stop-opacity:1\"",
    "         offset=\"1\"",
    "         id=\"stop2255\" />",
    "    </linearGradient>",
    "    <linearGradient",
    "       inkscape:collect=\"always\"",
    "       xlink:href=\"#linearGradient2251\"",
    "       id=\"linearGradient2287\"",
    "       gradientUnits=\"userSpaceOnUse\"",
    "       gradientTransform=\"translate(92.2254,-0.313709)\"",
    "       x1=\"-133.5\"",
    "       y1=\"221.86218\"",
    "       x2=\"-50\"",
    "       y2=\"279.36218\" />",
    "    <linearGradient",
    "       inkscape:collect=\"always\"",
    "       id=\"linearGradient2243\">",
    "      <stop",
    "         style=\"stop-color:#92cbe2;stop-opacity:1\"",
    "         offset=\"0\"",
    "         id=\"stop2245\" />",
    "      <stop",
    "         style=\"stop-color:#7f95fe;stop-opacity:1\"",
    "         offset=\"1\"",
    "         id=\"stop2247\" />",
    "    </linearGradient>",
    "    <linearGradient",
    "       inkscape:collect=\"always\"",
    "       xlink:href=\"#linearGradient2243\"",
    "       id=\"linearGradient2285\"",
    "       gradientUnits=\"userSpaceOnUse\"",
    "       gradientTransform=\"translate(92.2254,-0.313709)\"",
    "       x1=\"-143\"",
    "       y1=\"207.36218\"",
    "       x2=\"-129.5\"",
    "       y2=\"108.86218\" />",
    "    <inkscape:perspective",
    "       sodipodi:type=\"inkscape:persp3d\"",
    "       inkscape:vp_x=\"0 : 526.18109 : 1\"",
    "       inkscape:vp_y=\"0 : 1000 : 0\"",
    "       inkscape:vp_z=\"744.09448 : 526.18109 : 1\"",
    "       inkscape:persp3d-origin=\"372.04724 : 350.78739 : 1\"",
    "       id=\"perspective2678\" />",
    "  </defs>",
    "  <sodipodi:namedview",
    "     id=\"base\"",
    "     pagecolor=\"#ffffff\"",
    "     bordercolor=\"#666666\"",
    "     borderopacity=\"1.0\"",
    "     gridtolerance=\"10000\"",
    "     guidetolerance=\"10\"",
    "     objecttolerance=\"10\"",
    "     inkscape:pageopacity=\"0.0\"",
    "     inkscape:pageshadow=\"2\"",
    "     inkscape:zoom=\"0.35\"",
    "     inkscape:cx=\"375\"",
    "     inkscape:cy=\"520\"",
    "     inkscape:document-units=\"px\"",
    "     inkscape:current-layer=\"layer1\"",
    "     showgrid=\"false\"",
    "     inkscape:window-width=\"1024\"",
    "     inkscape:window-height=\"750\"",
    "     inkscape:window-x=\"0\"",
    "     inkscape:window-y=\"18\" />",
    "  <metadata",
    "     id=\"metadata2675\">",
    "    <rdf:RDF>",
    "      <cc:Work",
    "         rdf:about=\"\">",
    "        <dc:format>image/svg+xml</dc:format>",
    "        <dc:type",
    "           rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />",
    "      </cc:Work>",
    "    </rdf:RDF>",
    "  </metadata>",
    "  <g",
    "     inkscape:label=\"Layer 1\"",
    "     inkscape:groupmode=\"layer\"",
    "     id=\"layer1\">",
    "    <g",
    "       id=\"g2972\">",
    "      <g",
    "         transform=\"matrix(1.0238907,0,0,0.9663314,87.464629,725.20244)\"",
    "         class=\"graph\"",
    "         id=\"graph0\">",
    "        <title",
    "           id=\"title5\">g</title>",
    "        <polygon",
    "           id=\"polygon7\"",
    "           points=\"-4,4 -4,-616 581,-616 581,4 -4,4 \"",
    "           style=\"fill:#ffffff;stroke:#ffffff\" />",
    "<!-- Node|0 -->        <g",
    "           class=\"node\"",
    "           id=\"node1\">",
    "          <title",
    "             id=\"title10\">Node|0</title>",
    "          <text",
    "             id=\"text12\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-590.20001\"",
    "             x=\"325\">Node</text>",
    "        </g>",
    "<!-- (:)|1 -->        <g",
    "           class=\"node\"",
    "           id=\"node4\">",
    "          <title",
    "             id=\"title15\">(:)|1</title>",
    "          <text",
    "             id=\"text17\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-518.20001\"",
    "             x=\"325\">(:)</text>",
    "        </g>",
    "<!-- Node|0&#45;&gt;(:)|1 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge3\">",
    "          <title",
    "             id=\"title20\">Node|0-&gt;(:)|1</title>",
    "          <path",
    "             id=\"path22\"",
    "             d=\"M 325,-576 C 325,-566 325,-554 325,-543\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon24\"",
    "             points=\"326.05,-543 325,-540 323.95,-543 326.05,-543 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (,)|2 -->        <g",
    "           class=\"node\"",
    "           id=\"node7\">",
    "          <title",
    "             id=\"title27\">(,)|2</title>",
    "          <text",
    "             id=\"text29\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-446.20001\"",
    "             x=\"289\">(,)</text>",
    "        </g>",
    "<!-- (:)|1&#45;&gt;(,)|2 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge6\">",
    "          <title",
    "             id=\"title32\">(:)|1-&gt;(,)|2</title>",
    "          <path",
    "             id=\"path34\"",
    "             d=\"M 316,-504 C 311,-494 304,-481 299,-471\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon36\"",
    "             points=\"299.945,-470.514 298,-468 297.953,-471.178 299.945,-470.514 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|3 -->        <g",
    "           class=\"node\"",
    "           id=\"node8\">",
    "          <title",
    "             id=\"title39\">(:)|3</title>",
    "          <text",
    "             id=\"text41\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-446.20001\"",
    "             x=\"361\">(:)</text>",
    "        </g>",
    "<!-- (:)|1&#45;&gt;(:)|3 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge7\">",
    "          <title",
    "             id=\"title44\">(:)|1-&gt;(:)|3</title>",
    "          <path",
    "             id=\"path46\"",
    "             d=\"M 334,-504 C 339,-494 346,-481 351,-471\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon48\"",
    "             points=\"352.047,-471.178 352,-468 350.055,-470.514 352.047,-471.178 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (,)|4 -->        <g",
    "           class=\"node\"",
    "           id=\"node11\">",
    "          <title",
    "             id=\"title51\">(,)|4</title>",
    "          <text",
    "             id=\"text53\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-374.20001\"",
    "             x=\"99\">(,)</text>",
    "        </g>",
    "<!-- (,)|2&#45;&gt;(,)|4 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge10\">",
    "          <title",
    "             id=\"title56\">(,)|2-&gt;(,)|4</title>",
    "          <path",
    "             id=\"path58\"",
    "             d=\"M 262,-440 C 227,-426 165,-403 129,-389\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon60\"",
    "             points=\"129.178,-387.953 126,-388 128.514,-389.945 129.178,-387.953 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- Leaf|5 -->        <g",
    "           class=\"node\"",
    "           id=\"node12\">",
    "          <title",
    "             id=\"title63\">Leaf|5</title>",
    "          <text",
    "             id=\"text65\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-158.2\"",
    "             x=\"298\">Leaf</text>",
    "        </g>",
    "<!-- (,)|2&#45;&gt;Leaf|5 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge11\">",
    "          <title",
    "             id=\"title68\">(,)|2-&gt;Leaf|5</title>",
    "          <path",
    "             id=\"path70\"",
    "             d=\"M 290,-432 C 292,-381 296,-238 297,-183\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon72\"",
    "             points=\"298.05,-183 297,-180 295.95,-183 298.05,-183 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (,)|16 -->        <g",
    "           class=\"node\"",
    "           id=\"node15\">",
    "          <title",
    "             id=\"title75\">(,)|16</title>",
    "          <text",
    "             id=\"text77\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-374.20001\"",
    "             x=\"353\">(,)</text>",
    "        </g>",
    "<!-- (:)|3&#45;&gt;(,)|16 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge14\">",
    "          <title",
    "             id=\"title80\">(:)|3-&gt;(,)|16</title>",
    "          <path",
    "             id=\"path82\"",
    "             d=\"M 359,-432 C 358,-422 356,-410 355,-399\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon84\"",
    "             points=\"356.05,-399 355,-396 353.95,-399 356.05,-399 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|17 -->        <g",
    "           class=\"node\"",
    "           id=\"node16\">",
    "          <title",
    "             id=\"title87\">(:)|17</title>",
    "          <text",
    "             id=\"text89\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-374.20001\"",
    "             x=\"427\">(:)</text>",
    "        </g>",
    "<!-- (:)|3&#45;&gt;(:)|17 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge15\">",
    "          <title",
    "             id=\"title92\">(:)|3-&gt;(:)|17</title>",
    "          <path",
    "             id=\"path94\"",
    "             d=\"M 378,-432 C 387,-422 399,-409 409,-398\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon96\"",
    "             points=\"409.621,-398.864 411,-396 408.136,-397.379 409.621,-398.864 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|6 -->        <g",
    "           class=\"node\"",
    "           id=\"node19\">",
    "          <title",
    "             id=\"title99\">(:)|6</title>",
    "          <text",
    "             id=\"text101\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-302.20001\"",
    "             x=\"99\">(:)</text>",
    "        </g>",
    "<!-- (,)|4&#45;&gt;(:)|6 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge18\">",
    "          <title",
    "             id=\"title104\">(,)|4-&gt;(:)|6</title>",
    "          <path",
    "             id=\"path106\"",
    "             d=\"M 99,-360 C 99,-350 99,-338 99,-327\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon108\"",
    "             points=\"100.05,-327 99,-324 97.9501,-327 100.05,-327 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- Sum[1]|7 -->        <g",
    "           class=\"node\"",
    "           id=\"node20\">",
    "          <title",
    "             id=\"title111\">Sum[1]|7</title>",
    "          <text",
    "             id=\"text113\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-302.20001\"",
    "             x=\"27\">Sum[1]</text>",
    "        </g>",
    "<!-- (,)|4&#45;&gt;Sum[1]|7 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge19\">",
    "          <title",
    "             id=\"title116\">(,)|4-&gt;Sum[1]|7</title>",
    "          <path",
    "             id=\"path118\"",
    "             d=\"M 81,-360 C 71,-350 58,-337 47,-326\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon120\"",
    "             points=\"47.8638,-325.379 45,-324 46.3789,-326.864 47.8638,-325.379 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (,)|16&#45;&gt;Leaf|5 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge53\">",
    "          <title",
    "             id=\"title123\">(,)|16-&gt;Leaf|5</title>",
    "          <path",
    "             id=\"path125\"",
    "             d=\"M 348,-360 C 341,-330 325,-268 312,-216 C 309,-205 306,-193 303,-183\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon127\"",
    "             points=\"303.945,-182.514 302,-180 301.953,-183.178 303.945,-182.514 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (,)|18 -->        <g",
    "           class=\"node\"",
    "           id=\"node53\">",
    "          <title",
    "             id=\"title130\">(,)|18</title>",
    "          <text",
    "             id=\"text132\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-302.20001\"",
    "             x=\"243\">(,)</text>",
    "        </g>",
    "<!-- (,)|16&#45;&gt;(,)|18 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge54\">",
    "          <title",
    "             id=\"title135\">(,)|16-&gt;(,)|18</title>",
    "          <path",
    "             id=\"path137\"",
    "             d=\"M 326,-360 C 310,-350 289,-337 273,-326\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon139\"",
    "             points=\"273.079,-324.79 270,-324 271.914,-326.538 273.079,-324.79 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (,)|21 -->        <g",
    "           class=\"node\"",
    "           id=\"node56\">",
    "          <title",
    "             id=\"title142\">(,)|21</title>",
    "          <text",
    "             id=\"text144\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-302.20001\"",
    "             x=\"427\">(,)</text>",
    "        </g>",
    "<!-- (:)|17&#45;&gt;(,)|21 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge57\">",
    "          <title",
    "             id=\"title147\">(:)|17-&gt;(,)|21</title>",
    "          <path",
    "             id=\"path149\"",
    "             d=\"M 427,-360 C 427,-350 427,-338 427,-327\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon151\"",
    "             points=\"428.05,-327 427,-324 425.95,-327 428.05,-327 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|22 -->        <g",
    "           class=\"node\"",
    "           id=\"node57\">",
    "          <title",
    "             id=\"title154\">(:)|22</title>",
    "          <text",
    "             id=\"text156\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-302.20001\"",
    "             x=\"508\">(:)</text>",
    "        </g>",
    "<!-- (:)|17&#45;&gt;(:)|22 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge58\">",
    "          <title",
    "             id=\"title159\">(:)|17-&gt;(:)|22</title>",
    "          <path",
    "             id=\"path161\"",
    "             d=\"M 447,-360 C 458,-350 473,-337 486,-326\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon163\"",
    "             points=\"486.621,-326.864 488,-324 485.136,-325.379 486.621,-326.864 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- 1|8 -->        <g",
    "           class=\"node\"",
    "           id=\"node25\">",
    "          <title",
    "             id=\"title166\">1|8</title>",
    "          <text",
    "             id=\"text168\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-230.2\"",
    "             x=\"99\">1</text>",
    "        </g>",
    "<!-- (:)|6&#45;&gt;1|8 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge24\">",
    "          <title",
    "             id=\"title171\">(:)|6-&gt;1|8</title>",
    "          <path",
    "             id=\"path173\"",
    "             d=\"M 99,-288 C 99,-278 99,-266 99,-255\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon175\"",
    "             points=\"100.05,-255 99,-252 97.9501,-255 100.05,-255 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|9 -->        <g",
    "           class=\"node\"",
    "           id=\"node26\">",
    "          <title",
    "             id=\"title178\">(:)|9</title>",
    "          <text",
    "             id=\"text180\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-230.2\"",
    "             x=\"27\">(:)</text>",
    "        </g>",
    "<!-- (:)|6&#45;&gt;(:)|9 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge25\">",
    "          <title",
    "             id=\"title183\">(:)|6-&gt;(:)|9</title>",
    "          <path",
    "             id=\"path185\"",
    "             d=\"M 81,-288 C 71,-278 58,-265 47,-254\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon187\"",
    "             points=\"47.8638,-253.379 45,-252 46.3789,-254.864 47.8638,-253.379 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- Sum[1]|7&#45;&gt;(:)|9 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge28\">",
    "          <title",
    "             id=\"title190\">Sum[1]|7-&gt;(:)|9</title>",
    "          <path",
    "             id=\"path192\"",
    "             d=\"M 27,-288 C 27,-278 27,-266 27,-255\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon194\"",
    "             points=\"28.0501,-255 27,-252 25.9501,-255 28.0501,-255 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- 2|10 -->        <g",
    "           class=\"node\"",
    "           id=\"node33\">",
    "          <title",
    "             id=\"title197\">2|10</title>",
    "          <text",
    "             id=\"text199\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-158.2\"",
    "             x=\"91\">2</text>",
    "        </g>",
    "<!-- (:)|9&#45;&gt;2|10 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge33\">",
    "          <title",
    "             id=\"title202\">(:)|9-&gt;2|10</title>",
    "          <path",
    "             id=\"path204\"",
    "             d=\"M 43,-216 C 52,-206 63,-193 73,-182\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon206\"",
    "             points=\"73.6211,-182.864 75,-180 72.1362,-181.379 73.6211,-182.864 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|11 -->        <g",
    "           class=\"node\"",
    "           id=\"node34\">",
    "          <title",
    "             id=\"title209\">(:)|11</title>",
    "          <text",
    "             id=\"text211\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-158.2\"",
    "             x=\"226\">(:)</text>",
    "        </g>",
    "<!-- (:)|9&#45;&gt;(:)|11 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge34\">",
    "          <title",
    "             id=\"title214\">(:)|9-&gt;(:)|11</title>",
    "          <path",
    "             id=\"path216\"",
    "             d=\"M 54,-220 C 57,-218 60,-217 63,-216 C 108,-197 162,-180 196,-170\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon218\"",
    "             points=\"196.486,-170.945 199,-169 195.822,-168.953 196.486,-170.945 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- 3|12 -->        <g",
    "           class=\"node\"",
    "           id=\"node39\">",
    "          <title",
    "             id=\"title221\">3|12</title>",
    "          <text",
    "             id=\"text223\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-86.199997\"",
    "             x=\"262\">3</text>",
    "        </g>",
    "<!-- (:)|11&#45;&gt;3|12 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge39\">",
    "          <title",
    "             id=\"title226\">(:)|11-&gt;3|12</title>",
    "          <path",
    "             id=\"path228\"",
    "             d=\"M 235,-144 C 240,-134 247,-121 252,-111\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon230\"",
    "             points=\"253.047,-111.178 253,-108 251.055,-110.514 253.047,-111.178 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|13 -->        <g",
    "           class=\"node\"",
    "           id=\"node40\">",
    "          <title",
    "             id=\"title233\">(:)|13</title>",
    "          <text",
    "             id=\"text235\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-86.199997\"",
    "             x=\"370\">(:)</text>",
    "        </g>",
    "<!-- (:)|11&#45;&gt;(:)|13 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge40\">",
    "          <title",
    "             id=\"title238\">(:)|11-&gt;(:)|13</title>",
    "          <path",
    "             id=\"path240\"",
    "             d=\"M 253,-148 C 278,-136 314,-118 340,-105\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon242\"",
    "             points=\"340.486,-105.945 343,-104 339.822,-103.953 340.486,-105.945 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- 4|14 -->        <g",
    "           class=\"node\"",
    "           id=\"node45\">",
    "          <title",
    "             id=\"title245\">4|14</title>",
    "          <text",
    "             id=\"text247\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-14.2\"",
    "             x=\"370\">4</text>",
    "        </g>",
    "<!-- (:)|13&#45;&gt;4|14 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge45\">",
    "          <title",
    "             id=\"title250\">(:)|13-&gt;4|14</title>",
    "          <path",
    "             id=\"path252\"",
    "             d=\"M 370,-72 C 370,-62 370,-50 370,-39\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon254\"",
    "             points=\"371.05,-39 370,-36 368.95,-39 371.05,-39 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- []|15 -->        <g",
    "           class=\"node\"",
    "           id=\"node46\">",
    "          <title",
    "             id=\"title257\">[]|15</title>",
    "          <text",
    "             id=\"text259\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-14.2\"",
    "             x=\"478\">[]</text>",
    "        </g>",
    "<!-- (:)|13&#45;&gt;[]|15 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge46\">",
    "          <title",
    "             id=\"title262\">(:)|13-&gt;[]|15</title>",
    "          <path",
    "             id=\"path264\"",
    "             d=\"M 397,-72 C 413,-62 433,-49 448,-38\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon266\"",
    "             points=\"449.086,-38.5378 451,-36 447.921,-36.7905 449.086,-38.5378 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|19 -->        <g",
    "           class=\"node\"",
    "           id=\"node60\">",
    "          <title",
    "             id=\"title269\">(:)|19</title>",
    "          <text",
    "             id=\"text271\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-230.2\"",
    "             x=\"171\">(:)</text>",
    "        </g>",
    "<!-- (,)|18&#45;&gt;(:)|19 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge61\">",
    "          <title",
    "             id=\"title274\">(,)|18-&gt;(:)|19</title>",
    "          <path",
    "             id=\"path276\"",
    "             d=\"M 225,-288 C 215,-278 202,-265 191,-254\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon278\"",
    "             points=\"191.864,-253.379 189,-252 190.379,-254.864 191.864,-253.379 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- Sum[1]|20 -->        <g",
    "           class=\"node\"",
    "           id=\"node61\">",
    "          <title",
    "             id=\"title281\">Sum[1]|20</title>",
    "          <text",
    "             id=\"text283\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-230.2\"",
    "             x=\"243\">Sum[1]</text>",
    "        </g>",
    "<!-- (,)|18&#45;&gt;Sum[1]|20 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge62\">",
    "          <title",
    "             id=\"title286\">(,)|18-&gt;Sum[1]|20</title>",
    "          <path",
    "             id=\"path288\"",
    "             d=\"M 243,-288 C 243,-278 243,-266 243,-255\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon290\"",
    "             points=\"244.05,-255 243,-252 241.95,-255 244.05,-255 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (,)|21&#45;&gt;Leaf|5 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge72\">",
    "          <title",
    "             id=\"title293\">(,)|21-&gt;Leaf|5</title>",
    "          <path",
    "             id=\"path295\"",
    "             d=\"M 412,-288 C 397,-269 372,-240 350,-216 C 340,-205 328,-192 318,-182\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon297\"",
    "             points=\"318.864,-181.379 316,-180 317.379,-182.864 318.864,-181.379 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (,)|23 -->        <g",
    "           class=\"node\"",
    "           id=\"node68\">",
    "          <title",
    "             id=\"title300\">(,)|23</title>",
    "          <text",
    "             id=\"text302\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-230.2\"",
    "             x=\"429\">(,)</text>",
    "        </g>",
    "<!-- (,)|21&#45;&gt;(,)|23 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge73\">",
    "          <title",
    "             id=\"title305\">(,)|21-&gt;(,)|23</title>",
    "          <path",
    "             id=\"path307\"",
    "             d=\"M 428,-288 C 428,-278 428,-266 429,-255\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon309\"",
    "             points=\"430.05,-255 429,-252 427.95,-255 430.05,-255 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|22&#45;&gt;[]|15 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge76\">",
    "          <title",
    "             id=\"title312\">(:)|22-&gt;[]|15</title>",
    "          <path",
    "             id=\"path314\"",
    "             d=\"M 523,-288 C 531,-278 539,-265 544,-252 C 570,-176 585,-143 550,-72 C 541,-54 523,-41 508,-32\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon316\"",
    "             points=\"508.079,-30.7905 505,-30 506.914,-32.5378 508.079,-30.7905 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (,)|26 -->        <g",
    "           class=\"node\"",
    "           id=\"node71\">",
    "          <title",
    "             id=\"title319\">(,)|26</title>",
    "          <text",
    "             id=\"text321\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-230.2\"",
    "             x=\"508\">(,)</text>",
    "        </g>",
    "<!-- (:)|22&#45;&gt;(,)|26 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge77\">",
    "          <title",
    "             id=\"title324\">(:)|22-&gt;(,)|26</title>",
    "          <path",
    "             id=\"path326\"",
    "             d=\"M 508,-288 C 508,-278 508,-266 508,-255\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon328\"",
    "             points=\"509.05,-255 508,-252 506.95,-255 509.05,-255 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|19&#45;&gt;2|10 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge65\">",
    "          <title",
    "             id=\"title331\">(:)|19-&gt;2|10</title>",
    "          <path",
    "             id=\"path333\"",
    "             d=\"M 151,-216 C 140,-206 125,-193 113,-182\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon335\"",
    "             points=\"113.864,-181.379 111,-180 112.379,-182.864 113.864,-181.379 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|19&#45;&gt;(:)|11 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge66\">",
    "          <title",
    "             id=\"title338\">(:)|19-&gt;(:)|11</title>",
    "          <path",
    "             id=\"path340\"",
    "             d=\"M 185,-216 C 192,-206 202,-193 210,-183\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon342\"",
    "             points=\"211.21,-183.079 212,-180 209.462,-181.914 211.21,-183.079 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- Sum[1]|20&#45;&gt;(:)|11 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge69\">",
    "          <title",
    "             id=\"title345\">Sum[1]|20-&gt;(:)|11</title>",
    "          <path",
    "             id=\"path347\"",
    "             d=\"M 239,-216 C 236,-206 234,-193 231,-183\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon349\"",
    "             points=\"231.945,-182.514 230,-180 229.953,-183.178 231.945,-182.514 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|24 -->        <g",
    "           class=\"node\"",
    "           id=\"node74\">",
    "          <title",
    "             id=\"title352\">(:)|24</title>",
    "          <text",
    "             id=\"text354\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-158.2\"",
    "             x=\"370\">(:)</text>",
    "        </g>",
    "<!-- (,)|23&#45;&gt;(:)|24 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge80\">",
    "          <title",
    "             id=\"title357\">(,)|23-&gt;(:)|24</title>",
    "          <path",
    "             id=\"path359\"",
    "             d=\"M 414,-216 C 406,-206 396,-193 387,-182\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon361\"",
    "             points=\"387.864,-181.379 385,-180 386.379,-182.864 387.864,-181.379 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- Sum[1]|25 -->        <g",
    "           class=\"node\"",
    "           id=\"node75\">",
    "          <title",
    "             id=\"title364\">Sum[1]|25</title>",
    "          <text",
    "             id=\"text366\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-158.2\"",
    "             x=\"442\">Sum[1]</text>",
    "        </g>",
    "<!-- (,)|23&#45;&gt;Sum[1]|25 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge81\">",
    "          <title",
    "             id=\"title369\">(,)|23-&gt;Sum[1]|25</title>",
    "          <path",
    "             id=\"path371\"",
    "             d=\"M 432,-216 C 434,-206 436,-194 438,-183\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon373\"",
    "             points=\"439.047,-183.178 439,-180 437.055,-182.514 439.047,-183.178 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (,)|26&#45;&gt;Leaf|5 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge91\">",
    "          <title",
    "             id=\"title376\">(,)|26-&gt;Leaf|5</title>",
    "          <path",
    "             id=\"path378\"",
    "             d=\"M 481,-222 C 475,-220 470,-218 465,-216 C 407,-196 390,-201 334,-180 C 332,-179 330,-178 328,-177\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon380\"",
    "             points=\"328.178,-175.953 325,-176 327.514,-177.945 328.178,-175.953 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (,)|27 -->        <g",
    "           class=\"node\"",
    "           id=\"node82\">",
    "          <title",
    "             id=\"title383\">(,)|27</title>",
    "          <text",
    "             id=\"text385\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-158.2\"",
    "             x=\"514\">(,)</text>",
    "        </g>",
    "<!-- (,)|26&#45;&gt;(,)|27 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge92\">",
    "          <title",
    "             id=\"title388\">(,)|26-&gt;(,)|27</title>",
    "          <path",
    "             id=\"path390\"",
    "             d=\"M 510,-216 C 511,-206 512,-194 512,-183\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon392\"",
    "             points=\"513.05,-183 512,-180 510.95,-183 513.05,-183 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|24&#45;&gt;3|12 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge84\">",
    "          <title",
    "             id=\"title395\">(:)|24-&gt;3|12</title>",
    "          <path",
    "             id=\"path397\"",
    "             d=\"M 343,-144 C 328,-134 307,-121 292,-110\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon399\"",
    "             points=\"292.079,-108.79 289,-108 290.914,-110.538 292.079,-108.79 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|24&#45;&gt;(:)|13 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge85\">",
    "          <title",
    "             id=\"title402\">(:)|24-&gt;(:)|13</title>",
    "          <path",
    "             id=\"path404\"",
    "             d=\"M 370,-144 C 370,-134 370,-122 370,-111\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon406\"",
    "             points=\"371.05,-111 370,-108 368.95,-111 371.05,-111 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- Sum[1]|25&#45;&gt;(:)|13 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge88\">",
    "          <title",
    "             id=\"title409\">Sum[1]|25-&gt;(:)|13</title>",
    "          <path",
    "             id=\"path411\"",
    "             d=\"M 424,-144 C 414,-134 401,-121 390,-110\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon413\"",
    "             points=\"390.864,-109.379 388,-108 389.379,-110.864 390.864,-109.379 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|28 -->        <g",
    "           class=\"node\"",
    "           id=\"node85\">",
    "          <title",
    "             id=\"title416\">(:)|28</title>",
    "          <text",
    "             id=\"text418\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-86.199997\"",
    "             x=\"442\">(:)</text>",
    "        </g>",
    "<!-- (,)|27&#45;&gt;(:)|28 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge95\">",
    "          <title",
    "             id=\"title421\">(,)|27-&gt;(:)|28</title>",
    "          <path",
    "             id=\"path423\"",
    "             d=\"M 496,-144 C 486,-134 473,-121 462,-110\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon425\"",
    "             points=\"462.864,-109.379 460,-108 461.379,-110.864 462.864,-109.379 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- Sum[1]|29 -->        <g",
    "           class=\"node\"",
    "           id=\"node86\">",
    "          <title",
    "             id=\"title428\">Sum[1]|29</title>",
    "          <text",
    "             id=\"text430\"",
    "             style=\"font-size:12px;text-anchor:middle;fill:#1f33b3;font-family:Arial\"",
    "             y=\"-86.199997\"",
    "             x=\"514\">Sum[1]</text>",
    "        </g>",
    "<!-- (,)|27&#45;&gt;Sum[1]|29 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge96\">",
    "          <title",
    "             id=\"title433\">(,)|27-&gt;Sum[1]|29</title>",
    "          <path",
    "             id=\"path435\"",
    "             d=\"M 514,-144 C 514,-134 514,-122 514,-111\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon437\"",
    "             points=\"515.05,-111 514,-108 512.95,-111 515.05,-111 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|28&#45;&gt;4|14 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge99\">",
    "          <title",
    "             id=\"title440\">(:)|28-&gt;4|14</title>",
    "          <path",
    "             id=\"path442\"",
    "             d=\"M 424,-72 C 414,-62 401,-49 390,-38\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon444\"",
    "             points=\"390.864,-37.3789 388,-36 389.379,-38.8638 390.864,-37.3789 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- (:)|28&#45;&gt;[]|15 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge100\">",
    "          <title",
    "             id=\"title447\">(:)|28-&gt;[]|15</title>",
    "          <path",
    "             id=\"path449\"",
    "             d=\"M 451,-72 C 456,-62 463,-49 468,-39\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon451\"",
    "             points=\"469.047,-39.1781 469,-36 467.055,-38.514 469.047,-39.1781 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "<!-- Sum[1]|29&#45;&gt;[]|15 -->        <g",
    "           class=\"edge\"",
    "           id=\"edge103\">",
    "          <title",
    "             id=\"title454\">Sum[1]|29-&gt;[]|15</title>",
    "          <path",
    "             id=\"path456\"",
    "             d=\"M 505,-72 C 500,-62 493,-49 488,-39\"",
    "             style=\"fill:none;stroke:#000000;stroke-dasharray:1, 5\" />",
    "          <polygon",
    "             id=\"polygon458\"",
    "             points=\"488.945,-38.514 487,-36 486.953,-39.1781 488.945,-38.514 \"",
    "             style=\"fill:#000000;stroke:#000000\" />",
    "        </g>",
    "      </g>",
    "      <g",
    "         transform=\"matrix(0.6239211,0,0,0.5888465,618.19146,80.227604)\"",
    "         id=\"g2279\">",
    "        <path",
    "           id=\"path2231\"",
    "           d=\"M -58.11348,213.41831 L -162.26826,151.37974 L -37.01785,100.04846 L 68.1307,158.15264 L -58.11348,213.41831 z\"",
    "           style=\"fill:url(#linearGradient2285);fill-opacity:1\"",
    "           sodipodi:nodetypes=\"ccccc\" />",
    "        <path",
    "           sodipodi:nodetypes=\"ccccc\"",
    "           id=\"path2229\"",
    "           d=\"M -51.56037,213.5916 L 72.03976,159.30293 L 54.30386,283.51375 L -62.14067,350.24024 L -51.56037,213.5916 z\"",
    "           style=\"fill:url(#linearGradient2287);fill-opacity:1\" />",
    "        <path",
    "           id=\"path2227\"",
    "           d=\"M -59.56037,347.18826 L -164.06037,282.96566 L -163.61166,159.3255 L -55.18554,215.49255 L -59.56037,347.18826 z\"",
    "           style=\"fill:url(#linearGradient2289);fill-opacity:1\"",
    "           sodipodi:nodetypes=\"ccccc\" />",
    "        <path",
    "           sodipodi:nodetypes=\"cssscssssssssssssccssssscccsscccccccccccssssssssscccccccccccccccssscccssssccssssssssssssssssssssssc\"",
    "           id=\"path2225\"",
    "           d=\"M -66.27108,352.00632 C -69.30467,350.22909 -85.32501,339.74485 -101.87184,328.708 C -141.06516,302.56578 -152.91684,294.99749 -161.22644,290.80508 C -172.47362,285.13057 -171.42166,291.71277 -171.76785,224.84661 C -172.09132,162.36876 -172.8819,151.51349 -166.98072,153.09872 C -171.59657,148.70486 -149.89048,141.93362 -115.91158,126.8062 C -82.71848,112.02862 -46.04303,97.12968 -37.99045,95.151731 C -35.80415,94.614711 -30.58575,96.813551 -10.91381,106.56083 C 32.31874,127.98218 67.99593,147.7319 72.54924,152.76325 C 73.58759,153.91062 75.03553,154.84937 75.76688,154.84937 C 77.84679,154.84937 78.37332,158.95768 77.25844,166.48751 C 76.69538,170.29042 72.64193,195.0632 68.25078,221.53813 C 63.85962,248.01307 59.96508,272.38204 59.59625,275.69141 C 58.29011,287.41076 58.65986,286.97385 40.74281,297.9691 C 31.80145,303.4562 9.14236,316.66419 -9.61071,327.3202 C -28.36378,337.97622 -47.03613,348.65729 -51.10479,351.05592 C -55.17346,353.45455 -59.00931,355.3767 -59.62892,355.32736 C -60.24852,355.27802 -63.23749,353.78356 -66.27108,352.00632 z M -61.87106,326.08404 C -61.53987,317.12117 -60.90739,291.51119 -60.46556,269.17296 C -59.4846,219.57813 -59.22374,221.12454 -69.76193,214.06246 C -82.68227,205.404 -96.65592,197.35576 -120.42436,184.88302 C -133.38604,178.08124 -147.58846,170.35597 -151.98528,167.71576 C -157.4594,164.42865 -160.30462,163.24026 -161.01083,163.94598 C -162.29568,165.22992 -162.39159,217.12625 -161.17615,253.40053 L -160.31016,279.24588 L -153.15345,283.38712 C -146.97571,286.96188 -111.07469,311.08894 -76.29946,335.03636 C -70.50807,339.02452 -65.02796,342.30839 -64.12144,342.33386 C -62.68203,342.3743 -62.39695,340.31581 -61.87106,326.08404 z M -123.45447,281.63953 L -121.92862,231.56652 L -140.98255,221.52843 L -138.49408,200.85313 L -78.16426,229.52961 L -79.29283,248.93423 L -101.23414,239.89866 C -104.02169,251.08704 -104.55187,273.56224 -106.14867,290.70437 L -123.45447,281.63953 z M -9.61071,316.77116 C 34.17351,291.73098 48.95974,282.50651 49.5137,279.88608 C 49.76822,278.68213 50.70639,272.73304 51.59851,266.66586 C 52.49065,260.59869 56.34489,236.69937 60.1635,213.55627 C 66.55191,174.8386 67.87919,164.87776 66.64985,164.87776 C 65.35309,164.87776 41.87183,175.43096 11.95031,189.46143 C -6.52698,198.1256 -27.06009,207.7061 -33.67883,210.75142 C -40.29756,213.79674 -46.27431,216.80609 -46.96048,217.43886 C -49.01471,219.33318 -53.67591,334.1607 -51.88774,338.82058 C -51.3077,340.33214 -50.58188,340.15475 -45.46483,337.25078 C -42.2919,335.45011 -26.15754,326.23429 -9.61071,316.77116 z M -31.52427,299.57413 C -31.87775,272.99487 -33.95338,246.76003 -28.00983,218.92136 L -15.53109,218.69485 L -14.46997,256.84948 L 28.82336,204.1205 L 49.21858,198.81459 L 11.17117,250.94644 L 46.4177,267.81063 L 31.8401,277.19567 L -4.08659,261.22054 L -17.12774,267.67154 L -17.55651,293.33323 L -31.52427,299.57413 z M -44.98935,205.62376 C -42.07793,204.31693 -20.74221,194.22819 2.42335,183.20435 C 25.58892,172.18051 47.47586,161.89162 51.06101,160.34015 C 60.54302,156.23684 60.93348,155.67356 56.37503,152.674 C 49.35425,148.05414 13.69688,129.08139 -11.32169,116.65357 L -36.09795,104.34612 L -44.41536,107.15536 C -56.14763,111.11798 -89.54973,125.04344 -118.35237,137.97996 C -148.44864,151.49752 -152.51519,153.54112 -152.51519,155.14816 C -152.51519,155.84547 -145.85885,160.0994 -137.72332,164.60135 C -111.60287,179.05557 -54.81147,207.96913 -52.51064,207.98472 C -51.28536,207.99302 -47.90078,206.93059 -44.98935,205.62376 z M -60.75547,189.54567 C -75.28072,185.96717 -91.81229,177.57594 -96.0911,171.6097 C -99.47879,166.88602 -102.37326,159.22994 -102.37326,154.99297 C -102.37326,149.88393 -99.16092,142.63741 -94.96571,138.28271 C -89.64934,132.76424 -82.2461,130.42562 -63.03287,128.19542 C -43.93665,125.97879 -27.42575,126.6637 -17.57474,130.08117 C -8.29082,133.30188 -4.52834,135.30727 -2.61559,138.05436 C 2.38083,145.2301 5.93328,151.2196 5.93328,152.46787 C 5.93328,154.32106 1.29355,156.06801 -2.01293,155.45979 C -3.4339,155.1984 -6.26937,154.71168 -8.31397,154.3782 C -11.32445,153.88717 -12.45855,153.01191 -14.27723,149.776 C -16.85292,145.19318 -23.60657,140.51818 -30.15247,138.78687 C -40.15132,136.14228 -67.5992,138.35316 -76.05306,142.48406 C -80.82949,144.81802 -83.39709,149.93235 -84.08312,158.47883 C -84.55072,164.304 -84.36299,165.20843 -82.20604,167.52215 C -79.28527,170.65521 -71.09215,174.87312 -62.00697,177.92089 C -56.94497,179.61901 -54.0594,180.03115 -50.5755,179.55363 C -42.46315,178.44171 -41.49161,176.3237 -46.32188,170.28055 C -50.48755,165.06888 -51.4992,162.24318 -50.12122,159.66839 C -49.15472,157.86249 -48.34815,157.66756 -43.78911,158.13809 C -37.32875,158.80486 -23.72903,163.98325 -20.5194,166.99854 C -17.69425,169.65263 -17.2471,175.88082 -19.70523,178.33895 C -22.5073,181.14102 -34.94771,188.0685 -39.80016,189.52887 C -45.45811,191.23166 -53.88454,191.23841 -60.75547,189.54567 z\"",
    "           style=\"fill:#000000\" />",
    "      </g>",
    "      <text",
    "         transform=\"scale(1.0293517,0.9714853)\"",
    "         id=\"text2881\"",
    "         y=\"174.54315\"",
    "         x=\"100.85123\"",
    "         style=\"font-size:39.78779221px;font-style:normal;font-weight:normal;fill:#000000;fill-opacity:1;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1;font-family:Bitstream Vera Sans\"",
    "         xml:space=\"preserve\"><tspan",
    "           style=\"font-size:21.88328552px\"",
    "           y=\"174.54315\"",
    "           x=\"100.85123\"",
    "           id=\"tspan2883\"",
    "           sodipodi:role=\"line\">Vacuum:</tspan><tspan",
    "           style=\"font-size:21.88328552px\"",
    "           id=\"tspan2885\"",
    "           y=\"201.89725\"",
    "           x=\"100.85123\"",
    "           sodipodi:role=\"line\">Live</tspan><tspan",
    "           style=\"font-size:21.88328552px\"",
    "           id=\"tspan2887\"",
    "           y=\"229.25137\"",
    "           x=\"100.85123\"",
    "           sodipodi:role=\"line\">Haskell</tspan><tspan",
    "           style=\"font-size:21.88328552px\"",
    "           id=\"tspan2889\"",
    "           y=\"256.60547\"",
    "           x=\"100.85123\"",
    "           sodipodi:role=\"line\">Data</tspan><tspan",
    "           style=\"font-size:21.88328552px\"",
    "           id=\"tspan2891\"",
    "           y=\"283.95959\"",
    "           x=\"100.85123\"",
    "           sodipodi:role=\"line\">Visualization</tspan></text>",
    "      <text",
    "         transform=\"scale(1.0293517,0.9714853)\"",
    "         id=\"text2893\"",
    "         y=\"161.30353\"",
    "         x=\"266.34671\"",
    "         style=\"font-size:39.78779221px;font-style:normal;font-weight:normal;fill:#000000;fill-opacity:1;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1;font-family:Bitstream Vera Sans\"",
    "         xml:space=\"preserve\"><tspan",
    "           y=\"161.30353\"",
    "           x=\"266.34671\"",
    "           id=\"tspan2895\"",
    "           sodipodi:role=\"line\" /></text>",
    "      <text",
    "         id=\"text2897\"",
    "         y=\"697.0545\"",
    "         x=\"101.70216\"",
    "         style=\"font-size:50px;font-style:normal;font-weight:normal;fill:#000000;fill-opacity:1;stroke:none;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1;font-family:Bitstream Vera Sans\"",
    "         xml:space=\"preserve\"><tspan",
    "           style=\"font-size:15px\"",
    "           y=\"697.0545\"",
    "           x=\"101.70216\"",
    "           id=\"tspan2899\"",
    "           sodipodi:role=\"line\">Type any key to close</tspan></text>",
    "    </g>",
    "  </g>",
    "</svg>"]
