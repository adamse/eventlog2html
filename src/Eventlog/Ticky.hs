{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- Functions for rendering ticky sample information
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
module Eventlog.Ticky where

import Eventlog.Types
import qualified Data.Map as Map
import Data.Word

import Data.String
import qualified Data.Text as T
--import Text.Blaze.Html
import qualified Text.Blaze.Html5            as H
import Text.Blaze.Html5 as H
    ( preEscapedToHtml,
      toHtml,
      dataAttribute,
      preEscapedStringValue,
      stringComment,
      Html,
      (!),
      AttributeValue,
      body,
      button,
      code,
      div,
      docTypeHtml,
      h1,
      head,
      link,
      meta,
      script,
      style,
      table,
      td,
      th,
      thead,
      title,
      tr )
import Text.Blaze.Html5.Attributes as A
    ( charset, class_, hidden, href, id, onclick, rel, src )
import Text.Blaze.Html.Renderer.String

import Eventlog.Javascript
import Eventlog.Args
import Eventlog.AssetVersions
import Paths_eventlog2html
import Data.Version ( showVersion )
import Text.RawString.QQ
import Data.Fixed
import Control.Monad

renderTicky :: Word64 -> Map.Map TickyCounterId TickyCounter
                      -> Map.Map InfoTablePtr InfoTableLoc
                      -> [TickySample] -> (Double, Html)
renderTicky total_allocs counters ipes samples = (percentage_ticked, renderTickyInfo (not (Map.null ipes)) joined_with_ipe)
  where
    percentage_ticked = realToFrac (sum (Map.map allocs accum_samples)) / realToFrac total_allocs
    joined_with_ipe   = mkClosureInfo (\_ (v, _, _) -> tickyCtrInfo v) joined_data ipes

    joined_data   = Map.mergeWithKey (\_ b c -> Just (b, c, realToFrac (allocs c) / realToFrac total_allocs)) (const mempty) (const mempty) counters accum_samples
    accum_samples = accumulateSamples samples


data AccumStats = AccumStats { entries :: !Word64, allocs :: !Word64, allocd :: !Word64 } deriving Show

instance Semigroup AccumStats where
  (<>) (AccumStats a b c) (AccumStats d e f) = AccumStats (a + d) (b + e) (c + f)

instance Monoid AccumStats where
  mempty = AccumStats 0 0 0

accumulateSamples ::  [TickySample] -> Map.Map TickyCounterId AccumStats
accumulateSamples samples = Map.fromListWith (<>) [(TickyCounterId a, AccumStats b c d) | TickySample a b c d <- samples]


jsScript :: String -> Html
jsScript url = script ! src (fromString $ url) $ ""
css :: AttributeValue -> Html
css url = link ! rel "stylesheet" ! href url

htmlHeader :: Args -> Html
htmlHeader as =
    H.head $ do
    H.title "eventlog2html - Ticky Profile"
    meta ! charset "UTF-8"
    if not (noIncludejs as)
      then do
        script $ preEscapedToHtml vegaLite
        script $ preEscapedToHtml vega
        script $ preEscapedToHtml vegaEmbed
        script $ preEscapedToHtml jquery
        H.style  $ preEscapedToHtml bootstrapCSS
        script $ preEscapedToHtml bootstrap
        script $ preEscapedToHtml fancytable
        script $ preEscapedToHtml sparkline
      else do
        jsScript vegaURL
        jsScript vegaLiteURL
        jsScript vegaEmbedURL
        jsScript jqueryURL
        css (preEscapedStringValue bootstrapCSSURL)
        jsScript bootstrapURL
        css "https://fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"
        jsScript fancyTableURL
        css "https://cdn.datatables.net/1.11.3/css/jquery.dataTables.min.css"
        jsScript "https://cdn.datatables.net/1.11.3/js/jquery.dataTables.min.js"
        jsScript "https://cdn.datatables.net/plug-ins/1.11.3/dataRender/ellipsis.js"
        jsScript sparklinesURL
    -- Include this last to overwrite some milligram styling
    H.style $ preEscapedToHtml stylesheet


template :: Header -> Word64 -> Double ->  Html -> Args -> Html
template header' total ticked_percen v as = docTypeHtml $ do
  H.stringComment $ "Generated with eventlog2html-" <> showVersion version
  htmlHeader as
  body $ H.div ! class_ "container" $ do
    H.div ! class_ "row" $ do
      H.div ! class_ "column" $ do
        h1 $ H.a ! href "https://mpickering.github.io/eventlog2html" $ "eventlog2html"

    H.div ! class_ "row" $ do
      H.div ! class_ "column" $ do
        "Options: "
        code $ toHtml $ hJob header'

    H.div ! class_ "row" $ do
      H.div ! class_ "column" $ do
        "Created at: "
        code $ toHtml $ hDate header'

    H.div ! class_ "row" $ do
      H.div ! class_ "column" $ do
        "Total Allocations: "
        code $ toHtml $ toHtml total
      H.div ! class_ "column cheader" $ do
        "Allocations Ticked (%): "
        code $ toHtml $ toHtml (render  $ trunc (ticked_percen * 100))

    H.div ! class_ "row" $ do
      H.div ! class_ "column" $ do
        button ! class_ "tablink button-black" ! onclick "changeTab('table', this)" ! A.id "defaultOpen" $ "Table"
    H.div ! class_ "row" $ do
          H.div ! A.id "table" ! class_ "tabviz" $ v
    script $ preEscapedToHtml tablogic


tickyTemplateString :: Header -> Word64 -> Double -> Html -> Args -> String
tickyTemplateString header' tot_allocs ticked_per ticky_table as =
  renderHtml $ template header' tot_allocs ticked_per ticky_table as

-- Table rendering
trunc :: Double -> Fixed E2
trunc = realToFrac
render :: Fixed E2 -> String
render = showFixed True


renderTickyInfo :: Bool
                  -> Map.Map TickyCounterId (InfoTableLocStatus, (TickyCounter, AccumStats, Double))
                  -> Html
renderTickyInfo with_ipe ticky_samples = do
  H.table ! A.id "closure_table" ! A.class_ "table table-striped closureTable" ! A.hidden "true" $ do
    H.thead $ H.tr $ headFoot
--      H.th "Profile"
--      numTh "n"
    Map.foldr (\a res -> renderEntry a >> res) (mempty :: Html) ticky_samples
    H.tfoot $ H.tr $ headFoot
  H.script $ preEscapedToHtml (initTable with_ipe)
  where
    headFoot = do
      H.th "Label"
      H.th "Arguments"
      when (with_ipe) $ do
        H.th "Description"
        H.th "CTy"
        H.th "Type"
        H.th "Module"
        H.th "Loc"
      numTh "Allocs"
      numTh "Allocs (%)"
      numTh "Allocd"
      numTh "Entries"
      numTh "Allocs/Entries"
      numTh "Allocd/Entries"
    numTh lbl = H.th ! H.dataAttribute "sortas" "numeric" $ lbl

    renderInfoTableLoc :: InfoTableLoc -> Html
    renderInfoTableLoc (InfoTableLoc table_name cd tydesc _lbl m sloc) = do
      H.td (toHtml table_name)
      H.td (toHtml (show @ClosureType cd))
      H.td (toHtml tydesc)
      H.td (toHtml m)
      H.td (toHtml sloc)


    renderInfoTableLocStatus :: InfoTableLocStatus -> Html
    renderInfoTableLocStatus itls =
      case itls of
        Here itl -> renderInfoTableLoc itl
        Missing  -> emptyItlColumns
        None     -> mempty

    emptyItlColumns = do
      H.td ""
      H.td ""
      H.td ""
      H.td ""
      H.td ""


    renderEntry :: (InfoTableLocStatus, (TickyCounter, AccumStats, Double)) -> Html
    renderEntry (loc, ((TickyCounter _id _arity kinds label _), AccumStats {..}, percent)) = do
          H.tr $ do
--            H.td (renderSpark (getBandValues n (ts, bs)))
            H.td (toHtml label)
            H.td (toHtml kinds)
            renderInfoTableLocStatus loc
            H.td (toHtml allocs)
            H.td (toHtml $ render $ trunc (percent * 100))
            H.td (toHtml allocd)
            H.td (toHtml entries)
            H.td (toHtml (case entries of
                            0 -> 0
                            _ -> allocs `Prelude.div` entries))
            H.td (toHtml (render (trunc (case entries of
                            0 -> 0
                            _ -> realToFrac allocd / realToFrac entries))))



{-
renderSpark :: [(Double, Double)] -> Html
renderSpark vs = H.span ! A.class_ "linechart" $ toHtml (T.intercalate "," (map renderLine vs))
  where
    rdouble = T.pack . showFixed True . realToFrac @Double @(Fixed E2)
    renderLine (x,y) = rdouble x <> ":" <> rdouble y
    -}

initTable :: Bool -> T.Text
initTable ipe =

  "var ipe = " <> (if ipe then "true" else "false") <> ";\n" <>
  [r|// Setup - add a text input to each footer cell
    $(document).ready(function(){
    $('.closureTable tfoot th').each( function () {
        var title = $(this).text();
        if (! ($(this).data("sortas") == "numeric")){
          $(this).html( '<input type="text" style="width:100%"; placeholder="Search"/>' );
        }
        else {
          $(this).html('')
        }
    } );

    // DataTable
    var table = $('.closureTable').DataTable({
        "order": [[ ipe ? 7 : 2, "desc" ]],
        "autoWidth": true,
        "columnDefs": [
          { "orderSequence": ["desc", "asc"],  "targets": (ipe ? [7,8,9,10,11] : [ 2,3,4,5,6])}
          , {"render": $.fn.dataTable.render.ellipsis( 30, true, false ), "targets": (ipe ? [4] : []) }
          ],

        "deferRender" : true,
        initComplete: function () {
            // Apply the search
            $(".closureTable").removeAttr("hidden");
            this.api().columns().every( function () {
                var that = this;
                $( 'input', this.footer() ).on( 'keyup change clear', function () {
                    if ( that.search() !== this.value ) {
                        that
                            .search( this.value )
                            .draw();
                    }
                } );
            } );
        }
    });
    })
    |]


{-
getBandValues :: Int
            -> (UArray Int Double, UArray (Int, Int) Double)
            -> [(Double, Double)]
getBandValues k (ts, vs) =
  let (t1, tn) = bounds ts
      go i = flip map [t1 .. tn] $ \t -> ((ts A.! t), (vs A.! (i, t)))

  in go k
  -}


