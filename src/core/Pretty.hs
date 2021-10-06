module Pretty (mkPretty, pPrint) where
      
import Text.Pretty.Simple
import Data.Text.Lazy (toStrict)
import Control.Lens ((^.), to, from)
import Control.Lens.Iso.Extended (stext)
import Text.Pretty.Simple.Internal.OutputPrinter

mkPretty :: Show a => String -> a -> String
mkPretty msg x = msg ++ "\n" ++ pStringOpt defaultOutputOptionsNoColor (escapeNonPrintable (show x))^.to toStrict.from stext