{-
  Example file for typesetting the Lord's Prayer using Graphics.DVI
  You have to change load_tenrm to the location of the Computer Modern Roman 10pt on your computer
  You can also use a different font if you prefer, but you might have to adjust other things to work with another font
-}

module Main (main) where {
  import Data.Int;
  import Data.Ratio;
  import Data.Word;
  import Graphics.DVI;

  load_tenrm :: IO Font;
  load_tenrm = loadFont (Left 1000) "f:/program files2/MiKTeX/fonts/tfm/public/cm/cmr10.tfm";

  typeset :: Font -> String -> DocStat -> Page;
  typeset fo txt = Page (pageNum (1 :: Int)) o' where {
    o = shiftPage (0, -points 10) $ renderNode n;
    o' = ((-inches 1, 0), Rule (inches 8.5) (points 1)) : o;
    n = Node . fst $ vPack True maxBound id il;
    il = interlineGlue 0 (fixedGlue $ points 12) (Node $ RuleNode (Just 1000) (Just 1000) (Just 1000)) par;
    par = simpleMakeParagraph ps tys;
    parindent = Node (GlueNode $ fixedGlue (inches 0.5));
    parendfill = Node (GlueNode $ Glue 0 (GlueSS 0 1 0 0) nullGlueSS);
    ps = simpleParagraphSetting { spsWidth = inches 5 };
    tys = parindent : typesetCustomString withFrenchSpacing fo txt ++ [parendfill, Node . PenaltyNode $ Just 0];
  };

  lordsPrayer :: String;
  lordsPrayer =
    "Our Father who art in heaven, " ++
    "hallowed be thy name. " ++
    "Thy kingdom come. " ++
    "Thy will be done " ++
    "on earth as it is in heaven. " ++
    "Give us this day our daily bread, " ++
    "and forgive us our trespasses, " ++
    "as we forgive those who trespass against us, " ++
    "and lead us not into temptation, " ++
    "but deliver us from evil. " ++
    "[For thine is the kingdom, " ++
    "and the power, and the glory, " ++
    "for ever and ever.] " ++
    "Amen.";

  main :: IO ();
  main = load_tenrm >>= \tenrm -> createDVI "example.dvi" 1000 dviUnitsTeX >>=
   shipOut . typeset tenrm lordsPrayer >>= finishDVI;
}
