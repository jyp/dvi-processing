{-
  Example program to make a table using hAlign
  You have to change load_tenrm to the location of the Computer Modern Roman 10pt on your computer
-}

module Main (main) where {
  import Control.Applicative;
  import Data.Int;
  import Data.List;
  import Data.Ratio;
  import Data.Word;
  import Graphics.DVI;
  import Graphics.DVI.Alignment;

  load_tenrm :: IO Font;
  load_tenrm = loadFont (Left 1000) "f:/program files2/MiKTeX/fonts/tfm/public/cm/cmr10.tfm";

  makeTable :: Font -> [[String]] -> Node;
  makeTable fo d = (Node . fst $ vPack True maxBound id al) where {
    da = map ((++ [hfil]) . (strut :) . typesetCustomString withFrenchSpacing fo) <$> d;
    da' = head da : [[Node NoAlignNode, hbar]] : tail da;
    hfil = Node . GlueNode $ Glue 0 (GlueSS 0 1 0 0) nullGlueSS;
    hbar = Node $ RuleNode Nothing (Just $ points 0.5) (Just $ points 0.5);
    strut = Node $ RuleNode (Just 0) (Just $ points 11) (Just $ points 4);
    tm = (\x -> Node (DataNode x fixedGlue)) <$> [0..length (head d) - 1];
    tm' = UnsetBoxNode id $ intersperse (Node . KernNode $ picas 2) tm;
    al = hAlign 0 [Node tm'] da';
  };

  tableData :: [[String]];
  tableData = [
    ["#", "Abbreviation", "Full Name"],
    ["1", "DVI", "Device Independent"],
    ["2", "PS", "PostScript"],
    ["3", "PDF", "Portable Document Format"]
  ];

  renderTable :: Font -> DocStat -> Page;
  renderTable f = Page (pageNum (1 :: Int)) (renderNode (makeTable f tableData));

  main :: IO ();
  main = load_tenrm >>= \tenrm -> createDVI "example.dvi" 1000 dviUnitsTeX >>=
   shipOut . renderTable tenrm >>= finishDVI;
}
