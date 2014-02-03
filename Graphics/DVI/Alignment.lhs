\: Introduction to Alignment. This module allows to make alignment of
tables typesetting similar to how \TeX\ does so but different.

> {-# LANGUAGE DeriveDataTypeable #-}

> module Graphics.DVI.Alignment (
>   -- * Alignment
>   DataNode(..), NoAlignNode(..), UnsetBoxNode(..), hAlign, vAlign
> ) where {

> import Control.Applicative;
> import Control.Monad;
> import Control.Monad.Trans.Writer;
> import Data.List;
> import Data.Maybe;
> import Data.Monoid;
> import Data.Typeable;
> import Graphics.DVI;

> bool :: x -> x -> Bool -> x;
> bool x _ False = x;
> bool _ x True = x;

\: Alignment Nodes. Data nodes: Used in a template. It contains a column
number, and a function to convert a distance to a glue value. The distance
to be converted shall be the largest natural width (height for vertical
alignments) of the box in all records; the glue value will be used to
determine the actual width of this field. The data node is then replaced
by a horizontal box packaging the contents of this field.

> data DataNode = DataNode Int (Dimen -> Glue) deriving Typeable;

> instance NodeClass DataNode where {
>   showNode (DataNode x _) = "Data " ++ show x ++ ";";
> };

No-align nodes: If this is the first node in a field data, the rest of its
contents are copied to output without using the template.

> data NoAlignNode = NoAlignNode deriving Typeable;

> instance NodeClass NoAlignNode where {
>   showNode NoAlignNode = "NoAlign;";
> };

Unset box nodes: The width, height, and depth of a box might not be known
at first due to the rows varying, so the template can contain unset box
nodes, which will be packaged after the data nodes are replaced by the
boxes, allowing width and height and depth to be calculated.

> data UnsetBoxNode = UnsetBoxNode (Dimen -> Dimen) [Node]
>  deriving Typeable;

> instance NodeClass UnsetBoxNode where {
>   showNode (UnsetBoxNode _ x) = "UnsetBox [" ++ (x >>= showNode)
>    ++ "];";
>   traverseBox f (UnsetBoxNode z n) = (Node . UnsetBoxNode z <$>
>    sequence (traverseBox f <$> n)) >>= f;
> };

\: Alignment. This is a code for a horizontal alignment. First parameter
is the distance which the glue should be set to, second is the template,
third is the nodes for data, and then the output will be the list of
output. This list includes adjustments too; it will be a vertical list,
which can then be made into a vertical box.

> hAlign :: Dimen -> [Node] -> [[[Node]]] -> [Node];
> hAlign d n f = dat' where {
>   f' = nna <$> f;
>   fna = f' >>= either (const []) return;
>   nna [x : y] | isJust (castNode x :: Maybe NoAlignNode) = Left y;
>   nna x = Right x;
>   awd = map (wd1 . hPack id) <$> fna;
>   wd1 (x, _, _) = nodeWidth x;
>   mwd = maximum <$> transpose awd;
>   tg = foldl addGlue (fixedGlue 0) $ execWriter
>    (mapM (travBoxFunc tg') n);
>   tg' z@(DataNode i x) = writer (Node z, [x (mwd !! i)]);
>   gs = calcGlueSet d tg;
>   dat = f' >>= either id (uncurry (++) . runWriter .
>    \x -> mapM (travBoxFunc (hp x)) n);
>   dat' = travBoxPure sb <$> dat;
>   hp y (DataNode i x) = hp' $ hPack (const . round $ calcGlue gs
>    (x (mwd !! i))) (y !! i);
>   hp' (x, _, y) = writer (Node x, y);
>   sb (UnsetBoxNode f x) = sb' $ hPack f x;
>   sb' (x, _, _) = Node x;
> };

As well as the vertical alignment. It is mostly the same as horizontal
alignment, but this time, there is also a flag to tell it to package
vertical boxes upward or downward.

> vAlign :: Bool -> Dimen -> [Node] -> [[[Node]]] -> [Node];
> vAlign vt d n f = dat' where {
>   f' = nna <$> f;
>   fna = f' >>= either (const []) return;
>   nna [x : y] | isJust (castNode x :: Maybe NoAlignNode) = Left y;
>   nna x = Right x;
>   awd = map (wd1 . vPack False maxBound id) <$> fna;
>   wd1 (x, _) = nodeHeight x;
>   mwd = maximum <$> transpose awd;
>   tg = foldl addGlue (fixedGlue 0) $
>    execWriter (mapM (travBoxFunc tg') n);
>   tg' z@(DataNode i x) = writer (Node z, [x (mwd !! i)]);
>   gs = calcGlueSet d tg;
>   dat = f' >>= either id (uncurry (++) . runWriter .
>    \x -> mapM (travBoxFunc (hp x)) n);
>   dat' = travBoxPure sb <$> dat;
>   hp y (DataNode i x) = hp' $ vPack vt maxBound (const . round $
>    calcGlue gs (x (mwd !! i))) (y !! i);
>   hp' (x, _) = writer (Node x, []);
>   sb (UnsetBoxNode f x) = sb' $ vPack vt maxBound f x;
>   sb' (x, _) = Node x;
> };

\endinput

> }
