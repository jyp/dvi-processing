% DVI Processing Library
% [Public domain]

\input birdstyle

\birdleftrule=1pt
\emergencystretch=1em

\def\hugebreak{\penalty-600\vskip 30pt plus 8pt minus 4pt\relax}
\newcount\chapno
\def\: #1.{\advance\chapno by 1\relax\hugebreak{\bf\S\the\chapno. #1. }}

\: Introduction. This is a program for processing DVI (device independent)
files. It can read and write these files.

> {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RankNTypes,
>  GADTs, TypeSynonymInstances, DeriveDataTypeable #-}

Exports:

> module Graphics.DVI (
>   -- * Pages
>     PageObjects, Page(..), PageNumbers, PageNumberClass(..),
>     DocStat(..), docStat, Coordinates, PageObject(..), TextString,
>     dviUnitsTeX, withBoxes, withInnerBoxes, shiftPage, unshiftPage,
>     sortPage, unboxPage, textToChar, openSpecials, splitSpecials,
>     closeSpecials, maybeSpecial, objWidth, objHeight, objDepth,
>   -- * Units
>     points, picas, centimetres, millimetres, bigPoints, didot, cicero,
>     inches,
>   -- * Fonts
>     Font(..), FontChar(..), LigKern(..), Extensible(..), emptyFont,
>     fontEq, FontMag, loadFont, readFont, getFontChar, findLK,
>     widestChar,
>   -- * Reading
>     withDVI,
>   -- * Writing
>     createDVI, shipOut, finishDVI, singlePageDocument,
>   -- * Glue
>     Glue(..), GlueSS(..), GlueRank(..), GlueSet(..), GlueSign(..),
>     spaceGlue, fixedGlue, nullGlueSS, addGlue, subtractGlue, badness,
>     Dimen, RenderPos, toRenderPos, calcGlue, calcGlueSet, selectGlueSS,
>   -- * Typesetting Nodes
>     Node(..), NodeClass(..), BoxDirection(..), BoxNode(..),
>     RuleNode(..), KernNode(..), GlueNode(..), SpecialNode(..),
>     PenaltyNode(..), ShiftNode(..), TextNode(..), wrapNode, castNode,
>     renderNode, travBoxPure, travBoxFunc,
>   -- * Typesetting
>     typesetSimpleString, sumPackage, hPack, vPack, findBreaks, vSplit,
>     interlineGlue, typesetCustomString, typesetSingleWord,
>     CustomStringFn, withFrenchSpacing, SimpleParagraphSetting(..),
>     simpleParagraphSetting, simpleMakeParagraph
> ) where {

Imports:

> import Control.Applicative;
> import Control.Arrow;
> import Control.Monad;
> import Data.Bits;
> import Data.ByteString (ByteString);
> import qualified Data.ByteString as B;
> import Data.Function;
> import Data.Functor.Identity;
> import Data.Int;
> import Data.List;
> import Data.Maybe;
> import Data.Monoid;
> import Data.Ratio;
> import Data.Traversable (sequenceA);
> import Data.Typeable;
> import Data.Word;
> import System.FilePath;
> import System.IO;

\: Utility Functions. The first few utility functions deal with numbers
with bits.

> transEnum :: (Enum t, Enum u) => t -> u;
> transEnum = toEnum . fromEnum;

> transInt :: (Integral t, Integral u) => t -> u;
> transInt = fromInteger . toInteger;

> low8bits :: (Integral t, Bits t) => t -> Word8;
> low8bits = transInt . (.&. 255);

> modifyBit :: Bits a => Bool -> a -> Int -> a;
> modifyBit x = bool clearBit setBit x;

> reSigned :: (Integral t, Bits t, Integral u, Bits u) => t -> u;
> reSigned x = modifyBit (testBit x n) (transInt $ clearBit x n) n
>  where { n = pred (bitSize x) };

> getBits :: (Bits t, Integral t, Integral u) => Int -> Int -> t -> u;
> getBits h l n = transInt $ shiftR n (h + 1 - l) .&. ((2 ^ l) - 1);

One more deals with byte strings.

> fromStr :: String -> ByteString;
> fromStr = B.pack . fmap transEnum;

There is also for monad bind and use first result.

> (<>>=) :: (Functor m, Monad m) => m a -> (a -> m b) -> m a;
> x <>>= f = x >>= ap (<$) f;
> infixl 1 <>>=;

And boolean.

> bool :: x -> x -> Bool -> x;
> bool x _ False = x;
> bool _ x True = x;

Enumeration could be subtracted.

> (#-) :: Enum x => x -> x -> Int;
> x #- y = fromEnum x - fromEnum y;
> infixl 6 #-;

Finally, monoids should have a operator form of {\tt mappend}.

> (|*|) :: Monoid t => t -> t -> t;
> (|*|) = mappend;
> infixr 5 |*|;

\: Page Data Types. A page is stored as a list of page objects (characters
and rules). The page numbers are also stored, as well as the document stat
values which are used by the file output.

> type PageObjects = [(Coordinates, PageObject)];

> data Page = Page {
>   pageNumbers :: PageNumbers,
>   pageObjects :: PageObjects,
>   pageStat :: DocStat
> };

Page numbers in the DVI format are ten registers. Usually the first one is
used for page numbers. Often you only want one, or a few, so this is the
definition of a class to make other page numbers.

> type PageNumbers = (Word32, Word32, Word32, Word32, Word32, Word32,
>  Word32, Word32, Word32, Word32);

> class PageNumberClass t where {
>   pageNum :: t -> PageNumbers;
> };
> instance PageNumberClass Int where {
>   pageNum a = (toEnum $ fromEnum a, 0, 0, 0, 0, 0, 0, 0, 0, 0);
> };
> instance PageNumberClass Integer where {
>   pageNum a = (toEnum $ fromEnum a, 0, 0, 0, 0, 0, 0, 0, 0, 0);
> };
> instance PageNumberClass Int32 where {
>   pageNum a = (toEnum $ fromEnum a, 0, 0, 0, 0, 0, 0, 0, 0, 0);
> };
> instance PageNumberClass Word32 where {
>   pageNum a = (toEnum $ fromEnum a, 0, 0, 0, 0, 0, 0, 0, 0, 0);
> };
> instance Integral t => PageNumberClass (t, t) where {
>   pageNum (a, b) = (toEnum $ fromEnum a, toEnum $ fromEnum b, 0,
>    0, 0, 0, 0, 0, 0, 0);
> };
> instance Integral t => PageNumberClass (t, t, t) where {
>   pageNum (a, b, c) = (toEnum $ fromEnum a, toEnum $ fromEnum b,
>    toEnum $ fromEnum c, 0, 0, 0, 0, 0, 0, 0);
> };
> instance Integral t => PageNumberClass (t, t, t, t) where {
>   pageNum (a, b, c, d) = (toEnum $ fromEnum a,
>    toEnum $ fromEnum b, toEnum $ fromEnum c, toEnum $ fromEnum d, 0, 0,
>    0, 0, 0, 0);
> };
> instance Integral t => PageNumberClass (t, t, t, t, t) where {
>   pageNum (a, b, c, d, e) = (toEnum $ fromEnum a,
>    toEnum $ fromEnum b, toEnum $ fromEnum c, toEnum $ fromEnum d,
>    toEnum $ fromEnum e, 0, 0, 0, 0, 0);
> };
> instance Integral t => PageNumberClass (t, t, t, t, t, t) where {
>   pageNum (a, b, c, d, e, f) = (toEnum $ fromEnum a,
>    toEnum $ fromEnum b, toEnum $ fromEnum c, toEnum $ fromEnum d,
>    toEnum $ fromEnum e, toEnum $ fromEnum f, 0, 0, 0, 0);
> };

This datatype is used to keep track of the filehandle reading/writing, the
offset of the previous page in the file, the fonts available, the tallest
and widest pages, the stack entries, the total number of pages, the
magnification, and the units. These must be specified at the end of the
DVI file, so they are kept track of for this purpose.

> data DocStat = DocStat {
>   pageHandle :: Maybe Handle,
>   prevPage :: Maybe Word32,
>   fonts :: [Font],
>   tallest :: Dimen,
>   widest :: Dimen,
>   maxStackDepth :: Word16,
>   numPages :: Word16,
>   magnification :: Word32,
>   dviUnits :: Ratio Word32
> };

A smart constructor for the {\tt DocStat} type:

> docStat :: DocStat;
> docStat = DocStat {
>   pageHandle = Nothing,
>   prevPage = Nothing,
>   fonts = [],
>   tallest = 0,
>   widest = 0,
>   maxStackDepth = 0,
>   numPages = 0,
>   magnification = 1000,
>   dviUnits = dviUnitsTeX
> };

The most common units used in DVI files are \TeX\ units, which are defined
here. Note this is not in lowest terms, but it doesn't matter. (Haskell
will change it to lowest terms; the resulting DVI file will still be
valid in this case.)

> dviUnitsTeX :: Ratio Word32;
> dviUnitsTeX = 25400000 % 473628672;

The coordinates on a page are relative to the top-left corner plus the
margins (usually one inch), and for a box they are relative to the
reference point of that box. Vertical distances down are negative (which
will be converted to positive on output to a file).

> type Dimen = Int32;

> type Coordinates = (Dimen, Dimen);

> data PageObject = Special ByteString PageObjects | Box PageObjects
>  | Character Font Word32 | Text Font TextString | Rule Dimen Dimen
>  | DVI_Binary ByteString  deriving (Eq, Show);

This type is used to represent a text string on the page, possibly with
kerning. Left values are used for kerns and right values for characters.

> type TextString = [Either Dimen Word32];

\: Units. These are common units of measuring distance which are used in
typesetting. They are specified here for use with \TeX\ units (so they
won't work if you use different DVI units).

> applyUnits :: Ratio Dimen -> Ratio Dimen -> Dimen;
> applyUnits x y = round (65536 * x * y);

> points :: Ratio Int32 -> Int32;
> points = applyUnits (1 % 1);

> inches :: Ratio Int32 -> Int32;
> inches = applyUnits (7227 % 100);

> bigPoints :: Ratio Int32 -> Int32;
> bigPoints = applyUnits (7227 % 7200);

> centimetres :: Ratio Int32 -> Int32;
> centimetres = applyUnits (7227 % 254);

> millimetres :: Ratio Int32 -> Int32;
> millimetres = applyUnits (7227 % 2540);

> picas :: Ratio Int32 -> Int32;
> picas = applyUnits (12 % 1);

> didot :: Ratio Int32 -> Int32;
> didot = applyUnits (1238 % 1157);

> cicero :: Ratio Int32 -> Int32;
> cicero = applyUnits (14856 % 1157);

\: Font Data Types. These data types keep track of the data stored in the
font metric files; however, they are already processed and scaled to the
correct sizes. There are only up to 256 characters in a font metric file,
and other character codes are modulo 256 to find the correct font metric
data for that character.

> data Font = Font {
>   fontName :: ByteString,
>   fontChecksum :: Word32,
>   atSize :: Word32,
>   designSize :: Word32,
>   fontDimen :: [Dimen],
>   firstChar :: Word8,
>   leftBoundChar :: [LigKern],
>   rightBoundChar :: Maybe Word8,
>   characters :: [FontChar]
> } deriving (Eq, Show);

> data FontChar = FontChar {
>   charWidth :: Dimen,
>   charHeight :: Dimen,
>   charDepth :: Dimen,
>   charItalCorr :: Dimen,
>   charLarger :: Maybe Word8,
>   charLigKern :: [LigKern],
>   charExten :: Maybe Extensible
> } deriving (Eq, Show);

> data LigKern = Kerning { lkChar :: Word8, kernDist :: Dimen }
>  | Ligature { lkChar :: Word8, ligChar :: Word8, ligCode :: Int }
>  deriving (Eq, Show);

> data Extensible = Extensible {
>   extTop :: Word8,
>   extMid :: Word8,
>   extBot :: Word8,
>   extRep :: Word8
> } deriving (Eq, Show);

When a font number in a DVI file is not in order, we can pad the list with
empty font values (which are never valid).

> emptyFont :: Font;
> emptyFont = Font {
>   fontName = B.empty,
>   fontChecksum = 0,
>   atSize = 0,
>   designSize = 0,
>   fontDimen = [],
>   firstChar = 0,
>   leftBoundChar = [],
>   rightBoundChar = Nothing,
>   characters = []
> };

Although the above instance can check for equality of fonts, you might
sometimes want a simpler and faster check.

> fontEq :: Font -> Font -> Bool;
> fontEq x y = (fontName x, fontChecksum x, atSize x)
>  == (fontName y, fontChecksum y, atSize y);

\: File I/O. These are the class for read/write data in a file. DVI uses
big endian format, so that is what is used here.

> class Eq t => FileData t where {
>   fromByteString :: ByteString -> t;
>   toByteString :: t -> ByteString;
>   readDataL :: Handle -> Int -> IO t;
>   readData :: Handle -> IO t;
>   writeData :: Handle -> t -> IO ();
>
>   readDataL h l = fromByteString <$> B.hGet h l;
>   writeData h = B.hPut h . toByteString;
> };

> instance FileData Word8 where {
>   fromByteString = B.head;
>   toByteString = B.singleton;
>   readData = flip readDataL 1;
> };

> instance FileData Word16 where {
>   fromByteString = B.foldl (\x y -> x * 256 + transInt y) 0;
>   toByteString = B.reverse . fst . B.unfoldrN 2 (\x -> Just
>    (low8bits x, shiftR x 8));
>   readData = flip readDataL 2;
> };

> instance FileData Word32 where {
>   fromByteString = B.foldl (\x y -> x * 256 + transInt y) 0;
>   toByteString = B.reverse . fst . B.unfoldrN 4 (\x -> Just
>    (low8bits x, shiftR x 8));
>   readData = flip readDataL 4;
> };

> instance FileData Int32 where {
>   fromByteString = B.foldl (\x y -> x * 256 + transInt y) 0;
>   toByteString = toByteString . (reSigned :: Int32 -> Word32);
>   readData = flip readDataL 4;
> };

> instance FileData ByteString where {
>   fromByteString = id;
>   toByteString = id;
>   readData = error "Cannot use readData for ByteString; use readDataL";
> };

> instance FileData (Ratio Word32) where {
>   fromByteString x = fromByteString (B.take 4 x)
>    % fromByteString (B.drop 4 x);
>   toByteString x = B.append (toByteString $ numerator x)
>    (toByteString $ denominator x);
>   readData = flip readDataL 8;
> };

> instance FileData PageNumbers where {
>   fromByteString x = (fromByteString $ B.take 4 x, fromByteString .
>    fromByteString . B.take 4 $ B.drop 4 x, fromByteString . B.take 4 $
>    B.drop 8 x, fromByteString . B.take 4 $ B.drop 12 x, fromByteString .
>    B.take 4 $ B.drop 16 x, fromByteString . B.take 4 $ B.drop 20 x,
>    fromByteString . B.take 4 $ B.drop 24 x, fromByteString . B.take 4 $
>    B.drop 28 x, fromByteString . B.take 4 $ B.drop 32 x, fromByteString
>    . B.take 4 $ B.drop 36 x);
>   toByteString (a, b, c, d, e, f, g, h, i, j) = B.concat [toByteString
>    a, toByteString b, toByteString c, toByteString d, toByteString e,
>    toByteString f, toByteString g, toByteString h, toByteString i,
>    toByteString j];
>   readData = flip readDataL 40;
> };

\: Reading Font Metrics. Information about fonts must be loaded from the
font metric files, which are called {\tt TFM} files; the `{\tt T}' in `%
{\tt TFM}' stands for \TeX, but other programs (such as this one) know
about them too.

The exact format of the font metric files will not be described here; see
other literate programs (such as \TeX) for more information.

> type FontMag = Either Int Word32;

This function shall be used to load a font metric file. The magnification
(or at size) and filename must be specified; it is assumed that the font
name is the same as the base name of the filename (with the directory and
extension stripped).

> loadFont :: FontMag -> FilePath -> IO Font;
> loadFont m n = withBinaryFile n ReadMode $
>  readFont (fromStr $ takeBaseName n) m;

> mReadData :: FileData t => Handle -> Int -> IO [t];
> mReadData h i = sequence . replicate i $ readData h;

The file effectively consists of many arrays, where the length of the
arrays is specified at the beginning of the file. The length of the
characters array is actually given by the smallest and largest codes,
instead.

> readFont :: ByteString -> FontMag -> Handle -> IO Font;
> readFont n m h = (map fromEnum <$> (mReadData h 12 :: IO [Word16]))
>  >>= \z -> makeFont n m z <$> mReadData h (z !! 1) <*>
>  mReadData h ((z !! 3) + 1 - (z !! 2)) <*> mReadData h (z !! 4)
>  <*> mReadData h (z !! 5) <*> mReadData h (z !! 6)
>  <*> mReadData h (z !! 7) <*> mReadData h (z !! 8)
>  <*> mReadData h (z !! 9) <*> mReadData h (z !! 10)
>  <*> mReadData h (z !! 11);

These three values are the ones called {\it alpha}, {\it beta}, and $z$
in \TeX\ (in that order).

> type FontScale = (Int64, Int64, Int64);

This is a complicated function with thirteen parameters. It has to rescale
everything and calculate a bunch of other stuff in order to load the font
metrics data into memory. Note that it must ignore the ligature/kerning
program for the left and right boundary characters if it is empty, and
that font dimension numbering is one-based instead of zero-based.

> makeFont :: ByteString -> FontMag -> [Int] -> [Word32] -> [Word32]
>  -> [Word32] -> [Word32] -> [Word32] -> [Word32] -> [Word32] -> [Word32]
>  -> [Word32] -> [Word32] -> Font;
> makeFont nam mag hdr hd ci wd ht dp ic lk ke ex pa = Font {
>   fontName = nam,
>   fontChecksum = chk,
>   atSize = ats,
>   designSize = des,
>   fontDimen = 0 : transInt (shiftR (head pa) 4) :
>    (rescale fsc <$> tail pa),
>   firstChar = transInt $ hdr !! 2,
>   leftBoundChar = if lk == [] then [] else (guard (last lk >=
>    0xFF000000) >> makeLigKern ke' (drop (fromEnum $ last lk
>    .&. 0xFFFF) lk)),
>   rightBoundChar = makeRightBoundChar lk,
>   characters = makeCharacter wd' ht' dp' ic' ke' lk ex <$> ci
> } where {
>   chk = head hd;
>   dez = hd !! 1;
>   ats = either (flip div 1000 . (* des) . toEnum) id mag;
>   des :: (Integral t, Bits t) => t;
>   des = reSigned $ shiftR dez 4;
>   fsc = (shiftL (transInt ats) (4 + fsc_comp), shiftR 0x100000
>    fsc_comp, shiftR (transInt ats) fsc_comp) :: FontScale;
>   fsc_comp = if ats < 0o40000000 then 0 else if ats < 0o100000000 then
>    1 else if ats < 0x200000000 then 2 else if ats < 0x400000000 then 3
>    else 4 :: Int;
>   wd' = (rescale fsc <$> wd) :: [Dimen];
>   ht' = (rescale fsc <$> ht) :: [Dimen];
>   dp' = (rescale fsc <$> dp) :: [Dimen];
>   ic' = (rescale fsc <$> ic) :: [Dimen];
>   ke' = (rescale fsc <$> ke) :: [Dimen];
> };

This is the algorithm for rescaling a {\it fix\_word} number to the at
size of the font.

> rescale :: (Integral u, Bits u) => FontScale -> Word32 -> u;
> rescale (alpha, beta, z) v = transInt $ div (z * transInt
>  (v .&. 0xFFFFFF)) beta - bool 0 alpha (testBit v 31);

If the first instruction of the ligature/kerning array has its first byte
equal to 255, then it specifies a right boundary character for the font.
This character need not actually exist in the font.

> makeRightBoundChar :: [Word32] -> Maybe Word8;
> makeRightBoundChar [] = Nothing;
> makeRightBoundChar (h:_) = getBits 23 8 h <$
>  guard (getBits 31 8 h == 255);

> makeCharacter :: [Int32] -> [Int32] -> [Int32] -> [Int32] -> [Int32]
>  -> [Word32] -> [Word32] -> Word32 -> FontChar;
> makeCharacter wd ht dp ic ke lk ex ci = FontChar {
>   charWidth = wd !! getBits 31 8 ci,
>   charHeight = ht !! getBits 23 4 ci,
>   charDepth = dp !! getBits 19 4 ci,
>   charItalCorr = ic !! getBits 15 6 ci,
>   charLarger = low8bits ci <$ guard (tag == 2),
>   charLigKern = guard (tag == 1) >> makeLigKern ke (drop
>    (if lks > 0x80FFFFFF then fromEnum (lks .&. 0xFFFF) else
>    getBits 7 8 ci) lk),
>   charExten = makeExten (ex !! getBits 7 8 ci) <$ guard (tag == 3)
> } where {
>   tag = getBits 9 2 ci :: Int;
>   lks = lk !! getBits 7 8 ci;
> };

Actually, if {\tt extTop}, {\tt extMid}, and/or {\tt extBot} are zero,
then those parts are omitted; but it is stored here as a number instead of
as a {\tt Maybe} type.

> makeExten :: Word32 -> Extensible;
> makeExten w = Extensible {
>   extTop = getBits 31 8 w,
>   extMid = getBits 23 8 w,
>   extBot = getBits 15 8 w,
>   extRep = low8bits w
> };

> makeLigKern :: [Int32] -> [Word32] -> [LigKern];
> makeLigKern ke lk = (if op < 128 then Ligature {
>   lkChar = nc,
>   ligChar = toEnum rm,
>   ligCode = op
> } else Kerning {
>   lkChar = nc,
>   kernDist = ke !! ((op - 128) * 256 + rm)
> }) : if sk < 128 then makeLigKern ke (drop (sk + 1) lk) else []
>  where {
>   i = head lk;
>   sk = getBits 31 8 i :: Int;
>   nc = getBits 23 8 i :: Word8;
>   op = getBits 15 8 i :: Int;
>   rm = getBits 7 8 i :: Int;
> };

\: Page Manipulation. These are the functions for manipulating the page
objects.

> withBoxes :: (PageObjects -> PageObjects) -> PageObjects -> PageObjects;
> withBoxes f = (>>= \x -> case x of {
>   (c, Box y) -> [(c, Box $ withBoxes f y)];
>   (c, Special s y) -> [(c, Special s $ withBoxes f y)];
>   y -> f [y];
> });

Same as above, but does not effect outer level.

> withInnerBoxes :: (PageObjects -> PageObjects) -> PageObjects
>  -> PageObjects;
> withInnerBoxes f = (>>= \x -> case x of {
>   (c, Box y) -> [(c, Box $ withBoxes f y)];
>   (c, Special s y) -> [(c, Special s $ withBoxes f y)];
>   y -> [y];
> });

All objects on the page shifted by a displacement horizontal and vertical.

> shiftPage :: Coordinates -> PageObjects -> PageObjects;
> shiftPage (dx, dy) = map $ \((x, y), z) -> ((x + dx, y + dy), z);

> unshiftPage :: Coordinates -> PageObjects -> PageObjects;
> unshiftPage (dx, dy) = shiftPage (-dx, -dy);

You can sort all objects on the page into the order, top to bottom, and in
left to right order for each line.

> sortPage :: PageObjects -> PageObjects;
> sortPage = withInnerBoxes sortPage . (sortBy $ (. fst)
>  . sortPageCoord . fst);

> sortPageCoord :: Coordinates -> Coordinates -> Ordering;
> sortPageCoord (x1, y1) (x2, y2) = compare y2 y1 |*| compare x1 x2;

You can unbox all the boxes on a page, moving everything to the outer
level.

> unboxPage :: PageObjects -> PageObjects;
> unboxPage = (>>= unboxPageObj);

> unboxPageObj :: (Coordinates, PageObject) -> PageObjects;
> unboxPageObj (c, Box p) = shiftPage c $ unboxPage p;
> unboxPageObj (c, Special s p) = [(c, Special s $ unboxPage p)];
> unboxPageObj p = [p];

Page objects can be measured by width, height, and depth. Boxes have the
smallest boundaries such that their contents will fit. Texts have width
equal to the total width of kerns and characters it contains, and
height/depth equal to the largest height/depth of any character it
contains. Rules always have depth zero.

> objWidth :: PageObject -> Dimen;
> objWidth (Special _ p) = objWidth $ Box p;
> objWidth (Box p) = maximum $ 0 : map (\((x, _), z) -> x + objWidth z) p;
> objWidth (Character f c) = let { ch = (fromEnum $ mod c 256) - (fromEnum
>  $ firstChar f); } in if (ch >= 0) && (ch < length (characters f)) then
>  charWidth (characters f !! ch) else 0;
> objWidth (Text f t) = sum $ either id (objWidth . Character f) <$> t;
> objWidth (Rule w _) = w;
> objWidth (DVI_Binary _) = 0;

> objHeight :: PageObject -> Dimen;
> objHeight (Special _ p) = objHeight $ Box p;
> objHeight (Box p) = maximum $ 0 : map (\((_, y), z) -> y + objHeight
>  z) p;
> objHeight (Character f c) = let { ch = (fromEnum $ mod c 256) -
>  (fromEnum $ firstChar f); } in if (ch >= 0) && (ch < length (characters
>  f)) then charHeight (characters f !! ch) else 0;
> objHeight (Text f t) = maximum $ 0 : (either (const 0) (objHeight .
>  Character f) <$> t);
> objHeight (Rule _ h) = h;
> objHeight (DVI_Binary _) = 0;

> objDepth :: PageObject -> Dimen;
> objDepth (Special _ p) = objDepth $ Box p;
> objDepth (Box p) = maximum $ 0 : map (\((_, y), z) -> objDepth z - y) p;
> objDepth (Character f c) = let { ch = (fromEnum $ mod c 256) - (fromEnum
>  $ firstChar f); } in if (ch >= 0) && (ch < length (characters f)) then
>  charDepth (characters f !! ch) else 0;
> objDepth (Text f t) = maximum $ 0 : (either (const 0) (objDepth .
>  Character f) <$> t);
> objDepth (Rule _ _) = 0;
> objDepth (DVI_Binary _) = 0;

Specials also store contents like a box, allowing you to specify if a
special requires ordering, meaning it affects things after it (such as
colors of text), or it can be a position only special (such as pasting a
picture); however some specials might be irrelevant of position or order
(such as portrait/landscape orientation, paper size).

> openSpecials :: (ByteString -> Bool) -> PageObjects -> PageObjects;
> openSpecials _ [] = [];
> openSpecials f ((c, Special s p) : y) = if f s
>  then ((c, Special s []) : openSpecials f (shiftPage c p ++ y))
>  else ((c, Special s $ openSpecials f p) : openSpecials f y);
> openSpecials f ((c, Box p) : y) = (c, Box $ openSpecials f p) : y;
> openSpecials f (x : y) = x : openSpecials f y;

> closeSpecials :: (ByteString -> Bool) -> PageObjects -> PageObjects;
> closeSpecials _ [] = [];
> closeSpecials f ((c, Special s p) : y) = if f s
>  then [(c, Special s . closeSpecials f $ p ++ unshiftPage c y)]
>  else ((c, Special s $ closeSpecials f p) : closeSpecials f y);
> closeSpecials f ((c, Box p) : y) = (c, Box $ closeSpecials f p) : y;
> closeSpecials f (x : y) = x : closeSpecials f y;

> splitSpecials :: (ByteString -> ByteString -> Bool) -> PageObjects
>  -> PageObjects;
> splitSpecials _ [] = [];
> splitSpecials f ((c, Special s p) : y) = let { (a, b) = break
>  (maybe False (f s) . maybeSpecial) p; } in (c, Special s a) : b ++ y;
> splitSpecials f ((c, Box p) : y) = (c, Box $ splitSpecials f p) : y;
> splitSpecials f (x : y) = x : splitSpecials f y;

> maybeSpecial :: (Coordinates, PageObject) -> Maybe ByteString;
> maybeSpecial (_, Special x _) = Just x;
> maybeSpecial _ = Nothing;

Another thing to do is replace all {\tt Text} objects with the characters.

> textToChar :: PageObjects -> PageObjects;
> textToChar = withBoxes (>>= \x -> case x of {
>   ((h, v), Text f t) -> textToChar' v h f t;
>   _ -> [x];
> });

> textToChar' :: Int32 -> Int32 -> Font -> TextString -> PageObjects;
> textToChar' _ _ _ [] = [];
> textToChar' v h f (Left x : t) = textToChar' v (h + x) f t;
> textToChar' v h f (Right x : t) = let { c = Character f (transEnum x); }
>  in (((h, v), c) : textToChar' v (h + objWidth c) f t);

\: Font Access. Just a few functions to access things in the font.

> getFontChar :: (Integral t, Bits t) => Font -> t -> Maybe FontChar;
> getFontChar f c = let { ch = (fromEnum $ c .&. 255) - (fromEnum $
>  firstChar f); } in (characters f !! ch) <$ guard (ch >= 0 &&
>  ch < length (characters f));

> findLK :: [LigKern] -> Word8 -> Maybe LigKern;
> findLK [] _ = Nothing;
> findLK (h:t) c = if lkChar h == c then Just h else findLK t c;

One more finds the widest character in a font. This can be used as a way
to ensure the maximum page width is not computed smaller than it actually
is.

> widestChar :: Font -> Int32;
> widestChar (Font { characters = c }) = maximum $ 0 : (charWidth <$> c);

\: Read DVI File. These codes read the DVI file. You must specify the font
resolving function, which reads a font name and returns the font. The
function {\tt withDVI} is exposed; the others are used internally.

> withDVI :: FilePath -> (Word32 -> ByteString -> IO Font) -> s
>  -> (s -> Page -> IO (Maybe (s, t))) -> IO [t];
> withDVI n r s f = withBinaryFile n ReadMode $ withDVI_start >=>
>  withDVI_page r s f;

At first it reads the heading, which specifies units and magnification.
The other document stat fields (other than fonts) are not used for reading
(they are used for writing), and the DVI comment is not used by this
program, and is skipped.

> withDVI_start :: Handle -> IO DocStat;
> withDVI_start h = ((\0xF702 a b -> docStat {
>   pageHandle = Just h,
>   magnification = b,
>   dviUnits = a
> }) <$> (readData h :: IO Word16) <*> (readData h :: IO (Ratio Word32))
>  <*> (readData h :: IO Word32)) <* (((readData h :: IO Word8) >>=
>  readDataL h . fromEnum) :: IO ByteString);

This function is called for every page.

> withDVI_page :: (Word32 -> ByteString -> IO Font) -> s
>  -> (s -> Page -> IO (Maybe (s, t))) -> DocStat -> IO [t];
> withDVI_page r s f d@(DocStat { pageHandle = Just h }) = (readData h ::
>  IO Word8) >>= withDVI_page_ r s f d;

When reading between pages, the only codes expected are {\it nop}, {\it
post}, and {\it bop}. The postamble is not used by this program.

> withDVI_page_ :: (Word32 -> ByteString -> IO Font) -> s
>  -> (s -> Page -> IO (Maybe (s, t))) -> DocStat -> Word8 -> IO [t];
> withDVI_page_ r s f d 138 = withDVI_page r s f d;
> withDVI_page_ _ _ _ _ 248 = return [];
> withDVI_page_ r s f d 139 = (readPage r d >>= \x -> (,) (pageStat x)
>  <$> f s x) >>= \x -> case x of {
>   (_, Nothing) -> return [];
>   (d', Just (s', t')) -> (t' :) <$> withDVI_page r s' f d';
> };

> readPage :: (Word32 -> ByteString -> IO Font) -> DocStat -> IO Page;
> readPage r d@(DocStat { pageHandle = Just h }) = flip (uncurry $ flip .
>  flip Page) <$> (readData h <* (readData h :: IO Word32)) <*>
>  fmap (\(x, y, z) -> (x, y)) (readObjects (emptyFont, 0, 0, 0, 0, 0,
>  0, d) r);

Because the fonts defined in a previous page must be kept later on, and
because when popping the coordinates from the stack, that the current font
is kept as it was at the end of that block instead of popping the font
from the stack.

> readObjects :: (Font, Int32, Int32, Int32, Int32, Int32, Int32, DocStat)
>  -> (Word32 -> ByteString -> IO Font) -> IO (PageObjects, DocStat, Font);
> readObjects a@(_, _, _, _, _, _, _, DocStat { pageHandle = Just ha }) r
>  = readData ha >>= readObj a r ha;

Many DVI commands read signed numbers. This allows such numbers to be
read.

> readSigned :: Handle -> Int -> IO Int32;
> readSigned h i = (\x -> let { y = shiftL 1 . pred $ 8 * i; } in x -
>  bool 0 (2 * y) (x >= y)) <$> readDataL h i;

This function {\tt readObj} is used to process individual DVI commands.

> readObj :: (Font, Int32, Int32, Int32, Int32, Int32, Int32, DocStat)
>  -> (Word32 -> ByteString -> IO Font) -> Handle -> Word8
>  -> IO (PageObjects, DocStat, Font);
> readObj a r ha 128 = readDataL ha 1 >>= readObj_set_char r a; -- set1
> readObj a r ha 129 = readDataL ha 2 >>= readObj_set_char r a; -- set2
> readObj a r ha 130 = readDataL ha 3 >>= readObj_set_char r a; -- set3
> readObj a r ha 131 = readDataL ha 4 >>= readObj_set_char r a; -- set4
> readObj a r ha 132 = join $ readObj_set_rule r a <$> readData ha <*>
>  readData ha; -- set_rule
> readObj a r ha 133 = readDataL ha 1 >>= readObj_put_char r a; -- put1
> readObj a r ha 134 = readDataL ha 2 >>= readObj_put_char r a; -- put2
> readObj a r ha 135 = readDataL ha 3 >>= readObj_put_char r a; -- put3
> readObj a r ha 136 = readDataL ha 4 >>= readObj_put_char r a; -- put4
> readObj a r ha 137 = join $ readObj_put_rule r a <$> readData ha <*>
>  readData ha; -- put_rule
> readObj a r _ 138 = readObjects a r; -- nop
> readObj _ _ _ 139 = error "Unexpected bop"; -- bop
> readObj a@(f, h, v, w, x, y, z, d) r _ 140 = return ([], d, f); -- eop
> readObj a@(f, h, v, w, x, y, z, d) r _ 141 = readObjects (f, 0, 0, w, x,
>  y, z, d) r >>= (\(p, d', f') -> sendReadObj (f', h, v, w, x, y, z, d')
>  ((h, -v), Box p) r); -- push
> readObj a@(f, h, v, w, x, y, z, d) _ r 142 = return ([], d, f); -- pop
> readObj a r ha 143 = readSigned ha 1 >>= readObj_right r a; -- right1
> readObj a r ha 144 = readSigned ha 2 >>= readObj_right r a; -- right2
> readObj a r ha 145 = readSigned ha 3 >>= readObj_right r a; -- right3
> readObj a r ha 146 = readSigned ha 4 >>= readObj_right r a; -- right4
> readObj a@(f, h, v, w, x, y, z, d) r ha 147 = readObj_right r a w; -- w0
> readObj a r ha 148 = readSigned ha 1 >>= readObj_w r a; -- w1
> readObj a r ha 149 = readSigned ha 2 >>= readObj_w r a; -- w2
> readObj a r ha 150 = readSigned ha 3 >>= readObj_w r a; -- w3
> readObj a r ha 151 = readSigned ha 4 >>= readObj_w r a; -- w4
> readObj a@(f, h, v, w, x, y, z, d) r ha 152 = readObj_right r a x; -- x0
> readObj a r ha 153 = readSigned ha 1 >>= readObj_x r a; -- x1
> readObj a r ha 154 = readSigned ha 2 >>= readObj_x r a; -- x2
> readObj a r ha 155 = readSigned ha 3 >>= readObj_x r a; -- x3
> readObj a r ha 156 = readSigned ha 4 >>= readObj_x r a; -- x4
> readObj a r ha 157 = readSigned ha 1 >>= readObj_down r a; -- down1
> readObj a r ha 158 = readSigned ha 2 >>= readObj_down r a; -- down2
> readObj a r ha 159 = readSigned ha 3 >>= readObj_down r a; -- down3
> readObj a r ha 160 = readSigned ha 4 >>= readObj_down r a; -- down4
> readObj a@(f, h, v, w, x, y, z, d) r ha 161 = readObj_down r a y; -- y0
> readObj a r ha 162 = readSigned ha 1 >>= readObj_y r a; -- y1
> readObj a r ha 163 = readSigned ha 2 >>= readObj_y r a; -- y2
> readObj a r ha 164 = readSigned ha 3 >>= readObj_y r a; -- y3
> readObj a r ha 165 = readSigned ha 4 >>= readObj_y r a; -- y4
> readObj a@(f, h, v, w, x, y, z, d) r ha 166 = readObj_down r a z; -- z0
> readObj a r ha 167 = readSigned ha 1 >>= readObj_z r a; -- z1
> readObj a r ha 168 = readSigned ha 2 >>= readObj_z r a; -- z2
> readObj a r ha 169 = readSigned ha 3 >>= readObj_z r a; -- z3
> readObj a r ha 170 = readSigned ha 4 >>= readObj_z r a; -- z4
> readObj a r ha 235 = readDataL ha 1 >>= selectFont a r; -- fnt1
> readObj a r ha 236 = readDataL ha 2 >>= selectFont a r; -- fnt2
> readObj a r ha 237 = readDataL ha 3 >>= selectFont a r; -- fnt3
> readObj a r ha 238 = readDataL ha 4 >>= selectFont a r; -- fnt4
> readObj a r ha 239 = readDataL ha 1 >>= readObj_xxx r a; -- xxx1
> readObj a r ha 240 = readDataL ha 2 >>= readObj_xxx r a; -- xxx2
> readObj a r ha 241 = readDataL ha 3 >>= readObj_xxx r a; -- xxx3
> readObj a r ha 242 = readDataL ha 4 >>= readObj_xxx r a; -- xxx4
> readObj a r ha 243 = readDataL ha 1 >>= readObj_fntdef r a; -- fnt_def1
> readObj a r ha 244 = readDataL ha 2 >>= readObj_fntdef r a; -- fnt_def2
> readObj a r ha 245 = readDataL ha 3 >>= readObj_fntdef r a; -- fnt_def3
> readObj a r ha 246 = readDataL ha 4 >>= readObj_fntdef r a; -- fnt_def4
> readObj _ _ _ 247 = error "Unexpected preamble"; -- pre
> readObj _ _ _ 248 = error "Unexpected postamble"; -- post
> readObj _ _ _ 249 = error "Unexpected postpostamble"; -- post_post
> readObj a r ha ch | ch < 128 = readObj_set_char r a $ transEnum ch;
>  -- set_char_0
> readObj a r ha f' | f' < 235 = selectFont a r (transEnum $ f' - 171);
>  -- fnt_num_0
> readObj _ _ _ ch = error "Invalid DVI command"; -- Undefined commands

> selectFont :: (Font, Int32, Int32, Int32, Int32, Int32, Int32, DocStat)
>  -> (Word32 -> ByteString -> IO Font) -> Word32
>  -> IO (PageObjects, DocStat, Font);
> selectFont (f, h, v, w, x, y, z, d) r f' = readObjects (fonts d !!
>  fromEnum f', h, v, w, x, y, z, d) r;

> sendReadObj :: (Font, Int32, Int32, Int32, Int32, Int32, Int32, DocStat)
>  -> (Coordinates, PageObject) -> (Word32 -> ByteString -> IO Font)
>  -> IO (PageObjects, DocStat, Font);
> sendReadObj a p r = (\(k, d', f') -> (p : k, d', f')) <$>
>  readObjects a r;

> readObj_set_char :: (Word32 -> ByteString -> IO Font) -> (Font, Int32,
>  Int32, Int32, Int32, Int32, Int32, DocStat) -> Word32
>  -> IO (PageObjects, DocStat, Font);
> readObj_set_char r a@(f, h, v, w, x, y, z, d) ch = sendReadObj (f, h +
>  objWidth (Character f ch), v, w, x, y, z, d) ((h, -v), Character f ch)
>  r;

The following one is the same as above except that it doesn't move the
cursor after typing a character.

> readObj_put_char :: (Word32 -> ByteString -> IO Font) -> (Font, Int32,
>  Int32, Int32, Int32, Int32, Int32, DocStat) -> Word32
>  -> IO (PageObjects, DocStat, Font);
> readObj_put_char r a@(f, h, v, w, x, y, z, d) ch = sendReadObj a
>  ((h, -v), Character f ch) r;

> readObj_set_rule :: (Word32 -> ByteString -> IO Font) -> (Font, Int32,
>  Int32, Int32, Int32, Int32, Int32, DocStat) -> Int32 -> Int32
>  -> IO (PageObjects, DocStat, Font);
> readObj_set_rule r a@(f, h, v, w, x, y, z, d) ra rb = sendReadObj (f,
>  h + rb, v, w, x, y, z, d) ((h, -v), Rule rb ra) r;

> readObj_put_rule :: (Word32 -> ByteString -> IO Font) -> (Font, Int32,
>  Int32, Int32, Int32, Int32, Int32, DocStat) -> Int32 -> Int32 
>  -> IO (PageObjects, DocStat, Font);
> readObj_put_rule r a@(f, h, v, w, x, y, z, d) ra rb = sendReadObj a
>  ((h, -v), Rule rb ra) r;

> readObj_right :: (Word32 -> ByteString -> IO Font) -> (Font, Int32,
>  Int32, Int32, Int32, Int32, Int32, DocStat) -> Int32
>  -> IO (PageObjects, DocStat, Font);
> readObj_right r a@(f, h, v, w, x, y, z, d) b = readObjects (f, h + b, v,
>  w, x, y, z, d) r;

> readObj_down :: (Word32 -> ByteString -> IO Font) -> (Font, Int32,
>  Int32, Int32, Int32, Int32, Int32, DocStat) -> Int32
>  -> IO (PageObjects, DocStat, Font);
> readObj_down r a@(f, h, v, w, x, y, z, d) b = readObjects (f, h, v + b,
>  w, x, y, z, d) r;

> readObj_w :: (Word32 -> ByteString -> IO Font) -> (Font, Int32, Int32,
>  Int32, Int32, Int32, Int32, DocStat) -> Int32
>  -> IO (PageObjects, DocStat, Font);
> readObj_w r a@(f, h, v, w, x, y, z, d) w' = readObjects (f, h + w', v,
>  w', x, y, z, d) r;

> readObj_x :: (Word32 -> ByteString -> IO Font) -> (Font, Int32, Int32,
>  Int32, Int32, Int32, Int32, DocStat) -> Int32
>  -> IO (PageObjects, DocStat, Font);
> readObj_x r a@(f, h, v, w, x, y, z, d) x' = readObjects (f, h + x', v,
>  w, x', y, z, d) r;

> readObj_y :: (Word32 -> ByteString -> IO Font) -> (Font, Int32, Int32,
>  Int32, Int32, Int32, Int32, DocStat) -> Int32
>  -> IO (PageObjects, DocStat, Font);
> readObj_y r a@(f, h, v, w, x, y, z, d) y' = readObjects (f, h, v + y',
>  w, x, y', z, d) r;

> readObj_z :: (Word32 -> ByteString -> IO Font) -> (Font, Int32, Int32,
>  Int32, Int32, Int32, Int32, DocStat) -> Int32
>  -> IO (PageObjects, DocStat, Font);
> readObj_z r a@(f, h, v, w, x, y, z, d) z' = readObjects (f, h, v + z',
>  w, x, y, z', d) r;

> readObj_xxx :: (Word32 -> ByteString -> IO Font) -> (Font, Int32, Int32,
>  Int32, Int32, Int32, Int32, DocStat) -> Word32
>  -> IO (PageObjects, DocStat, Font);
> readObj_xxx r a@(f, h, v, w, x, y, z, d@DocStat { pageHandle = Just ha
>  }) l = readDataL ha (fromEnum l) >>= \s -> sendReadObj a ((h, -v),
>  Special s []) r;

This one is used for loading fonts. The {\tt r} parameter is used here.

> readObj_fntdef :: (Word32 -> ByteString -> IO Font) -> (Font, Int32,
>  Int32, Int32, Int32, Int32, Int32, DocStat) -> Word32
>  -> IO (PageObjects, DocStat, Font);
> readObj_fntdef r a@(f, h, v, w, x, y, z, d@DocStat { pageHandle = Just
>  ha }) n = join $ readObj_fntdef_ r a n <$> ((readData ha :: IO Word32)
>  *> readData ha <* (readData ha :: IO Word32)) <*> (((+) <$> readData ha
>  <*> readData ha :: IO Word8) >>= readDataL ha . fromEnum);

> readObj_fntdef_ :: (Word32 -> ByteString -> IO Font) -> (Font, Int32,
>  Int32, Int32, Int32, Int32, Int32, DocStat) -> Word32 -> Word32
>  -> ByteString -> IO (PageObjects, DocStat, Font);
> readObj_fntdef_ r a@(f, h, v, w, x, y, z, d@DocStat { fonts = fo }) n sc
>  na = r sc na >>= \f' -> readObjects (f, h, v, w, x, y, z, d { fonts =
>  addFont (fromEnum n) fo f'}) r;

> addFont :: Int -> [Font] -> Font -> [Font];
> addFont i h n = (take i $ h ++ repeat emptyFont) ++ [n] ++ drop
>  (succ i) h;

\: Write DVI File. These are the functions for writing a DVI file. First
you call {\tt createDVI}, and then you use the resulting {\tt DocStat} to
create a page and ship it out by {\tt shipOut}; the result is then used as
the {\tt DocStat} for the next page being shipped out, or for {\tt
finishDVI} if the document is finished.

First the function to begin a DVI file for writing.

> createDVI :: FilePath -> Word32 -> Ratio Word32 -> IO DocStat;
> createDVI n m u = (\h -> docStat { pageHandle = Just h, magnification
>  = m, dviUnits = u }) <$> (openBinaryFile n WriteMode <>>= flip hPutStr
>  "\247\2" <>>= flip writeData u <>>= flip writeData m <>>= flip hPutStr
>  "\7Haskell");

Next the function to ship out a page.

> shipOut :: Page -> IO DocStat;
> shipOut p@(Page { pageNumbers = n, pageObjects = o, pageStat =
>  ds@DocStat { pageHandle = Just h } }) = (\pr (f', ta, wi, sd)
>  -> ds {
>    prevPage = Just $ transInt pr,
>    fonts = f',
>    tallest = max ta $ tallest ds,
>    widest = max wi $ widest ds,
>    maxStackDepth = max sd $ maxStackDepth ds,
>    numPages = succ $ numPages ds
>  }) <$> (hTell h <* writeData h (139 :: Word8) <* writeData h n <*
>  writeData h (maybe (-1) id $ prevPage ds)) <*> (shipOut2
>  (pageToCommands (fonts ds) (0, 0) . textToChar . openSpecials (const
>  True) $ sortPage o) h (fonts ds) <* writeData h (140 :: Word8));

Finally, the function to call when it is finished.

> finishDVI :: DocStat -> IO ();
> finishDVI ds = let { Just h = pageHandle ds; Just p = prevPage ds; } in
>  (hTell h <* writeData h (248 :: Word8) <* writeData h p <* writeData h
>  (dviUnits ds) <* writeData h (magnification ds) <* writeData h (tallest
>  ds) <* writeData h (widest ds) <* writeData h (maxStackDepth ds) <*
>  writeData h (numPages ds) <* sequence_ (writeFont h <$> zip [0..]
>  (fonts ds)) <* writeData h (249 :: Word8)) >>= writeData h . (transInt
>  :: Integer -> Word32) >> writeData h (fromStr "\2\223\223\223\223")
>  >> hClose h;

To write font information to DVI file:

> writeFont :: Handle -> (Int, Font) -> IO ();
> writeFont h (n, f) = case n of {
>   _ | n < 0x100 -> writeData h (243 :: Word8) >> writeData h (transInt
>    n :: Word8);
>   _ | n < 0x10000 -> writeData h (244 :: Word8) >> writeData h (transInt
>    n :: Word16);
>   _ -> writeData h (246 :: Word8) >> writeData h (transInt n :: Word32);
> } >> writeData h (fontChecksum f) >> writeData h (atSize f) >>
>  writeData h (designSize f) >> writeData h ((transInt . B.length $
>  fontName f) :: Word16) >> writeData h (fontName f);

First you have to convert the page into the commands. Here are datatypes
for making the list of page commands. (For the {\tt CharC} constructor,
the first parameter is the character code, the second is the font number,
and the third is {\tt True} if it moves.)

> data PageCommand = RightC Int32 MoveReg | DownC Int32 MoveReg
>  | BoxC [PageCommand] | CharC Word32 Int Bool | RuleC Int32 Int32 Bool
>  | SpecialC ByteString | DVI_BinaryC ByteString  deriving (Eq, Show);

> data MoveReg = RegNone | RegW | RegX | RegY | RegZ  deriving (Eq, Show);

To convert page objects to page commands (also numbering the fonts):

> pageToCommands :: [Font] -> Coordinates -> PageObjects
>  -> ([Font], [PageCommand]);
> pageToCommands f _ [] = (f, []);
> pageToCommands f (h, v) (((h', v'), o) : t) = let { x = objToCmd f o; }
>  in ([RightC (h' - h) RegNone, DownC (v - v') RegNone, snd x] ++) <$>
>  pageToCommands (fst x) (h', v') t;

Note, the {\tt Text} objects should have been removed by now. Optimization
is not done yet, but is done later.

> objToCmd :: [Font] -> PageObject -> ([Font], PageCommand);
> objToCmd f (Special s []) = (f, SpecialC s);
> objToCmd f (Box o) = BoxC <$> pageToCommands f (0, 0) o;
> objToCmd f (Character x c) = maybe (f ++ [x], CharC c (length f) False)
>  (\z -> (f, CharC c z False)) $ findIndex (fontEq x) f;
> objToCmd f (Rule x y) = (f, RuleC x y False);
> objToCmd f (DVI_Binary x) = (f, DVI_BinaryC x);

This is the main internal function for writing a page out. It optimizes
page objects, writes their commands out, updates the font list with any
new fonts in the page, and updates parameters for use in the postamble.

> shipOut2 :: ([Font], [PageCommand]) -> Handle
>  -> [Font] -> IO ([Font], Int32, Int32, Word16);
> shipOut2 (f', p) h f = sequence_ (writeFont h <$> drop (length f)
>  (zip [0..] f')) >> ((\(_, x, y, z) -> (f', x, y, z)) <$>
>  shipOutCmd (widestChar <$> f') (-1, 0, 0, 0, 0, 0, 0) (0, 0, 0, 1)
>  (applyMoveReg . optimizePage f' . optimizePage f' . optimizePage f' .
>  optimizePage f' . optimizePage f' . optimizePage f' . optimizePage f' .
>  optimizePage f' . optimizePage f' . optimizePage f' $ optimizePage f'
>  p) h);

Optimizations:

> optimizePage :: [Font] -> [PageCommand] -> [PageCommand];
> optimizePage _ [] = [];

Movements at the end of a box are irrelevant and can be removed.

> optimizePage _ [RightC _ _] = [];

> optimizePage _ [DownC _ _] = [];

Zero movements are irrelevant and can be removed.

> optimizePage f (RightC 0 _ : t) = optimizePage f t;

> optimizePage f (DownC 0 _ : t) = optimizePage f t;

Multiple movements can be combined together.

> optimizePage f (RightC x _ : RightC x' _ : t) = optimizePage f
>  (RightC (x + x') RegNone : t);

> optimizePage f (DownC x _ : DownC x' _ : t) = optimizePage f
>  (DownC (x + x') RegNone : t);

Horizontal and vertical movements should be reordered to allow more
optimization to be done.

> optimizePage f (DownC a b : RightC c d : t) = RightC c d :
>  optimizePage f (DownC a b : t);

A non-moving character followed by a movement of at least its width is
replaced by the moving character and modifies the movement.

Note that it sometimes must go forward and then partially backspace if
this way has smaller explicit movements, which makes the file smaller.

> optimizePage f (CharC c n False : RightC h _ : t) | h > shiftR
>  (objWidth $ Character (f !! n) c) 1 = CharC c n True : optimizePage f
>  (RightC (h - objWidth (Character (f !! n) c)) RegNone : t);

A moving character that is immediately backspaced is replaced by a non-%
moving character.

> optimizePage f (CharC c n True : RightC h _ : t) | (-h) == objWidth
>  (Character (f !! n) c) && h < 0 = CharC c n False : optimizePage f
>  (RightC 0 RegNone : t);

The same things applying to characters can also apply to rules.

> optimizePage f (RuleC x y False : RightC h _ : t) | h > shiftR x 1 =
>  RuleC x y True : optimizePage f (RightC (h - x) RegNone : t);

> optimizePage f (RuleC x y True : RightC h _ : t) | h == (-x)
>  && h < 0 = RuleC x y False : optimizePage f (RightC 0 RegNone : t);

Boxes containing non-moving commands can be unboxed at that point.

> optimizePage f (BoxC (x@(CharC _ _ False) : b) : t) = x : optimizePage f
>  (BoxC b : t);

> optimizePage f (BoxC (x@(RuleC _ _ False) : b) : t) = x : optimizePage f
>  (BoxC b : t);

Empty boxes can be eliminated.

> optimizePage f (BoxC [] : t) = optimizePage f t;

Boxes can be optimized inside of the boxes too.

> optimizePage f (BoxC b : t) = BoxC (optimizePage f b) : optimizePage f t;

The final rule in a box can be made non-moving to allow further
optimizations to be performed.

> optimizePage f [RuleC x y True] = [RuleC x y False];

The final character in a box will be made moving if its code is less than
128 because it saves disk space.

> optimizePage f [CharC c n False] | c < 128 = [CharC c n True];

Other things remain as they are.

> optimizePage f (h : t) = h : optimizePage f t;

Next is to apply movement registers to the movement commands.
% [TODO: This part is not yet properly implemented!]

> applyMoveReg :: [PageCommand] -> [PageCommand];
> applyMoveReg [] = [];
> applyMoveReg (BoxC b : t) = BoxC (applyMoveReg b) : applyMoveReg t;
> applyMoveReg (h : t) = h : applyMoveReg t;

Finally is shipping out each command. But that requires to write commands
with the proper number of bytes.

> writeUnsigned :: Handle -> Word8 -> Word32 -> IO ();
> writeUnsigned h b v = case v of {
>   _ | v < 0x100 -> writeData h b >> writeData h (low8bits v);
>   _ | v < 0x10000 -> writeData h (b + 1) >>
>    writeData h (transInt v :: Word16);
>   _ | v < 0x1000000 -> writeData h (b + 2) >>
>    writeData h (low8bits $ shiftR v 16) >>
>    writeData h (transInt (v .&. 0xFFFF) :: Word16);
>   _ -> writeData h (b + 3) >> writeData h v;
> };

> writeSigned :: Handle -> Word8 -> Int32 -> IO ();
> writeSigned h b v = case v of {
>   _ | v < 0x80 && v >= (-0x80) -> writeData h b >>
>    writeData h (low8bits v);
>   _ | v < 0x8000 && v >= (-0x8000) -> writeData h (b + 1) >>
>    writeData h (transInt (v .&. 0xFFFF) :: Word16);
>   _ | v < 0x800000 && v >= (-0x800000) -> writeData h (b + 2) >>
>    writeData h (low8bits $ shiftR v 16) >>
>    writeData h (transInt (v .&. 0xFFFF) :: Word16);
>   _ -> writeData h (b + 3) >> writeData h v;
> };

Note, fonts do not use the stack. Therefore, it must keep the current font
separately from the parameters.

> shipOutCmd :: [Int32] -> (Int, Int32, Int32, Int32, Int32, Int32, Int32)
>  -> (Int32, Int32, Word16, Word16) -> [PageCommand] -> Handle
>  -> IO (Int, Int32, Int32, Word16);

> shipOutCmd _ (f, h, v, _, _, _, _) (ta, wi, sd, ms) [] _ = return
>  (f, max ta $ abs v, max wi $ abs h, max ms sd);

> shipOutCmd wc (f, h, v, w, x, y, z) (ta, wi, sd, ms) (RightC m r : t) ha
>  =
>  (if m == w then writeData ha (147 :: Word8) else
>  if m == x then writeData ha (152 :: Word8) else
>  writeSigned ha (regNumRight r) m) >> shipOutCmd wc (f, h + m, v,
>  if r == RegW then m else w, if r == RegX then m else x, y, z)
>  (ta, max wi $ h + m, sd, ms) t ha;

> shipOutCmd wc (f, h, v, w, x, y, z) (ta, wi, sd, ms) (DownC m r : t) ha
>  =
>  (if m == y then writeData ha (161 :: Word8) else
>  if m == z then writeData ha (166 :: Word8) else
>  writeSigned ha (regNumDown r) m) >> shipOutCmd wc (f, h, v + m, w, x,
>  if r == RegY then m else y, if r == RegZ then m else z)
>  (max ta $ v + m, wi, sd, ms) t ha;

> shipOutCmd wc (f, h, v, w, x, y, z) (ta, wi, sd, ms) (BoxC b : t) ha =
>  writeData ha (141 :: Word8) >> (shipOutCmd wc (f, 0, 0, w, x, y, z)
>  (0, 0, succ sd, ms) b ha <* writeData ha (142 :: Word8)) >>=
>  \(f', ta', wi', ms') -> shipOutCmd wc (f', h, v, w, x, y, z) (max ta $
>  ta' + v, max wi $ wi' + h, sd, max ms ms') t ha;

> shipOutCmd wc (f, h, v, w, x, y, z) (ta, wi, sd, ms) (CharC c n m : t)
>  ha =
>  when (f /= n) (if n < 64 then writeData ha (low8bits n + 171)
>  else writeUnsigned ha 235 (toEnum n)) >>
>  (if (c < 128) && m then writeData ha (low8bits c) else writeUnsigned ha
>  (if m then 128 else 133) c) >> shipOutCmd wc (n, h + (if m then
>  (wc !! n) else 0), v, w, x, y, z) (ta, max wi $ h + (wc !! n), sd, ms)
>  t ha;

> shipOutCmd wc (f, h, v, w, x, y, z) (ta, wi, sd, ms) (RuleC j k m : t)
>  ha =
>  writeData ha (if m then 132 else 137 :: Word8) >> writeData ha k >>
>  writeData ha j >> shipOutCmd wc (f, h + (if m then j else 0), v, w, x,
>  y, z) (max ta k, max wi $ w + j, sd, ms) t ha;

> shipOutCmd wc r q (SpecialC s : t) ha = writeUnsigned ha 239 (transInt $
>  B.length s) >> writeData ha s >> shipOutCmd wc r q t ha;

> shipOutCmd wc r q (DVI_BinaryC s : t) ha = writeData ha s >>
>  shipOutCmd wc r q t ha;

Functions to specify command numbers for register movements:

> regNumRight :: MoveReg -> Word8;
> regNumRight RegNone = 143;
> regNumRight RegW = 148;
> regNumRight RegX = 153;

> regNumDown :: MoveReg -> Word8;
> regNumDown RegNone = 157;
> regNumDown RegY = 162;
> regNumDown RegZ = 167;

\: Typesetting. This first function is used for typesetting a string. It
doesn't work with spaces; you must use only a single word. Ligatures and
kerning will be applied.

> typesetSimpleString :: Font -> String -> PageObject;
> typesetSimpleString f s = Text f . doBackspaces . typesetSimpleChar f s
>  $ leftBoundChar f;

> doBackspaces :: [Maybe x] -> [x];
> doBackspaces [] = [];
> doBackspaces (Just x : Nothing : t) = doBackspaces t;
> doBackspaces (Just x : t) = x : doBackspaces t;
> doBackspaces (Nothing : t) = doBackspaces t;

> typesetSimpleChar :: Font -> String -> [LigKern]
>  -> [Maybe (Either Int32 Word32)];
> typesetSimpleChar _ [] [] = [];
> typesetSimpleChar f s lkl = o ++ typesetSimpleChar f (ns ++ drop 1 s) k
>  where {
>   lk = case s of {
>     [] -> rightBoundChar f;
>     (x:_) -> Just $ transEnum x;
>   } >>= findLK lkl;
>   (o, ns, k) = case lk of {
>     Just (Kerning { kernDist = x }) -> ((Just $ Left x) : g, [], nk);
>     Just (Ligature { ligChar = ch, ligCode = op }) -> case op of {
>       0 -> ([Nothing, rep ch], [], pk ch); --  =:
>       1 -> ([Nothing, rep ch], take 1 s, pk ch); --  =:|
>       2 -> ([], [transEnum ch], lkl); --  |=:
>       3 -> ([], transEnum ch : take 1 s, lkl); --  |=:|
>       5 -> (Nothing : rep ch : g, [], nk); --  =:|>
>       6 -> ([rep ch], [], pk ch); --  |=:>
>       7 -> ([rep ch], take 1 s, pk ch); --  |=:|>
>       11 -> (rep ch : g, [], nk); --  |=:|>>
>       _ -> error $ "Unexpected ligature type: " ++ show op;
>     };
>     _ -> (g, [], nk);
>   } :: ([Maybe (Either Dimen Word32)], String, [LigKern]);
>   nk = case s of {
>     [] -> [];
>     (x:_) -> pk x;
>   };
>   rep = Just . Right . transEnum :: Word8 -> Maybe (Either Dimen Word32);
>   pk :: Enum x => x -> [LigKern];
>   pk x = maybe [] charLigKern $ getFontChar f (transEnum x :: Int);
>   g = (Just . Right . transEnum <$> take 1 s)
>    :: [Maybe (Either Dimen Word32)];
> };

\: Glue. Glue setting is defined by using these datatypes. They in general
mean the same thing as they do in \TeX.

> data Glue = Glue {
>   naturalGlue :: Dimen,
>   stretchability :: GlueSS,
>   shrinkability :: GlueSS
> } deriving (Eq, Show);

> data GlueSS = GlueSS {
>   finiteSS :: Dimen,
>   filSS :: Dimen,
>   fillSS :: Dimen,
>   filllSS :: Dimen
> } deriving (Eq, Show);

> nullGlueSS :: GlueSS;
> nullGlueSS = GlueSS 0 0 0 0;

> fixedGlue :: Dimen -> Glue;
> fixedGlue x = Glue x nullGlueSS nullGlueSS;

> data GlueRank = Finite | Fil | Fill | Filll
>  deriving (Bounded, Eq, Enum, Ord, Show);

> data GlueSet = GlueSet (Ratio Dimen) GlueRank GlueSign
>  deriving (Eq, Show);

> data GlueSign = Shrinking | Stretching  deriving (Eq, Show);

There is a custom instance for {\tt Enum GlueSign} so that it can result
negative or positive.

> instance Enum GlueSign where {
>   fromEnum Stretching = 1;
>   fromEnum Shrinking = -1;
>   toEnum x = bool Shrinking Stretching (x > 0);
>   enumFrom Shrinking = [Shrinking, Stretching];
>   enumFrom Stretching = [Stretching];
>   pred Stretching = Shrinking;
>   succ Shrinking = Stretching;
> };

To make a glue from a space factor:

> spaceGlue :: Font -> Int -> Glue;
> spaceGlue f sf = let { d = fontDimen f ++ repeat 0; } in Glue {
>   naturalGlue = (d !! 2) + bool 0 (d !! 7) (sf >= 2000), 
>   stretchability = GlueSS (div (toEnum sf * (d !! 3)) 1000) 0 0 0,
>   shrinkability = GlueSS (div (1000 * (d !! 4)) $ toEnum sf) 0 0 0
> };

To use glue settings, there will be a type for rendering positions, and
the functions to convert them.

> type RenderPos = Double;

> toRenderPos :: Real x => x -> RenderPos;
> toRenderPos = fromRational . toRational;

To calculate the distance from a glue setting:

> selectGlueSS :: GlueRank -> GlueSS -> Dimen;
> selectGlueSS Finite (GlueSS x _ _ _) = x;
> selectGlueSS Fil (GlueSS _ x _ _) = x;
> selectGlueSS Fill (GlueSS _ _ x _) = x;
> selectGlueSS Filll (GlueSS _ _ _ x) = x;

> calcGlue :: GlueSet -> Glue -> RenderPos;
> calcGlue (GlueSet r f Shrinking) (Glue n _ s) =
>  toRenderPos n - toRenderPos r * toRenderPos (selectGlueSS f s);
> calcGlue (GlueSet r f Stretching) (Glue n s _) =
>  toRenderPos n + toRenderPos r * toRenderPos (selectGlueSS f s);

To add/subtract glue:

> addGlue :: Glue -> Glue -> Glue;
> addGlue (Glue a (GlueSS b0 b1 b2 b3) (GlueSS c0 c1 c2 c3))
>  (Glue x (GlueSS y0 y1 y2 y3) (GlueSS z0 z1 z2 z3)) = Glue (a + x)
>  (GlueSS (b0 + y0) (b1 + y1) (b2 + y2) (b3 + y3))
>  (GlueSS (c0 + z0) (c1 + z1) (c2 + z2) (c3 + z3));

> subtractGlue :: Glue -> Glue -> Glue;
> subtractGlue (Glue a (GlueSS b0 b1 b2 b3) (GlueSS c0 c1 c2 c3))
>  (Glue x (GlueSS y0 y1 y2 y3) (GlueSS z0 z1 z2 z3)) = Glue (a - x)
>  (GlueSS (b0 - y0) (b1 - y1) (b2 - y2) (b3 - y3))
>  (GlueSS (c0 - z0) (c1 - z1) (c2 - z2) (c3 - z3));

To calculate glue setting from glue value:

> calcGlueSet :: Dimen -> Glue -> GlueSet;
> calcGlueSet x (Glue n _ _) | x == n = GlueSet 0 Finite Stretching;
> calcGlueSet x (Glue n s _) | x > n && s == nullGlueSS =
>  GlueSet 0 Filll Stretching;
> calcGlueSet x (Glue n s _) | x > n = let { (a, b) = glueMax s; } in
>  GlueSet ((x - n) % a) b Stretching;
> calcGlueSet x (Glue n _ s) | x < n && s == nullGlueSS =
>  GlueSet 0 Filll Shrinking;
> calcGlueSet x (Glue n _ s) | x < n = let { (a, b) = glueMax s; } in
>  GlueSet (bool id (min 1) (b == Finite) $ (n - x) % a) b Shrinking;

> glueMax :: GlueSS -> (Dimen, GlueRank);
> glueMax (GlueSS x 0 0 0) = (x, Finite);
> glueMax (GlueSS _ x 0 0) = (x, Fil);
> glueMax (GlueSS _ _ x 0) = (x, Fill);
> glueMax (GlueSS _ _ _ x) = (x, Filll);

And the way to calculate the badness. The maximum badness will be one
million.

> badness :: Dimen -> Glue -> Ratio Integer;
> badness b (Glue n (GlueSS 0 0 0 0) _) | b > n = 1000000;
> badness b (Glue n _ (GlueSS 0 0 0 0)) | b < n = 1000000;
> badness b (Glue n (GlueSS s 0 0 0) _) | b > n =
>  min 999999 (toRational ((b - n) % s) ^ 3);
> badness b (Glue n _ (GlueSS s 0 0 0)) | b < n =
>  let { x = (toRational  ((n - b) % s) ^ 3); } in bool x 1000000 (x > 1);
> badness _ _ = 0;

\: Typesetting Nodes. To make a part of a document for typesetting, it
will use a node list (similar to \TeX). Each node has a code associated
with it.

> data Node where {
>   Node :: forall x. NodeClass x => x -> Node;
> } deriving Typeable;

> class Typeable x => NodeClass x where {
>   showNode :: x -> String;
>   hRender :: x -> Node -> RenderPos -> (PageObjects, RenderPos);
>   vRender :: x -> Node -> RenderPos -> (PageObjects, RenderPos);
>   hPackNode :: x -> ([Node], Glue, Dimen, Dimen, [Node]);
>   vPackNode :: x -> ([Node], Glue, Dimen, Dimen);
>   nodePenalty :: x -> Maybe Int;
>   nodeWidth :: x -> Dimen;
>   nodeHeight :: x -> Dimen;
>   nodeDepth :: x -> Dimen;
>   nodeGlueSet :: x -> GlueSet;
>   isNodeDiscardable :: x -> Bool;
>   traverseBox :: (Applicative f, Monad f) => (Node -> f Node)
>    -> x -> f Node;
>
>   showNode _ = "Node;";
>   hRender x _ y = ([], y + toRenderPos (nodeWidth x));
>   vRender x _ y = ([], y + toRenderPos (nodeHeight x + nodeDepth x));
>   hPackNode x = ([Node x], fixedGlue (nodeWidth x), nodeHeight x,
>    nodeDepth x, []);
>   vPackNode x = ([Node x], fixedGlue (nodeHeight x), nodeDepth x,
>    nodeWidth x);
>   nodePenalty _ = Nothing;
>   nodeWidth _ = 0;
>   nodeHeight _ = 0;
>   nodeDepth _ = 0;
>   nodeGlueSet _ = GlueSet 0 Finite Stretching;
>   isNodeDiscardable _ = False;
>   traverseBox f = f . wrapNode;
> };

Here is explanation of class methods.

{\tt showNode}: Convert to a string representation.

{\tt hRender}: Using the containing box and the position within the box,
render this node as a part of a horizontal box, and then return the next
position as well.

{\tt vRender}: As above, but for vertical boxes instead. The position will
be positive for down, but the page object coordinates will be positive for
up, so you must place the object in the other direction.

{\tt hPackNode}: Package a single node in a horizontal box. The first
output is a list of nodes to add (in case they are modified), second is
the width, and then height, depth, and the adjustment nodes to add to the
vertical box it is contained in, if any.

{\tt vPackNode}: As above, but after the list of nodes the outputs are the
height, depth, and width. There are no adjustment nodes.

{\tt nodePenalty}: Access a penalty value associated with a node. (Nothing
if it is not breakable.)

{\tt nodeWidth}: Width of a node.

{\tt nodeHeight}: Height of a node.

{\tt nodeDepth}: Depth of a node.

{\tt nodeGlueSet}: The glue setting of the node, used when a node is
being rendered inside of a box.

{\tt isNodeDiscardable}: Should be true if this node is discardable after
a line break or page break.

{\tt traverseBox}: Allows the contents of a node that can contain others
to be traversed and then the nodes it contains to be accessed and modified
by a monad.

> instance NodeClass Node where {
>   showNode (Node x) = showNode x;
>   hRender (Node x) = hRender x;
>   vRender (Node x) = vRender x;
>   hPackNode (Node x) = hPackNode x;
>   vPackNode (Node x) = vPackNode x;
>   nodePenalty (Node x) = nodePenalty x;
>   nodeWidth (Node x) = nodeWidth x;
>   nodeHeight (Node x) = nodeHeight x;
>   nodeDepth (Node x) = nodeDepth x;
>   nodeGlueSet (Node x) = nodeGlueSet x;
>   isNodeDiscardable (Node x) = isNodeDiscardable x;
>   traverseBox f (Node x) = traverseBox f x;
> };

Safe wrapping/casting of nodes:

> wrapNode :: NodeClass x => x -> Node;
> wrapNode x = maybe (Node x) id (cast x :: Maybe Node);

> castNode :: NodeClass x => Node -> Maybe x;
> castNode (Node x) = maybe (cast x) castNode (cast x);

Rendering nodes to page objects:

> renderNode :: NodeClass x => x -> PageObjects;
> renderNode n = fst $ vRender n (Node $ KernNode 0) 0;

Some of the kind of nodes are horizontal and vertical boxes.

> data BoxDirection = Horizontal | Vertical
>  deriving (Bounded, Eq, Enum, Show);

> data BoxNode = BoxNode BoxDirection Dimen Dimen Dimen GlueSet [Node]
>  deriving Typeable;

> instance NodeClass BoxNode where {
>   showNode (BoxNode dir w h d g n) = (case dir of
>    { Horizontal -> 'H'; Vertical -> 'V'; }) : "box " ++ show w ++ " "
>    ++ show h ++ " " ++ show d ++ " [" ++ (n >>= showNode) ++ "];";
>   hRender x@(BoxNode dir w h d g n) _ p =
>    ([((round p, 0), Box $ boxRender x h 0 dir n)], p + toRenderPos w);
>   vRender x@(BoxNode dir w h d g n) _ p =
>    ([((0, -round p), Box $ boxRender x h 0 dir n)],
>    p + toRenderPos (h + d));
>   nodeWidth (BoxNode _ x _ _ _ _) = x;
>   nodeHeight (BoxNode _ _ x _ _ _) = x;
>   nodeDepth (BoxNode _ _ _ x _ _) = x;
>   nodeGlueSet (BoxNode _ _ _ _ x _) = x;
>   traverseBox f (BoxNode z w h d g n) = (Node . BoxNode z w h d g <$>
>    sequenceA (traverseBox f <$> n)) >>= f;
> };

> boxRender :: BoxNode -> Dimen -> RenderPos -> BoxDirection -> [Node]
>  -> PageObjects;
> boxRender x z _ Vertical n | z /= 0 = boxRender x 0 (-toRenderPos z)
>  Vertical n;
> boxRender _ _ _ _ [] = [];
> boxRender x _ p d (h : t) = q' ++ boxRender x 0 p' d t where {
>   (q, p') = (case d of {
>     Horizontal -> hRender;
>     Vertical -> vRender;
>   }) h (Node x) p;
>   q' = bool id (shiftPage (0, -nodeHeight h)) (d == Vertical) q;
> };

Others is rule nodes. With rule nodes, the dimensions can be unset which
means to fill up the space of the box it is contained inside of.

> data RuleNode = RuleNode (Maybe Dimen) (Maybe Dimen) (Maybe Dimen)
>  deriving (Show, Typeable);

> instance NodeClass RuleNode where {
>   showNode (RuleNode w h d) = "Rule " ++ maybe "*" show w ++ " " ++
>    maybe "*" show h ++ " " ++ maybe "*" show d ++ ";";
>   hRender (RuleNode (Just w) hh dd) x y = let {
>    h = maybe (nodeHeight x) id hh; d = maybe (nodeDepth x) id dd; } in
>    ([((round y, -d), Rule w (h + d))], y + toRenderPos w);
>   hRender (RuleNode Nothing _ _) _ _ = error
>    "Rules in horizontal lists must have a width";
>   vRender (RuleNode ww (Just h) (Just d)) x y = let {
>    w = maybe (nodeWidth x) id ww; } in
>    ([((0, -(round y + h + d)), Rule w (h + d))],
>    y + toRenderPos (h + d));
>   vRender (RuleNode _ _ _) _ _ = error
>    "Rules in vertical lists must have a height and depth";
>   nodeWidth (RuleNode x _ _) = maybe 0 id x;
>   nodeHeight (RuleNode _ x _) = maybe 0 id x;
>   nodeDepth (RuleNode _ _ x) = maybe 0 id x;
> };

Now there is special nodes.

> data SpecialNode = SpecialNode ByteString deriving Typeable;

> instance NodeClass SpecialNode where {
>   showNode _ = "Special;";
>   hRender (SpecialNode x) _ y = ([((round y, 0), Special x [])], y);
>   vRender (SpecialNode x) _ y = ([((0, -round y), Special x [])], y);
> };

And the nodes shifting (vertical in horizontal lists, and horizontal in
vertical lists).

> data ShiftNode = ShiftNode Dimen Node deriving Typeable;

> instance NodeClass ShiftNode where {
>   showNode (ShiftNode x y) = "Shift " ++ show x ++
>    " [" ++ showNode y ++ "];";
>   hRender (ShiftNode a b) x y = first (map . first $ second (a +))
>    $ hRender b x y;
>   vRender (ShiftNode a b) x y = first (map . first $ first (a +))
>    $ hRender b x y;
>   hPackNode (ShiftNode x y) = let { (a, b, c, d, e) = hPackNode y } in
>    (Node . ShiftNode x <$> a, b, c + x, d - x, e);
>   vPackNode (ShiftNode x y) = let { (a, b, c, d) = vPackNode y } in
>    (Node . ShiftNode x <$> a, b, c + x, d);
>   nodePenalty (ShiftNode _ x) = nodePenalty x;
>   nodeWidth (ShiftNode _ x) = nodeWidth x;
>   nodeHeight (ShiftNode _ x) = nodeHeight x;
>   nodeDepth (ShiftNode _ x) = nodeDepth x;
>   nodeGlueSet (ShiftNode _ x) = nodeGlueSet x;
>   traverseBox f (ShiftNode i x) = (Node . ShiftNode i
>    <$> traverseBox f x) >>= f;
> };

Kern nodes.

> data KernNode = KernNode Dimen deriving (Show, Typeable);

> instance NodeClass KernNode where {
>   showNode (KernNode x) = "Kern " ++ show x ++ ";";
>   hPackNode (KernNode x) = ([Node $ KernNode x], fixedGlue x, 0, 0, []);
>   vPackNode (KernNode x) = ([Node $ KernNode x], fixedGlue x, 0, 0);
>   nodeWidth (KernNode x) = x;
>   nodeHeight (KernNode x) = x;
>   isNodeDiscardable _ = True;
> };

Glue nodes.

> data GlueNode = GlueNode Glue deriving (Show, Typeable);

> instance NodeClass GlueNode where {
>   showNode _ = "Glue;";
>   hRender (GlueNode x) z y = ([], y + calcGlue (nodeGlueSet z) x);
>   vRender (GlueNode x) z y = ([], y + calcGlue (nodeGlueSet z) x);
>   hPackNode (GlueNode x) = ([Node $ GlueNode x], x, 0, 0, []);
>   vPackNode (GlueNode x) = ([Node $ GlueNode x], x, 0, 0);
>   nodePenalty _ = Just 0;
>   nodeWidth (GlueNode (Glue x _ _)) = x;
>   nodeHeight (GlueNode (Glue x _ _)) = x;
>   isNodeDiscardable _ = True;
> };

Penalty nodes.

> data PenaltyNode = PenaltyNode (Maybe Int) deriving (Show, Typeable);

> instance NodeClass PenaltyNode where {
>   showNode (PenaltyNode (Just x)) = "Penalty " ++ show x ++ ";";
>   showNode (PenaltyNode Nothing) = "Penalty *;";
>   nodePenalty (PenaltyNode x) = x;
>   isNodeDiscardable _ = True;
> };

Finally we have text nodes.

> data TextNode = TextNode Font TextString deriving Typeable;

> instance NodeClass TextNode where {
>   showNode _ = "Text;";
>   hRender z@(TextNode f s) _ y = ([((round y, 0), Text f s)],
>    y + toRenderPos (nodeWidth z));
>   vRender _ _ _ = error "Text nodes not allowed in vertical lists";
>   vPackNode _ = error "Text nodes not allowed in vertical lists";
>   nodeWidth (TextNode f s) = objWidth (Text f s);
>   nodeHeight (TextNode f s) = objHeight (Text f s);
>   nodeDepth (TextNode f s) = objDepth (Text f s);
> };

\: Node Utilities. Just some convenient function using with nodes.

> travBoxPure :: NodeClass x => (x -> Node) -> Node -> Node;
> travBoxPure f = runIdentity . traverseBox (\x -> Identity (maybe x f $
>  castNode x));

> travBoxFunc :: (Applicative f, Monad f, NodeClass x) =>
>  (x -> f Node) -> Node -> f Node;
> travBoxFunc f = traverseBox (\x -> maybe (pure x) f $ castNode x);

\: Packaging. At first, there is the function to tell the dimensions and
total glue of a box from its components.

> sumPackage :: [(Glue, Dimen, Dimen)] -> (Glue, Dimen, Dimen);
> sumPackage [] = (fixedGlue 0, 0, 0);
> sumPackage ((x, y, z) : t) = let {
>  (a, b, c) = sumPackage t; } in (addGlue x a, max y b, max z c);

Horizontal packing is first because it is simpler than vertical packing.

> hPack :: (Dimen -> Dimen) -> [Node] -> (BoxNode, Glue, [Node]);
> hPack ii n = (BoxNode Horizontal wd ht dp gs nod, sg, adj) where {
>   (sg, ht, dp) = sumPackage
>    ((\(_, x, y, z, _) -> (x, y, z)) . hPackNode <$> n);
>   wd = ii $ naturalGlue sg;
>   adj = n >>= (\(_, _, _, _, x) -> x) . hPackNode;
>   nod = n >>= (\(x, _, _, _, _) -> x) . hPackNode;
>   gs = calcGlueSet (ii $ naturalGlue sg) sg;
> };

Vertical packing is a bit more complicated: you set the reference point to
top (if first parameter is true) or bottom (otherwise); and there is also
the maximum depth. In addition, it should keep track of top item height
and bottom item depth as well.

> vPack :: Bool -> Dimen -> (Dimen -> Dimen) -> [Node] -> (BoxNode, Glue);
> vPack False 2147483647 ii n = (BoxNode Vertical wd ht dp gs nod, sg)
>  where {
>   vs [] = (fixedGlue 0, 0, 0);
>   vs [(_, h, d, w)] = (h, d, w);
>   vs ((_, h, _, w) : t) = let { (h', d', w') = vs t; } in
>    (addGlue h h', d', max w w');
>   (sg, dp, wd) = vs (vPackNode <$> n);
>   ht = ii $ naturalGlue sg;
>   nod = n >>= (\(x, _, _, _) -> x) . vPackNode;
>   gs = calcGlueSet (ii $ naturalGlue sg) sg;
> };
> vPack False d ii n = (BoxNode Vertical wd ht dp gs nod, sg) where {
>   (BoxNode Vertical wd ht_ dp_ gs nod, sg) =
>    vPack False 2147483647 ii n;
>   dp = min d dp_;
>   ht = ht_ + dp_ - dp;
> };
> vPack True d ii n = (BoxNode Vertical wd ht dp gs nod, sg) where {
>   (BoxNode Vertical wd ht_ dp_ gs nod, sg) = vPack False d ii n;
>   nod_ = nod ++ [Node $ KernNode 0];
>   ht = nodeHeight (head nod_);
>   dp = ht_ + dp_ - ht;
> };

\: Splitting. First to split, it must find the list of valid break points.
It should not split a glue if it follows a discardable node, so this must
check for that case, too.

Input is a function to convert a node to glue and penalty; output includes
list index, glue, and penalty, for each break point.

> findBreaks :: Bool -> (Node -> (Glue, Maybe Int)) -> [Node]
>  -> [(Int, Glue, Int)];
> findBreaks _ _ [] = [];
> findBreaks True f (h : t) | isJust (castNode h :: Maybe GlueNode) =
>  ((\(x, y, z) -> (succ x, addGlue y (fst $ f h), z)) <$>
>  findBreaks (isNodeDiscardable h) f t);
> findBreaks b f (h : t) = maybe id (\x -> ((0, fixedGlue 0, x) :))
>  (snd $ f h) ((\(x, y, z) -> (succ x, addGlue y (fst $ f h), z))
>  <$> findBreaks (isNodeDiscardable h) f t);

There is function check if it can shrink; if it is unable, it should not
try any more break points.

> doesNotFit :: Dimen -> Glue -> Bool;
> doesNotFit _ (Glue _ _ (GlueSS _ x y z)) | x > 0 && y > 0 && z > 0
>  = False;
> doesNotFit x (Glue y _ (GlueSS z _ _ _)) | x >= y - z = False;
> doesNotFit _ _ = True;

> vSplit :: Dimen -> [Node] -> (BoxNode, [Node]);
> vSplit toh n = (fst $ vPack False 2147483647 (const toh) hn, tn) where {
>   (brk, _) = break (\(_, x, _) -> doesNotFit toh x) . filter nz $
>    findBreaks False (\x -> (ng $ vPackNode x, nodePenalty x)) n;
>   ng (_, x, _, _) = x;
>   nz (x, _, _) = x > 0;
>   bad = (\(x, y, z) -> (x, badness toh y + (toEnum z % 100))) <$> brk;
>   bb = fst $ maximumBy be ((length n, 999999999 % 1) : bad);
>   be (x1, y1) (x2, y2) = compare (max (-999999) y2) (max (-999999) y1)
>    |*| (bool id flip (y1 < -500000 && y2 < -500000) compare) x1 x2;
>   hn = take bb n;
>   tn = dropWhile isNodeDiscardable $ drop bb n;
> };

\: Paragraphs. This implements a simple paragraph typesetter; not as well
as \TeX. The idea is that other packages may implement a better
typesetting; for example HPDF implements much of \TeX's typesetting but
that uses the inferior PDF format instead of DVI.

> data SimpleParagraphSetting = SimpleParagraphSetting {
>   spsLeft :: [Node],
>   spsRight :: [Node],
>   spsInterline :: [Node],
>   spsWidth :: Dimen,
>   spsBackground :: Glue
> };

> simpleParagraphSetting :: SimpleParagraphSetting;
> simpleParagraphSetting = SimpleParagraphSetting {
>   spsLeft = [],
>   spsRight = [],
>   spsInterline = [],
>   spsWidth = inches 7.5,
>   spsBackground = fixedGlue 0
> };

To make a paragraph: This will not package into the box or page or add
interline glue; you have to do that afterward.

> simpleMakeParagraph :: SimpleParagraphSetting -> [Node] -> [Node];
> simpleMakeParagraph _ [] = [];
> simpleMakeParagraph ps n = Node hp : adj ++ nx' where {
>   pl = spsLeft ps;
>   pr = spsRight ps;
>   pil = spsInterline ps;
>   pw = spsWidth ps;
>   pbg = spsBackground ps;
>   brk = map abg . fst . break (\(_, x, _) -> doesNotFit pw x) . filter
>    nz $ findBreaks False (\x -> (ng $ hPackNode x, nodePenalty x)) n;
>   ng (_, x, _, _, _) = x;
>   nz (x, _, _) = x > 0;
>   abg (x, y, z) = (x, addGlue pbg y, z);
>   bad = (\(x, y, z) -> (x, badness pw y + (toEnum z % 100))) <$> brk;
>   bb = fst $ maximumBy be ((length n, 999999999 % 1) : bad);
>   be (x1, y1) (x2, y2) = compare (max (-999999) y2) (max (-999999) y1)
>    |*| (bool id flip (y1 < -500000 && y2 < -500000) compare) x1 x2;
>   hn = take bb n;
>   tn = dropWhile isNodeDiscardable $ drop bb n;
>   (hp, _, adj) = hPack (const pw) hn;
>   nx = simpleMakeParagraph ps tn;
>   nx' = bool (pil ++) id (null nx) nx;
> };

To add interline glue:

> interlineGlue :: Dimen -> Glue -> Node -> [Node] -> [Node];
> interlineGlue lsl bls ls n = ig Nothing n where {
>   aig x = isJust (castNode x :: Maybe BoxNode) ||
>    isJust (castNode x :: Maybe ShiftNode);
>   sig x = isJust (castNode x :: Maybe RuleNode);
>   ig _ [] = [];
>   ig _ (h : t) | sig h = h : ig Nothing t;
>   ig Nothing (h : t) | aig h = h : ig (Just $ nodeDepth h) t;
>   ig (Just pd) (h : t) | aig h = bool (Node . GlueNode . subtractGlue
>    bls $ fixedGlue (nodeHeight h + pd)) ls (naturalGlue bls -
>    nodeHeight h - pd < lsl) : h : ig (Just $ nodeDepth h) t;
>   ig pd (h : t) = h : ig pd t;
> };

\: Text Typesetting. Here is a simple program for typesetting text. It is
only a simple one, dealing with not much else than space factors, and only
uses a single font. Another program that uses this library and requires
more complicated things may write something else.

> type CustomStringFn x = Font -> x -> (Int, Either Char (Int -> [Node]));

> typesetCustomString :: CustomStringFn x -> Font -> [x] -> [Node];
> typesetCustomString f fo x = tcs 1000 [] (f fo <$> x) where {
>   tcs _ [] [] = [];
>   tcs _ ch [] = [typesetSingleWord fo ch];
>   tcs sf ch ((i, Right z) : t) = bool (typesetSingleWord fo ch :) id
>    (null ch) $ z sf ++ tcs (bool (cycle (sort (nub [i, sf, 1000]))
>    !! 1) sf (i == 0)) [] t;
>   tcs sf ch ((i, Left z) : t) = tcs (bool (cycle (sort (nub [i, sf,
>    1000])) !! 1) sf (i == 0)) (ch ++ [z]) t;
> };

There is also the {\tt typesetSimpleString} but with node instead of a
page object.

> typesetSingleWord :: Font -> String -> Node;
> typesetSingleWord f s = (\(Text f x) -> Node $ TextNode f x) $
>  typesetSimpleString f s;

The function using as the parameter to {\tt typesetCustomString}:

> withFrenchSpacing :: CustomStringFn Char;
> withFrenchSpacing f ' ' = (1000, Right
>  (pure . Node . GlueNode . spaceGlue f));
> withFrenchSpacing _ c = (1000, Left c);

\: Miscellaneous. Just a few miscellaneous commands for use.

One thing is making document in a more quickly way.

> singlePageDocument :: Node -> FilePath -> IO ();
> singlePageDocument n x = createDVI x 1000 dviUnitsTeX >>= shipOut . Page
>  (pageNum (1 :: Int)) (shiftPage (0, -nodeHeight n) $ renderNode n)
>  >>= finishDVI;

\: Extra Modules. Following here is the code for additional modules in
this package.

\input DVI/Alignment.lhs

% End of document (final "}" is suppressed from printout)
\toks0={{

> } -- }\bye

%  f:/program files2/MiKTeX/fonts/tfm/public/cm/cmr10.tfm
