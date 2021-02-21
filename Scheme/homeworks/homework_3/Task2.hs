module Task2 where 

import Data.Word
-- Даниел Здравков Стоянов, 45574, Информатика, 1-ва група
data Rgb = Rgb { red   :: Word8
               , green :: Word8
               , blue  :: Word8 } deriving (Show,Read)

data Image = Image { width   :: Int
                   , height  :: Int
                   , content :: [[Rgb]] } deriving (Show,Read)

emptyImage :: Image
emptyImage = Image 0 0 []

roundToWord8 :: Word8 -> Word8
roundToWord8 val
  | val < 0 = 0
  | val > 255 = 255
  | otherwise = val

round' :: Int -> Int
round' val
  | val < 0 = 0
  | val > 255 = 255
  | otherwise = val

grayscalePixel :: Rgb -> Rgb
grayscalePixel (Rgb r g b) = (Rgb val val val)
  where val :: Word8
        val = roundToWord8 $ truncate $ 0.30 * (fromIntegral r) + 0.59 * (fromIntegral g) + 0.11 * (fromIntegral b)

grayscale :: Image -> Image
grayscale img@(Image a b content) = Image a b $ map (\row -> map (\pixel -> grayscalePixel pixel) row) $ content

isGrayscalePixel :: Rgb -> Bool 
isGrayscalePixel (Rgb r g b) = r == g && g == b 

isGrayscaleImage :: Image -> Bool
isGrayscaleImage (Image a b content) = foldr (\row res -> if (foldr (\pixel next -> if isGrayscalePixel pixel then True && next else False) True row) then True && res else False) True content

getGrayscaleVal :: Rgb -> Word8
getGrayscaleVal (Rgb r g b) = r

subRange matrix x = drop (x-1) $ take (x+2) matrix 
submatrix matrix x y = map (\row -> subRange row y ) $ subRange matrix x

submatrixCords img@(Image a b matrix) = [ (x,y) | x <- [1..(a - 2)], y <- [1..(b - 2)]]

parseRgbToIntVec :: [Rgb] -> [Float]
parseRgbToIntVec vec = map (\rgb -> fromIntegral $ getGrayscaleVal rgb) vec

parseRgbToFloatMatrix :: [[Rgb]] -> [[Float]]
parseRgbToFloatMatrix matrix = map (\vec -> parseRgbToIntVec vec) matrix

parseIntToRgbVec :: [Int] -> [Rgb]
parseIntToRgbVec vec = map (\x -> (Rgb x x x)) $ map (\x -> (fromIntegral x)) vec

parseIntToRgbMatrix :: [[Int]] -> [[Rgb]]
parseIntToRgbMatrix matrix = map (\vec -> parseIntToRgbVec vec) matrix

convolution [[a,b,c],[d,e,f],[g,h,i]] [[j,k,l],[m,n,o],[p,q,r]] = i*j + h*k + g*l + f*m + e*n + d*o + c*p + b*q + a*r

gX :: [[Float]]
gX = [[1,0,-1],[2,0,-2],[1,0,-1]]

gY :: [[Float]]
gY = [[1,2,1],[0,0,0],[-1,-2,-1]]

gXA :: [[Float]] -> Float
gXA matrix = convolution gX matrix 

gYA :: [[Float]] -> Float
gYA matrix = convolution gY matrix

sobelVal :: [[Float]] -> Int
sobelVal matrix = truncate $ sqrt $ (gXA matrix) ** 2 + (gYA matrix) ** 2

sobel :: [[Rgb]] -> (Int,Int) -> Int
sobel matrix (x,y) = round' $ sobelVal $ parseRgbToFloatMatrix $ submatrix matrix x y

vecToMatrix [] _ = []
vecToMatrix vec n = [take n vec] ++ vecToMatrix (drop n vec) n

edgeDetectValues img@(Image a b content) = vecToMatrix (map (\(x,y) -> sobel content (x,y)) $ submatrixCords img) (b-2)

edgeDetect :: Image -> Image
edgeDetect img@(Image a b matrix)
  | isGrayscaleImage img = Image (a-2) (b-2) $ parseIntToRgbMatrix $ edgeDetectValues img
  | otherwise            = emptyImage
