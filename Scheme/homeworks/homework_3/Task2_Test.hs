module Task2_Test where 

import Test.HUnit
import Task2
-- Даниел Здравков Стоянов, 45574, Информатика, 1-ва група

testImage = Image 4 4 [[Rgb 0 0 0,   Rgb 155 128 0, Rgb 255 255 0, Rgb 155 128 0],
                       [Rgb 0 255 0, Rgb 0 255 0,   Rgb 128 0 128, Rgb 0 128 0],
                       [Rgb 0 128 0, Rgb 128 0 255, Rgb 128 0 128, Rgb 155 255 0],
                       [Rgb 0 0 0,   Rgb 255 0 255, Rgb 0 128 128, Rgb 0 128 0]]

testGrayScaleImage = Image 4 4 [[Rgb 0 0 0,       Rgb 122 122 122, Rgb 226 226 226, Rgb 122 122 122],
                                [Rgb 150 150 150, Rgb 150 150 150, Rgb 52 52 52,    Rgb 75 75 75],
                                [Rgb 75 75 75,    Rgb 66 66 66,    Rgb 52 52 52,    Rgb 196 196 196],
                                [Rgb 0 0 0,       Rgb 104 104 104, Rgb 89 89 89,    Rgb 75 75 75]]

randomGrayScaleImage = Image 4 4 [[Rgb 140 140 140, Rgb 0 0 0,       Rgb 0 0 0,       Rgb 50 50 50],
                                  [Rgb 2 2 2,       Rgb 150 150 150, Rgb 26 26 26,    Rgb 75 75 75],
                                  [Rgb 170 170 170, Rgb 66 66 66,    Rgb 52 52 52,    Rgb 98 98 98],
                                  [Rgb 0 0 0,       Rgb 68 68 68,    Rgb 89 89 89,    Rgb 120 120 120]]

testGrayscaleImage testImage testGrayScaleImage = True
testGrayscaleImage _ _ = False
testGrayscale = TestCase $ assertEqual "grayscale" (testGrayscaleImage testImage (grayscale testImage)) True
testIsGrayscale = TestCase $ do assertEqual "isGrayscale" (isGrayscaleImage testImage) False
                                assertEqual "isGrayscale" (isGrayscaleImage (grayscale testImage)) True
                                assertEqual "isGrayscale" (isGrayscaleImage testGrayScaleImage) True
                                assertEqual "isGrayscale" (isGrayscaleImage randomGrayScaleImage) True
    
tests = TestList[testGrayscale, testIsGrayscale]
main = runTestTT tests