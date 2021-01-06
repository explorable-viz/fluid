import Control.Monad
import Data.Array

-- Extend with zero-padding

conv_zero :: Array (Int, Int) Double -> Array (Int, Int) Double
          -> Array (Int, Int) Double
conv_zero image filter =
    array (bounds image)
    [((c, d), (sum [ image!(image_y, image_x) * filter!(filter_y, filter_x)
                    |  (y, x) <- range (bounds filter),
                       let filter_y = iz - y,
                       let filter_x = jz - x,
                       let image_y  = c + y - half_height,
                       let image_x  = d + x - half_width,
                           image_y  >= 0, image_y <= mz,
                           image_x  >= 0, image_x <= nz ])
              / spatial_extent)
     | (c, d) <- range (bounds image) ]
    where ((0, 0), (mz, nz)) = bounds image
          ((0, 0), (iz, jz)) = bounds filter
          half_width           = quot jz 2
          half_height          = quot iz 2
          spatial_extent       = fromIntegral ((iz + 1) * (jz + 1))

-- Extend with edge values

conv_extend :: Array (Int, Int) Double -> Array (Int, Int) Double
            -> Array (Int, Int) Double
conv_extend image filter =
    array (bounds image)
    [((c, d), (sum [ image!(image_y, image_x) * filter!(filter_y, filter_x)
                    |  (y, x) <- range (bounds filter),
                       let filter_y = iz - y,
                       let filter_x = jz - x,
                       let image_y  = bound mz (c + y - half_height),
                       let image_x  = bound nz (d + x - half_width) ])
              / spatial_extent)
     | (c, d) <- range (bounds image) ]
    where ((0, 0), (mz, nz)) = bounds image
          ((0, 0), (iz, jz)) = bounds filter
          half_width         = quot jz 2
          half_height        = quot iz 2
          spatial_extent     = fromIntegral ((iz + 1) * (jz + 1))
          bound :: Int -> Int -> Int
          bound upper x
            | x > upper = upper
            | x < 0 = 0
            | otherwise = x

-- Wrap edges around image

conv_wrap :: Array (Int, Int) Double -> Array (Int, Int) Double
            -> Array (Int, Int) Double
conv_wrap image filter =
    array (bounds image)
    [((c, d), (sum [ image!(image_y, image_x) * filter!(filter_y, filter_x)
                    |  (y, x) <- range (bounds filter),
                       let filter_y = iz - y,
                       let filter_x = jz - x,
                       let image_y  = (c + y - half_height) `mod` img_h,
                       let image_x  = (d + x - half_width) `mod` img_w ])
               / spatial_extent)
     | (c, d) <- range (bounds image) ]
    where ((ma, na), (mz, nz)) = bounds image
          ((ia, ja), (iz, jz)) = bounds filter
          (img_h, img_w)       = (mz - ma + 1, nz - na + 1)
          half_width           = quot (jz - ja) 2
          half_height          = quot (iz - ia) 2
          spatial_extent       = fromIntegral ((iz - ia + 1) * (jz - ja + 1))

-- Mirror edge values

conv_mirror :: Array (Int, Int) Double -> Array (Int, Int) Double
            -> Array (Int, Int) Double
conv_mirror image filter =
    array (bounds image)
    [((c, d), (sum [ image!(image_y, image_x) * filter!(filter_y, filter_x)
                    |  (y, x) <- range (bounds filter),
                       let filter_y = iz - y,
                       let filter_x = jz - x,
                       let image_y  = mirror (0, mz) (c + y - half_height),
                       let image_x  = mirror (0, nz) (d + x - half_width)
                    ]) / spatial_extent)
                        | (c, d) <- range (bounds image) ]
    where ((0, 0), (mz, nz)) = bounds image
          ((ia, ja), (iz, jz)) = bounds filter
          half_width           = quot (jz - ja) 2
          half_height          = quot (iz - ia) 2
          spatial_extent       = fromIntegral ((iz - ia + 1) * (jz - ja + 1))
          mirror :: (Int, Int) -> Int -> Int
          mirror (lower, upper) x
            | x > upper = upper - (x - upper) + 1
            | x < lower = lower + (lower - x) -1
            | otherwise = x


test_image :: Array (Int, Int) Double
test_image = array ((0,0), (5, 5)) [ ((i, j), fromIntegral $ i * j) | i <- [0..5], j <-[0..5]]

test_filter :: Array (Int, Int) Double
test_filter = array ((0,0), (2,2)) [ ((i, j), 2) | i <- [0..2], j <- [0..2]]

expected_zero :: String
expected_zero = "array ((0,0),(5,5)) [((0,0),0.2222222222222222),((0,1),0.6666666666666666),((0,2),1.3333333333333333),((0,3),2.0),((0,4),2.6666666666666665),((0,5),2.0),((1,0),0.6666666666666666),((1,1),2.0),((1,2),4.0),((1,3),6.0),((1,4),8.0),((1,5),6.0),((2,0),1.3333333333333333),((2,1),4.0),((2,2),8.0),((2,3),12.0),((2,4),16.0),((2,5),12.0),((3,0),2.0),((3,1),6.0),((3,2),12.0),((3,3),18.0),((3,4),24.0),((3,5),18.0),((4,0),2.6666666666666665),((4,1),8.0),((4,2),16.0),((4,3),24.0),((4,4),32.0),((4,5),24.0),((5,0),2.0),((5,1),6.0),((5,2),12.0),((5,3),18.0),((5,4),24.0),((5,5),18.0)]"

expected_extend :: String
expected_extend = "array ((0,0),(5,5)) [((0,0),0.2222222222222222),((0,1),0.6666666666666666),((0,2),1.3333333333333333),((0,3),2.0),((0,4),2.6666666666666665),((0,5),3.111111111111111),((1,0),0.6666666666666666),((1,1),2.0),((1,2),4.0),((1,3),6.0),((1,4),8.0),((1,5),9.333333333333334),((2,0),1.3333333333333333),((2,1),4.0),((2,2),8.0),((2,3),12.0),((2,4),16.0),((2,5),18.666666666666668),((3,0),2.0),((3,1),6.0),((3,2),12.0),((3,3),18.0),((3,4),24.0),((3,5),28.0),((4,0),2.6666666666666665),((4,1),8.0),((4,2),16.0),((4,3),24.0),((4,4),32.0),((4,5),37.333333333333336),((5,0),3.111111111111111),((5,1),9.333333333333334),((5,2),18.666666666666668),((5,3),28.0),((5,4),37.333333333333336),((5,5),43.55555555555556)]"

main :: IO ()
main = do
   let xs = conv_zero test_image test_filter
   unless (show xs == expected_zero) $
      error ("Expected: " ++ show xs)
   let xs = conv_extend test_image test_filter
   unless (show xs == expected_extend) $
      error ("Expected: " ++ show xs)
