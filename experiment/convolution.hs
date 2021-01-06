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
                           image_y  >= ma, image_y <= mz, 
                           image_x  >= na, image_x <= nz ]) / spatial_extent)
                        | (c, d) <- range (bounds image) ]
    where ((ma, na), (mz, nz)) = bounds image 
          ((ia, ja), (iz, jz)) = bounds filter
          half_width           = quot (jz - ja) 2
          half_height          = quot (iz - ia) 2
          spatial_extent       = fromIntegral ((iz - ia + 1) * (jz - ja + 1))

-- Extend with edge values 

conv_extend :: Array (Int, Int) Double -> Array (Int, Int) Double 
            -> Array (Int, Int) Double
conv_extend image filter = 
    array (bounds image) 
    [((c, d), (sum [ image!(image_y, image_x) * filter!(filter_y, filter_x) 
                    |  (y, x) <- range (bounds filter),
                       let filter_y = iz - y,
                       let filter_x = jz - x,
                       let image_y  = bound (ma, mz) (c + y - half_height), 
                       let image_x  = bound (na, nz) (d + x - half_width)
                    ]) / spatial_extent)
                        | (c, d) <- range (bounds image) ]
    where ((ma, na), (mz, nz)) = bounds image 
          ((ia, ja), (iz, jz)) = bounds filter
          half_width           = quot (jz - ja) 2
          half_height          = quot (iz - ia) 2
          spatial_extent       = fromIntegral ((iz - ia + 1) * (jz - ja + 1))
          bound :: (Int, Int) -> Int -> Int 
          bound (lower, upper) x 
            | x > upper = upper 
            | x < lower = lower 
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
                       let image_x  = (d + x - half_width) `mod` img_w
                    ]) / spatial_extent)
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
                       let image_y  = mirror (ma, mz) (c + y - half_height), 
                       let image_x  = mirror (na, nz) (d + x - half_width)
                    ]) / spatial_extent)
                        | (c, d) <- range (bounds image) ]
    where ((ma, na), (mz, nz)) = bounds image 
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
