import Data.Array


convolution :: Array (Int, Int) Double -> Array (Int, Int) Double 
            -> Array (Int, Int) Double
convolution image filter = 
    array ((ma, na),(mz, nz)) 
    [((c, d), (sum [ image!(image_y, image_x) * filter!(iz - y + 1, jz - x + 1 ) 
                    |  (y,x) <- range (bounds filter),
                       let image_y = c + y - 1 - half_height, 
                       let image_x = d + x - 1 - half_width,
                           image_y >= ma, image_y <= mz, 
                           image_x >= na, image_x <= nz ]) / spatial_extent)
                        | (c, d) <- range ((ma, na), (mz, nz)) ]
    where ((ma, na), (mz, nz)) = bounds image 
          ((ia, ja), (iz, jz)) = bounds filter
          half_width           = quot (jz - ja) 2
          half_height          = quot (iz - ia) 2
          spatial_extent       = fromIntegral ((iz - ia + 1) * (jz - ja + 1))

test_image :: Array (Int, Int) Double
test_image = array ((1,1), (6, 6)) [ ((i, j), fromIntegral $ i * j) | i <- [1..6], j <-[1..6]]

test_filter :: Array (Int, Int) Double
test_filter = array ((1,1), (3,3)) [ ((i, j), 2) | i <- [1..3], j <- [1..3]]

