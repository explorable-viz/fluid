import Data.Array


convolution :: Array (Int, Int) Double -> Array (Int, Int) Double 
            -> Array (Int, Int) Double
convolution image filter = 
    image //
    [((c, d), (sum [ image!(c + x - 1 - half_height, d + y - 1 - half_width) 
                     * filter!(iz - x + 1, jz - y + 1 ) 
                    |  (x,y) <- range (bounds filter) ]) / spatial_extent)
                        | (c, d) <- range ((u_bound, l_bound), (d_bound, r_bound)) ]
    where ((ma, na), (mz, nz)) = bounds image 
          ((ia, ja), (iz, jz)) = bounds filter
          half_width           = quot (jz - ja) 2
          half_height          = quot (iz - ia) 2
          l_bound              = na + half_width
          u_bound              = ma + half_height
          r_bound              = nz - half_width
          d_bound              = mz - half_height
          spatial_extent       = fromIntegral ((iz - ia + 1) * (jz - ja + 1))

test_image :: Array (Int, Int) Double
test_image = array ((1,1), (6, 6)) [ ((i, j), fromIntegral $ i * j) | i <- [1..6], j <-[1..6]]

test_filter :: Array (Int, Int) Double
test_filter = array ((1,1), (3,3)) [ ((i, j), 2) | i <- [1..3], j <- [1..3]]

-- convolution' :: Array (Int, Int) Double -> Array (Int, Int) Double 
--             ->  [[((Int, Int), (Int, Int, Int, Int))]]
-- convolution' image filter = 
--     [[ ((c, d), (c + x - 1 - half_height, d + y - 1 - half_width
--                         , iz - x + 1, jz - y + 1)) 
--                 |  (x,y) <- range (bounds filter) ]
--                     | (c, d) <- range ((u_bound, l_bound), (d_bound, r_bound)) ]
--     where ((ma, na), (mz, nz)) = bounds image 
--           ((ia, ja), (iz, jz)) = bounds filter
--           l_bound              = na + quot (jz - ja) 2
--           u_bound              = ma + quot (iz - ia) 2
--           r_bound              = nz - quot (jz - ja) 2 
--           d_bound              = mz - quot (iz - ia) 2
--           half_width           = quot (jz - ja) 2
--           half_height          = quot (iz - ia) 2
--           spatial_extent       = fromIntegral ((iz - ia) * (jz - ja))

-- pad :: Array (Int, Int) Int -> Array (Int, Int) Int 
-- pad image = image // [ ((i, j), 0) | (i,j) <- range ((1, 1), (2,2))]

-- x :: (Ix a, Num a, Enum a) => Array (a, a) a
-- x = array ((1,1),(6,5)) [ ((i, j), i + j) | i <- [1..6], j <- [1..5] ]
