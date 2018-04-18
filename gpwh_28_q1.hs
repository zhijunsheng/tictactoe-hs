type LatLong = (Double, Double)

readLatLong :: IO LatLong
readLatLong = (read <$> getLine, read <$> getLine)

haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO _ _ = return 163.0

