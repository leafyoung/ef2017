import           Data.Colour.Palette.ColorSet
import           Diagrams.Backend.POVRay
import           Diagrams.Prelude
import           Control.Monad (forM_)
import           Data.Fixed (mod')

-- cam = mm50Camera # translateZ 20
cam = mm50Camera # translate (r3 (0.2,0.1,30))
cam1 = mm50Camera # translate (r3 (2,0.1,30))

xy :: Direction V3 Double
xy = direction . r3 $ (-1, -0.75, -0.75)

light = parallelLight xy white

select a -- ( a `mod'` 3.0 == 1.0 ) || ( a `mod'` 9.0 == 1.0 ) || ( a `mod'` 9.0 == 1.0 )
  | a == 4.0 || a == 9.0 || a == 13.0 || a == 16.0 = True
  | otherwise = False

s = skin cube # scaleX 10 # translateY 3 # transform (aboutY (0.25 @@ turn)) # diffuse 1.5 # ambient 0.1
bb = skin cube # translateZ (-10) # translateX (-0.35) # translateY (-0.55) # scaleX 11 # scaleY 11 # diffuse 1.5 # ambient 0.1 # sc blue
s2     = skin cube # scaleX 10 # translateY (-2) # transform (aboutY (0.25 @@ turn)) # diffuse 1.5 # ambient 0.1
s2r :: Double -> Diagram POVRay
-- s2r rz = skin cube # scaleX 10 # translateY (-2) # transform (about Y (0.25 @@ turn)) # transform (aboutZ (rz @@ turn)) # diffuse 1.5 # ambient 0.1
-- s2r rz = skin cylinder # scaleX 10 # translateY (-2) # transform (aboutY (0.25 @@ turn)) # diffuse 1.5 # ambient 0.1
s2r rz | select rz = skin cylinder # scaleZ 10 # scaleX 0.5 # scaleY 0.5 # translateY (-2) # diffuse 1.5 # ambient 0.1
       | otherwise            = skin cube # scaleZ 10 # translateY (-2) # diffuse 1.5 # ambient 0.1

-- s2 = skin cube # scaleY 0.6 # translateX 3 # transform (aboutX (0.25 @@ turn)) # diffuse 1.5 # ambient 0.1

color :: Double -> Diagram POVRay -> Diagram POVRay
color theta = sc $ rybColor (floor $ theta * 24)

e1 :: Diagram POVRay
e1 = mconcat
  [transform (aboutZ (t @@ turn)) (s # color t) | t <- [0,1/8..7/8]]

e2 = mconcat [transform (aboutZ (t @@ turn)) (s2 # color t) | t <- [-1/16,1/16..15/16]]
e3 = mconcat [transform (translation $ r3 (x + (x - 1) * 0.2,y + (y - 1) * 0.2,0)) (s2 # color (x * y / 16)) | x <- [1.0,2.0..4.0], y <-[1.0..4.0]]
ee = mappend (mappend (mappend e1 e2) bb) (transform (translationX (-3)) e2)
ee2 = mappend bb (transform (translation $ r3 (-3, -3, 0)) e2)
ee3 = mappend bb (transform (translation $ r3 (-3, -3, 0)) e3)

e4 ss = mconcat [transform (translation $ r3 (x + (x - 1) * ss ,y + (y - 1) * ss,0)) (s2 # color (x * y / 16)) | x <- [1.0,2.0..4.0], y <-[1.0..4.0]]
ee4 ss total = mappend bb (transform (translation $ r3 (-3, -3, 0)) $ e4 (0.2 + 0.8 * ss / total))
outPov4 n total = do
  writeFile ("a" ++ show n ++ ".povray") $ renderDia POVRay POVRayOptions $ mconcat [ee4 n total, cam, Main.light]

e5 ss = mconcat [transform (translation $ r3 (x + (x - 1) * ss * (offs x y) ,y + (y - 1) * ss * (offs x y) ,0)) (s2r (x * y) # color (x * y / 16.0)) | x <- [1.0,2.0..4.0], y <-[1.0..4.0]]
  where offs x y | select (x * y) = 2.5
                 | otherwise = 1.0

ee5 ss total = mappend bb (transform (translation $ r3 (-3, -3, 0)) $ e5 (0.2 + 0.8 * (total - ss) / total))
ee5r ss total = mappend bb (transform (translation $ r3 (-3, -3, 0)) $ e5 (0.2 + 0.8 * ss / total))
outPov5 total n = do
  if n < total
  then do
    writeFile ("a" ++ show n ++ ".povray") $ renderDia POVRay POVRayOptions $ mconcat [ee5 n total, cam1, Main.light]
    putStrLn $ "p:" ++ show n
    writeFile ("a" ++ show (n + total * 2) ++ ".povray") $ renderDia POVRay POVRayOptions $ mconcat [ee5r n total, cam1, Main.light]
    putStrLn $ "n:" ++ show (n + total * 2)
  else do
    forM_ [0..total] $ \ii -> do
      writeFile ("a" ++ show (n + ii) ++ ".povray") $ renderDia POVRay POVRayOptions $ mconcat [ee5 n total, cam1, Main.light]
      putStrLn $ "m:" ++ show (n + ii)

main :: IO ()
main = do
  writeFile "render.sh" ""
  let start = 1
  let end = 30
  forM_ [start..end] $ outPov5 end
  appendFile "render.sh" ("ls *.povray | time parallel -j+1 --eta 'povray -V -D +I{}'\n")
  appendFile "render.sh" "ffmpeg -y -i a%d.0.png output.gif\n"
  appendFile "render.sh" "eog output.gif\n"
