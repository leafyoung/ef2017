import           Data.Colour.Palette.ColorSet
import           Diagrams.Backend.POVRay
import           Diagrams.Prelude
import           Control.Monad (forM_)
import           Data.Fixed (mod')

cam :: Diagram POVRay
cam = mm50Camera # translate (r3 (0,0,15))

cam1 :: Diagram POVRay
cam1 = mm50Camera # translate (r3 (0,0,9))

xy :: Direction V3 Double
xy = direction . r3 $ (-0.75, -0.75, -0.75)

light :: Diagram POVRay
light = parallelLight xy white

-- [((<Package 85x85x45>, 0), 0), ((<Package 85x85x45>, 0), 0), ((<Package 85x85x45>, 0), 0), ((<Package 85x85x45>, 0), 0)
-- ,((<Package 85x85x45>, 1), 0), ((<Package 85x85x45>, 1), 0), ((<Package 85x85x45>, 1), 0), ((<Package 85x85x45>, 1), 0)
-- ,((<Package 85x85x45>, 2), 0), ((<Package 85x85x45>, 2), 0), ((<Package 85x85x45>, 2), 0), ((<Package 100x100x50>, 2), 0)
-- ,((<Package 145x65x65>, 0), 1), ((<Package 145x65x65>, 0), 1)
-- ,((<Package 110x75x75>, 1), 1), ((<Package 110x75x75>, 1), 1), ((<Package 110x75x75>, 1), 1)
-- ,((<Package 105x105x60>, 2), 1), ((<Package 160x85x85>, 2), 1)
-- ,((<Package 160x85x85>, 0), 2), ((<Package 120x100x100>, 0), 2)]]

can :: Double -> Double -> Diagram POVRay
can r h = skin cylinder # scaleX r # scaleY r # scaleZ h # diffuse 1.5 # ambient 1.0

box :: Double -> Double -> Double -> Diagram POVRay
box l w h = skin cube # scaleX l # scaleY w # scaleZ h # diffuse 1.5 # ambient 1.0

container :: Diagram POVRay
container = skin cube # scaleX 3.7 # scaleY 2.7 # scaleZ 0.01 # diffuse 1.5 # ambient 1.0

container3 :: Diagram POVRay
container3 = skin cube # scaleX 3.7 # scaleY 2.7 # scaleZ 2.5 # diffuse 1.5 # ambient 1.0

can1 :: Diagram POVRay
can1 = can 0.425 0.45

can2 :: Diagram POVRay
can2 = box 1.0 1.0 0.5

can3 :: Diagram POVRay
can3 = can 0.325 1.45 # transform (aboutY (0.25 @@ turn))

can4 :: Diagram POVRay
can4 = can 0.375 1.1 # transform (aboutY (0.25 @@ turn))

can5 :: Diagram POVRay
can5 = can 0.525 0.65

can6 :: Diagram POVRay
can6 = can 0.425 1.6 # transform (aboutY (0.25 @@ turn))

can6a :: Diagram POVRay
can6a = can 0.425 1.6

can7 :: Diagram POVRay
can7 = can 0.5 1.2

n :: Int
n = 24

color :: Double -> Diagram POVRay -> Diagram POVRay
color theta = sc $ rybColor (floor $ theta * fromIntegral n)

e0 :: Diagram POVRay
e0      = mconcat $ take 22 $
          zipWith (\t s -> s # color t) [0/fromIntegral n,1/fromIntegral n..fromIntegral n/fromIntegral n] $
          map (\t -> t # translate (r3 (-3.7/2, -2.7/2 + 1.0, -2.5/2)) # transform (aboutX (-0.15 @@ turn)) # transform (aboutY (-0.05 @@ turn)) # transform (aboutZ (0 @@ turn))) $
          [ container # translate (r3 (-0.5,-0.5,0)) ]
          ++ [ can1 # translate (r3 (0,0,(t - 1) * 0.45 )) | t <- [1..11] ]
          ++ [ can4 # transform (aboutY (-0.25 @@ turn)) # translate (r3 (0.9,0,(t - 1) * 1.1)) | t <- [1..3] ]
          ++ [ can3 # transform (aboutY (-0.25 @@ turn)) # translate (r3 (0.9 + 0.8,0,(t - 1) * 1.45)) | t <- [1..2] ]
          ++ [ can6 # transform (aboutY (-0.25 @@ turn)) # translate (r3 (0.9 + 0.8 + 0.7,0, (t - 1) * 1.6)) | t <- [1..2] ]
          ++ [ can7 # translate (r3 (0.9 + 0.8 + 0.7 + 0.9,0,(t - 1) * 1.2)) | t <- [1..1] ]
          ++ [ can2 # translate (r3 (0.9 + 0.8 + 0.7 + 0.9 + 1.1 - 0.5, -0.5,(t - 1) * 0.5)) | t <- [1..1] ]
          ++ [ can5 # translate (r3 (0.9 + 0.8 + 0.7 + 0.9 + 1.1 + 1.1,0,(t - 1) * 0.65)) | t <- [1..1] ]
          ++ [ container # translate (r3 (-0.5,-0.5,2.5)), container3 # translate (r3 (-0.5,-0.5,0)) ]

outPov4 = do
  writeFile ("b0.povray") $ renderDia POVRay POVRayOptions $ mconcat [e0, cam, Main.light]
  appendFile "render.sh" "eog b0.png\n"

e1 :: Int -> Diagram POVRay
e1 fn = mconcat $ take fn $
        zipWith (\t s -> s # color t) [0/fromIntegral n,1/fromIntegral n..fromIntegral n/fromIntegral n] $
        map (\t -> t # translate (r3 (-3.7/2, -2.7/2 + 1.0, -2.5/2)) # transform (aboutX (-0.1 @@ turn)) # transform (aboutY (-0.1 @@ turn)) # transform (aboutZ (0 @@ turn)))
        [ container # translate (r3 (-0.5,-0.5,0))
        , can1 # translate (r3 (0,0,0)), can1 # translate (r3 (0,0,0.45)), can1 # translate (r3 (0,0,0.9)), can1 # translate (r3 (0,0,1.35))
        , can1 # translate (r3 (0,0.85,0)), can1 # translate (r3 (0,0.85,0.45)), can1 # translate (r3 (0,0.85,0.9)), can1 # translate (r3 (0,0.85,1.35))
        , can1 # translate (r3 (0,1.7,0)), can1 # translate (r3 (0,1.7,0.45)), can1 # translate (r3 (0,1.7,0.9)), can2 # translate (r3 (-0.5,1.2,1.35))
        , can3 # translate (r3 (0.6,-0.15,0.325)), can3 # translate (r3 (0.6,-0.15, 0.65 + 0.325))
        , can4 # translate (r3 (0.6,-0.15 + 0.85, 0.375)), can4 # translate (r3 (0.6,-0.15 + 0.85, 0.375 + 0.75)), can4 # translate (r3 (0.6,-0.15 + 0.85, 0.375 + 1.5))
        , can5 # translate (r3 (1.1,-0.25 + 0.85 + 0.7 + 0.3, 0)), can6 # translate (r3 (0.6,-0.25 + 0.85 + 0.8 + 0.3,1.11))
        , can6a # translate (r3 (1.1 + 1.5,0,0)), can7 # translate (r3 (1.1 + 1.5, 1.0,0))
        , container # translate (r3 (-0.5,-0.5,2.5))
        , container3 # translate (r3 (-0.5,-0.5,0)) ]

outPov6 = do
  let interval = 13
  forM_ [1..n] $ \fn -> do
    forM_ [1..interval] $ \fi -> do
      let sn = (fn - 1) * interval + (fi - 1)
      let filen = "a" ++ show sn ++ ".povray"
      writeFile filen $ renderDia POVRay POVRayOptions $ mconcat [e1 fn, cam1, Main.light]
  appendFile "render.sh" "ffmpeg -y -i a%d.png output.gif\n"
  appendFile "render.sh" "eog output.gif\n"

main :: IO ()
main = do
  writeFile "render.sh" ""
  appendFile "render.sh" ("ls *.povray | time parallel -j+1 --eta 'povray -V -D +I{}'\n")
  outPov4
  outPov6
