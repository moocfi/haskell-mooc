{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Set8Test where

import Data.List hiding (union)
import Test.QuickCheck

import Mooc.Th
import Mooc.Test

import Set8 hiding (main)

main = score [(1,"dotAndLine",[ex1_line, ex1_dot, ex1_black, ex1_line_far])
             ,(2,"blend",[ex2_blendColor, ex2_render, ex2_solid, ex2_const])
             ,(3,"rectangle",[ex3])
             ,(4,"union",[ex4_union, ex4_cut])
             ,(5,"paintSolid",[ex5_render, ex5_dot])
             ,(6,"paint",[ex6_render, ex6_dot])
             ,(7,"Transform",[ex7_fill, ex7_zoom_xy, ex7_zoom_dot, ex7_flipx, ex7_flipy, ex7_flipxy])
             ,(8,"Chain",[ex8_inverse, ex8_order, ex8_zoom])
             ,(9,"Blur",[ex9_blur_dot, ex9_blur_solid])
             ,(10,"BlurMany",[ex10_blur_once, ex10_blur_many])]

--

checkPicture x y exp (Picture f) =
  counterexample (" color of point (" ++ show x ++ "," ++ show y ++ ")") $
  (f (Coord x y)) ?== exp

checkShape x y exp (Shape f) =
  counterexample (" contains point (" ++ show x ++ "," ++ show y ++ ")") $
  f (Coord x y) ?== exp

instance Arbitrary Color where
  arbitrary = Color <$> comp <*> comp <*> comp
    where comp = choose (0,255)

ex1_line = $(testing' [|renderList dotAndLine (2,4) (8,8)|]) (?==[["ff69b4","ff69b4","ff69b4"]])
ex1_dot = $(testing' [|renderList dotAndLine (2,4) (4,4)|]) (?==[["000000","ffffff","000000"]])
ex1_black = forAll_ $ \(x,y) ->
  (x,y) /= (3,4) ==>
  y /= 8 ==>
  $(testing' [|getPixel dotAndLine x y|]) $ \val ->
  counterexample ("  with x = "++show x++" and y = "++show y) $
  val ?== "000000"
ex1_line_far = forAll_ $ \x ->
  $(testing' [|getPixel dotAndLine x 8|]) $ \val ->
  counterexample ("  with x = "++show x) $
  val ?== "ff69b4"

ex2_blendColor =
  forAll_ $ \(Color r g b) ->
  forAllBlind (choose (0,min r (255-r))) $ \r' ->
  forAllBlind (choose (0,min g (255-g))) $ \g' ->
  forAllBlind (choose (0,min b (255-r))) $ \b' ->
  $(testing [|blendColor (Color (r-r') (g-g') (b-b')) (Color (r+r') (g+g') (b+b'))|]) (?== Color r g b)

ex2_render = conjoin
  [$(testing' [|renderList (combine blendColor (solid red) justADot) (9,11) (10,10)|])
   (?==[["7f0000","ff7f7f","7f0000"]])
  ,$(testing' [|renderList (combine blendColor justADot (solid white)) (9,11) (10,10)|])
   (?==[["7f7f7f","ffffff","7f7f7f"]])]

ex2_solid = forAll_ $ \c1 ->
  forAll_ $ \c2 ->
  forAll_ $ \(x,y) ->
  counterexample ("combine blendColor (solid ("++show c1++")) (solid ("++show c2++"))") $
  checkPicture x y (blendColor c1 c2) (combine blendColor (solid c1) (solid c2))

ex2_const = forAll_ $ \c1 ->
  forAll_ $ \c2 ->
  forAll_ $ \(x,y) ->
  counterexample ("combine (\\c1 c2 -> c1) (solid ("++show c1++")) (solid ("++show c2++"))") $
  checkPicture x y c1 (combine (\c1 c2 -> c1) (solid c1) (solid c2))

ex3 = forAll_ $ \(Positive x0,Positive y0) ->
  forAllBlind (choose (1,10)) $ \w ->
  forAllBlind (choose (1,10)) $ \h ->
  $(testing [|rectangle x0 y0 w h|]) $ \rect ->
  conjoin [checkShape x y True rect | x <- [x0..x0+w-1], y <- [y0..y0+h-1]]
  .&&. conjoin [checkShape (x0-1) y False rect | y <- [y0-1..y0+h]] -- left
  .&&. conjoin [checkShape (x0+w) y False rect | y <- [y0-1..y0+h]] -- right
  .&&. conjoin [checkShape x (y0-1) False rect | x <- [x0-1..x0+w]] -- top
  .&&. conjoin [checkShape x (y0+h) False rect | x <- [x0-1..x0+w]] -- bottom
  .&&. conjoin [checkShape (x0-h) (y0-w) False rect
               ,checkShape x0 (y0+h+w) False rect
               ,checkShape (x0+max h w) (y0+1) False rect]

ex4_union = forAll_ $ \(Positive x0,Positive y0,Positive x1,Positive y1) ->
  counterexample ("union (dot "++show x0++" "++show y0++") (dot "++show x1++" "++show y1++")") $
  let un = union (dot x0 y0) (dot x1 y1)
  in conjoin [checkShape x0 y0 True un
             ,checkShape x1 y1 True un
             ,x0 /= x1 && y0 /= y1 ==> checkShape x1 y0 False un
             ,x0 /= x1 && y0 /= y1 ==> checkShape x0 y1 False un
             ,forAll_ $ \(x,y) -> not (elem x [x0,x1]) && not (elem y [y0,y1]) ==> checkShape x y False un]

ex4_cut = forAll_ $ \(Positive x0,Positive y0) ->
  forAllBlind (choose (2,10)) $ \w ->
  forAllBlind (choose (2,10)) $ \h ->
  forAllBlind (choose (x0,x0+w-1)) $ \x ->
  forAllBlind (choose (y0,y0+h-1)) $ \y ->
  counterexample ("cut (rectangle "++show x0++" "++show y0++" "++show w++" "++show h++") (dot "++show x++" "++show y++")") $
  let c = cut (rectangle x0 y0 w h) (dot x y)
  in conjoin [checkShape x y False c
             ,forAllBlind (choose (x0,x0+w-1) `suchThat` (/=x)) $ \x' ->
              forAllBlind (choose (y0,y0+h-1) `suchThat` (/=y)) $ \y' ->
              checkShape x' y' True c]

ex5_render =
  $(testing' [|renderList (paintSolid white (dot 5 5) (solid black)) (4,6) (5,5)|])
  (?==[["000000","ffffff","000000"]])

ex5_dot = forAll_ $ \(x,y) ->
  forAll_ $ \bg ->
  forAll_ $ \fg ->
  let pict = paintSolid fg (dot x y) (solid bg)
  in counterexample ("paintSolid ("++show fg++") (dot "++show x++" "++show y++") (solid (" ++ show bg ++"))") $
  conjoin [checkPicture x y fg pict
          ,forAll_ $ \(x1,y1) -> x1/=x || y1/=y ==> checkPicture x1 y1 bg pict]

ex6_render =
  conjoin [$(testing' [|renderList (paint (stripes red white) (rectangle 0 0 2 4) (solid black)) (0,4) (1,1)|])
           (?==[["ffffff","ffffff","000000","000000","000000"]])
          ,$(testing' [|renderList (paint (stripes red white) (rectangle 0 0 2 4) (solid black)) (0,4) (0,0)|])
           (?==[["ff0000","ff0000","000000","000000","000000"]])]

ex6_dot = forAll_ $ \(x,y) ->
  forAll_ $ \bg ->
  forAll_ $ \fg ->
  let pict = paint (solid fg) (dot x y) (solid bg)
  in counterexample ("paint (solid ("++show fg++")) (dot "++show x++" "++show y++") (solid (" ++ show bg ++"))") $
  conjoin [checkPicture x y fg pict
          ,forAll_ $ \(x1,y1) -> x1/=x || y1/=y ==> checkPicture x1 y1 bg pict]

ex7_fill = $(testing' [|apply (Fill red) xy|]) $ \pic ->
  forAll_ $ \(x,y) ->
  checkPicture x y red pic

ex7_zoom_xy = $(testing' [|apply (Zoom 3) xy|]) $ \pic ->
  forAllBlind (choose (0,32)) $ \x ->
  forAllBlind (choose (0,32)) $ \y ->
  forAllBlind (choose (0,2)) $ \dx ->
  forAllBlind (choose (0,2)) $ \dy ->
  checkPicture (3*x+dx) (3*y+dy) (Color x y 0) pic

ex7_zoom_dot = forAll_ $ \(x,y) ->
  forAllBlind (choose (1,10)) $ \z ->
  $(testing' [|apply (Zoom z) (fill white (dot x y))|]) $ \pic ->
  counterexample ("with z=" ++ show z ++ ", x=" ++ show x ++ ", y=" ++ show y) $
  conjoin [checkPicture x' y' white pic | x' <- [x*z..(x+1)*z-1], y' <- [y*z..(y+1)*z-1]] -- inside
  .&&. conjoin [checkPicture (x*z-1) y' black pic | y' <- [y*z..(y+1)*z-1]] -- left
  .&&. conjoin [checkPicture ((x+1)*z) y' black pic | y' <- [y*z..(y+1)*z-1]] -- right
  .&&. conjoin [checkPicture x' (y*z-1) black pic | x' <- [x*z..(x+1)*z-1]] -- top
  .&&. conjoin [checkPicture x' ((y+1)*z) black pic | x' <- [x*z..(x+1)*z-1]] -- bottom

ex7_flipx = $(testing' [|apply FlipX xy|]) $ \pic ->
  forAllBlind (choose (0,255)) $ \x ->
  forAllBlind (choose (0,255)) $ \y ->
  checkPicture x y (Color (if x==0 then x else (256-x)) y 0) pic

ex7_flipy = $(testing' [|apply FlipY xy|]) $ \pic ->
  forAllBlind (choose (0,255)) $ \x ->
  forAllBlind (choose (0,255)) $ \y ->
  checkPicture x y (Color x (if y==0 then y else (256-y)) 0) pic

ex7_flipxy = $(testing' [|apply FlipXY xy|]) $ \pic ->
  forAllBlind (choose (0,255)) $ \x ->
  forAllBlind (choose (0,255)) $ \y ->
  checkPicture x y (Color y x 0) pic

ex8_inverse = $(testing' [|apply (Chain FlipX FlipX) xy|]) $ \pic ->
  forAllBlind (choose (0,255)) $ \x ->
  forAllBlind (choose (0,255)) $ \y ->
  checkPicture x y (Color x y 0) pic

ex8_order = $(testing' [|apply (Chain (Fill white) (Fill red)) xy|]) $ \pic ->
  forAll_ $ \(x,y) ->
  checkPicture x y white pic

ex8_zoom =
  forAllBlind (choose (1,4)) $ \z1 ->
  forAllBlind (choose (1,4)) $ \z2 ->
  $(testing [|apply (Chain (Zoom z1) (Zoom z2))|]) $ \app ->
  counterexample "applied to xy" $
  let pic = app xy
      z = z1*z2
  in forAllBlind (choose (0,32)) $ \x ->
    forAllBlind (choose (0,32)) $ \y ->
    forAllBlind (choose (0,z-1)) $ \dx ->
    forAllBlind (choose (0,z-1)) $ \dy ->
    checkPicture (z*x+dx) (z*y+dy) (Color x y 0) pic

ex9_blur_dot = forAll_ $ \(x,y) ->
  forAllBlind (choose (0,50)) $ \c ->
  let blurred = Color c c c
      c1 = c*5
      color = Color c1 c1 c1
  in $(testing' [|apply Blur (fill color (dot x y))|]) $ \pic ->
    counterexample ("with color = " ++ show color ++ ", x = " ++ show x ++ ", y = " ++ show y) $
    conjoin [checkPicture x y blurred pic
            ,checkPicture (x+1) y blurred pic
            ,checkPicture (x-1) y blurred pic
            ,checkPicture x (y+1) blurred pic
            ,checkPicture x (y-1) blurred pic
            ,checkPicture (x-1) (y-1) black pic
            ,checkPicture (x+1) (y+1) black pic
            ,checkPicture (x-1) (y+1) black pic
            ,checkPicture (x+1) (y-1) black pic]

ex9_blur_solid =
  forAll_ $ \color ->
  forAll_ $ \(x,y) ->
  $(testing' [|apply Blur (solid color)|]) $ \pic ->
  counterexample ("with color = "++show color) $
  checkPicture x y color pic

ex10_blur_once = forAll_ $ \(x,y) ->
  forAllBlind (choose (0,50)) $ \c ->
  let blurred = Color c c c
      c1 = c*5
      color = Color c1 c1 c1
  in $(testing' [|apply (BlurMany 1) (fill color (dot x y))|]) $ \pic ->
    counterexample ("with color = " ++ show color ++ ", x = " ++ show x ++ ", y = " ++ show y) $
    conjoin [checkPicture x y blurred pic
            ,checkPicture (x+1) y blurred pic
            ,checkPicture (x-1) y blurred pic
            ,checkPicture x (y+1) blurred pic
            ,checkPicture x (y-1) blurred pic
            ,checkPicture (x-1) (y-1) black pic
            ,checkPicture (x+1) (y+1) black pic
            ,checkPicture (x-1) (y+1) black pic
            ,checkPicture (x+1) (y-1) black pic]

ex10_blur_many = forAll_ $ \(x,y) ->
  forAllBlind (choose (1,6)) $ \n ->
  forAll_ $ \color ->
  let b = BlurMany n
      b' = BlurMany (n-1)
      f = apply b
      g = apply Blur . apply b'
      img = fill color (dot x y)
  in counterexample ("Checking that\n  apply ("++show b++")\ngives the same result as\n  apply Blur . apply ("++show b'++")\non the image") $
     counterexample ("  fill ("++show color++") (dot "++show x++" "++show y++")") $
     forAllBlind (choose (x-n,x+n)) $ \x' ->
     forAllBlind (choose (y-n,y+n)) $ \y' ->
     let (Picture color) = g img
         expected = color (Coord x' y')
     in checkPicture x' y' expected (f img)
