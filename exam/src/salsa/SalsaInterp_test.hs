module SalsaInterp_test (test)
where

--import Test.HUnit
import SalsaInterp
import SalsaAst
import Gpx

--Can make a standard view
test1 =  ([("Default",0,0)],[])
     ==  runProg 1
    [ Def (Viewdef "Default" (Const 0) (Const 0))]
--Frame rate does not touch view definitions
test2 = ([("Default",0,0)],[]) 
     ==  runProg 100
    [ Def (Viewdef "Default" (Const 0) (Const 0))]
--Can make multiple views
test3 = ([("Default",0,0), ("New",100,100)],[]) 
     ==  runProg 100
    [ Def (Viewdef "Default" (Const 0) (Const 0)),Def (Viewdef "New" (Const 100) (Const 100))]
--Can make a rectangle with correct active view
test4 = ([("Default",100,100)],[[DrawRect 20 20 20 20 "Default" "blue"]]) 
     ==  runProg 1
    [ Def (Viewdef "Default" (Const 100) (Const 100)), Def (Rectangle "box" (Const 20) (Const 20) (Const 20) (Const 20) Blue)]
--Can make a circle
test5 = ([("Default",100,100)],[[DrawCirc 20 20 50 "Default" "orange"]]) 
     ==  runProg 1
    [ Def (Viewdef "Default" (Const 100) (Const 100)), Def (Circle "oval" (Const 20) (Const 20) (Const 50) Orange)]
--Can make a group (of views), which is set to the active view. One frame with same shape active on both views.
test6 = ([("Default",200,200), ("New",100,100)],[[DrawCirc 20 20 50 "Default" "orange", DrawCirc 20 20 50 "New" "orange"]]) 
     ==  runProg 1
    [ Def (Viewdef "Default" (Const 200) (Const 200)), Def (Viewdef "New" (Const 100) (Const 100)), Def (Group "Group_A" ["Default", "New"]), Def (View "Group_A"), Def (Circle "oval" (Const 20) (Const 20) (Const 50) Orange)]
--Test Active views. Only for the last active view should the circle be drawn
test7 = ([("Default",200,200), ("New",100,100)],[[DrawCirc 20 20 50 "New" "orange"]]) 
     ==  runProg 1
    [ Def (Viewdef "Default" (Const 200) (Const 200)), Def (Viewdef "New" (Const 100) (Const 100)), Def (Group "Group_A" ["Default", "New"]), Def (View "Group_A"), Def (View "New"), Def (Circle "oval" (Const 20) (Const 20) (Const 50) Orange)]
--Shows that it is not the last defined nor the first defined view that stays active. It is the last activated view that counts.
test8 = ([("Default",200,200), ("New",100,100)],[[DrawCirc 20 20 50 "Default" "orange"]]) 
     ==  runProg 1
    [ Def (Viewdef "Default" (Const 200) (Const 200)), Def (Viewdef "New" (Const 100) (Const 100)), Def (Group "Group_A" ["Default", "New"]), Def (View "Group_A"), Def (View "Default"), Def (Circle "oval" (Const 20) (Const 20) (Const 50) Orange)]
--Show multiple shapes on same view, the last active, and frame
test9 = ([("Default",200,200), ("New",100,100)],[[DrawCirc 20 20 50 "Default" "orange", DrawRect 20 20 20 20 "Default" "blue"]]) 
     ==  runProg 1
    [ Def (Viewdef "Default" (Const 200) (Const 200)), Def (Viewdef "New" (Const 100) (Const 100)), Def (Group "Group_A" ["Default", "New"]), Def (View "Group_A"), Def (View "Default"), Def (Circle "oval" (Const 20) (Const 20) (Const 50) Orange), Def (Rectangle "box" (Const 20) (Const 20) (Const 20) (Const 20) Blue)]
--Show multiple shapes on multiple views via group, same frame
test10 = ([("Default",200,200), ("New",100,100)],[[DrawCirc 20 20 50 "Default" "orange", DrawCirc 20 20 50 "New" "orange", DrawRect 20 20 20 20 "Default" "blue", DrawRect 20 20 20 20 "New" "blue"]]) 
     ==  runProg 1
    [ Def (Viewdef "Default" (Const 200) (Const 200)), Def (Viewdef "New" (Const 100) (Const 100)), Def (Group "Group_A" ["Default", "New"]), Def (View "Group_A"), Def (View "Default"), Def (View "Group_A"), Def (Circle "oval" (Const 20) (Const 20) (Const 50) Orange),Def (Rectangle "box" (Const 20) (Const 20) (Const 20) (Const 20) Blue)]
--Show move command, abs, 1 frame
test11 = ([("Default",100,100)],[[DrawCirc 20 20 50 "Default" "orange"], [DrawCirc 10 5 50 "Default" "orange"]]) 
     ==  runProg 1
    [ Def (Viewdef "Default" (Const 100) (Const 100)), Def (Circle "oval" (Const 20) (Const 20) (Const 50) Orange), Com (Move ["oval"] (Abs (Const 10) (Const 5)))]
--Show move command, Rel, 1 frame
test12 = ([("Default",100,100)],[[DrawCirc 20 20 50 "Default" "orange"], [DrawCirc 30 30 50 "Default" "orange"]]) 
     ==  runProg 1
    [ Def (Viewdef "Default" (Const 100) (Const 100)), Def (Circle "oval" (Const 20) (Const 20) (Const 50) Orange), Com (Move ["oval"] (Rel (Const 10) (Const 10)))]
----Show shape on same view on multiple frames via move command, Rel, 5 frames
test13 = ([("Default",100,100)],[[DrawCirc 20 20 50 "Default" "orange"],[DrawCirc 22 22 50 "Default" "orange"], [DrawCirc 24 24 50 "Default" "orange"], [DrawCirc 26 26 50 "Default" "orange"], [DrawCirc 28 28 50 "Default" "orange"], [DrawCirc 30 30 50 "Default" "orange"]]) 
     ==  runProg 5
    [ Def (Viewdef "Default" (Const 100) (Const 100)), Def (Circle "oval" (Const 20) (Const 20) (Const 50) Orange), Com (Move ["oval"] (Rel (Const 10) (Const 10)))]
--Show shape on multiple views on multiple frames via move command, Rel, 5 frames
test14 = ([("Default",100,100), ("New",100,100)],[[DrawCirc 20 20 50 "Default" "orange", DrawCirc 20 20 50 "New" "orange"],[DrawCirc 22 22 50 "Default" "orange", DrawCirc 22 22 50 "New" "orange"], [DrawCirc 24 24 50 "Default" "orange", DrawCirc 24 24 50 "New" "orange"], [DrawCirc 26 26 50 "Default" "orange", DrawCirc 26 26 50 "New" "orange"], [DrawCirc 28 28 50 "Default" "orange", DrawCirc 28 28 50 "New" "orange"], [DrawCirc 30 30 50 "Default" "orange", DrawCirc 30 30 50 "New" "orange"]]) 
     ==  runProg 5
    [ Def (Viewdef "Default" (Const 100) (Const 100)), Def (Viewdef "New" (Const 100) (Const 100)), Def (Group "Group_A" ["Default", "New"]), Def (View "Group_A"), Def (Circle "oval" (Const 20) (Const 20) (Const 50) Orange), Com(Move ["oval"] (Rel (Const 10) (Const 10)))]
--Show multiple shapes, one after the other, on multiple frames on differenc views
test15 = ([("Default",100,100), ("New",100,100)],[[DrawRect 40 40 20 20 "Default" "blue", DrawCirc 20 20 50 "New" "orange"],[DrawRect 40 40 20 20 "Default" "blue", DrawCirc 22 22 50 "New" "orange"], [DrawRect 40 40 20 20 "Default" "blue", DrawCirc 24 24 50 "New" "orange"], [DrawRect 40 40 20 20 "Default" "blue", DrawCirc 26 26 50 "New" "orange"], [DrawRect 40 40 20 20 "Default" "blue", DrawCirc 28 28 50 "New" "orange"], [DrawRect 40 40 20 20 "Default" "blue", DrawCirc 30 30 50 "New" "orange"], [DrawRect 42 42 20 20 "Default" "blue", DrawCirc 30 30 50 "New" "orange"], [DrawRect 44 44 20 20 "Default" "blue", DrawCirc 30 30 50 "New" "orange"], [DrawRect 46 46 20 20 "Default" "blue", DrawCirc 30 30 50 "New" "orange"], [DrawRect 48 48 20 20 "Default" "blue", DrawCirc 30 30 50 "New" "orange"], [DrawRect 50 50 20 20 "Default" "blue", DrawCirc 30 30 50 "New" "orange"]]) 
     ==  runProg 5
    [ Def (Viewdef "Default" (Const 100) (Const 100)), Def (Rectangle "box" (Const 40) (Const 40) (Const 20) (Const 20) Blue),  Def (Viewdef "New" (Const 100) (Const 100)), Def (Circle "oval" (Const 20) (Const 20) (Const 50) Orange), Com(Move ["oval"] (Rel (Const 10) (Const 10))), Def (View "Default"), Com(Move ["box"] (Rel (Const 10) (Const 10)))]
--Show command Par; multiple shapes, at the same time, on multiple frames on differenc views
test16 = ([("Default",100,100), ("New",100,100)],[[DrawRect 40 40 20 20 "Default" "blue", DrawCirc 20 20 50 "New" "orange"],[DrawRect 35 35 20 20 "Default" "blue", DrawCirc 25 25 50 "New" "orange"], [DrawRect 30 30 20 20 "Default" "blue", DrawCirc 30 30 50 "New" "orange"]]) 
     ==  runProg 2
    [ Def (Viewdef "Default" (Const 100) (Const 100)), Def (Rectangle "box" (Const 40) (Const 40) (Const 20) (Const 20) Blue),  Def (Viewdef "New" (Const 100) (Const 100)), Def (Circle "oval" (Const 20) (Const 20) (Const 50) Orange), Def (Group "Group_A" ["Default", "New"]), Def (View "Group_A"), Com (Par (Move ["box"] (Abs (Const 30) (Const 30))) (Move ["oval"] (Abs (Const 30) (Const 30))))]
--Show command At.

test17 = ([("Default",100,100), ("New",100,100)],[[DrawRect 40 40 20 20 "Default" "blue", DrawRect 40 40 20 20 "New" "blue"],[DrawRect 40 40 20 20 "Default" "blue", DrawRect 45 45 20 20 "New" "blue"], [DrawRect 40 40 20 20 "Default" "blue", DrawRect 50 50 20 20 "New" "blue"]]) 
     ==  runProg 2
    [ Def (Viewdef "Default" (Const 100) (Const 100)), Def (Viewdef "New" (Const 100) (Const 100)), Def (Group "Group_A" ["Default", "New"]), Def (View "Group_A"), Def (Rectangle "box" (Const 40) (Const 40) (Const 20) (Const 20) Blue), Com (At (Move ["box"] (Abs (Const 50) (Const 50))) "New")]


--Taken form appendice a of assignment
test19 = ([("Default", 400, 400)], [[DrawRect 10 400 20 20 "Default" "green"], [DrawRect 10 200 20 20 "Default" "green"], [DrawRect 110 200 20 20 "Default" "green"], [DrawRect 110 400 20 20 "Default" "green"], [DrawRect 10 400 20 20 "Default" "green"]])
     ==  runProg 1
    [ Def (Viewdef "Default" (Const 400) (Const 400)), Def (Rectangle "box" (Const 10) (Const 400) (Const 20) (Const 20) Green), Com (Move ["box"] (Abs (Const 10) (Const 200))), Com (Move ["box"] (Rel (Const 100) (Const 0))), Com (Move ["box"] (Abs (Const 110) (Const 400))), Com (Move ["box"] (Rel (Minus (Const 0) (Const 100)) (Const 0)))]
--Taken from appendice b of assignment
test20 = ([("Default",400,400)],[[DrawRect 10 400 20 20 "Default" "green"],[DrawRect 10 200 20 20 "Default" "green"],[DrawRect 110 200 20 20 "Default" "green"],[DrawRect 110 400 20 20 "Default" "green"],[DrawRect 10 400 20 20 "Default" "green"]])
     ==  runProg 1
    [ Def (Viewdef "Default" (Const 400) (Const 400)), Def (Rectangle "box" (Const 10) (Const 400) (Const 20) (Const 20) Green), Com (Move ["box"] (Abs (Const 10) (Const 200))), Com (Move ["box"] (Rel (Const 100) (Const 0))), Com (Move ["box"] (Abs (Const 110) (Const 400))), Com (Move ["box"] (Rel (Minus (Const 0) (Const 100)) (Const 0)))]


tests :: [Bool]
tests = [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17, test19, test20]

test :: [Bool]
test = tests