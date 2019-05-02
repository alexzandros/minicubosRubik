module Cubos 
open System


    type Color = 
    | RED
    | GREEN
    | BLUE
    | YELLOW
    | PINK
    | WHITE

    type SmallCube = SmallCube of (Color * Color * Color)

    type FaceTurn = 
    | Left  of int
    | Right of int

    type CubeRotation = 
    | XQuarterFront
    | XQuarterBack
    | XHalf
    | YQuarterLeft
    | YQuarterRight
    | YHalf
    | ZQuarterLeft
    | ZQuarterRight
    | ZHalf


    type Cube = Cube of ((SmallCube * SmallCube * SmallCube * SmallCube) * (SmallCube * SmallCube * SmallCube * SmallCube))

    let initialCube = Cube ((   SmallCube (RED,GREEN,WHITE), 
                                SmallCube (RED,WHITE,BLUE), 
                                SmallCube (RED,BLUE, PINK),
                                SmallCube (RED,PINK, GREEN)),
                            (   SmallCube (YELLOW,GREEN,WHITE), 
                                SmallCube (YELLOW,WHITE,BLUE), 
                                SmallCube (YELLOW,BLUE, PINK),
                                SmallCube (YELLOW,PINK, GREEN)))                    

    let rotateCube rot cube = 
        match cube with
        | Cube ((   SmallCube (c11,c12,c13),
                    SmallCube (c21,c22,c23),
                    SmallCube (c31,c32,c33),
                    SmallCube (c41,c42,c43)),
                (   SmallCube (c51,c52,c53),
                    SmallCube (c61,c62,c63),
                    SmallCube (c71,c72,c73),
                    SmallCube (c81,c82,c83))) ->
                        match rot with
                        | XQuarterFront     
                            -> Cube ((
                                        SmallCube (c53,c52,c51),
                                        SmallCube (c62,c61,c63),
                                        SmallCube (c22,c23,c21),
                                        SmallCube (c13,c11,c12)),
                                    (
                                        SmallCube (c82,c83,c81),
                                        SmallCube (c73,c71,c72),
                                        SmallCube (c33,c32,c31),
                                        SmallCube (c42,c41,c43)
                                    ))
                        | XQuarterBack      
                            ->   Cube ((
                                        SmallCube (c42,c43,c41),
                                        SmallCube (c33,c31,c32),
                                        SmallCube (c73,c72,c71),
                                        SmallCube (c82,c81,c83)),
                                    (
                                        SmallCube (c13,c12,c11),
                                        SmallCube (c22,c21,c23),
                                        SmallCube (c62,c63,c61),
                                        SmallCube (c53,c51,c52)
                                    ))
                        | XHalf             
                            ->   Cube ((
                                        SmallCube (c81,c83,c82),
                                        SmallCube (c71,c73,c72),
                                        SmallCube (c61,c63,c62),
                                        SmallCube (c51,c53,c52)),
                                    (
                                        SmallCube (c41,c43,c42),
                                        SmallCube (c31,c33,c32),
                                        SmallCube (c21,c23,c22),
                                        SmallCube (c11,c13,c12)
                                    ))
                        | YQuarterLeft
                            -> Cube ((
                                        SmallCube (c23,c21,c22),
                                        SmallCube (c63,c62,c61),
                                        SmallCube (c72,c71,c73),
                                        SmallCube (c32,c33,c31)),
                                    (
                                        SmallCube (c12,c11,c13),
                                        SmallCube (c52,c53,c51),
                                        SmallCube (c83,c81,c82),
                                        SmallCube (c43,c42,c41)
                                    ))
                        | YQuarterRight     
                            -> Cube ((
                                        SmallCube (c52,c51,c53),
                                        SmallCube (c12,c13,c11),
                                        SmallCube (c43,c41,c42),
                                        SmallCube (c83,c82,c81)),
                                    (
                                        SmallCube (c63,c61,c62),
                                        SmallCube (c23,c22,c21),
                                        SmallCube (c32,c31,c33),
                                        SmallCube (c72,c73,c71)
                                    ))
                        | YHalf             
                            -> Cube ((
                                        SmallCube (c61,c63,c62),
                                        SmallCube (c51,c53,c52),
                                        SmallCube (c81,c83,c82),
                                        SmallCube (c71,c73,c72)),
                                    (
                                        SmallCube (c21,c23,c22),
                                        SmallCube (c11,c13,c12),
                                        SmallCube (c41,c43,c42),
                                        SmallCube (c31,c33,c32)
                                    ))
                        | ZQuarterLeft      
                            -> Cube ((
                                        SmallCube (c21,c22,c23),
                                        SmallCube (c31,c32,c33),
                                        SmallCube (c41,c42,c43),
                                        SmallCube (c11,c12,c13)),
                                    (
                                        SmallCube (c61,c62,c63),
                                        SmallCube (c71,c72,c73),
                                        SmallCube (c81,c82,c83),
                                        SmallCube (c51,c52,c53)
                                    ))
                        | ZQuarterRight     
                            -> Cube ((
                                        SmallCube (c41,c42,c43),
                                        SmallCube (c11,c12,c13),
                                        SmallCube (c21,c22,c23),
                                        SmallCube (c31,c32,c33)),
                                    (
                                        SmallCube (c81,c82,c83),
                                        SmallCube (c51,c52,c53),
                                        SmallCube (c61,c62,c63),
                                        SmallCube (c71,c72,c73)
                                    ))
                        | ZHalf             
                            -> Cube ((
                                        SmallCube (c31,c32,c33),
                                        SmallCube (c41,c42,c43),
                                        SmallCube (c11,c12,c13),
                                        SmallCube (c21,c22,c23)),
                                    (
                                        SmallCube (c71,c72,c73),
                                        SmallCube (c81,c82,c83),
                                        SmallCube (c51,c52,c53),
                                        SmallCube (c61,c62,c63)
                                    ))
                    
    let turnFace turn cube = 
        match cube with
        | Cube ((   SmallCube (c11,c12,c13),
                    SmallCube (c21,c22,c23),
                    SmallCube (c31,c32,c33),
                    SmallCube (c41,c42,c43)),
                (   SmallCube (c51,c52,c53),
                    SmallCube (c61,c62,c63),
                    SmallCube (c71,c72,c73),
                    SmallCube (c81,c82,c83))) ->
                        match turn with
                        | Left _n   
                            -> Cube ((
                                        SmallCube (c21,c22,c23),
                                        SmallCube (c31,c32,c33),
                                        SmallCube (c41,c42,c43),
                                        SmallCube (c11,c12,c13)),
                                    (
                                        SmallCube (c51,c52,c53),
                                        SmallCube (c61,c62,c63),
                                        SmallCube (c71,c72,c73),
                                        SmallCube (c81,c82,c83)
                                    ))
                        | Right _n  
                            -> Cube ((
                                        SmallCube (c41,c42,c43),
                                        SmallCube (c11,c12,c13),
                                        SmallCube (c21,c22,c23),
                                        SmallCube (c31,c32,c33)),
                                    (
                                        SmallCube (c51,c52,c53),
                                        SmallCube (c61,c62,c63),
                                        SmallCube (c71,c72,c73),
                                        SmallCube (c81,c82,c83)
                                    ))

    let colorToString = 
        function
        | YELLOW    -> "&"
        | RED       -> "/"
        | GREEN     -> "+"
        | BLUE      -> "*"
        | WHITE     -> "-"
        | PINK      -> "$"

    let cubeToString =
        function
        | Cube ((   SmallCube (c11,c12,c13),
                    SmallCube (c21,c22,c23),
                    SmallCube (c31,c32,c33),
                    SmallCube (c41,c42,c43)),
                (   SmallCube (c51,c52,c53),
                    SmallCube (c61,c62,c63),
                    SmallCube (c71,c72,c73),
                    SmallCube (c81,c82,c83))) ->
                    let sb = System.Text.StringBuilder()
                    let agregar = Printf.bprintf sb 
                    for i in 0 .. 4 do
                        agregar "%s" (
                            (String.replicate (10 - i) " ") + 
                            (String.replicate 6 (colorToString c11)) + 
                            " " + 
                            (String.replicate 6 (colorToString c21)) + 
                            " " +
                            (String.replicate i (colorToString c23)) +
                            "\n")
                    for i in 5 .. 6 do
                        agregar "%s" (
                            (String.replicate (10 - i) " ") + 
                            (String.replicate 6 (colorToString c41)) + 
                            " " + 
                            (String.replicate 6 (colorToString c31)) + 
                            " " +
                            (String.replicate (i - 5) (colorToString c32)) + 
                            (String.replicate 5 (colorToString c23)) +
                            "\n")
                    for i in 7 .. 9 do
                        agregar "%s" (
                            (String.replicate (10 - i) " ") + 
                            (String.replicate 6 (colorToString c41)) + 
                            " " + 
                            (String.replicate 6 (colorToString c31)) + 
                            " " +
                            (String.replicate (i - 5) (colorToString c32)) + 
                            (String.replicate (11 - i) (colorToString c23)) +
                            " " +
                            (String.replicate (i - 7) (colorToString c63)) +
                            "\n")
                    for i in 10 .. 11 do
                        agregar "%s" (
                            (String.replicate 7 (colorToString c42)) + 
                            (String.replicate 7 (colorToString c33)) + 
                            (String.replicate 5 (colorToString c32)) + 
                            (String.replicate (11 - i) (colorToString c23)) +
                            " " +
                            (String.replicate (i - 7) (colorToString c63)) +
                            "\n")
                    for i in 12 .. 13 do
                        agregar "%s" (
                            (String.replicate 7 (colorToString c42)) + 
                            (String.replicate 7 (colorToString c33)) + 
                            (String.replicate (16 - i) (colorToString c32)) +
                            " " +
                            (String.replicate (i - 12) (colorToString c72)) +
                            (String.replicate 5 (colorToString c63)) +
                            "\n")
                    for i in 14 .. 16 do
                        agregar "%s" (
                            (String.replicate 7 (colorToString c42)) + 
                            (String.replicate 7 (colorToString c33)) + 
                            (String.replicate (16 - i) (colorToString c32)) +
                            " " +
                            (String.replicate (i - 12) (colorToString c72)) +
                            (String.replicate (18 - i) (colorToString c63)) +
                            (String.replicate (i - 13) " ") + 
                            "\n")
                    for i in 17 .. 18 do
                        agregar "%s" (
                            (String.replicate 7 (colorToString c82)) + 
                            (String.replicate 7 (colorToString c73)) + 
                            (String.replicate 5 (colorToString c72)) +
                            (String.replicate (18 - i) (colorToString c63)) +
                            (String.replicate (i - 13) " ") + 
                            "\n")
                    for i in 19 .. 23 do
                        agregar "%s" (
                            (String.replicate 7 (colorToString c82)) + 
                            (String.replicate 7 (colorToString c73)) + 
                            (String.replicate (23 - i) (colorToString c72)) +
                            (String.replicate (i - 13) " ") + 
                            "\n")
                    
                    sb.ToString()