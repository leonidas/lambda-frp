
module Main where

import Lambda.OpenGL

import Logic
import Rendering

main :: IO ()
main = glInteract logic render