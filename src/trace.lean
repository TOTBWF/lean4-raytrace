--------------------------------------------------------------------------------
-- A Lean 4 Raytracer
--
-- Author: Reed Mullanix
--
-- Inspiration Drawn from https://raytracing.github.io/books/RayTracingInOneWeekend.html

--------------------------------------------------------------------------------
-- Weird missing stuff

-- This is pretty dumb, but it works. Because the stdlib
-- has no way of casting floats to integral types, we have to do a
-- binary search to find the correct value.
partial def UInt8.ofFloat (x : Float) : UInt8 :=
  if x >= 255 then 255
  else if x < 0 then 0
  else go 0 255

  where
    go (lo : Nat) (hi : Nat) : UInt8 :=
      let mid := (lo + hi)/2
      if hi <= lo then (UInt8.ofNat $ lo - 1)
      else if x < (Float.ofNat mid) then go lo mid else go (mid + 1) hi

-- Why does this not exist?
instance : Neg Float where
  neg x := 0 - x

--------------------------------------------------------------------------------
-- Our Basic Datatypes

structure V3 : Type where
  x : Float
  y : Float
  z : Float

inductive Object : Type where
| sphere : Float → V3 → Object

instance : Add V3 where
  add v1 v2 := ⟨ v1.x + v2.x, v1.y + v2.y, v1.z + v2.z ⟩

instance : Sub V3 where
  sub v1 v2 := ⟨ v1.x - v2.x, v1.y - v2.y, v1.z - v2.z ⟩

instance : Mul V3 where
  mul v1 v2 := ⟨ v1.x * v2.x, v1.y * v2.y, v1.z * v2.z ⟩

instance : HMul Float V3 V3 where
  hMul s v := ⟨ s * v.x, s * v.y, s * v.z ⟩

def dot (v1 v2 : V3) : Float := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z
infixl:70 " ∙ "  => dot

def length (v : V3) : Float := Float.sqrt $ v ∙ v
notation:100 "∥" v "∥" => length v

-- Numerically unstable but this is a toy
def normalize (v : V3) : V3 := (1 / ∥ v ∥) * v

structure Ray : Type where
  origin    : V3
  direction : V3

def ray (source target : V3) : Ray := ⟨ source, (target - source)  ⟩

def scale (r : Ray) (t : Float) : V3 := r.origin + t * r.direction

def intersect (r : Ray) (o : Object) : Option Float :=
  match o with
  | Object.sphere radius center =>
    let oc := r.origin - center
    let a := r.direction ∙ r.direction
    let b := 2*(oc ∙ r.direction)
    let c := (oc ∙ oc) - radius^2
    let discriminant : Float := b^2 - 4*a*c
    if discriminant < 0
      then Option.none
      else Option.some $ (- b - Float.sqrt discriminant)/(2.0 * a)

--------------------------------------------------------------------------------
-- Rendering

-- Stupid representation, but easy nonetheless
def Matrix (h w : Nat) (A : Type) := Fin h → Fin w → A

def iterate_ (n : Nat) (f : Fin (Nat.succ n) → IO Unit) : IO Unit :=
  Nat.fold (fun k m => m *> f (Fin.ofNat k)) n (return ())

def iterateRev_ (n : Nat) (f : Fin (Nat.succ n) → IO Unit) : IO Unit :=
  Nat.foldRev (fun k m => m *> f (Fin.ofNat k)) n (return ())

-- Note that we want to iterate the column values _backwards_, as PPM expects the top rows to come first.
def traverse_ {x y : Nat} {A : Type} (m : Matrix (Nat.succ x) (Nat.succ y) A) (act : A → IO Unit) : IO Unit :=
  iterateRev_ y (fun col => iterate_ x (fun row => act (m row col)))

structure Pixel : Type where
  r : UInt8
  g : UInt8
  b : UInt8

def Pixel.ofUnitVector (v : V3) : Pixel := ⟨ UInt8.ofFloat $ 255 * v.x , UInt8.ofFloat $ 255 * v.y, UInt8.ofFloat $ 255 * v.z ⟩

instance : ToString Pixel where
  toString pixel := s!"{pixel.r} {pixel.g} {pixel.b}"

def render (obj : Object) : Matrix 512 256 Pixel := λ x y =>
  -- Note that our camera is fixed at '⟨0,0,0⟩' and the viewport has a fixed size 4×2
  let r := ray ⟨0,0,0⟩ ⟨(Float.ofNat x)/128.0 - 2, (Float.ofNat y)/128 - 1, -1⟩
  match intersect r obj with
  | Option.some t =>
    let normal := normalize (scale r t - ⟨0, 0, -1⟩)
    Pixel.ofUnitVector $ (0.5 : Float) * (normal + ⟨1,1,1⟩)
  | Option.none =>
    let normal := normalize r.direction
    let t := 0.5 * (normal.y + 1.0)
    Pixel.ofUnitVector $ (1.0 - t) * (⟨1,1,1⟩ : V3) + t * (⟨0.5, 0.7, 1.0⟩ : V3)

--------------------------------------------------------------------------------
-- IO

open IO

-- Write out a matrix of pixels as a PPM image
def writePPM {x y : Nat} (path : String) (pixels : Matrix (Nat.succ x) (Nat.succ y) Pixel) : IO Unit := FS.withFile path FS.Mode.write $ fun handle => do
  -- Indicates that we are using the ASCII PPM format
  FS.Handle.putStrLn handle "P3"
  -- The dimensions of the image, in row column form.
  FS.Handle.putStrLn handle $ (toString x) ++ " " ++ (toString y)
  -- Channel depth for each pixel
  FS.Handle.putStrLn handle "255"
  -- Output all the pixel values
  traverse_ pixels $ fun pixel => FS.Handle.putStrLn handle $ toString pixel
  return ()

def main : IO UInt32 := do
  writePPM "img.ppm" $ render (Object.sphere 0.5 ⟨ 0, 0, -1.0 ⟩)
  return 0
