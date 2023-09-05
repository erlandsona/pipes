module Pipes exposing
    ( Pipe(..)
    , Boolean, Option
    , pipe, patch
    , when, catch
    , Linked
    , nil, pre, head, tail
    , I, L, new, raise
    )

{-|

@docs Pipe
@docs Void, Unit, Boolean, Option
@docs pipe, patch
@docs when, catch

@docs Linked
@docs nil, pre, head, tail

@docs Tree

-}


{-| Pipe is the "anonymous" "Sum Type".
-}
type Pipe e a
    -- L Looks like a 90deg pipe, a Turn if you will.
    = L e
    -- The Roman numeral capital I looks like a Pipe fitting with no turns.
    | I a



-- {-| If type aliases were allowed to be recursive this would just be
--     type alias Void =
--         Either Void Void
-- Since they're not I'll cheat a bit and use Elm's built-in Never type cause it's the same thing.
-- No values inhabit this type.
-- -}
-- type alias Void =
--     Pipe Never Never
-- {-| This type has one value. (I ())
-- -}
-- type alias Unit =
--     Pipe Never ()
-- This type has two values. (I (I ())) and (L (I ()))


type alias Boolean =
    Pipe () ()


{-| This type has accounted for all errors and only inhabits valid values.
-- The Roman numeral capital I looks like a Pipe fitting with no turns.
-}
type alias I a =
    Pipe Never a



-- This type must be handled via `else` in order for the program to continue.


{-| L Looks like a 90deg pipe, a Turn if you will. -}
type alias L e =
    Pipe e Never


type alias Option a =
    Pipe () a


{-| Again no recursive type aliases so this is fine.
-}
type Linked a
    = Linked (Pipe () ( a, Linked a ))


nil : Linked a
nil =
    Linked (L ())


pre : a -> Linked a -> Linked a
pre a ls =
    Linked (I ( a, ls ))


head : Linked a -> Option a
head (Linked ls) =
    ls |> pipe (\( hd, _ ) -> hd)


tail : Linked a -> Option (Linked a)
tail (Linked ls) =
    ls |> pipe (\( _, tl ) -> tl)


type Tree a
    = Tree (Pipe (Pipe (Tree a) (Tree a)) a)


pipe : (a -> b) -> Pipe e a -> Pipe e b
pipe k p =
    case p of
        I data ->
            I (k data)

        L no ->
            L no


patch : (e -> a) -> Pipe (Pipe e f) a -> Pipe f a
patch k p =
    case p of
        I data ->
            I data

        L (I f) ->
            L f

        L (L e) ->
            I (k e)


raise : e -> Pipe e a
raise =
    L


new : a -> Pipe e a
new =
    I


when : (a -> Pipe e b) -> Pipe e a -> Pipe e b
when throwable p =
    case p of
        I data ->
            throwable data

        L no ->
            L no


catch : (e -> Pipe f a) -> Pipe e a -> Pipe f a
catch recover p =
    case p of
        I data ->
            I data

        L no ->
            recover no
