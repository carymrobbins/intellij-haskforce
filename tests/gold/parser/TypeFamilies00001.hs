module TypeFamilies00001 where

-- Mostly copy-pasta from https://stackoverflow.com/questions/20870432/type-family-vs-data-family-in-brief

class GMapKey k where
  data GMap k :: * -> *
  empty       :: GMap k v
  lookup      :: k -> GMap k v -> Maybe v
  insert      :: k -> v -> GMap k v -> GMap k v

class Collects ce where
  type Elem ce
  empty  :: ce
  insert :: Elem ce -> ce -> ce
  member :: Elem ce -> ce -> Bool
  toList :: ce -> [Elem ce]

type family Foo
data family Bar

class C where
   type Foo
   data Bar

-- Declare a family of type synonyms, called `Element`
-- `Element` has kind `* -> *`; it takes one parameter, which we call `container`
type family Element container

-- ByteString is a container for Word8, so...
-- The Element of a `S.ByteString` is a `Word8`
type instance Element S.ByteString = Word8

-- and the Element of a `L.ByteString` is also `Word8`
type instance Element L.ByteString = Word8

-- Declare a list-like data family
data family XList a

-- Declare a list-like instance for Char
data instance XList Char = XCons Char (XList Char) | XNil

-- Declare a number-like instance for ()
data instance XList () = XListUnit Int

-- ERROR: "Multiple declarations of `XListUnit'"
data instance XList () = XListUnit Bool
-- (Note: GHCI accepts this; the new declaration just replaces the previous one.)
