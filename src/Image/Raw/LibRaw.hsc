{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Image.Raw.LibRaw
  (Image.Raw.LibRaw.init
  , strError
  , recycle
  , openFile
  , getIParams
  , getImgOther
  , getLensInfo
  , IParams(..)
  , ImgOther(..)
  , LensInfo(..))
where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

data ThumbnailFormat = Unknown | Jpeg | Bitmap | Bitmap16 | Layer | Rollei
  deriving (Show, Eq)

-- https://www.libraw.org/docs/API-datastruct-eng.html
data LibRawPtr

#include <libraw/libraw.h>

data IParams = IParams { make :: String, model :: String, software :: String }
  deriving (Show)

-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/utils.html#writing-haskell-interfaces-to-c-code-hsc2hs
-- https://wiki.haskell.org/Foreign_Function_Interface
instance Storable IParams where
    sizeOf    _ = (#size libraw_iparams_t)
    alignment _ = (#alignment libraw_iparams_t)
    -- string handling: https://wiki.haskell.org/FFI_cook_book#Working_with_structs
    -- #ptr instead of #peek
    peek ptr = do
      mk <- peekCString $ (#ptr libraw_iparams_t, make) ptr
      md <- peekCString $ (#ptr libraw_iparams_t, model) ptr
      sw <- peekCString $ (#ptr libraw_iparams_t, software) ptr
      return IParams { make = mk, model = md, software = sw }
    poke ptr (IParams make model software) = undefined
      -- (#poke libraw_iparams_t, make) ptr make
      -- (#poke libraw_iparams_t, model) ptr model
      -- (#poke libraw_iparams_t, software) ptr software

data ImgOther =
  ImgOther { isoSpeed :: Float, shutter :: Float, aperture :: Float,
             focalLen :: Float, timestamp :: Int64 } deriving Show

instance Storable ImgOther where
  sizeOf    _ = (#size libraw_imgother_t)
  alignment _ = (#alignment libraw_imgother_t)
  peek ptr = do
    iso <- (#peek libraw_imgother_t, iso_speed) ptr
    s <- (#peek libraw_imgother_t, shutter) ptr
    a <- (#peek libraw_imgother_t, aperture) ptr
    mm <- (#peek libraw_imgother_t, focal_len) ptr
    ts <- (#peek libraw_imgother_t, timestamp) ptr
    return ImgOther { isoSpeed = iso, shutter = s, aperture = a,
                      focalLen = mm, timestamp = ts }
  poke _ _ = undefined

-- data CameraMount =
--   MountUnknown | MinoltaA | SonyE | CanonEF | CanonEFS | CanonEFM |
--   NikonF | NikonCX | FT | MFT | PentaxK | PentaxQ | Pentax645 |
--   FujiX |  LeicaM | LeicaR | LeicaS | SamsungNX | RicohModule |
--   SamsungNXM | LeicaT | ContaxN | SigmaX3F | LeicaSL | FixedLens

-- data CameraFormat = APSC | FF | MF | APSH | OneInch | FT

data LensInfo = LensInfo { lensMake :: String, lens :: String } deriving Show

instance Storable LensInfo where
  sizeOf    _ = (#size libraw_lensinfo_t)
  alignment _ = (#alignment libraw_lensinfo_t)
  peek ptr = do
    mk <- peekCString $ (#ptr libraw_lensinfo_t, LensMake) ptr
    nm <- peekCString $ (#ptr libraw_lensinfo_t, Lens) ptr
    return LensInfo {lensMake = mk, lens = nm }
  poke _ _ = undefined

type LibRawHandle = ForeignPtr LibRawPtr

foreign import ccall unsafe "libraw/libraw.h libraw_init"
  c_libraw_init :: CUInt -> IO (Ptr LibRawPtr)

-- Pay attention to the "&"!
foreign import ccall unsafe "libraw/libraw.h &libraw_close"
  c_libraw_close :: FunPtr(Ptr LibRawPtr -> IO ())

foreign import ccall unsafe "libraw/libraw.h libraw_recycle"
  c_libraw_recycle :: Ptr LibRawPtr -> IO ()

foreign import ccall unsafe "libraw/libraw.h libraw_strerror"
  c_libraw_strerror :: CInt -> CString

foreign import ccall unsafe "libraw/libraw.h libraw_open_file"
  c_libraw_open_file :: Ptr LibRawPtr -> CString -> IO CInt

foreign import ccall unsafe "libraw/libraw.h libraw_get_iparams"
  c_libraw_get_iparams :: Ptr LibRawPtr -> IO (Ptr IParams)

foreign import ccall unsafe "libraw/libraw.h libraw_get_imgother"
  c_libraw_get_imgother :: Ptr LibRawPtr -> IO (Ptr ImgOther)

foreign import ccall unsafe "libraw/libraw.h libraw_get_lensinfo"
  c_libraw_get_lensinfo :: Ptr LibRawPtr -> IO (Ptr LensInfo)

init :: IO (Maybe LibRawHandle)
init = do
  ptr <- c_libraw_init 0
  if ptr /= nullPtr then
    do
      -- automatic cleanup
      foreignPtr <- newForeignPtr c_libraw_close ptr
      return $ Just foreignPtr
    else return Nothing

strError :: Int -> IO String
strError errCode = peekCString $ c_libraw_strerror (fromIntegral errCode)

recycle :: LibRawHandle -> IO ()
recycle handle = withForeignPtr handle c_libraw_recycle

openFile :: LibRawHandle -> String -> IO Int
openFile handle filePath = do
  cPath <- newCString filePath
  cInt <- withForeignPtr handle (\ptr -> c_libraw_open_file ptr cPath)
  return $ fromIntegral cInt

getIParams :: LibRawHandle -> IO IParams
getIParams handle = do
  ptr <- withForeignPtr handle c_libraw_get_iparams
  peek ptr

getImgOther :: LibRawHandle -> IO ImgOther
getImgOther handle = do
  ptr <- withForeignPtr handle c_libraw_get_imgother
  peek ptr

getLensInfo :: LibRawHandle -> IO LensInfo
getLensInfo handle = do
  ptr <- withForeignPtr handle c_libraw_get_lensinfo
  peek ptr
