{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{-
Module: Language.FFI

English:
  The Foreign Function Interface: the machinery that lets a Bern program call
  real C functions in a shared library (.dll on Windows, .so on Linux). This is
  what makes `foreign rand("msvcrt.dll") -> "int"` actually reach into msvcrt and
  call C's rand().

  The hard part of calling C is that C and Bern speak different "languages" at the
  byte level. C wants raw machine values in registers and memory with exact sizes;
  Bern has high-level values (Integer, Double, an ADT). So most of this file is
  TRANSLATION (called "marshalling"):
    * deciding the C type of each argument/return (BernCType, parseType, typeToCType);
    * turning a Bern Value into the bytes a C function expects (valueToFFIArg,
      and adtToStruct / packStruct* for passing whole structs);
    * turning the bytes C hands back into a Bern Value (callCFunctionUniversal's
      return handling, structToAdt).

  The actual "place the bytes in the right registers and jump" step is done by the
  `libffi` library (imported qualified as FFI); we just feed it typed arguments
  and a return type. Two more responsibilities live here: loading a library and
  finding a symbol in it (loadLib / getSymbol), which differ per OS and so are
  selected with CPP `#ifdef`; and a small registry of struct layouts so we know
  each field's offset and size.

  Haskell behaviour to keep in mind: working with C means working in IO with raw
  Ptr values, `poke` (write bytes to memory) and `peek` (read bytes back), and
  manual `mallocBytes`/`free`. Sizes and casts must be exact -- a wrong cast is a
  silent corruption, not a type error -- which is why the type tables here are so
  explicit.

Português:
  A Interface de Função Estrangeira: a maquinaria que permite a um programa Bern
  chamar funções C de verdade numa biblioteca compartilhada (.dll no Windows, .so
  no Linux). É isto que faz `foreign rand("msvcrt.dll") -> "int"` de fato alcançar
  o msvcrt e chamar o rand() do C.

  A parte difícil de chamar C é que C e Bern falam "idiomas" diferentes no nível
  de bytes. C quer valores de máquina crus em registradores e memória com tamanhos
  exatos; Bern tem valores de alto nível (Integer, Double, um ADT). Então a maior
  parte deste arquivo é TRADUÇÃO (chamada "marshalling"):
    * decidir o tipo C de cada argumento/retorno (BernCType, parseType, typeToCType);
    * transformar um Value de Bern nos bytes que uma função C espera (valueToFFIArg,
      e adtToStruct / packStruct* para passar structs inteiros);
    * transformar os bytes que o C devolve em um Value de Bern (o tratamento de
      retorno do callCFunctionUniversal, structToAdt).

  O passo de fato de "colocar os bytes nos registradores certos e saltar" é feito
  pela biblioteca `libffi` (importada qualificada como FFI); nós só a alimentamos
  com argumentos tipados e um tipo de retorno. Mais duas responsabilidades moram
  aqui: carregar uma biblioteca e achar um símbolo nela (loadLib / getSymbol), que
  diferem por SO e por isso são selecionados com CPP `#ifdef`; e um pequeno
  registro de layouts de struct para sabermos o offset e o tamanho de cada campo.

  Comportamento de Haskell a ter em mente: trabalhar com C significa trabalhar em
  IO com valores Ptr crus, `poke` (escrever bytes na memória) e `peek` (ler bytes
  de volta), e `mallocBytes`/`free` manuais. Tamanhos e casts precisam ser exatos
  -- um cast errado é uma corrupção silenciosa, não um erro de tipo -- e é por isso
  que as tabelas de tipo aqui são tão explícitas.
-}
module Language.FFI where

import Foreign
import Foreign.C.Types    (CInt(..), CDouble(..), CChar(..), CUChar(..), CFloat(..), CShort(..), CUShort(..))
import Foreign.C.String   (CString, newCString, peekCString, withCString,
                           castCharToCChar, castCCharToChar)
import Foreign.Ptr
import Control.Exception  (try, SomeException, catch)
import System.IO          (hPutStrLn, stderr)
import System.FilePath    (takeDirectory, normalise)
import Data.Word          (Word8, Word16, Word32, Word64)
import Data.Bits          (shiftL, (.|.))
import Data.IORef
import System.IO.Unsafe   (unsafePerformIO)
import qualified Data.Map.Strict as Map
import qualified Foreign.LibFFI as FFI

#ifdef mingw32_HOST_OS
import System.Win32.DLL      (getProcAddress, loadLibrary, freeLibrary)
import System.Win32.Types     (HMODULE, Addr, failIfNull)
import Foreign.Ptr            (Ptr, castPtrToFunPtr)
#else
import qualified System.Posix.DynamicLinker as Posix
#endif

import Language.Helpers
import Language.Ast

{-
English:
  The set of C types Bern can pass across the boundary. Each one corresponds to a
  specific machine representation with a known size, which the rest of the file
  reads off (getFieldSize) and matches on. `BernStruct name` is the catch-all for a
  user ADT mapped to a C struct.

Português:
  O conjunto de tipos C que o Bern consegue passar pela fronteira. Cada um
  corresponde a uma representação de máquina específica com tamanho conhecido, que
  o resto do arquivo lê (getFieldSize) e casa por padrão. `BernStruct name` é o
  coringa para um ADT do usuário mapeado a um struct C.
-}
-- Supported Bern types for FFI (ENHANCED WITH C-SPECIFIC TYPES)
data BernCType
    = BernInt           -- 32-bit signed int
    | BernDouble        -- 64-bit double
    | BernBool          -- 8-bit bool
    | BernChar          -- 8-bit char
    | BernString        -- char*
    | BernPtr           -- opaque pointer/handle (void*)
    | BernVoid          -- void
    | BernByte          -- 8-bit unsigned (uint8_t / unsigned char)
    | BernShort         -- 16-bit signed short
    | BernUShort        -- 16-bit unsigned short
    | BernLong          -- 32-bit signed (same as Int for most platforms)
    | BernFloat         -- 32-bit float
    | BernStruct String -- Custom struct
    deriving (Show, Eq)

{-
English:
  A struct's memory layout, described field by field. To read or write a C struct
  we need to know, for each field, its type, its byte OFFSET from the struct's
  start, and its SIZE. `StructLayout` bundles those for a whole struct. Layouts are
  kept in `structRegistry`, a single global mutable map (same unsafePerformIO +
  NOINLINE trick as elsewhere) so a struct described once can be reused on every
  later call without re-deriving it.

Português:
  O layout de memória de um struct, descrito campo a campo. Para ler ou escrever um
  struct C precisamos saber, para cada campo, seu tipo, seu OFFSET em bytes a
  partir do início do struct, e seu TAMANHO. `StructLayout` agrupa isso para um
  struct inteiro. Os layouts ficam em `structRegistry`, um único mapa mutável
  global (o mesmo truque de unsafePerformIO + NOINLINE de outros lugares) para que
  um struct descrito uma vez possa ser reutilizado em toda chamada seguinte sem
  re-derivá-lo.
-}
-- Field definition in a struct
data FieldDef = FieldDef
    { fieldName :: String
    , fieldType :: BernCType
    , fieldOffset :: Int
    , fieldSize :: Int
    } deriving (Show, Eq)

-- Struct layout definition
data StructLayout = StructLayout
    { structName :: String
    , structSize :: Int
    , structFields :: [FieldDef]
    } deriving (Show, Eq)

-- Global registry for struct layouts
{-# NOINLINE structRegistry #-}
structRegistry :: IORef (Map.Map String StructLayout)
structRegistry = unsafePerformIO $ newIORef Map.empty

-- Register a struct layout
registerStruct :: StructLayout -> IO ()
registerStruct layout = do
    modifyIORef' structRegistry $ Map.insert (structName layout) layout

-- Get a registered struct layout
getStructLayout :: String -> IO (Maybe StructLayout)
getStructLayout name = do
    registry <- readIORef structRegistry
    return $ Map.lookup name registry

{-
English:
  The byte size of each C type -- the numbers that drive offset arithmetic when
  laying out a struct. These are fixed by the C ABI (an int is 4 bytes, a double 8,
  ...); a pointer's size is asked of the platform via `sizeOf (nullPtr)` since it
  is 4 bytes on 32-bit and 8 on 64-bit.

Português:
  O tamanho em bytes de cada tipo C -- os números que guiam a aritmética de offset
  ao montar um struct. Eles são fixados pela ABI do C (um int tem 4 bytes, um
  double 8, ...); o tamanho de um ponteiro é perguntado à plataforma via
  `sizeOf (nullPtr)`, já que ele é 4 bytes em 32-bit e 8 em 64-bit.
-}
-- Helper function to calculate field sizes
getFieldSize :: BernCType -> Int
getFieldSize BernInt = 4      -- 32-bit int
getFieldSize BernDouble = 8   -- 64-bit double
getFieldSize BernBool = 1     -- 8-bit bool
getFieldSize BernChar = 1     -- 8-bit char
getFieldSize BernPtr = sizeOf (nullPtr :: Ptr ())
getFieldSize BernByte = 1     -- 8-bit unsigned char
getFieldSize BernShort = 2    -- 16-bit short
getFieldSize BernUShort = 2   -- 16-bit unsigned short
getFieldSize BernLong = 4     -- 32-bit long
getFieldSize BernFloat = 4    -- 32-bit float
getFieldSize _ = 0

-- Map Bern Type (from Ast.hs) to C type
typeToCType :: Type -> BernCType
typeToCType TInt = BernInt
typeToCType TDouble = BernDouble
typeToCType TBool = BernBool
typeToCType TChar = BernChar
typeToCType TString = BernString
typeToCType (TCustom "Ptr") = BernPtr
typeToCType (TCustom "Pointer") = BernPtr
typeToCType (TCustom "Byte") = BernByte
typeToCType (TCustom "UChar") = BernByte
typeToCType (TCustom "Short") = BernShort
typeToCType (TCustom "UShort") = BernUShort
typeToCType (TCustom "Float") = BernFloat
typeToCType (TCustom name) = BernStruct name
typeToCType TList = BernStruct "List"
typeToCType TSet = BernStruct "Set"
-- `Auto` has no fixed C representation; treat it as a generic pointer at the
-- FFI boundary. (Auto is meant for dynamic Bern values, not for C interop.)
typeToCType TAuto = BernPtr

{-
English:
  Two ways to FIGURE OUT a struct's layout when it wasn't registered explicitly.
  Both walk the fields left to right, placing each at the running offset and
  advancing by its size (a simple packed layout, no alignment padding):
    * inferStructLayoutFromDef uses the ADT's declared field TYPES (from
      `adt ... Double Int`), which is the accurate source.
    * inferStructLayoutFromValues is the fallback: it guesses each field's type
      from the runtime VALUE (e.g. a small positive Integer is assumed to be a
      byte). Less reliable, but lets a struct round-trip even without annotations.

Português:
  Duas formas de DESCOBRIR o layout de um struct quando ele não foi registrado
  explicitamente. Ambas percorrem os campos da esquerda para a direita, colocando
  cada um no offset corrente e avançando pelo seu tamanho (um layout compactado
  simples, sem padding de alinhamento):
    * inferStructLayoutFromDef usa os TIPOS de campo declarados do ADT (de
      `adt ... Double Int`), que é a fonte precisa.
    * inferStructLayoutFromValues é o fallback: adivinha o tipo de cada campo pelo
      VALOR de runtime (ex.: um Integer positivo pequeno é assumido como byte).
      Menos confiável, mas permite que um struct faça ida e volta mesmo sem
      anotações.
-}
-- UNIVERSAL FIX: Infer struct layout from ADT definition (uses type annotations!)
inferStructLayoutFromDef :: String -> [Type] -> StructLayout
inferStructLayoutFromDef typeName types =
    let fields = inferFields types 0
        totalSize = if null fields then 0 else fieldOffset (last fields) + fieldSize (last fields)
    in StructLayout typeName totalSize fields
  where
    inferFields :: [Type] -> Int -> [FieldDef]
    inferFields [] _ = []
    inferFields (t:ts) offset =
        let ctype = typeToCType t
            fsize = getFieldSize ctype
            field = FieldDef ("field" ++ show (length ts)) ctype offset fsize
            nextOffset = offset + fsize
        in field : inferFields ts nextOffset

-- Fallback: Infer from runtime values (less accurate but still universal)
inferStructLayoutFromValues :: String -> [Value] -> StructLayout
inferStructLayoutFromValues typeName values =
    let fields = inferFields values 0
        totalSize = if null fields then 0 else fieldOffset (last fields) + fieldSize (last fields)
    in StructLayout typeName totalSize fields
  where
    inferFields :: [Value] -> Int -> [FieldDef]
    inferFields [] _ = []
    inferFields (v:vs) offset =
        let (ftype, fsize) = inferFieldType v
            field = FieldDef ("field" ++ show (length vs)) ftype offset fsize
            nextOffset = offset + fsize
        in field : inferFields vs nextOffset
    
    inferFieldType :: Value -> (BernCType, Int)
    inferFieldType (Integer n) 
        | n >= 0 && n <= 255 = (BernByte, 1)  -- Assume byte for small positive ints
        | otherwise = (BernInt, 4)
    inferFieldType (Double _) = (BernDouble, 8)
    inferFieldType (Boolean _) = (BernBool, 1)
    inferFieldType (Character _) = (BernChar, 1)
    inferFieldType _ = (BernInt, 4)

{-
English:
  Turn a type name written in a `foreign` declaration (a plain string like "int"
  or "double") into a BernCType. Anything it does not recognise is assumed to be a
  user struct name, so it always returns Just (never fails here -- a truly bad name
  surfaces later as a struct with no registered layout). getReturnType is just an
  alias, named for readability at the call site.

Português:
  Transforma um nome de tipo escrito numa declaração `foreign` (uma string simples
  como "int" ou "double") em um BernCType. Qualquer coisa que ele não reconheça é
  assumida como um nome de struct do usuário, então sempre retorna Just (nunca
  falha aqui -- um nome de fato ruim aparece depois como um struct sem layout
  registrado). getReturnType é só um alias, nomeado para legibilidade no ponto de
  chamada.
-}
parseType :: String -> Maybe BernCType
parseType "int"    = Just BernInt
parseType "double" = Just BernDouble
parseType "float"  = Just BernFloat
parseType "bool"   = Just BernBool
parseType "char"   = Just BernChar
parseType "byte"   = Just BernByte
parseType "uchar"  = Just BernByte
parseType "short"  = Just BernShort
parseType "ushort" = Just BernUShort
parseType "string" = Just BernString
parseType "ptr"    = Just BernPtr
parseType "pointer" = Just BernPtr
parseType "void*"  = Just BernPtr
parseType "void"   = Just BernVoid
parseType typeName = Just (BernStruct typeName)

getReturnType :: String -> Maybe BernCType
getReturnType = parseType

{-
English:
  Loading a shared library and looking up a function in it are done with totally
  different OS calls on Windows vs Unix, so this whole section is split with CPP
  `#ifdef mingw32_HOST_OS`. The compiler keeps only the branch for the platform it
  is building on.
    * LibHandle hides whichever handle type the OS uses behind one name.
    * On Windows we also nudge the DLL search path toward the library's own folder
      (SetDllDirectory/AddDllDirectory) so a .dll that depends on sibling .dlls can
      find them.
    * loadLib opens the library, getSymbol resolves a function name to a raw code
      pointer, and closeLib releases it.

Português:
  Carregar uma biblioteca compartilhada e procurar uma função nela são feitos com
  chamadas de SO totalmente diferentes no Windows e no Unix, então esta seção
  inteira é dividida com CPP `#ifdef mingw32_HOST_OS`. O compilador mantém só o
  ramo da plataforma em que está compilando.
    * LibHandle esconde, atrás de um nome só, qualquer que seja o tipo de handle do SO.
    * No Windows também empurramos o caminho de busca de DLL na direção da pasta da
      própria biblioteca (SetDllDirectory/AddDllDirectory) para que uma .dll que
      dependa de .dlls vizinhas consiga encontrá-las.
    * loadLib abre a biblioteca, getSymbol resolve um nome de função para um
      ponteiro de código cru, e closeLib a libera.
-}
-- Platform-independent library handle
data LibHandle
#ifdef mingw32_HOST_OS
    = WinLib HMODULE
#else
    = PosixLib Posix.DL
#endif

#ifdef mingw32_HOST_OS
-- Windows-specific: Add DLL directory to search path
foreign import ccall unsafe "windows.h SetDllDirectoryA"
    c_SetDllDirectory :: CString -> IO Bool

foreign import ccall unsafe "windows.h AddDllDirectory"
    c_AddDllDirectory :: CString -> IO (Ptr ())

setDllDirectory :: FilePath -> IO ()
setDllDirectory path = withCString path $ \cpath -> do
    _ <- c_SetDllDirectory cpath
    return ()

addDllDirectory :: FilePath -> IO ()
addDllDirectory path = withCString path $ \cpath -> do
    _ <- c_AddDllDirectory cpath
    return ()
#endif

-- Load shared library (cross-platform)
loadLib :: FilePath -> IO LibHandle
#ifdef mingw32_HOST_OS
loadLib path = do
    let normalizedPath = normalise path
    let dllDir = takeDirectory normalizedPath
    catch (addDllDirectory dllDir) (\(_ :: SomeException) -> return ())
    catch (setDllDirectory dllDir) (\(_ :: SomeException) -> return ())
    h <- loadLibrary normalizedPath
    return (WinLib h)
#else
loadLib path = do
    h <- Posix.dlopen path [Posix.RTLD_LAZY, Posix.RTLD_GLOBAL]
    return (PosixLib h)
#endif

-- Get function pointer from symbol name (cross-platform)
getSymbol :: LibHandle -> String -> IO (Ptr ())
#ifdef mingw32_HOST_OS
getSymbol (WinLib h) name = do
    failIfNull "getProcAddress" $
        getProcAddress h name
#else
getSymbol (PosixLib h) name = do
    fp <- Posix.dlsym h name
    return (castFunPtrToPtr fp)
#endif

-- | Close library handle
closeLib :: LibHandle -> IO ()
#ifdef mingw32_HOST_OS
closeLib (WinLib h) = freeLibrary h >> return ()
#else
closeLib (PosixLib h) = Posix.dlclose h >> return ()
#endif

{-
English:
  A looser "value to string" than Helpers.valueToString: it also accepts numbers,
  booleans and a single character (rendering each), falling back to the strict one
  for the rest. Used where a C call wants a string-ish argument.

Português:
  Um "valor para string" mais frouxo que o Helpers.valueToString: ele também aceita
  números, booleanos e um único caractere (renderizando cada um), recorrendo ao
  estrito para o resto. Usado onde uma chamada C quer um argumento tipo string.
-}
valueToString' :: Value -> Maybe String
valueToString' (Integer n)     = Just (show n)
valueToString' (Double d)      = Just (show d)
valueToString' (Boolean True)  = Just "true"
valueToString' (Boolean False) = Just "false"
valueToString' (Character c)   = Just [c]
valueToString' v               = valueToString v

{-
English:
  Build a C struct in memory from a Bern ADT and return a pointer to it. Steps:
  look up (or infer + register) the layout, malloc exactly `structSize` bytes, then
  walk the fields writing each Bern value into its slot with `poke` at the field's
  offset. The big case in `fillFields` is just "for each (field C type, Bern value)
  pair, cast the pointer to the right C type and poke the bytes"; pairs that don't
  line up are skipped.

  Memory note: this mallocs and does NOT free -- the pointer is handed to the C
  call, and for the short-lived calls Bern makes that is acceptable (the OS
  reclaims it on exit). A long-running program doing many large-struct calls would
  want explicit frees.

Português:
  Constrói um struct C na memória a partir de um ADT de Bern e retorna um ponteiro
  para ele. Passos: busca (ou infere + registra) o layout, faz malloc de exatamente
  `structSize` bytes, e então percorre os campos escrevendo cada valor Bern no seu
  espaço com `poke` no offset do campo. O grande case em `fillFields` é só "para
  cada par (tipo C do campo, valor Bern), faz cast do ponteiro para o tipo C certo
  e poke dos bytes"; pares que não se alinham são pulados.

  Nota de memória: isto faz malloc e NÃO faz free -- o ponteiro é entregue à
  chamada C, e para as chamadas de vida curta que o Bern faz isso é aceitável (o SO
  recupera na saída). Um programa de longa duração fazendo muitas chamadas com
  structs grandes iria querer frees explícitos.
-}
-- Convert Bern ADT to C struct
adtToStruct :: String -> [Value] -> IO (Maybe (Ptr ()))
adtToStruct typeName values = do
    maybeLayout <- getStructLayout typeName
    layout <- case maybeLayout of
        Just l -> return l
        -- Auto-infer layout if not registered
        Nothing -> do
            let inferredLayout = inferStructLayoutFromValues typeName values
            registerStruct inferredLayout
            return inferredLayout
    
    ptr <- mallocBytes (structSize layout)
    fillFields ptr (structFields layout) values
    return (Just (castPtr ptr))
  where
    fillFields :: Ptr Word8 -> [FieldDef] -> [Value] -> IO ()
    fillFields _ [] _ = return ()
    fillFields _ _ [] = return ()
    fillFields ptr (field:fields) (val:vals) = do
        let offset = fieldOffset field
        case (fieldType field, val) of
            (BernInt, Integer n) -> do
                let fieldPtr = plusPtr ptr offset
                poke (castPtr fieldPtr :: Ptr CInt) (fromIntegral n)
            
            (BernByte, Integer n) -> do  -- Proper byte handling!
                let fieldPtr = plusPtr ptr offset
                poke (castPtr fieldPtr :: Ptr Word8) (fromIntegral n)
            
            (BernShort, Integer n) -> do  -- 16-bit short
                let fieldPtr = plusPtr ptr offset
                poke (castPtr fieldPtr :: Ptr CShort) (fromIntegral n)
            
            (BernUShort, Integer n) -> do  -- 16-bit unsigned
                let fieldPtr = plusPtr ptr offset
                poke (castPtr fieldPtr :: Ptr CUShort) (fromIntegral n)
            
            (BernFloat, Double d) -> do  -- 32-bit float
                let fieldPtr = plusPtr ptr offset
                poke (castPtr fieldPtr :: Ptr CFloat) (realToFrac d)
            
            (BernDouble, Double d) -> do
                let fieldPtr = plusPtr ptr offset
                poke (castPtr fieldPtr :: Ptr CDouble) (realToFrac d)
            
            (BernBool, Boolean b) -> do
                let fieldPtr = plusPtr ptr offset
                poke (castPtr fieldPtr :: Ptr Word8) (if b then 1 else 0)
            
            (BernChar, Character c) -> do
                let fieldPtr = plusPtr ptr offset
                poke (castPtr fieldPtr :: Ptr CChar) (castCharToCChar c)
            
            _ -> return ()
        fillFields ptr fields vals

{-
English:
  The reverse of adtToStruct: read a C struct back into a Bern ADT value. Look up
  the layout, then `peek` each field from its offset and wrap the bytes in the
  matching Bern value (a CInt becomes Integer, a CDouble becomes Double, ...). The
  result is an AlgebraicDataType carrying the recovered fields. Returns Nothing if
  the type has no registered layout (we wouldn't know how to read it).

Português:
  O inverso do adtToStruct: lê um struct C de volta para um valor ADT de Bern.
  Busca o layout, e então faz `peek` de cada campo do seu offset e embrulha os
  bytes no valor Bern correspondente (um CInt vira Integer, um CDouble vira Double,
  ...). O resultado é um AlgebraicDataType carregando os campos recuperados.
  Retorna Nothing se o tipo não tem layout registrado (não saberíamos como lê-lo).
-}
-- Convert C struct back to Bern ADT
structToAdt :: String -> Ptr () -> IO (Maybe Value)
structToAdt typeName ptr = do
    maybeLayout <- getStructLayout typeName
    case maybeLayout of
        Nothing -> return Nothing
        Just layout -> do
            values <- mapM (readField (castPtr ptr)) (structFields layout)
            return $ Just (AlgebraicDataType typeName values)
  where
    readField :: Ptr Word8 -> FieldDef -> IO Value
    readField basePtr field = do
        let ptr = plusPtr basePtr (fieldOffset field)
        case fieldType field of
            BernInt -> do
                val <- peek (castPtr ptr :: Ptr CInt)
                return $ Integer (fromIntegral val)
            BernByte -> do
                val <- peek (castPtr ptr :: Ptr Word8)
                return $ Integer (fromIntegral val)
            BernShort -> do
                val <- peek (castPtr ptr :: Ptr CShort)
                return $ Integer (fromIntegral val)
            BernUShort -> do
                val <- peek (castPtr ptr :: Ptr CUShort)
                return $ Integer (fromIntegral val)
            BernFloat -> do
                val <- peek (castPtr ptr :: Ptr CFloat)
                return $ Double (realToFrac val)
            BernDouble -> do
                val <- peek (castPtr ptr :: Ptr CDouble)
                return $ Double (realToFrac val)
            BernBool -> do
                val <- peek (castPtr ptr :: Ptr Word8)
                return $ Boolean (val /= 0)
            BernChar -> do
                val <- peek (castPtr ptr :: Ptr CChar)
                return $ Character (castCCharToChar val)
            _ -> return Undefined

{-
English:
  The same field-writing loop as adtToStruct's `fillFields`, but standalone so the
  pack* helpers below can reuse it on a tiny buffer. (The two exist separately for
  historical reasons; both poke each value at its field offset.)

Português:
  O mesmo laço de escrita de campos do `fillFields` do adtToStruct, mas autônomo
  para que os auxiliares pack* abaixo possam reutilizá-lo num buffer minúsculo. (Os
  dois existem separadamente por razões históricas; ambos fazem poke de cada valor
  no offset do seu campo.)
-}
-- Helper to fill bytes in struct layout order
fillStructBytes :: Ptr Word8 -> [FieldDef] -> [Value] -> IO ()
fillStructBytes _ [] _ = return ()
fillStructBytes _ _ [] = return ()
fillStructBytes ptr (field:fields) (val:vals) = do
    let offset = fieldOffset field
    case (fieldType field, val) of
        (BernByte, Integer n) -> do
            let fieldPtr = plusPtr ptr offset
            poke fieldPtr (fromIntegral n :: Word8)
        
        (BernInt, Integer n) -> do
            let fieldPtr = plusPtr ptr offset
            poke (castPtr fieldPtr :: Ptr CInt) (fromIntegral n)
        
        (BernShort, Integer n) -> do
            let fieldPtr = plusPtr ptr offset
            poke (castPtr fieldPtr :: Ptr CShort) (fromIntegral n)
        
        (BernUShort, Integer n) -> do
            let fieldPtr = plusPtr ptr offset
            poke (castPtr fieldPtr :: Ptr CUShort) (fromIntegral n)
        
        (BernFloat, Double d) -> do
            let fieldPtr = plusPtr ptr offset
            poke (castPtr fieldPtr :: Ptr CFloat) (realToFrac d)
        
        (BernDouble, Double d) -> do
            let fieldPtr = plusPtr ptr offset
            poke (castPtr fieldPtr :: Ptr CDouble) (realToFrac d)
        
        (BernBool, Boolean b) -> do
            let fieldPtr = plusPtr ptr offset
            poke fieldPtr (if b then 1 else 0 :: Word8)
        
        (BernChar, Character c) -> do
            let fieldPtr = plusPtr ptr offset
            poke (castPtr fieldPtr :: Ptr CChar) (castCharToCChar c)
        
        _ -> return ()
    fillStructBytes ptr fields vals

{-
English:
  How a SMALL struct is passed by value. On x86-64, a struct that fits in 4 (or 8)
  bytes is not passed in memory at all -- the compiler crams its bytes into a single
  CPU register. libffi has no "small struct" argument, so we mimic the convention by
  hand: write the fields into a 4-byte (or 8-byte) buffer, then read that buffer back
  as one Word32 (or Word64) and pass THAT integer as the argument. The receiving C
  function sees its struct in the register exactly as expected.

Português:
  Como um struct PEQUENO é passado por valor. No x86-64, um struct que cabe em 4 (ou
  8) bytes não é passado pela memória de jeito nenhum -- o compilador enfia seus
  bytes num único registrador da CPU. O libffi não tem um argumento de "struct
  pequeno", então imitamos a convenção na mão: escrevemos os campos num buffer de 4
  bytes (ou 8), e então lemos esse buffer de volta como um Word32 (ou Word64) e
  passamos ESSE inteiro como argumento. A função C que recebe vê o struct dela no
  registrador exatamente como esperado.
-}
-- Pack struct fields into Word32 for passing small structs (≤4 bytes) by value
-- This matches x86-64 calling convention where small structs go in registers
packStructWord32 :: StructLayout -> [Value] -> IO Word32
packStructWord32 layout values = do
    -- Create a byte buffer
    buffer <- mallocBytes 4 :: IO (Ptr Word8)
    
    -- Fill the buffer with struct fields
    fillStructBytes buffer (structFields layout) values
    
    -- Read as Word32 (matches the CPU register size)
    result <- peek (castPtr buffer :: Ptr Word32)
    free buffer
    return result

-- Pack struct fields into Word64 for passing structs (5-8 bytes) by value
packStructWord64 :: StructLayout -> [Value] -> IO Word64
packStructWord64 layout values = do
    -- Create a byte buffer
    buffer <- mallocBytes 8 :: IO (Ptr Word8)
    
    -- Fill the buffer with struct fields
    fillStructBytes buffer (structFields layout) values
    
    -- Read as Word64
    result <- peek (castPtr buffer :: Ptr Word64)
    free buffer
    return result

{-
English:
  Convert one Bern value into one typed libffi argument, guided by the declared C
  type. Each clause matches an expected (C type, Bern value) pair and wraps it with
  the right FFI.arg* constructor. A few notes:
    * libffi has no short/char argument helpers, so 16-bit values go through
      argWord16 and bytes through argWord8.
    * A string is copied into a fresh C string (newCString) and passed as a pointer.
    * A pointer can be carried as a plain Integer (an address) -- that is how an
      opaque handle returned by an earlier call is passed back in.
    * A struct argument picks small-by-value (Word32/Word64) or by-pointer based on
      its size, reusing the pack*/adtToStruct helpers.
  Anything unmatched falls through to a null pointer rather than crashing.

Português:
  Converte um valor Bern em um argumento tipado do libffi, guiado pelo tipo C
  declarado. Cada cláusula casa um par esperado (tipo C, valor Bern) e o embrulha
  com o construtor FFI.arg* certo. Algumas notas:
    * o libffi não tem auxiliares de argumento short/char, então valores de 16 bits
      passam por argWord16 e bytes por argWord8.
    * Uma string é copiada para uma nova string C (newCString) e passada como ponteiro.
    * Um ponteiro pode ser carregado como um Integer simples (um endereço) -- é assim
      que um handle opaco retornado por uma chamada anterior é passado de volta.
    * Um argumento de struct escolhe pequeno-por-valor (Word32/Word64) ou por
      ponteiro conforme seu tamanho, reutilizando os auxiliares pack*/adtToStruct.
  Qualquer coisa não casada cai para um ponteiro nulo em vez de quebrar.
-}
valueToFFIArg :: BernCType -> Value -> IO FFI.Arg
valueToFFIArg BernInt (Integer n) =
    return $ FFI.argCInt (fromIntegral n)

valueToFFIArg BernByte (Integer n) = 
    return $ FFI.argWord8 (fromIntegral n)

-- Short types: libffi doesn't have argCShort, so we pass as Word16
valueToFFIArg BernShort (Integer n) = 
    return $ FFI.argWord16 (fromIntegral n)

valueToFFIArg BernUShort (Integer n) = 
    return $ FFI.argWord16 (fromIntegral n)

valueToFFIArg BernFloat (Double d) = 
    return $ FFI.argCFloat (realToFrac d)

valueToFFIArg BernDouble (Double d) = 
    return $ FFI.argCDouble (realToFrac d)

valueToFFIArg BernBool (Boolean b) = 
    return $ FFI.argWord8 (if b then 1 else 0)

valueToFFIArg BernChar (Character c) = 
    return $ FFI.argCChar (castCharToCChar c)

valueToFFIArg BernString val = do
    case valueToString val of
        Just s -> do
            cstr <- newCString s
            return $ FFI.argPtr cstr
        Nothing -> 
            return $ FFI.argPtr nullPtr

valueToFFIArg BernPtr (Integer n) =
    let ptr = intPtrToPtr (fromIntegral n :: IntPtr)
    in return $ FFI.argPtr (ptr :: Ptr ())
valueToFFIArg BernPtr Undefined =
    return $ FFI.argPtr nullPtr

valueToFFIArg (BernStruct typeName) (AlgebraicDataType name values)
    | typeName == name = do
        maybeLayout <- getStructLayout typeName
        case maybeLayout of
            Just layout -> do
                let size = structSize layout
                if size <= 4 then do
                    packed <- packStructWord32 layout values
                    return $ FFI.argWord32 packed
                else if size <= 8 then do

                    packed <- packStructWord64 layout values
                    return $ FFI.argWord64 packed
                else do

                    maybeStructPtr <- adtToStruct typeName values
                    case maybeStructPtr of
                        Just structPtr -> return $ FFI.argPtr structPtr
                        Nothing -> return $ FFI.argPtr nullPtr
            Nothing -> return $ FFI.argPtr nullPtr

valueToFFIArg _ _ = return $ FFI.argPtr nullPtr

{-
English:
  The actual call. Given a function pointer, the C types of its arguments and
  return, and the Bern argument values, it:
    1. refuses if the argument count is wrong (returns Undefined);
    2. converts every argument with valueToFFIArg;
    3. dispatches on the RETURN type, because libffi needs to be told the return
       representation up front (retCInt, retCDouble, ...), and then wraps whatever
       comes back into the matching Bern value.
  The string return reads the C char* back into a Bern character-list; a pointer
  return is handed back as an Integer address; a struct return is rebuilt with
  structToAdt; void returns Undefined.

Português:
  A chamada de fato. Dado um ponteiro de função, os tipos C de seus argumentos e
  retorno, e os valores de argumento Bern, ela:
    1. recusa se a contagem de argumentos estiver errada (retorna Undefined);
    2. converte cada argumento com valueToFFIArg;
    3. decide pelo tipo de RETORNO, porque o libffi precisa ser informado da
       representação de retorno de antemão (retCInt, retCDouble, ...), e então
       embrulha o que voltar no valor Bern correspondente.
  O retorno de string lê o char* do C de volta numa lista de caracteres de Bern; um
  retorno de ponteiro é devolvido como um endereço Integer; um retorno de struct é
  reconstruído com structToAdt; void retorna Undefined.
-}
callCFunctionUniversal :: Ptr () -> [BernCType] -> BernCType -> [Value] -> IO Value
callCFunctionUniversal funPtr argTypes retType args = do
    if length args /= length argTypes
        then return Undefined
        else do

            ffiArgs <- mapM (uncurry valueToFFIArg) (zip argTypes args)
            
            -- Call based on return type
            case retType of
                BernInt -> do
                    (result :: CInt) <- FFI.callFFI (castPtrToFunPtr funPtr) FFI.retCInt ffiArgs
                    return $ Integer (fromIntegral result)
                
                BernByte -> do
                    (result :: Word8) <- FFI.callFFI (castPtrToFunPtr funPtr) FFI.retWord8 ffiArgs
                    return $ Integer (fromIntegral result)
                
                -- Short types: libffi doesn't have retCShort, use retWord16
                BernShort -> do
                    (result :: Word16) <- FFI.callFFI (castPtrToFunPtr funPtr) FFI.retWord16 ffiArgs
                    return $ Integer (fromIntegral (fromIntegral result :: CShort))
                
                BernUShort -> do
                    (result :: Word16) <- FFI.callFFI (castPtrToFunPtr funPtr) FFI.retWord16 ffiArgs
                    return $ Integer (fromIntegral result)
                
                BernFloat -> do
                    (result :: CFloat) <- FFI.callFFI (castPtrToFunPtr funPtr) FFI.retCFloat ffiArgs
                    return $ Double (realToFrac result)
                
                BernDouble -> do
                    (result :: CDouble) <- FFI.callFFI (castPtrToFunPtr funPtr) FFI.retCDouble ffiArgs
                    return $ Double (realToFrac result)
                
                BernBool -> do
                    (result :: Word8) <- FFI.callFFI (castPtrToFunPtr funPtr) FFI.retWord8 ffiArgs
                    return $ Boolean (result /= 0)
                
                BernChar -> do
                    (result :: CChar) <- FFI.callFFI (castPtrToFunPtr funPtr) FFI.retCChar ffiArgs
                    return $ Character (castCCharToChar result)
                
                BernString -> do
                    (cstrPtr :: CString) <- FFI.callFFI (castPtrToFunPtr funPtr) (FFI.retPtr FFI.retCChar) ffiArgs
                    if cstrPtr == nullPtr
                        then return $ List [] 0
                        else do
                            str <- peekCString cstrPtr
                            return $ List (map Character str) (length str)

                BernPtr -> do
                    (rawPtr :: Ptr Word8) <- FFI.callFFI (castPtrToFunPtr funPtr) (FFI.retPtr FFI.retWord8) ffiArgs
                    let asInt = fromIntegral (ptrToIntPtr (castPtr rawPtr :: Ptr ()))
                    return $ Integer asInt
                 
                BernStruct typeName -> do
                    (structPtr :: Ptr Word8) <- FFI.callFFI (castPtrToFunPtr funPtr) (FFI.retPtr FFI.retWord8) ffiArgs
                    if structPtr == nullPtr
                        then return Undefined
                        else do
                            maybeVal <- structToAdt typeName (castPtr structPtr :: Ptr ())
                            case maybeVal of
                                Just val -> return val
                                Nothing -> return Undefined
                
                BernVoid -> do
                    FFI.callFFI (castPtrToFunPtr funPtr) FFI.retVoid ffiArgs
                    return Undefined

{-
English:
  The public face of this module, called by the evaluator when it runs a `foreign`
  declaration. loadCFunction validates the argument and return type strings, then
  loadAndCall opens the library, resolves the symbol, and returns a CLOSURE: a
  Haskell function `[Value] -> IO Value` that Bern can store as a CBinding and call
  like any other function. Each call into that closure runs the C function and
  marshals the result.

  Error handling is layered with `try`/`catch`: a missing library or symbol is
  reported (and turned into a Left/error message), and a failure DURING a call is
  caught so it returns Undefined instead of taking the whole interpreter down.

Português:
  A face pública deste módulo, chamada pelo avaliador quando ele roda uma declaração
  `foreign`. loadCFunction valida as strings de tipo de argumento e retorno, e então
  loadAndCall abre a biblioteca, resolve o símbolo, e retorna uma CLOSURE: uma função
  Haskell `[Value] -> IO Value` que o Bern pode guardar como um CBinding e chamar
  como qualquer outra função. Cada chamada nessa closure roda a função C e faz o
  marshalling do resultado.

  O tratamento de erro é em camadas com `try`/`catch`: uma biblioteca ou símbolo
  ausente é reportado (e virado em um Left/mensagem de erro), e uma falha DURANTE uma
  chamada é capturada para que retorne Undefined em vez de derrubar o interpretador
  inteiro.
-}
-- Cross platform function loader
loadCFunction
    :: String
    -> String
    -> [String]
    -> String
    -> IO (Either String ([Value] -> IO Value))
loadCFunction libPath funcName argTypeStrings retTypeStr = do
    let argTypesMb = map parseType argTypeStrings
    if any (== Nothing) argTypesMb
        then return $ Left $ "Invalid argument type(s) in FFI for " ++ funcName
        else case parseType retTypeStr of
            Nothing -> return $ Left $ "Invalid return type '" ++ retTypeStr ++ "' for " ++ funcName
            Just retType -> do
                result <- try $ loadAndCall libPath funcName (map (\(Just t) -> t) argTypesMb) retType
                case result of
                    Left  (e :: SomeException) -> return $ Left $ "Failed to load " ++ funcName ++ ": " ++ show e
                    Right f                     -> return $ Right f

loadAndCall :: String -> String -> [BernCType] -> BernCType -> IO ([Value] -> IO Value)
loadAndCall libPath funcName argTypes retType = do
    lib <- catch (loadLib libPath)
                 (\(e :: SomeException) -> do
                     hPutStrLn stderr $ "Cannot load library " ++ libPath ++ ": " ++ show e
                     error $ "Library load failed: " ++ show e)

    funPtrRaw <- catch (getSymbol lib funcName)
                       (\(e :: SomeException) -> do
                           hPutStrLn stderr $ "Symbol '" ++ funcName ++ "' not found in " ++ libPath ++ ": " ++ show e
                           error $ "Symbol lookup failed: " ++ show e)

    return $ \args -> do
        result <- try $ callCFunctionUniversal funPtrRaw argTypes retType args
        case result of
            Left  (e :: SomeException) -> do
                hPutStrLn stderr $ "FFI call failed: " ++ show e
                return Undefined
            Right val -> return val

-- A compact debug rendering of a Value, used in FFI diagnostics. Not the
-- user-facing pretty printer (that lives in Eval.prettyValue).
-- Uma renderização de debug compacta de um Value, usada nos diagnósticos do FFI.
-- Não é o pretty printer voltado ao usuário (esse mora em Eval.prettyValue).
showValue :: Value -> String
showValue (Integer n)   = "Integer(" ++ show n ++ ")"
showValue (Double d)    = "Double(" ++ show d ++ ")"
showValue (Boolean b)   = "Boolean(" ++ show b ++ ")"
showValue (Character c) = "Char('" ++ [c] ++ "')"
showValue v
    | Just s <- valueToString v = "String(\"" ++ s ++ "\")"
showValue (List _ len)  = "List[" ++ show len ++ "]"
showValue (Set _ len)   = "Set{" ++ show len ++ "}"
showValue _             = "<?>"
