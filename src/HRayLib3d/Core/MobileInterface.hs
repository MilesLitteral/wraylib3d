module HRayLib3d.Core.MobileInterface where

    import qualified Data.Text as T
    
    data Show a => DEnum   a    = DEnum     { enums          :: [a] } deriving (Eq, Show)
    data DConst = DConst { constType :: String, constValues :: [String]} deriving (Eq, Show)
    data Show a => DFlags  a    = Flags     { flags          :: [a] } deriving (Eq, Show)
    data Show a => DRecord a    = Record    { recordContents :: [a] } deriving (Eq, Show)
    data Show a => DInterface a = Interface { iValues        :: [a], iComments :: String, iConstants :: [DConst]} deriving (Eq, Show)
   
    data DClass =
        DClass {
            option1 :: Int,
            option2 :: Int,
            option3 :: Int
        } deriving (Eq, Show)

    -- This is a one way translation, it's not necessary to read *from* DJ types
    -- as instances of these types will be auto-generated to make sure WRayLib3d 
    -- (HRayLib3d) compatability is maintained on mobile, put succintly: 
    -- where hs_init and hs_exit can be invoked by a C++ interface which Java(Android) 
    -- or ObjC(iOS) can also use. Not so much for Haskell to talk to these platforms
    -- per-se. Users are also not expected to invoke these DJ types, they will instead
    -- invoke functions against Haskell types which handle putting Djinni in the 
    -- bottle and the C++ interfaces bridge the engine to the platform.

    -- Also keep in mind .djinni(s) can reference and import other .djinni files
    -- thus each [Djinni] will create one .djinni file on export, keep this in mind
    -- for how to most efficiently export these interfaces.
    class ToMobile a where 
        makeEnum       :: a -> DEnum   a
        makeFlag       :: a -> DFlags  a
        makeRecord     :: a -> DRecord a
        makeRecordGist :: a -> DRecord a
        makeInterface  :: a -> DInterface a

    -- Multi-line comments can be added here. This comment will be propagated
    -- to each generated definition.
    -- my_enum = enum {
    --     option1;
    --     option2;
    --     option3;
    -- }
    djinniMakeEnum :: ToMobile a => a -> DEnum a
    djinniMakeEnum = makeEnum

    -- my_flags = flags {
    --     flag1;
    --     flag2;
    --     flag3;
    --     no_flags  = none;
    --     all_flags = all;
    -- }
    djinniMakeFlags :: ToMobile a => a -> DFlags a
    djinniMakeFlags = makeFlag 

    -- djinniMakeRecord makes a compiled String which looks like this:
    -- my_record = record {
    --     id:    i32;
    --     info:  string;
    --     store: set<string>;
    --     hash:  map<string, i32>;
    --     values: list<another_record>;

    --     # Comments can also be put here

    --     # Constants can be included
    --     const string_const: string = \"Constants can be put here\";
    --     const min_value: another_record = {
    --         key1 = 0,
    --         key2 = ""
    --     };
    -- }
    djinniMakeRecord :: ToMobile a => a -> DRecord a
    djinniMakeRecord     = makeRecord

    -- djinniMakeRecordGist makes a compiled String which looks like this:
    -- another_record = record {
    --     key1: i32;
    --     key2: string;
    -- } deriving (eq, ord)
    djinniMakeRecordGist :: ToMobile a => a -> DRecord a
    djinniMakeRecordGist = makeRecordGist

    -- djinniMakeCPPInterface makes a compiled String which looks like this:
    -- This interface will be implemented in C++ and can be called from any applicable language (C++, ObjC, Java).
    -- my_cpp_interface = interface +c {
    --     method_returning_nothing(value: i32);
    --     method_returning_some_type(key: string): another_record;
    --     static get_version(): i32;

    --     # Interfaces can also have constants
    --     const version: i32 = 1;
    -- }
    djinniMakeCPPInterface :: ToMobile a => a -> DInterface a
    djinniMakeCPPInterface = makeInterface

    -- djinniMakeOJInterface makes a compiled String which looks like this:
    -- This interface will be implemented in Java and ObjC and can be called from C++.
    -- my_client_interface = interface +j +o {
    --     log_string(str: string): bool;
    -- }
    djinniMakeOJInterface  :: ToMobile a => a -> DInterface a
    djinniMakeOJInterface  = makeInterface