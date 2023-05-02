{-# LANGUAGE FlexibleInstances #-}

module HRayLib3d.ScriptEngine.Instances where 
    
    import HRayLib3d.ScriptEngine.HRubyHS
    import HRayLib3d.ScriptEngine.Internal
    
    instance ToRuby Int where
        toRuby i  = RINT i

    instance ToRuby Float where
        toRuby fl = RFLOAT32 fl

    instance ToRuby Double where
        toRuby d  = RDOUBLE d

    instance ToRuby Char   where
        toRuby c  = RCHAR c

    instance ToRuby String where
        toRuby s  = RSTR  s

    instance ToRuby [Int] where
        toRuby s  = RLIST $ Prelude.map toRuby s    

    instance ToRuby [Float] where
        toRuby s  = RLIST $ Prelude.map toRuby s  

    instance ToRuby [Double] where
        toRuby s  = RLIST $ Prelude.map toRuby s   

    instance ToRuby [String] where
        toRuby s  = RLIST $ Prelude.map toRuby s    

    instance ToRuby [(String, Int)] where
        toRuby s  = RDICT $ Prelude.map (\x -> (fst x, toRuby $ snd x)) s
    
    instance ToRuby [(String, Float)] where
        toRuby s  = RDICT $ Prelude.map (\x -> (fst x, toRuby $ snd x)) s

    instance ToRuby [(String, Double)] where
        toRuby s  = RDICT $ Prelude.map (\x -> (fst x, toRuby $ snd x)) s

    instance ToRuby [(String, String)] where
        toRuby s  = RDICT $ Prelude.map (\x -> (fst x, toRuby $ snd x)) s

    instance FromRuby Int where
        fromRuby i  = rint i

    instance FromRuby Float where
        fromRuby fl = rfloat fl

    instance FromRuby Double where
        fromRuby d  = rdouble d

    instance FromRuby Char   where
        fromRuby c  = rchar c

    instance FromRuby String where
        fromRuby s  = rstr  s

    instance FromRuby [Int] where
        fromRuby s  = Prelude.map (fromRuby) $ rlist s    

    instance FromRuby [Float] where
        fromRuby s  = Prelude.map (fromRuby) $ rlist s    

    instance FromRuby [Double] where
        fromRuby s  = Prelude.map (fromRuby) $ rlist s    

    instance FromRuby [String] where
        fromRuby s  = Prelude.map (fromRuby) $ rlist s     

    instance FromRuby [(String, Int)] where
        fromRuby s  = Prelude.map (\x -> (fst x, fromRuby $ snd x)) $ rdict s
    
    instance FromRuby [(String, Float)] where
        fromRuby s  = Prelude.map (\x -> (fst x, fromRuby $ snd x)) $ rdict s

    instance FromRuby [(String, Double)] where
        fromRuby s  = Prelude.map (\x -> (fst x, fromRuby $ snd x)) $ rdict s

    instance FromRuby [(String, String)] where
        fromRuby s  = Prelude.map (\x -> (fst x, fromRuby $ snd x)) $ rdict s
