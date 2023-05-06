
{-# LANGUAGE OverloadedStrings #-}
module HRayLib3d.Network.Cloud () where

    import qualified Data.ByteString as BS

    data Supabase
    data Firebase
    -- Translate this react into SupaBase Connection Code
    -- Make some FireBase Conns later
    -- import { useEffect, useState } from "react";
    -- import { createClient } from "@supabase/supabase-js";
  
    -- const supabase = createClient("https://<project>.supabase.co", "<your-anon-key>");
  
    -- function App() {
    --   const [countries, setCountries] = useState([]);
  
    --   useEffect(() => {
    --     getCountries();
    --   }, []);
  
    --   async function getCountries() {
    --     const { data } = await supabase.from("countries").select();
    --     setCountries(data);
    --   }
  
    --   return (
    --     <ul>
    --       {countries.map((country) => (
    --         <li key={country.name}>{country.name}</li>
    --       ))}
    --     </ul>
    --   );
    -- }
  
    -- export default App;

--Firebase
--     Example
-- // Initialize default app
-- // Retrieve your own options values by adding a web app on
-- // https://console.firebase.google.com
-- firebase.initializeApp({
--   apiKey: "AIza....",                             // Auth / General Use
--   appId: "1:27992087142:web:ce....",              // General Use
--   projectId: "my-firebase-project",               // General Use
--   authDomain: "YOUR_APP.firebaseapp.com",         // Auth with popup/redirect
--   databaseURL: "https://YOUR_APP.firebaseio.com", // Realtime Database
--   storageBucket: "YOUR_APP.appspot.com",          // Storage
--   messagingSenderId: "123456789",                 // Cloud Messaging
--   measurementId: "G-12345"                        // Analytics
-- });
-- Example


-- // Initialize another app
-- var otherApp = firebase.initializeApp({
--   apiKey: "AIza....",
--   appId: "1:27992087142:web:ce....",
--   projectId: "my-firebase-project",
--   databaseURL: "https://<OTHER_DATABASE_NAME>.firebaseio.com",
--   storageBucket: "<OTHER_STORAGE_BUCKET>.appspot.com"
-- }, "nameOfOtherApp");
--  import { initializeApp } from "firebase/app";
--  import { getFirestore } from "@firebase/firestore"

-- const firebaseConfig = {
--   apiKey: process.env.REACT_APP_apiKey,
--   authDomain: process.env.REACT_APP_authDomain,
--   projectId: process.env.REACT_APP_projectId,
--   storageBucket: process.env.REACT_APP_storageBucket,
--   messagingSenderId: process.env.REACT_APP_messagingSenderId,
--   appId: process.env.REACT_APP_appId,
--   measurementId: process.env.REACT_APP_measurementId
-- };

-- const app = initializeApp(firebaseConfig);
-- export const firestore = getFirestore(app)