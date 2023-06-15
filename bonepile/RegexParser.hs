-- module HRayLib3d.Utils.RegexParser (
--     selectEntityObjectsRegex
--     , selectEntityContentsRegex
--     , entitiesFileToObjects
--     , objectsToContents
--     )where 

--     -- This will match a raw .Entities file into EntityData with Regex
--     -- This is also used to parse .Entities into .json or .xml, it replaces the
--     -- megaparsec parser as it was breaking on whitespace this should be more
--     -- durable a parser
 
--     import Text.Regex.TDFA ()
--     import Text.Regex.TDFA.Text ()

--     class MatchRegex a where
--         match :: a -> String 

--     -- Matches all Objects ({..})
--     selectEntityObjectsRegex  = "(?=\\{)([\\s\\S]*?)(?<=\\})"
--     selectEntityContentsRegex = "(?<=\")[^ \n\n].*?(?=\")"
--     -- (?=\{)([\s\S]*?)(?<=\}) 
--     -- {([\s\S]*?)}

--     entitiesFileToObjects :: String -> String
--     entitiesFileToObjects input  = input =~ selectEntityObjectsRegex

--     objectsToContents :: [String] -> [String]
--     objectsToContents matchCases = matchCases =~ selectEntityContentsRegex
