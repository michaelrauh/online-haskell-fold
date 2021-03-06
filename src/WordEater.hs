module WordEater (Answer (Answer), wordToUniqueAnswer) where
import Config ( Config(Config) )
import Data.Set as Set ( empty, toList, Set, fromList )
import Data.Map as Map ( findWithDefault )
import Control.Monad ( guard )
import Data.List ( nub )
import Data.Text ( Text )

data Answer = Answer Text Text Text Text deriving (Show)

instance Eq Answer where
    (Answer a b c d) == (Answer a' b' c' d') =
        a == a' &&
        d == d' &&
        (b == b' && c == c') ||
        (b == c' && c == b')

wordToAnswer :: Config -> Text -> [Answer]
wordToAnswer (Config next prev _ _) d = do
    c <- Set.toList $ Map.findWithDefault Set.empty d prev
    a <- Set.toList $ Map.findWithDefault Set.empty c prev
    b <- Set.toList $ Map.findWithDefault Set.empty a next
    d' <- Set.toList $ Map.findWithDefault Set.empty b next
    guard (d == d' && b /= c)
    return (Answer a b c d)

wordToUniqueAnswer :: Config -> Text -> [Answer]
wordToUniqueAnswer config word = nub $ wordToAnswer config word