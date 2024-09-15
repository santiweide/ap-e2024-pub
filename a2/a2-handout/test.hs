import Control.Monad.State

-- Define a simple Val type for keys and values
data Val = CstInt Int | CstBool Bool deriving (Eq, Show)

-- Define the KVStore type as a list of key-value pairs
type KVStore = [(Val, Val)]

kvPut :: Val -> Val -> State KVStore ()
kvPut key value = do
    -- Retrieve the current store
    store <- get
    -- Update the store: add the new key-value pair, removing any existing pair with the same key
    let newStore = (key, value) : filter (\(k, _) -> k /= key) store
    -- Put the updated store back into the state
    put newStore

kvGet :: Val -> State KVStore (Maybe Val)
kvGet key = do
    -- Retrieve the current store
    store <- get
    -- Look up the key in the store
    return $ lookup key store

