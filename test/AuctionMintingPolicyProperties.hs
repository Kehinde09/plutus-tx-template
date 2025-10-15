import Test.QuickCheck
import AuctionMintingPolicy

-- Property: The minting policy should only allow minting exactly one token
property_oneTokenMinted :: PubKeyHash -> Property
property_oneTokenMinted pkh =
    let redeemer = ()
        context  = mockMintingScriptContext pkh
    in auctionTypedMintingPolicy pkh redeemer context ==> 
        mintedExactlyOneToken context

-- Entry point to run the QuickCheck property
main :: IO ()
main = quickCheck property_oneTokenMinted
