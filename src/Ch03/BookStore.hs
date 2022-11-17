module Ch03.BookStore(
  Customer(..)
) where

type CustomerID = Int
type Address = [String]

data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)

-- data Customer = Customer Int String [String]
--                 deriving (Show)

-- customerID :: Customer -> Int
-- customerID (Customer id _ _) = id

-- customerName :: Customer -> String
-- customerName (Customer _ name _) = name

-- customerAddress :: Customer -> [String]
-- customerAddress (Customer _ _ address) = address

customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct", "Milpitas, CA 95134", "USA"]
          
customer2 = Customer {
              customerID = 271828
            , customerAddress = ["1048576 Disk Drive",
                                 "Milpitas, CA 95134",
                                 "USA"]
            , customerName = "Jane Q. Citizen"
            }